#!/usr/bin/env python
#
# fortran_namelist
#
# ------------------------------------------------------------------------
# Copyleft 2009, Jesus Fernandez <jesusff IN THE DOMAIN gmail DOT com>
#
# Santader Meteorology Group, Universidad de Cantabria, Spain
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
# ------------------------------------------------------------------------

import sys

def listify(item):
  if type(item) == type([]):
    return item
  else:
    return [item,]

def flat_list(l):
  rval = []
  for item in l:
    if type(item) == type([]):
      rval.extend(item)
    else:
      rval.append(item)
  return rval

def math_expansion(expr):
  try:
    exec("rval=%s" % expr)
    return "%s" % rval
  except:
    return expr

def asterisk_expansion(expr):
  if '*' in expr:
    [mult, value] = expr.split('*')
    rval = int(mult)*[value,]
    return map(math_expansion, rval)
  else:
    return expr

def coerce_value_list(vlist, math_exp=True):
  if math_exp:
    vlist = map(math_expansion, vlist)
  else:
    vlist = map(asterisk_expansion, vlist)
    vlist = flat_list(vlist)
  try:
    values=[int(element) for element in vlist]
  except ValueError:
    try:
      values=[float(element) for element in vlist]
    except ValueError:
      values=[value.strip() for value in vlist]
  return values

class FortranNamelistRecord:
  def __init__(self, name):
    self.name = name
    self.data = {}
    self.ordered_keys = []
  def setValue(self, variable, value):
    self.data[variable] = listify(value)
    if not variable in self.ordered_keys:
      self.ordered_keys.append(variable)
  def delVariable(self, variable):
    if variable in self.ordered_keys:
      del self.data[variable]
      self.ordered_keys.remove(variable)
  def appendValue(self, variable, value):
    if not variable in self.ordered_keys:
      self.ordered_keys.append(variable)
      self.data[variable]=[]
    self.data[variable].extend(listify(value))
  def hasVariable(self, var):
    return var in self.ordered_keys
  def printRecord(self, sorted=False):
    rval = "&%s\n" % self.name
    keys = self.ordered_keys
    if sorted:
      keys.sort()
    for key in keys:
      thisvar = "  %-24s =" % key
      for item in self.data[key]:
        if type(item) == type("hello"):
          if not item.startswith("'") and not item[:2].upper() in [".T", ".F"]:
            item = "'%s'" % item
        if len(thisvar) < 75:
          thisvar += " %s," % item
        else:
          rval += "%s\n" % thisvar
          thisvar = 28*" "+" %s," % item
      rval += "%s\n" % thisvar
    rval += "/\n"
    return rval
  def __str__(self):
    self.printRecord(sorted=False)
  def __getitem__(self, item):
    return self.data[item]

class FortranNamelist:
  def __init__(self, namelist_file, math_exp=True):
    """read contents of namelist file and return dictionary containing all options
    
    Created 20/01/08 by Thom Chubb.
    Modified 20/01/08 by Thom Chubb and Valerio Bisignesi
    Modified 20/05/09 by Jesus Fernandez 
      Implemented as a class
      Added support for multi-line variables
    """
    self.namelist_file = namelist_file
    self.math_exp = math_exp
    fid=open(namelist_file, 'r')
    data = fid.readlines()
    fid.close()
    self.record_dict={}
    self.ordered_records=[]
    is_comment = True
    for line in data:
      if line.strip().startswith('!'): continue
      if '&' in line:
        # Then this line is a namelist title
        is_comment=False
        current_label = line.strip().lstrip('&')
        current_label = current_label.lower()
        self.record_dict[current_label] = FortranNamelistRecord(current_label)
        if not current_label in self.ordered_records:
          self.ordered_records.append(current_label)
      elif line.strip() == "/":
        # Then lines following this are comments until the next '&'
        is_comment=True
      elif '=' in line:
        # Then this line contains variable information to be stored
        if not is_comment:
          variable,values = line.split('=')
          variable = variable.lower()
          values = values.strip().rstrip(',')
          if values.startswith("'") and values.endswith("'") and not "'" in values[1:-1]:
            # This is a single string with comma-separated values. Do not interpret it as 
            # as comma-separated strings.
            values = values[1:-1]
          else:
            values = coerce_value_list(values.split(','), self.math_exp)
          self.record_dict[current_label].setValue(variable.strip(), values)
      else:
        if not is_comment:
          # This line contains variable information to be added to the last variable read
          values = line.strip().rstrip(',')
          values = coerce_value_list(values.split(','), self.math_exp)
          self.record_dict[current_label].appendValue(variable.strip(), values)
  def __getitem__(self, item):
    return self.record_dict[item]
  def printNamelist(self, record=None, sorted=False):
    rval = ""
    if record:
      rval += "%s\n" % self.record_dict[record].printRecord(sorted)
    else:
      records = self.ordered_records
      if sorted:
        records.sort() 
      for rec in records:
        rval += "%s\n" % self.record_dict[rec].printRecord(sorted)
    return rval
  def setValue(self, variable, value, record=""):
    if record:
      if not self.record_dict.has_key(record):
        self.record_dict[record] = FortranNamelistRecord(record)
        if not record in self.ordered_records:
          self.ordered_records.append(record)
      self[record].setValue(variable, value)
    else:
      gotit=False
      for rec in self.ordered_records:
        if self[rec].hasVariable(variable):
          self[rec].setValue(variable, value)
          gotit=True
          break
      if not gotit:
        raise KeyError, "The variable '%s' was not found and no record was specified!" % variable
  def getValue(self, variable, record=""):
    if record:
      return self[record][variable]
    else:
      gotit=False
      for rec in self.ordered_records:
        if self[rec].hasVariable(variable):
          return self[rec][variable]
          gotit=True
          break
      if not gotit:
        raise KeyError, "The variable '%s' was not found." % variable
  def hasVariable(self, variable, record=""):
    if record:
      return self[record].hasVariable(variable)
    else:
      rval = False
      for rec in self.ordered_records:
        if self[rec].hasVariable(variable):
          rval = True
          break
      return rval
  def variableList(self):
    rval = []
    for rec in self.ordered_records:
      rval.extend(self[rec].ordered_keys)
    return rval
  def appendValue(self, variable, value, record=""):
    if record:
      self[record].appendValue(variable, value)
    else:
      gotit=False
      for rec in self.ordered_records:
        if self[rec].hasVariable(variable):
          self[rec].appendValue(variable, value)
          gotit = True
          break
      if not gotit:
        raise KeyError, "The variable '%s' was not found and no record was specified!" % variable
  def delVariable(self, variable, record=""):
    if record:
      self[record].delVariable(variable)
    else:
      for rec in self.ordered_records:
        if self[rec].hasVariable(variable):
          self[rec].delVariable(variable)
          break
  def overWriteNamelist(self):
    fid = open(self.namelist_file, 'w')
    fid.write(self.printNamelist())
    fid.close()

class WrfNamelist(FortranNamelist):
  # Get an updated list for your WRF version with:
  # grep '^rconfig' Registry |awk '$5=="max_domains"{printf "  %s,\n", tolower($8)}' | sort | uniq
  # "feedback" wrongly appears as max_dom var (how can this be posible?)
  # and was removed by hand. "frames_per_outfile" and "history_interval"
  # were missing and added by hand.
  # Last updated from WRF v3.3.1 Registry.EM (J. Fernandez, 20120203)
  MAX_DOM_VARIABLES = [
    "allowed", "bdyfrq", "bl_pbl_physics", "bldt", "c_k", "c_s", "cen_lat",
    "cen_lon", "chem_adv_opt", "chem_opt", "coriolis2d", "cu_diag",
    "cu_physics", "cudt", "cycle_x", "cycle_y", "dampcoef", "decoupled",
    "dfi_stage", "diff_6th_factor", "diff_6th_opt", "do_coriolis",
    "do_curvature", "do_gradp", "dt", "dx", "dy", "e_sn", "e_vert", "e_we",
    "emdiv", "end_day", "end_hour", "end_minute", "end_month", "end_second",
    "end_year", "epssm", "fdda_end", "fdda_start", "fgdt", "fgdtzero",
    "fine_input_stream", "gmt", "grav_settling", "grid_fdda", "gsmdt",
    "h_mom_adv_order", "h_sca_adv_order", "i_parent_start", "id",
    "input_from_file", "input_from_hires", "isice", "islake", "isoilwater",
    "isurban", "iswater", "j_parent_start", "julday", "julyr", "khdif",
    "kvdif", "map_proj", "max_step_increase_pct", "max_time_step",
    "min_time_step", "mix_full_field", "mix_isotropic", "mix_upper_bound",
    "moad_cen_lat", "moad_grid_ratio", "moad_time_step_ratio",
    "moist_adv_dfi_opt", "moist_adv_opt", "mp_physics", "mp_physics_dfi",
    "naer", "nested", "non_hydrostatic", "obs_coef_mois", "obs_coef_pstr",
    "obs_coef_temp", "obs_coef_wind", "obs_ionf", "obs_no_pbl_nudge_q",
    "obs_no_pbl_nudge_t", "obs_no_pbl_nudge_uv", "obs_nudge_mois",
    "obs_nudge_opt", "obs_nudge_pstr", "obs_nudge_temp", "obs_nudge_wind",
    "obs_prt_freq", "obs_rinxy", "obs_twindo", "open_xe", "open_xs", "open_ye",
    "open_ys", "parent_grid_ratio", "parent_id", "parent_time_step_ratio",
    "periodic_x", "periodic_y", "pert_coriolis", "polar", "pole_lat",
    "pole_lon", "prec_acc_dt", "progn", "pxlsm_smois_init", "pxlsm_soil_nudge",
    "ra_lw_physics", "ra_sw_physics", "radt", "s_sn", "s_vert", "s_we",
    "scalar_adv_opt", "sf_sfclay_physics", "sf_surface_physics",
    "sf_urban_physics", "shcu_physics", "slope_rad", "smdiv", "specified",
    "stand_lon", "start_day", "start_hour", "start_minute", "start_month",
    "start_second", "start_year", "starting_time_step", "stencil_half_width",
    "swap_x", "swap_y", "symmetric_xe", "symmetric_xs", "symmetric_ye",
    "symmetric_ys", "target_cfl", "target_hcfl", "time_step_sound",
    "tke_adv_opt", "tke_drag_coefficient", "tke_heat_flux", "tke_upper_bound",
    "top_lid", "top_radiation", "topo_shading", "tracer_adv_opt", "tracer_opt",
    "true_lat1", "true_lat2", "v_mom_adv_order", "v_sca_adv_order", "zdamp",
    "ztop", "frames_per_outfile", "history_interval" 
  ]
  def __init__(self, namelist_file, math_exp=True):
    FortranNamelist.__init__(self, namelist_file, math_exp)
  def setMaxDomValue(self, variable, value, record=""):
    mxd = self['domains']['max_dom'][0]
    if record:
      self[record].setValue(variable, coerce_value_list(mxd*[value,], self.math_exp))
    else:
      gotit=False
      for rec in self.ordered_records:
        if self[rec].hasVariable(variable):
          self[rec].setValue(variable, coerce_value_list(mxd*[value,], self.math_exp))
          gotit=True
          break
      if not gotit:
        raise KeyError, "The variable was not found and no record was specified!"
  def checkMaxDomPatterns(self, var):
    prefixes = ["start_", "end_"]
    suffixes = ["_inname", "_outname", "_interval"]
    rval = False
    for prefix in prefixes:
      if var.startswith(prefix):
        rval = True
        break
    for suffix in suffixes:
      if var.endswith(suffix):
        rval = True
        break
    return rval
  def trimMaxDom(self, ncols=None):
    mxd = self['domains']['max_dom'][0]
    if ncols:
       mxd = ncols
    for var in self.variableList():
      if ncols or var in self.MAX_DOM_VARIABLES:# or self.checkMaxDomPatterns(var):
        self.setValue(var, self.getValue(var)[:mxd])
  def printWrfWarning(self, message):
    sys.stderr.write("WRF Check Warning: %s\n" % message)
  def wrfCheck(self):
    """
    Check for some recomendations/mandatory WRF specific issues in the namelist.
    """
    tsratio = self.getValue('time_step')[0] * 1000 / self.getValue('dx')[0]
    if   tsratio > 6: self.printWrfWarning("Time step is larger than 6 times dx (%f)" % tsratio)
    elif tsratio < 5: self.printWrfWarning("Time step is shorter than 5 times dx (%f)" % tsratio)
    raratio = self.getValue('radt')[0] * 1000 / self.getValue('dx')[0]
    if   raratio > 1.1: self.printWrfWarning("radt is larger than dx (%f)" % raratio)
    if   raratio < 0.9: self.printWrfWarning("radt is shorter than dx (%f)" % raratio)
    #
    # SST update
    #
    if self.hasVariable('sst_update', 'physics'):
      if self.getValue('sst_update','physics')[0] == 1 and not self.hasVariable('auxinput4_inname', 'time_control'):
         self.printWrfWarning("sst_update enabled but auxinput4_inname not defined! Fixing... (check interval!)")
         self['time_control'].setValue('auxinput4_inname', "wrflowinp_d<domain>")
         self['time_control'].setValue('io_form_auxinput4', 2)
         self['time_control'].setValue('auxinput4_end_h', 0)
         self.setMaxDomValue('auxinput4_interval', 360, 'time_control')
    #
    # CAM radiation
    #
    if self.getValue('ra_lw_physics')[0] == 3 and not self.hasVariable('paerlev', 'physics'):
      self.printWrfWarning('CAM radiation selected but paerlev/levsiz/cam_abs_dim1/cam_abs_dim2 was not set. Fixing...')
      self['physics'].setValue('paerlev', 29)
      self['physics'].setValue('levsiz', 59)
      self['physics'].setValue('cam_abs_dim1', 4)
      self['physics'].setValue('cam_abs_dim2', self.getValue('e_vert')[0])
    if self.getValue('ra_lw_physics')[0] == 3 and self.getValue('cam_abs_dim2')[0]!=self.getValue('e_vert')[0]:
      self.printWrfWarning('cam_abs_dim2 not set to e_vert. Fixing...')
      self['physics'].setValue('cam_abs_dim2', self.getValue('e_vert')[0])
    #
    # PBL issues
    #
    if self.getValue('bl_pbl_physics')[0] == 1 and self.getValue('sf_sfclay_physics')[0] != 1:
      self.printWrfWarning('YSU PBL selected but the surface layer selected is not 1. Fixing...')
      self['physics'].setValue('sf_sfclay_physics', 1)
    #
    # LSM issues
    #
    if self.getValue('sf_surface_physics')[0] == 7 and self.getValue('num_soil_layers')[0] != 2:
      self.printWrfWarning('Pleim Xiu LSM selected but the soil levels are not 2. Fixing...')
      self['physics'].setValue('num_soil_layers', 2)
    if self.getValue('sf_surface_physics')[0] == 3 and self.getValue('num_soil_layers')[0] != 6:
      self.printWrfWarning('RUC LSM selected but the soil levels are not 6. Fixing...')
      self['physics'].setValue('num_soil_layers', 6)
    if self.getValue('sf_surface_physics')[0] == 2 and self.getValue('num_soil_layers')[0] != 4:
      self.printWrfWarning('Noah LSM selected but the soil levels are not 4. Fixing...')
      self['physics'].setValue('num_soil_layers', 4)
    if self.getValue('sf_surface_physics')[0] == 1 and self.getValue('num_soil_layers')[0] != 5:
      self.printWrfWarning('Simple soil selected but the soil levels are not 5. Fixing...')
      self['physics'].setValue('num_soil_layers', 5)
  def extendMaxDomVariables(self):
    #
    # Provide enough columns in max_dom variables. Risky stuff..
    #
    for var in self.MAX_DOM_VARIABLES:
      if self.hasVariable(var):
        theval = self.getValue(var)
        if len(theval) < self.getValue("max_dom")[0]:
          self.printWrfWarning('Variable %s = %s requires as many entries as domains.' % (var, theval))
          self.setValue(var, theval + (self.getValue("max_dom")[0]-len(theval))*[theval[-1],])
          self.printWrfWarning('Filling with last domain entry!! -> %s = %s' % (var, self.getValue(var)))

