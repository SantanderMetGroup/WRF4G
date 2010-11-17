import Numeric as np
from wrfncxnj_base import get_oncvar, Constants

def compute_temperature(p, t):
  # Some required physical constants:
  return (t+300.)*(p/Constants.p1000mb)**Constants.rcp

def compute_RH2(varobj, onc, wnfiles, wntimes):
  """Computation of relative humidity at 2 m 
  t2: 2m temperature [K]
  q2: 2m mixing ratio [kg kg-1]
  psfc: surface prssure (assumed the same at 2m) [Pa]
  e: vapor pressure in air [Pa]
  es: saturation vapor pressure [Pa]

  Some required physical constants: epsilon_gamma, tkelvin, es_[A/B]tetens_[ice/vapor]
  """
  t2 =  wnfiles.current.variables["T2"][:]
  q2 =  wnfiles.current.variables["Q2"][:]
  psfc =  wnfiles.current.variables["PSFC"]
  e = q2*psfc[:]/(100.*(Constants.epsilon_gamma+q2))
  es = np.where(
    t2-Constants.tkelvin <= 0., 
    Constants.es_base_tetens*10.**(((t2-Constants.tkelvin)*Constants.es_Atetens_ice)/
      ((t2-Constants.tkelvin)+Constants.es_Btetens_ice)), 
    Constants.es_base_tetens*10.**(((t2-Constants.tkelvin)*Constants.es_Atetens_vapor)/
      ((t2-Constants.tkelvin)+Constants.es_Btetens_vapor)))
  copyval = e/es
  copyval.shape=psfc.shape[:1]+(1,)+psfc.shape[1:]
  oncvar = get_oncvar(varobj, psfc, onc, screenvar_at_2m=True)
#  rh2 = e/es
#  copyval = max(min(rh2, 1.), 0.)
  return oncvar, copyval

def compute_TDPS(varobj, onc, wnfiles, wntimes):
  """Computation of relative humidity at 2 m 
  t2: 2m temperature [K]
  q2: 2m mixing ratio [kg kg-1]
  psfc: surface prssure (assumed the same at 2m) [Pa]
  e: vapor pressure in air [hPa]
  es: saturation vapor pressure [hPa]

  Some required physical constants: tkelvin, es_base_bolton, es_[A/B]bolton, RdRv
  """
  t2 =  wnfiles.current.variables["T2"][:]
  q2 =  wnfiles.current.variables["Q2"][:]
  psfc =  wnfiles.current.variables["PSFC"]
  es = 10.* Constants.es_base_bolton * np.exp(Constants.es_Abolton*(t2-Constants.tkelvin)
    /(t2-Constants.tkelvin+Constants.es_Bbolton))
  rh = 0.01*psfc[:]/es*(q2/(Constants.RdRv+q2))
  rh = np.where(rh > 1., 1., rh)
  rh = np.where(rh < 0., 0., rh)

  e = rh*Constants.es_base_bolton*np.exp(Constants.es_Abolton*(t2-Constants.tkelvin)
    /(t2-Constants.tkelvin+Constants.es_Bbolton))
  copyval = (116.9+237.3*np.log(e))/(16.78-np.log(e))+Constants.tkelvin
  copyval.shape=psfc.shape[:1]+(1,)+psfc.shape[1:]
  oncvar = get_oncvar(varobj, psfc, onc, screenvar_at_2m=True)
  return oncvar, copyval

def compute_TDPS_its90(varobj, onc, wnfiles, wntimes):
  """Computation of dew point temperature at 2 m
  q2: 2m mixing ratio [kg kg-1]
  psfc: surface prssure (assumed the same at 2m) [Pa]
  lnes: pure saturated vapor pressure [Pa]
  Following ITS-90 formulation

  Some required physical constants: epsilon_gamma, es_[A/B]tetens_vapor
  """
  t2 =  wnfiles.current.variables["T2"][:]
  psfc =  wnfiles.current.variables["PSFC"]

  g_es0 = -2.8365744e3
  g_es1 = -6.028076559e3
  g_es2 = 1.954263612e1
  g_es3 = -2.737830188e-2
  g_es4 = 1.6261698e-5
  g_es5 = 7.0229056e-10
  g_es6 = -1.8680009e-13
  g_es7 = 2.7150305

  k_es0 = -5.8666426e3
  k_es1 = 2.232870244e1
  k_es2 = 1.39387003e-2
  k_es3 = -3.4262402e-5
  k_es4 = 2.7040955e-8
  k_es5 = 6.7063522e-1

  c_tdv0 = 2.0798233e2
  c_tdv1 = -2.0156028e1
  c_tdv2 = 4.6778925e-1
  c_tdv3 = -9.2288067e-6
  d_tdv0 = 1.
  d_tdv1 = -1.3319669e-1
  d_tdv2 = 5.6577518e-3
  d_tdv3 = -7.5172865e-5

  c_tdi0 = 2.1257969e2
  c_tdi1 = -1.0264612e1
  c_tdi2 = 1.4354796e-1
  d_tdi0 = 1.
  d_tdi1 = -8.2871619e-2
  d_tdi2 = 2.3540411e-3
  d_tdi3 = -2.4363951e-5

  lnes = np.where(
    t2-Constants.tkelvin <= 0.,
    k_es0*t2**(-1)+ k_es1*t2**(0)+ k_es2*t2**(1)+ k_es3*t2**(2)+ k_es4*t2**(3)+ 
    k_es5*np.log(t2),
    g_es0*t2**(-2)+ g_es1*t2**(-1)+ g_es2*t2**(0)+ g_es3*t2**(1)+ g_es4*t2**(2)+    
    g_es5*t2**(3)+ g_es6*t2**(4)+ g_es7*np.log(t2))
  td2A = np.where(
    t2-Constants.tkelvin <= 0.,
    c_tdi0*lnes**0+ c_tdi1*lnes**1+ c_tdi2*lnes**2,
    c_tdv0*lnes**0+ c_tdv1*lnes**1+ c_tdv2*lnes**2+ c_tdv3*lnes**3)
  td2B = np.where(
    t2-Constants.tkelvin <= 0.,
    d_tdi0*lnes**0+ d_tdi1*lnes**1+ d_tdi2*lnes**2+ d_tdi3*lnes**3,
    d_tdv0*lnes**0+ d_tdv1*lnes**1+ d_tdv2*lnes**2+ d_tdv3*lnes**3)
 
  copyval = td2A/td2B
  copyval.shape=psfc.shape[:1]+(1,)+psfc.shape[1:]
  oncvar = get_oncvar(varobj, psfc, onc, screenvar_at_2m=True)
  return oncvar, copyval

def compute_UER(varobj, onc, wnfiles, wntimes):
  if wnfiles.current.variables.has_key("UU"):  # wind on p-levels from p_interp
    u = wnfiles.current.variables["UU"]
    v = wnfiles.current.variables["VV"]
  else:
    u = wnfiles.current.variables["U"]
    v = wnfiles.current.variables["V"]
  if not wnfiles.geo:
    print "I need the geo_em file to rotate winds!"
  else:
    sina = wnfiles.geo.variables["SINALPHA"][:]
    cosa = wnfiles.geo.variables["COSALPHA"][:]
  copyval = (
    u[:]*cosa[np.NewAxis,...] - v[:]*sina[np.NewAxis,...]
  )
  oncvar = get_oncvar(varobj, u, onc)
  return oncvar, copyval

def compute_VER(varobj, onc, wnfiles, wntimes):
  if wnfiles.current.variables.has_key("UU"):  # wind on p-levels from p_interp
    u = wnfiles.current.variables["UU"]
    v = wnfiles.current.variables["VV"]
  else:
    u = wnfiles.current.variables["U"]
    v = wnfiles.current.variables["V"]
  if not wnfiles.geo:
    print "I need the geo_em file to rotate winds!"
  else:
    sina = wnfiles.geo.variables["SINALPHA"][:]
    cosa = wnfiles.geo.variables["COSALPHA"][:]
  copyval = (
    u[:]*sina[np.NewAxis,...] + v[:]*cosa[np.NewAxis,...]
  )
  oncvar = get_oncvar(varobj, v, onc)
  return oncvar, copyval

def compute_SMOIS1(varobj, onc, wnfiles, wntimes):
  incvar = wnfiles.current.variables['SMOIS']
  copyval = incvar[:,0,:,:]*1000 # m3/m3 -> Kg/m3
  oncvar = get_oncvar(varobj, incvar, onc, out_is_2D_but_in_3D=True)
  return oncvar, copyval

def compute_RAINF(varobj, onc, wnfiles, wntimes):
  """Deaccumulates the precipitation field

  This function looks for RAINTOT if available, otherwise adds up RAINNC and
  RAINCV. It deaccumulates from the value on the previous output time step.
  A flux is computed dividing by the timestep in seconds.
  """
  if wnfiles.current.variables.has_key("RAINTOT"): # The file was processed by p_interp
    incvar = wnfiles.current.variables["RAINTOT"]
    pr = incvar[:]
  else:  # We should add convective and large-scale rainfall
    incvar = wnfiles.current.variables["RAINNC"]
    pr = incvar[:] + wnfiles.current.variables["RAINC"][:]
  if not wnfiles.prv:
    lastpr = pr[0]
  elif wnfiles.current.variables.has_key("RAINTOT"):
    lastpr = wnfiles.prv.variables["RAINTOT"][-1]
  else:
    lastpr = wnfiles.prv.variables["RAINNC"][-1] + wnfiles.prv.variables["RAINC"][-1]
  lastpr.shape = (1,) + lastpr.shape
  copyval = pr - np.concatenate([lastpr,pr[:-1]])
  copyval = np.where(copyval<0., 0, copyval)/float(wntimes.outstep_s)
  oncvar = get_oncvar(varobj, incvar, onc)
  return oncvar, copyval

def compute_RAIN(varobj, onc, wnfiles, wntimes):
  """Deaccumulates the precipitation field

  This function looks for RAINTOT if available, otherwise adds up RAINNC and
  RAINCV. It deaccumulates from the value on the previous output time step.
  A flux is computed dividing by the timestep in seconds.
  """
  if wnfiles.current.variables.has_key("RAINTOT"): # The file was processed by p_interp
    incvar = wnfiles.current.variables["RAINTOT"]
    pr = incvar[:]
  else:  # We should add convective and large-scale rainfall
    incvar = wnfiles.current.variables["RAINNC"]
    pr = incvar[:] + wnfiles.current.variables["RAINC"][:]
  if not wnfiles.prv:
    lastpr = pr[0]
  elif wnfiles.current.variables.has_key("RAINTOT"):
    lastpr = wnfiles.prv.variables["RAINTOT"][-1]
  else:
    lastpr = wnfiles.prv.variables["RAINNC"][-1] + wnfiles.prv.variables["RAINC"][-1]
  lastpr.shape = (1,) + lastpr.shape
  copyval = pr - np.concatenate([lastpr,pr[:-1]])
  copyval = np.where(copyval<0., 0, copyval)
  oncvar = get_oncvar(varobj, incvar, onc)
  return oncvar, copyval

def compute_RAINFORWARD(varobj, onc, wnfiles, wntimes):
  if wnfiles.current.variables.has_key("RAINTOT"): # The file was processed by p_interp
    incvar = wnfiles.current.variables["RAINTOT"]
    pr = incvar[:]
  else:  # We should add convective and large-scale rainfall
    incvar = wnfiles.current.variables["RAINNC"]
    pr = incvar[:] + wnfiles.current.variables["RAINC"][:]
  if not wnfiles.nxt:
    nextpr = pr[-1]
  elif wnfiles.current.variables.has_key("RAINTOT"):
    nextpr = wnfiles.nxt.variables["RAINTOT"][0]
  else:
    nextpr = wnfiles.nxt.variables["RAINNC"][0] + wnfiles.nxt.variables["RAINC"][0]
  nextpr.shape = (1,) + nextpr.shape
  copyval = np.concatenate([pr[1:], nextpr]) - pr
  copyval = np.where(copyval<0., 0, copyval)
  oncvar = get_oncvar(varobj, incvar, onc)
  return oncvar, copyval

def compute_ACRLS(varobj, onc, wnfiles, wntimes):
  incvar = wnfiles.current.variables["ACSWUPB"]
  rlt = incvar[:] - wnfiles.current.variables["ACSWDNB"][:]
  if not wnfiles.prv:
    lastrlt = rlt[0]
  else:
    lastrlt = wnfiles.prv.variables["ACSWUPB"][-1] - wnfiles.prv.variables["ACSWDNB"][-1]
  lastrlt.shape = (1,) + lastrlt.shape
  copyval = (np.concatenate([lastrlt, rlt[:-1]]) - rlt)/float(wntimes.outstep_s)
  oncvar = get_oncvar(varobj, incvar, onc)
  return oncvar, copyval

def compute_RST(varobj, onc, wnfiles, wntimes):
  incvar = wnfiles.current.variables["ACSWUPT"]
  rlt = incvar[:] - wnfiles.current.variables["ACSWDNT"][:]
  if not wnfiles.prv:
    lastrlt = rlt[0]
  else:
    lastrlt = wnfiles.prv.variables["ACSWUPT"][-1] - wnfiles.prv.variables["ACSWDNT"][-1]
  lastrlt.shape = (1,) + lastrlt.shape
  copyval = (np.concatenate([lastrlt, rlt[:-1]]) - rlt)/float(wntimes.outstep_s)
  oncvar = get_oncvar(varobj, incvar, onc)
  return oncvar, copyval

def compute_RLT(varobj, onc, wnfiles, wntimes):
  incvar = wnfiles.current.variables["OLR"]
  lwdnt = wnfiles.current.variables["ACLWDNT"]
  if not wnfiles.prv:
    lastlwdnt = lwdnt[0]
  else:
    lastlwdnt = wnfiles.prv.variables["ACLWDNT"][-1]
  lastlwdnt.shape = (1,) + lastlwdnt.shape
  copyval = incvar[:] - (np.concatenate([lastlwdnt, lwdnt[:-1]]) - lwdnt)/float(wntimes.outstep_s) # Deaccumulates ACLWDNT
  oncvar = get_oncvar(varobj, incvar, onc)
  return oncvar, copyval
