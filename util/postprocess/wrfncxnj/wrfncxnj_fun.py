import Numeric as np
from wrfncxnj_base import get_oncvar

def compute_temperature(p, t):
  # Some required physical constants:
  R=287.04
  g=9.81
  cp           = 7.*R/2.
  rcp          = R/cp
  p1000mb      = 100000.
  return (t+300.)*(p/p1000mb)**rcp

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

def compute_RAIN(varobj, onc, wnfiles, wntimes):
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
