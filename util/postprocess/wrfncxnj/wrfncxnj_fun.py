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


def compute_T2(varobj, onc, inc, incprev, incnext):
  # Works for any variable defined at 2m with no other transformation.
  incvar = inc.variables[varobj.varname]
  copyval = np.reshape(incvar[:],incvar.shape[:1]+(1,)+incvar.shape[1:])
  oncvar = get_oncvar(varobj, incvar, onc, screenvar_at_2m=True)
  return oncvar, copyval

compute_Q2 = compute_T2
compute_T2MAX = compute_T2
compute_T2MIN = compute_T2

def compute_RAIN(varobj, onc, inc, incprev, incnext):
  if inc.variables.has_key("RAINTOT"): # The file was processed by p_interp
    incvar = inc.variables["RAINTOT"]
    pr = incvar[:]
  else:  # We should add convective and large-scale rainfall
    incvar = inc.variables["RAINNC"]
    pr = incvar[:] + inc.variables["RAINC"][:]
  if not incprev:
    lastpr = pr[0]
  elif inc.variables.has_key("RAINTOT"):
    lastpr = incprev.variables["RAINTOT"][-1]
  else:
    lastpr = incprev.variables["RAINNC"][-1] + incprev.variables["RAINC"][-1]
  lastpr.shape = (1,) + lastpr.shape
  copyval = pr - np.concatenate([lastpr,pr[:-1]])
  copyval = np.where(copyval<0., 0, copyval)
  oncvar = get_oncvar(varobj, incvar, onc)
  return oncvar, copyval
