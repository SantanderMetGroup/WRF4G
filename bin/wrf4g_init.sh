WRF4G_BIN=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
WRF4G_DIR=$( dirname $WRF4G_BIN )
if [[ ":$PATH:" != *":$WRF4G_BIN:"* ]]; then export PATH=$WRF4G_BIN:${PATH}; fi
if [[ ":$PYTHONPATH:" != *":$WRF4G_DIR/lib/python:"* ]]; then export PYTHONPATH=$WRF4G_DIR/lib/python:${PYTHONPATH}; fi
if [[ ":$PYTHONPATH:" != *":$WRF4G_DIR/opt/drm4g/libexec:"* ]]; then export PYTHONPATH=$WRF4G_DIR/opt/drm4g/libexec:${PYTHONPATH}; fi
source ${WRF4G_BIN}/wrf4g_autocomplete.sh
