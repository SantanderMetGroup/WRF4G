WRF4G_DEPLOYMENT_DIR_BIN=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
WRF4G_DEPLOYMENT_DIR=$( dirname ${WRF4G_DEPLOYMENT_DIR_BIN} )
if [[ ":${PATH}:" != *":${WRF4G_DEPLOYMENT_DIR}:"* ]]; then export PATH=${WRF4G_DEPLOYMENT_DIR_BIN}:${PATH}; fi
if [[ ":${PYTHONPATH}:" != *":${WRF4G_DEPLOYMENT_DIR}/lib/python:"* ]]; then export PYTHONPATH=${WRF4G_DEPLOYMENT_DIR}/lib/python:${PYTHONPATH}; fi
if [[ ":${PYTHONPATH}:" != *":${WRF4G_DEPLOYMENT_DIR}/opt/drm4g/libexec:"* ]]; then export PYTHONPATH=${WRF4G_DEPLOYMENT_DIR}/opt/drm4g/libexec:${PYTHONPATH}; fi
source ${WRF4G_DEPLOYMENT_DIR_BIN}/wrf4g_autocomplete.sh
