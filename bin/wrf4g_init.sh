#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

WRF4G_DEPLOYMENT_DIR_BIN=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
WRF4G_DEPLOYMENT_DIR=$( dirname ${WRF4G_DEPLOYMENT_DIR_BIN} )
if [[ ":${PATH}:" != *":${WRF4G_DEPLOYMENT_DIR}:"* ]]; then export PATH=${WRF4G_DEPLOYMENT_DIR_BIN}:${PATH}; fi
if [[ ":${PYTHONPATH}:" != *":${WRF4G_DEPLOYMENT_DIR}/lib/python:"* ]]; then export PYTHONPATH=${WRF4G_DEPLOYMENT_DIR}/lib/python:${PYTHONPATH}; fi
if [[ ":${PYTHONPATH}:" != *":${WRF4G_DEPLOYMENT_DIR}/opt/drm4g/libexec:"* ]]; then export PYTHONPATH=${WRF4G_DEPLOYMENT_DIR}/opt/drm4g/libexec:${PYTHONPATH}; fi
source ${WRF4G_DEPLOYMENT_DIR_BIN}/wrf4g_autocomplete.sh
