DRM4G_BIN=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
DRM4G_DIR=$( dirname $DRM4G_BIN )
if [[ ":$PATH:" != *":$DRM4G_BIN:"* ]]; then export PATH=$DRM4G_BIN:${PATH}; fi
if [[ ":$PYTHONPATH:" != *":$DRM4G_DIR/libexec:"* ]]; then export PYTHONPATH=$DRM4G_DIR/libexec:${PYTHONPATH}; fi
source ${DRM4G_BIN}/drm4g_autocomplete.sh
