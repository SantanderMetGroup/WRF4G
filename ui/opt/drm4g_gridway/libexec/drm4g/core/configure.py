import os
import sys
import logging
from ConfigParser import SafeConfigParser
from drm4g.global_settings import PATH_HOSTS, COMMUNICATOR, RESOURCE_MANAGER, HOST_SECTION
from drm4g.utils.url import urlparse

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: configure.py 1357 2012-01-10 19:59:38Z carlos $"

class ConfigureException(Exception):
    pass
  
def readHostList():
    logger = logging.getLogger(__name__)    
    if not os.path.exists(PATH_HOSTS):
        out='Wrong PATH_HOSTS'
        logger.error(out)
        raise ConfigureException(out)
    parser = SafeConfigParser()
    try:
        parser.read(PATH_HOSTS)
    except Exception, err:
        error = 'Error reading ' + PATH_HOSTS + ' file: ' + str(err)
        logger.error(error)
        raise ConfigureException(error)
    host_list = { }
    if not parser.has_section(HOST_SECTION):
        error = PATH_HOSTS + ' file does not have ' + HOST_SECTION + ' section'
        logger.error(error)
        raise ConfigureException(error)
    else:
        for host in parser.options(HOST_SECTION):
            host_list[host] = parser.get(HOST_SECTION, host)
        return host_list

def parserHost(hostname, url):
    logger = logging.getLogger(__name__)
    url_result = urlparse(url)
    scheme     = url_result.scheme.lower()
    name       = url_result.host
    username   = url_result.username
    params     = url_result.params
    if not name:
        out='%s doesn\'t have hostname' % (hostname)
        logger.error(out)
        raise ConfigureException(out)
    if not username and (scheme != 'local'):
        out='%s doesn\'t have username' % (hostname)
        logger.error(out)
        raise ConfigureException(out)      
    if not COMMUNICATOR.has_key(scheme):
        out='%s has a wrong scheme "%s"' % (hostname, scheme)
        logger.error(out)
        raise ConfigureException(out)        
    if not params.has_key('LRMS_TYPE'):
        out='%s doesn\'t have a LRMS_TYPE' % (hostname)
        logger.error(out)
        raise ConfigureException(out)
    if not RESOURCE_MANAGER.has_key(params['LRMS_TYPE']):
        out='%s has a wrong LRMS_TYPE "%s"' % (hostname, params['LRMS_TYPE'])
        logger.error(out)
        raise ConfigureException(out)
    return HostConfiguration(scheme, name, username, params)
                
class HostConfiguration(object):
        
    def __init__(self, scheme, name, username, params):
        
        self._scheme     = scheme
        self._name       = name
        self._username   = username
        self._params     = params
     
    def get_hostname(self):
        return self._name
              
    def get_username(self):
        return self._username
    
    def get_scheme(self):
        return self._scheme

    def get_lrms_type(self):
        return self._params['LRMS_TYPE']

    def get_node_count(self):
        return self._params.setdefault('NODECOUNT')

    def get_queue_name(self):
        return self._params.setdefault('QUEUE_NAME')

    def get_run_dir(self):
        return self._params.setdefault('GW_SCRATCH_DIR',r'~')
 
    def set_run_dir(self, run_dir):
        self._params['GW_SCRATCH_DIR'] = run_dir
        
    def get_local_dir(self):
        return self._params.setdefault('GW_RUN_DIR')
        
    def get_project(self):
        return self._params.setdefault('PROJECT')

    def get_mpi_tag(self):
        return self._params.setdefault('MPI_TAG','mpi')

    HOST        = property(get_hostname)
    USERNAME    = property(get_username)
    SCHEME      = property(get_scheme)
    LRMS_TYPE   = property(get_lrms_type)
    NODECOUNT   = property(get_node_count)
    QUEUE_NAME  = property(get_queue_name)
    GW_SCRATCH_DIR   = property(get_run_dir, set_run_dir)
    GW_RUN_DIR = property(get_local_dir)
    PROJECT     = property(get_project)
    MPI_TAG     = property(get_mpi_tag)
