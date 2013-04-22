import os
import sys
import logging
from ConfigParser import SafeConfigParser
from drm4g.global_settings import PATH_HOSTS, COMMUNICATOR, RESOURCE_MANAGER, HOST_SECTION
from drm4g.utils.url import urlparse

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: configure.py 1788 2013-03-26 16:55:06Z carlos $"

class ConfigureException(Exception):
    pass

class CheckConfigFile():
    
    def __init__(self):
        if not os.path.exists(PATH_HOSTS):
            out='Wrong path for %s' % (PATH_HOSTS)
            logger.error(out)
            raise ConfigureException(out)
        else:
            self.init_time = os.stat(PATH_HOSTS).st_mtime
    def test(self):
        if os.stat(PATH_HOSTS).st_mtime != self.init_time:
            self.init_time = os.stat(PATH_HOSTS).st_mtime
            return True
        else:
            return False

  
def readHostList():
    logger = logging.getLogger(__name__)    
    if not os.path.exists(PATH_HOSTS):
        out='Wrong path for %s' % (PATH_HOSTS)
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
    logger     = logging.getLogger(__name__)
    url_result = urlparse(url)
    scheme     = url_result.scheme.lower()
    name       = url_result.host
    username   = url_result.username
    port       = url_result.port
    params     = url_result.params
    if not name:
        out='%s does not have hostname' % (hostname)
        logger.error(out)
        raise ConfigureException(out)
    if not username and (scheme != 'local'):
        out='%s does not have username' % (hostname)
        logger.error(out)
        raise ConfigureException(out)      
    if not COMMUNICATOR.has_key(scheme):
        out='%s has a wrong scheme "%s"' % (hostname, scheme)
        logger.error(out)
        raise ConfigureException(out)        
    if not params.has_key('LRMS_TYPE'):
        out='%s does not have LRMS_TYPE variable' % (hostname)
        logger.error(out)
        raise ConfigureException(out)
    if not RESOURCE_MANAGER.has_key(params['LRMS_TYPE']):
        out='%s has a wrong LRMS_TYPE "%s"' % (hostname, params['LRMS_TYPE'])
        logger.error(out)
        raise ConfigureException(out)
    if not params.has_key('NODECOUNT'):
        out='%s does not have NODECOUNT variable' % (hostname)
        logger.error(out)
        raise ConfigureException(out)
    if params.has_key('SSH_KEY_FILE'):
        key = params['SSH_KEY_FILE']
        if not os.path.isfile(os.path.expanduser(key)):
            out='%s file does not exist' % (key)
            logger.error(out)
            raise ConfigureException(out)
    return HostConfiguration(scheme, name, username, port, params)
                
class HostConfiguration(object):
        
    def __init__(self, scheme, name, username, port, params):
        
        self._scheme     = scheme
        self._name       = name
        self._username   = username
        self._port       = port
        self._params     = params
     
    def get_hostname(self):
        return self._name
              
    def get_username(self):
        return self._username
    
    def get_scheme(self):
        return self._scheme
    
    def get_port(self):
        if not self._port:
            return 22
        else:
            return int(self._port)

    def get_lrms_type(self):
        return self._params.setdefault('LRMS_TYPE', 'FORK')

    def get_node_count(self):
        return self._params.setdefault('NODECOUNT','1')

    def get_queue_name(self):
        return self._params.setdefault('QUEUE_NAME', 'default')

    def get_run_dir(self):
        return self._params.setdefault('TEMP_DIR', r'~')
 
    def set_run_dir(self, run_dir):
        if run_dir[-1] == '/': 
            run_dir =  run_dir[:-1]
        self._params['TEMP_DIR'] = run_dir
        
    def get_local_dir(self):
        return self._params.setdefault('RUN_DIR', '$HOME')
        
    def get_project(self):
        return self._params.setdefault('PROJECT')

    def get_PARALLEL_TAG(self):
        return self._params.setdefault('PARALLEL_TAG')
    
    def get_key_file(self):
        return self._params.setdefault('SSH_KEY_FILE','~/.ssh/id_rsa')
    
    def com_attrs(self):
        return (self.HOST, self.USERNAME, self.SCHEME, self.PORT, self.TEMP_DIR, self.SSH_KEY_FILE)
    
    def resource_attrs(self):
        return (self.LRMS_TYPE, self.PROJECT, self.PARALLEL_TAG)
    

    HOST       = property(get_hostname)
    USERNAME   = property(get_username)
    SCHEME     = property(get_scheme)
    LRMS_TYPE  = property(get_lrms_type)
    NODECOUNT  = property(get_node_count)
    QUEUE_NAME = property(get_queue_name)
    TEMP_DIR   = property(get_run_dir, set_run_dir)
    RUN_DIR    = property(get_local_dir)
    PROJECT        = property(get_project)
    PARALLEL_TAG   = property(get_PARALLEL_TAG)
    SSH_KEY_FILE   = property(get_key_file)
    PORT           = property(get_port)
