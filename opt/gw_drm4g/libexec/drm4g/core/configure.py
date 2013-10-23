import os
import os.path
import sys
import logging
import ConfigParser 
from drm4g.utils.importlib import import_module
from drm4g                 import DRM4G_CONFIG_FILE, COMMUNICATORS, RESOURCE_MANAGERS

__version__  = '1.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: configure.py 1925 2013-09-19 14:56:10Z carlos $"

logger = logging.getLogger(__name__)

class ConfigureException(Exception):
    pass

class Configuration(object):
    """
    Configuration class provides facilities to:

    * parse DRM4G_CONFIG_FILE resources
    * check key resources
    * instantiate objects such as communicators or managers
    
    """
    def __init__(self):
        self.resources  = dict()
        if not os.path.exists( DRM4G_CONFIG_FILE ):
            assert DRM4G_CONFIG_FILE, "dm4g.conf does not exist, please provide one"
        self.init_time = os.stat( DRM4G_CONFIG_FILE ).st_mtime
        
    def check_update(self):
        """
        It checks if DRM4G file configuration has been updated.
        """
        if os.stat(DRM4G_CONFIG_FILE).st_mtime != self.init_time:
            self.init_time = os.stat(DRM4G_CONFIG_FILE).st_mtime
            return True
        else:
            return False

    def load(self):
        """
        Read the configuration file.
        """
        logger.debug("Reading file '%s' ..." % DRM4G_CONFIG_FILE)
        try: 
            try:
                file = open(DRM4G_CONFIG_FILE, 'r')
                parser = ConfigParser.SafeConfigParser()
                try:
                    parser.readfp(file, DRM4G_CONFIG_FILE)
                except Exception, err:
                    output = "Configuration file '%s' is unreadable or malformed: %s" %(DRM4G_CONFIG_FILE, str(err))
                    logger.error(output)
                    raise ConfigureException(output)
                for sectname in parser.sections():
                    logger.debug("Reading configuration for resource '%s'." % sectname)
                    self.resources[sectname] = dict(parser.items(sectname))
                    logger.debug("Resource '%s' defined by: %s.",
                                 sectname, ', '.join([("%s=%s" % (k,v)) for k,v in sorted(self.resources[sectname].iteritems())]))
            except Exception, err:
                output = "Error reading '%s' file: %s" % (DRM4G_CONFIG_FILE, str(err)) 
                logger.error(output)
                raise ConfigureException(output)
        finally:
            file.close()
            
    def check(self):
        """
        Check if the drm4g.conf file has been configured well. 
        
        Return a list with the errors.
        """
        logger.debug("Checking file '%s' ..." % DRM4G_CONFIG_FILE)
        errors = []
        for resname, resdict in self.resources.iteritems() :
            logger.debug("Checking resource '%s' ..." % resname)
            reslist = resdict.keys( )
            for key in [ 'enable' , 'frontend' , 'lrms' , 'communicator' ] :
                if not key in reslist :
                    output = "'%s' resource does not have '%s' key" % (resname, key)
                    logger.error( output )
                    errors.append( output )
            if resdict.has_key( 'vo' ) :
                for key in [ "bdii" , "myproxy_server" ]:
                    if not key in reslist :
                        output = "'%s' has to be '%s' key" % (resname, key)
                        logger.error( output )
                        errors.append( output )
            else :
                if not 'ncores' in reslist :
                    output = "'ncores' key is mandatory for '%s' resource" % resname
                    logger.error( output )
                    errors.append( output )
                if not 'queue' in reslist :
                    self.resources[resname]['queue'] = "default"
                    output = "'queue' key will be called 'default' for '%s' resource" % resname
                    logger.debug( output )
            if not COMMUNICATORS.has_key( resdict[ 'communicator' ] ) :
                output = "'%s' has a wrong communicator: '%s'" % (resname , resdict[ 'communicator' ] )
                logger.error( output )
                errors.append( output )
            if resdict.has_key( 'ssh' ) :
                output = "'username' key is mandatory for 'ssh' communicator, '%s' resource" % resname 
                logger.error( output )
                errors.append( output )
            if not RESOURCE_MANAGERS.has_key( resdict[ 'lrms' ] ) :
                output = "'%s' has a wrong lrms: '%s'" % ( resname , resdict[ 'lrms' ] )
                logger.error( output )
                errors.append( output )
            private_key = resdict.get( 'private_key' )
            if private_key and not os.path.isfile( os.path.expanduser( private_key ) ) :
                output = "'%s' does not exist '%s' resource" % ( private_key , resname )
                logger.error( output )
                errors.append( output )
            if not errors:
                logger.debug( "'%s' passed the check with flying colours" % resname )
        return errors
                
    def make_communicators(self):
        """
        Make communicator objects corresponding to the configured resources.

        Return a dictionary, mapping the resource name into the corresponding objects.
        """
        communicators = dict()
        for name, resdict in self.resources.iteritems():
            try:
                if not resdict[ 'enable' ] : 
                    continue
                communicator                    = import_module(COMMUNICATORS[ resdict[ 'communicator' ] ] )
                com_object                      = getattr( communicator , 'Communicator' ) ()
                com_object.username             = resdict.get( 'username' )
                com_object.frontend             = resdict.get( 'frontend' )
                com_object.private_key          = resdict.get( 'private_key' )
                communicators[name]             = com_object
            except Exception, err:
                output = "Failed creating communicator for resource '%s' : %s" % ( name, str( err ) )
                logger.warning( output , exc_info=1 )
                raise ConfigureException( output )
        return communicators 


    def make_resources(self):
        """
        Make manager objects corresponding to the configured resources.

        Return a dictionary, mapping the resource name into the corresponding objects.
        """
        resources = dict()
        for name, resdict in self.resources.iteritems():
            try:
                if not resdict[ 'enable' ] : 
                    continue
                resources[name]                      = dict()
                manager                              = import_module(RESOURCE_MANAGERS[ resdict[ 'lrms' ] ] )
                resource_object                      = getattr( manager , 'Resource' ) ()
                resource_object.name                 = name
                resource_object.features             = resdict
                job_object                           = getattr( manager , 'Job' ) ()
                job_object.resfeatures               = resdict
                resources[name]['Resource']          = resource_object
                resources[name]['Job']               = job_object
            except Exception, err:
                output = "Failed creating objects for resource '%s' of type : %s" % ( name, str( err ) )
                logger.warning( output , exc_info=1 )
                raise ConfigureException( output )
        return resources


