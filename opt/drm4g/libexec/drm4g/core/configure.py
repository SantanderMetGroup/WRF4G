import os
import os.path
import sys
import logging
from drm4g.utils.importlib import import_module
from drm4g                 import ( DRM4G_CONFIG_FILE, 
                                    COMMUNICATORS, 
                                    RESOURCE_MANAGERS,
                                    REMOTE_JOBS_DIR, 
                                    SSH_PORT )

try :
    import configparser
except ImportError :
    import ConfigParser as configparser

__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: configure.py 2448 2015-05-12 11:56:52Z carlos $"

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
            assert DRM4G_CONFIG_FILE, "resources.conf does not exist, please provide one"
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
                file   = open(DRM4G_CONFIG_FILE, 'r')
                parser = configparser.RawConfigParser()
                try:
                    parser.readfp( file , DRM4G_CONFIG_FILE )
                except Exception as err:
                    output = "Configuration file '%s' is unreadable or malformed: %s" % ( DRM4G_CONFIG_FILE , str( err ) )
                    logger.error( output )
                
                for sectname in parser.sections():
                    name                   = sectname
                    logger.debug(" Reading configuration for resource '%s'." % name )
                    self.resources[ name ] = dict( parser.items( sectname ) )
                    logger.debug("Resource '%s' defined by: %s.",
                             sectname, ', '.join([("%s=%s" % (k,v)) for k,v in sorted(self.resources[name].items())]))
            except Exception as err:
                output = "Error reading '%s' file: %s" % (DRM4G_CONFIG_FILE, str(err)) 
                logger.error( output )
        finally:
            file.close()
            
    def check(self):
        """
        Check if the drm4g.conf file has been configured well. 
        
        Return a list with the errors.
        """
        errors = []
        for resname, resdict in self.resources.items() :
            logger.debug("Checking resource '%s' ..." % resname)
            reslist = list(resdict.keys( ))
            for key in [ 'enable' , 'frontend' , 'lrms' , 'communicator' ] :
                if not key in reslist :
                    output = "'%s' resource does not have '%s' key" % (resname, key)
                    logger.error( output )
                    errors.append( output )
            if ( not 'max_jobs_running' in reslist ) and ( resdict[ 'lrms' ] != 'cream' ) :
                output = "'max_jobs_running' key is mandatory for '%s' resource" % resname
                logger.error( output )
                errors.append( output )
            if ( not ( 'max_jobs_in_queue' in reslist ) and ( 'max_jobs_running' in reslist ) and ( resdict[ 'lrms' ] != 'cream' ) ) :
                self.resources[resname]['max_jobs_in_queue'] = resdict['max_jobs_running']
                logger.debug( "'max_jobs_in_queue' will be the same as the 'max_jobs_running'" )
            if ( not 'queue' in reslist ) and ( resdict[ 'lrms' ] != 'cream' ) :
                self.resources[resname]['queue'] = "default"
                output = "'queue' key will be called 'default' for '%s' resource" % resname
                logger.debug( output )
            if 'max_jobs_running' in reslist and resdict[ 'lrms' ] != 'cream' and resdict.get( 'max_jobs_in_queue' ).count( ',' ) !=  resdict.get( 'queue' ).count( ',' ) :
                output = "The number of elements in 'max_jobs_in_queue' are different to the elements of 'queue'"
                logger.error( output )
                errors.append( output )
            if 'max_jobs_running' in reslist and resdict[ 'lrms' ] != 'cream' and resdict.get( 'max_jobs_running' ).count( ',' ) !=  resdict.get( 'queue' ).count( ',' ) :
                output = "The number of elements in 'max_jobs_running' are different to the elements of 'queue'"
                logger.error( output )
                errors.append( output )
            if resdict[ 'lrms' ] != 'cream' and ( 'host_filter' in reslist ) :
                output = "'host_filter' key is only available for 'cream' lrms"
                logger.error( output )
                errors.append( output )
            if resdict[ 'communicator' ] not in COMMUNICATORS :
                output = "'%s' has a wrong communicator: '%s'" % (resname , resdict[ 'communicator' ] )
                logger.error( output )
                errors.append( output )
            if resdict[ 'communicator' ] == 'ssh' and 'username' not in resdict :
                output = "'username' key is mandatory for 'ssh' communicator, '%s' resource" % resname 
                logger.error( output )
                errors.append( output )
            if resdict[ 'lrms' ] not in RESOURCE_MANAGERS :
                output = "'%s' has a wrong lrms: '%s'" % ( resname , resdict[ 'lrms' ] )
                logger.error( output )
                errors.append( output )
            if resdict[ 'communicator' ] == 'ssh' :
                private_key = resdict.get( 'private_key' )
                if not private_key :
                    output = "'private_key' key is mandatory for '%s' resource" % resname
                    logger.error( output )
                    errors.append( output )
                else :
                    abs_private_key = os.path.expandvars( os.path.expanduser( private_key ) ) 
                    if not os.path.isfile( abs_private_key ) :
                        output = "'%s' does not exist for '%s' resource" % ( private_key , resname )
                        logger.error( output )
                        errors.append( output )
                    else :
                        self.resources[resname]['private_key'] = abs_private_key
                public_key = resdict.get( 'public_key' )
                if not public_key :
                    abs_public_key = abs_private_key + '.pub'
                else :
                    abs_public_key = os.path.expandvars( os.path.expanduser( public_key ) ) 
                if not os.path.isfile( abs_private_key ) :     
                    output = "'%s' does not exist for '%s' resource" % ( abs_public_key , resname )
                    logger.error( output )
                    errors.append( output )
                else :
                    self.resources[resname]['public_key'] = abs_public_key 
            grid_cert = resdict.get( 'grid_cert' )
            if grid_cert : 
                abs_grid_cert = os.path.expandvars( os.path.expanduser( grid_cert ) ) 
                if not os.path.isfile( abs_grid_cert ) :
                    output = "'%s' does not exist for '%s' resource" % ( abs_grid_cert , resname )
                    logger.error( output )
                    errors.append( output )
                else :
                    self.resources[resname]['grid_cert'] = abs_grid_cert
        return errors
                
    def make_communicators(self):
        """
        Make communicator objects corresponding to the configured resources.

        Return a dictionary, mapping the resource name into the corresponding objects.
        """
        communicators = dict()
        for name, resdict in self.resources.items():
            try:
                communicator              = import_module(COMMUNICATORS[ resdict[ 'communicator' ] ] )
                com_object                = getattr( communicator , 'Communicator' ) ()
                com_object.username       = resdict.get( 'username' )
                com_object.frontend       = resdict.get( 'frontend' )
                com_object.private_key    = resdict.get( 'private_key' )
                com_object.public_key     = resdict.get( 'public_key' )
                com_object.work_directory = resdict.get( 'scratch', REMOTE_JOBS_DIR ) 
                communicators[name]       = com_object
            except Exception as err:
                output = "Failed creating communicator for resource '%s' : %s" % ( name, str( err ) )
                logger.warning( output , exc_info=1 )
        return communicators 

    def make_resources(self):
        """
        Make manager objects corresponding to the configured resources.

        Return a dictionary, mapping the resource name into the corresponding objects.
        """
        resources = dict()
        for name, resdict in self.resources.items():
            try:
                resources[name]             = dict()
                manager                     = import_module(RESOURCE_MANAGERS[ resdict[ 'lrms' ] ] )
                resource_object             = getattr( manager , 'Resource' ) ()
                resource_object.name        = name
                resource_object.features    = resdict
                job_object                  = getattr( manager , 'Job' ) ()
                job_object.resfeatures      = resdict
                resources[name]['Resource'] = resource_object
                resources[name]['Job']      = job_object
            except Exception as err:
                output = "Failed creating objects for resource '%s' of type : %s" % ( name, str( err ) )
                logger.warning( output , exc_info=1 )
        return resources
