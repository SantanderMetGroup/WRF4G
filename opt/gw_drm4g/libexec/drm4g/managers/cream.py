from __future__      import with_statement
import re
import sys
import logging
import drm4g.managers
from os.path         import basename , dirname , exists, join
from drm4g           import REMOTE_VOS_DIR
from drm4g.managers  import JobException

__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: cream.py 2250 2014-08-27 09:04:57Z carlos $"

logger = logging.getLogger(__name__)

X509_USER_PROXY = 'X509_USER_PROXY=' +  join( REMOTE_VOS_DIR , 'x509up.%s ' )
# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
CREAM_SUBMIT = X509_USER_PROXY + 'glite-ce-job-submit'  
CREAM_STATUS = X509_USER_PROXY + 'glite-ce-job-status'     
CREAM_DEL    = X509_USER_PROXY + 'glite-ce-job-cancel' 
CREAM_PURGE  = X509_USER_PROXY + 'glite-ce-job-purge'  
GLOBUS_CP    = X509_USER_PROXY + 'globus-url-copy'     

# Regular expressions for parsing.
re_status          = re.compile( "Current Status\s*=\s*\[(.*)\]" )
re_input_files     = re.compile( "GW_INPUT_FILES\s*=\s*\"(.*)\"" )
re_output_files    = re.compile( "GW_OUTPUT_FILES\s*=\s*\"(.*)\"" )
re_executable_file = re.compile( "GW_EXECUTABLE\s*=\s*\"(.*)\"" )
re_obs_url         = re.compile( "CREAM OSB URI\s*=\s*\[(.*)\]" )


def sandbox_files(env_file):

    def parse_files(env, type, re_exp):
        files = []
        match = re_exp.search(env)
        if match :
            for file in match.group( 1 ).split(','):
                if file.startswith( "gsiftp://" ) : 
                    continue
                if " " in file:
                    if type == 'output' :
                        file , _  = file.split()
                    else:
                        _  , file = file.split()
                files.append( basename( file ) )
        return files
    
    with open( env_file , "r" ) as f :
        line_env = ' '.join( f.readlines() )
    input_files      = parse_files( line_env , 'input' ,  re_input_files )
    output_files     = parse_files( line_env , 'output' , re_output_files )
    return input_files, output_files

class Resource (drm4g.managers.Resource):
    pass

class Job (drm4g.managers.Job):
    
    default_output_files = [
                            'stdout.execution', 
                            'stderr.execution', 
                            'stdout.wrapper',
                            'stderr.wrapper'
                            ]
    #cream job status <--> GridWay job status
    cream_states = {
                    "REGISTERED"    : "PENDING",
                    "PENDING"       : "PENDING",
                    "IDLE"          : "PENDING",
                    "RUNNING"       : "ACTIVE",
                    "REALLY-RUNNING": "ACTIVE",
                    "HELD"          : "PENDING",
                    "CANCELLED"     : "DONE",
                    "DONE-OK"       : "DONE",
                    "DONE-FAILED"   : "FAILED",
                    "ABORTED"       : "FAILED",
                    }

    def _renew_proxy(self):
        output = "The proxy 'x509up.%s' has probably expired" %  self.resfeatures[ 'vo' ]  
        logger.debug( output )
        if self.resfeatures.has_key( 'myproxy_server' ) :
            LOCAL_X509_USER_PROXY = "X509_USER_PROXY=%s" % join ( REMOTE_VOS_DIR , self.resfeatures[ 'myproxy_server' ] ) 
        else :
            LOCAL_X509_USER_PROXY = "X509_USER_PROXY=%s/${MYPROXY_SERVER}" % ( REMOTE_VOS_DIR )
        cmd = "%s voms-proxy-init -ignorewarn -timeout 30 -valid 24:00 -q -voms %s -noregen -out %s" % (
                                                                                                        LOCAL_X509_USER_PROXY ,
                                                                                                        self.resfeatures[ 'vo' ] ,
                                                                                                        join( REMOTE_VOS_DIR , 'x509up.%s ' ) % self.resfeatures[ 'vo' ] 
                                                                                                        )
        logger.debug( "Executing command: %s" % cmd )
        out, err = self.Communicator.execCommand( cmd )
        if err :
            output = "Error renewing the proxy(%s): %s" % (cmd , err )
            logger.error( output )
            raise JobException( output )

    def jobSubmit(self, wrapper_file):
        cmd = '%s -a -r %s:8443/%s-%s %s' % ( 
                                     CREAM_SUBMIT % self.resfeatures[ 'vo' ] ,
                                     self.resfeatures[ 'host' ] ,
                                     self.resfeatures[ 'jm' ] , 
                                     self.resfeatures[ 'queue' ] ,
                                     wrapper_file 
                                     )
        out, err = self.Communicator.execCommand( cmd )
        logger.info(cmd)
        logger.info(out + err)
        if ( 'The proxy has EXPIRED' in out ) or ( ' is not accessible' in err ) :
            self._renew_proxy()
            out , err = self.Communicator.execCommand( cmd )
            logger.info(out + err)
            if not 'https://' in out : 
                output = "Error submitting job after renewing the proxy: %s %s" %  ( err , out ) 
                logger.error( output )
                raise JobException( output )
        if not 'https://' in out :
            output = "Error submitting job: %s %s" % ( out, err )
            logger.error( output )
            raise JobException( output )
        return out[ out.find("https://"): ].strip() #cream_id

    def jobStatus(self):
        cmd = '%s %s -L 2' % ( CREAM_STATUS % self.resfeatures[ 'vo' ] , self.JobId )
        out, err = self.Communicator.execCommand( cmd )
        if 'The proxy has EXPIRED' in out :
            self._renew_proxy()
            out , err = self.Communicator.execCommand( cmd )
            if "ERROR" in err: 
                 output = "Error checking '%s' job after renewing the proxy: %s" % ( self.JobId , err )
                 logger.error( output )
                 raise JobException( output )
        if "ERROR" in err:
            output = "Error checking '%s' job: %s" % ( self.JobId , err )
            logger.error( output )
            raise JobException( output )
        mo = re_status.search(out)
        if mo:
            job_status = self.cream_states.setdefault(mo.groups()[0], 'UNKNOWN')
            if job_status == 'DONE' or job_status == 'FAILED' :
               output_url = self._getOutputURL( out )
               self._getOutputFiles( output_url )
            return job_status
        else:
            return 'UNKNOWN'
    
    def jobCancel(self):
        cmd = '%s -N %s' % (CREAM_DEL % self.resfeatures[ 'vo' ]  , self.JobId )
        out, err = self.Communicator.execCommand( cmd )
        if 'The proxy has EXPIRED' in out :
            self._renew_proxy()
            out , err = self.Communicator.execCommand( cmd )
            if "ERROR" in err:
                 output = "Error canceling '%s' job after renewing the proxy: %s" % ( self.JobId , err )
                 logger.error( output )
                 raise JobException( output )
        if "ERROR" in err: 
            output = "Error canceling '%s' job: %s" % ( self.JobId , err )
            logger.error( output )
            raise JobException( output )
		
    def jobPurge(self):
        cmd = '%s -N %s' % (CREAM_PURGE % self.resfeatures[ 'vo' ] , self.JobId )
        out, err = self.Communicator.execCommand( cmd )
        if 'The proxy has EXPIRED' in out :
            self._renew_proxy()
            out , err = self.Communicator.execCommand( cmd )
            if "ERROR" in err:
                 output = "Error purging '%s' job after renewing the proxy: %s" % ( self.JobId , err )
                 logger.error( output )
                 raise JobException( output )
        if "ERROR" in err: 
            output = "Error purging '%s' job: %s" % ( self.JobId , err )
            logger.error( output )
            raise JobException( output )
        
    def jobTemplate(self, parameters):
        dir_temp   = self.local_output_directory = dirname( parameters['executable'] )
        executable = basename( parameters['executable'] ) 
        stdout     = basename( parameters['stdout'] ) 
        stderr     = basename( parameters['stderr'] ) 
        count      = parameters['count'] 
  
        input_sandbox, output_sandbox = sandbox_files( self.resfeatures[ 'env_file' ] )
        
        if input_sandbox :
            input_files = ' '.join( [ ',"%(dir_temp)s/' + '%s"' % (f) for f in input_sandbox] ) % {'dir_temp':dir_temp } 
        else :
            input_files = ''
        
        self.default_output_files.extend( output_sandbox )
        output_files = ','.join( [ '"%s"' % (f) for f in self.default_output_files ] )
            
        requirements = ''
        if parameters.has_key('maxWallTime'):  
            requirements += '(other.GlueCEPolicyMaxWallClockTime <= %s)' % parameters['maxWallTime']
        if parameters.has_key('maxCpuTime'):
            if requirements: 
                requirements += ' && '
            requirements += '(other.GlueCEPolicyMaxCPUTime <= %s)' % parameters['maxCpuTime']
        if parameters.has_key('maxMemory'):
            if requirements: 
                requirements += ' && '
            requirements += ' (other.GlueHostMainMemoryRAMSize <= %s)' % parameters['maxMemory'] 
        Requirements = 'Requirements=%s;' % (requirements) if requirements else ''
        
        env = ','.join(['"%s=%s"' %(k, v) for k, v in parameters['environment'].items()])
        
        return """
[
JobType = "Normal";
Executable = "%(executable)s";
StdOutput = "%(stdout)s";
StdError = "%(stderr)s"; 
CpuNumber = %(count)s;
OutputSandboxBaseDestURI = "gsiftp://localhost";
InputSandbox = { "%(dir_temp)s/job.env", "%(dir_temp)s/%(executable)s"  %(input_files)s };
OutputSandbox = { %(output_files)s };
Environment = { %(env)s };
%(Requirements)s
]""" % {
        'executable'   : executable,
        'stdout'       : stdout,
        'stderr'       : stderr,
        'dir_temp'     : dir_temp,
        'count'        : count,
        'input_files'  : input_files,
        'output_files' : output_files,
        'Requirements' : Requirements,
        'env'          : env,
        }
    
    def _getOutputURL( self, status_output ):
        """ 
        Resolve the URL for the output files
        """
        match = re_obs_url.search( status_output )
        if match :
            url = match.group( 1 )
            return url
        else :
            output = "Output URL not found in '%s'" % ( status_output )
            logger.error( output )
            raise JobException( output )

    def _getOutputFiles( self, output_url ):
        """ 
        Get output files from the remote output_url
        """
        for file in self.default_output_files :
            cmd = '%s %s file://%s' % ( 
                                       GLOBUS_CP % self.resfeatures[ 'vo' ],
                                       join( output_url , file ) ,
                                       join( self.local_output_directory , file )
                                       )
            logger.debug( "Coping file '%s' : %s" % ( file , cmd ) )
            out, err = self.Communicator.execCommand( cmd )
            if 'error' in err :
                output = "Error coping file '%s' : %s" % ( file , err )
                logger.error( output )
                raise JobException( output )
            
 
