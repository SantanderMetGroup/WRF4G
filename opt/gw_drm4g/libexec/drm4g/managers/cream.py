from __future__      import with_statement
import re
import sys
import logging
import drm4g.managers
from os.path         import basename , dirname , exists
from string          import Template
from drm4g           import REMOTE_VOS_DIR
from drm4g.managers  import JobException

__version__  = '1.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: cream.py 1983 2014-01-26 15:04:29Z carlos $"

logger = logging.getLogger(__name__)

X509_USER_PROXY = 'X509_USER_PROXY=' +  REMOTE_VOS_DIR + '/x509up.%s'
# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
CREAM_SUBMIT = '%s glite-ce-job-submit' % X509_USER_PROXY  
CREAM_STATUS = '%s glite-ce-job-status' % X509_USER_PROXY    
CREAM_DEL    = '%s glite-ce-job-cancel' % X509_USER_PROXY
CREAM_PURGE  = '%s glite-ce-job-purge'  % X509_USER_PROXY

# Regular expressions for parsing.
re_status          = re.compile( "Status\s*=\s*\[(.*)\]" )
re_input_files     = re.compile( "GW_INPUT_FILES\s*=\s*\"(.*)\"" )
re_output_files    = re.compile( "GW_OUTPUT_FILES\s*=\s*\"(.*)\"" )
re_executable_file = re.compile( "GW_EXECUTABLE\s*=\s*\"(.*)\"" )

def sandbox_files(env_file):

    def executable_file(env):
        re_executable = re_executable_file.search(env)
        executable    = re_executable.groups()[0].split()[0]
        if executable.startswith( "file:" ) or ( executable.startswith( "/" ) and exists ( executable  ) ) :
            return basename ( executable )
        elif not executable.startswith( "gsiftp:" ) :
            return executable
        else :
            return None

    def parse_files(env, type, re_exp):
        files_to_copy = []
        files         = re_exp.search(env)
        if files:
            for file in files.groups()[0].split(','):
                if file.startswith( "gsiftp://" ) : 
                    continue
                if " " in file:
                    file0, file1 = file.split()
                    if type == 'output' :
                        file = file0
                    else:
                        file = file1
                files_to_copy.append( basename( file ) )
        return files_to_copy
    with open( env_file , "r" ) as f :
        line_env = ' '.join( f.readlines() )
    f.close()
    input_files      = parse_files( line_env , 'input' , re_input_files )
    output_files     = parse_files( line_env , 'output' , re_output_files )
    executable_file = executable_file(line_env)
    if executable_file :
        input_files.append( executable_file )
    return input_files, output_files

class Resource (drm4g.managers.Resource):
    pass

class Job (drm4g.managers.Job):
   
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
        X509_USER_PROXY = "X509_USER_PROXY=%s/proxy" % REMOTE_VOS_DIR
        cmd = "%s voms-proxy-init -ignorewarn -timeout 30 -valid 24:00 -q -voms %s -noregen -out %s/x509up.%s" % (
                                                                                                         X509_USER_PROXY ,
                                                                                                         self.resfeatures[ 'vo' ] ,
                                                                                                         REMOTE_VOS_DIR ,
                                                                                                         self.resfeatures[ 'vo' ] ,
                                                                                                         )
        out, err = self.Communicator.execCommand( cmd )
        if err :
            output = "Error renewing the proxy: %s" % err
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
        cmd = '%s %s' % ( CREAM_STATUS % self.resfeatures[ 'vo' ] , self.JobId )
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
            return self.cream_states.setdefault(mo.groups()[0], 'UNKNOWN')
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
        dir_temp   = dirname( parameters['executable'] )
        executable = basename( parameters['executable'] ) 
        stdout     = basename( parameters['stdout'] ) 
        stderr     = basename( parameters['stderr'] ) 
        args  = '[\n'
        args += 'JobType = "Normal";\n'
        args += 'Executable = "%s";\n' % executable
        args += 'StdOutput = "%s";\n'  % stdout
        args += 'StdError = "%s";\n'   % stderr
        args += 'CpuNumber = $count;\n'

        input_sandbox, output_sandbox = sandbox_files( self.resfeatures[ 'env_file' ] )
        if input_sandbox:
            args += 'InputSandbox = {"job.env", "%s" , %s};\n' % ( executable , 
                                                                   ','.join(['"%s"' % (f) for f in input_sandbox])
                                                                    )
        else :
            args += 'InputSandbox = {"job.env", "%s"};\n' % executable 
        args += 'InputSandboxBaseURI = "gsiftp://%s/%s";\n' % ( self.resfeatures[ 'frontend' ] , dir_temp )
        if output_sandbox:
            args += 'OutputSandbox = {"stdout.execution" , "stderr.execution" , "%s", "%s", %s};\n' % (
                                                                                                    stdout ,
                                                                                                    stderr ,
                                                                                                    ', '.join( [ '"%s"' % (f) for f in output_sandbox ] ) ,
                                                                                                    )
        else:
            args += 'OutputSandbox = {"stdout.execution" , "stderr.execution" , "%s" , "%s"};\n' % ( stdout , stderr )                        
        args += 'OutputSandboxBaseDestURI = "gsiftp://%s/%s";\n' % ( self.resfeatures[ 'frontend' ] , dir_temp )
        requirements = ''
        if parameters.has_key('maxWallTime'):  
            requirements += '(other.GlueCEPolicyMaxWallClockTime <= $maxWallTime)' 
        if parameters.has_key('maxCpuTime'):
            if requirements: 
                requirements += ' && '
            requirements += '(other.GlueCEPolicyMaxCPUTime <= $maxCpuTime)' 
        if parameters.has_key('maxMemory'):
            if requirements: 
                requirements += ' && '
            requirements += ' (other.GlueHostMainMemoryRAMSize <= $maxMemory) '
        if requirements :
            args += 'Requirements=%s;\n' % (requirements)
        args += 'Environment={%s};\n' % (','.join(['"%s=%s"' %(k, v) for k, v in parameters['environment'].items()]))
        args += ']'
        return Template(args).safe_substitute(parameters)


 
