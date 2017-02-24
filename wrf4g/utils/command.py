import os
import logging
import subprocess
from distutils  import spawn

__version__  = '2.2.2'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

def cls():
    os.system( "clear" )

def which( command ):
    """
    Locate commands
    """
    return spawn.find_executable( command )

def exec_cmd_advance( cmd, nohup=False, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
              stderr=subprocess.STDOUT, env=os.environ ):
    """
    Execute shell commands
    """
    logging.debug( "Executing command ... " + cmd )
    cmd_to_exec = subprocess.Popen( cmd, shell = True, stdin = stdin,
                                    stdout = stdout, stderr = stderr,
                                    env = env )
    if not nohup :
        out , err =  cmd_to_exec.communicate()
    else :
        out = err = ''
    return out , err

def exec_cmd( cmd, stdout = subprocess.PIPE, stderr = subprocess.PIPE, env = os.environ ):
    logging.debug( "Executing command ... " + cmd )
    p      = subprocess.Popen( cmd, shell = True, stdout = stdout,
                               stderr = stderr, env = env )
    output = p.stdout.read().strip() + p.stderr.read().strip()
    code   = p.wait()
    return code, output
