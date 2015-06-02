import os
import logging
try :
    import subprocess
except :
    pass

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

def exec_cmd_subprocess( cmd, nohup=False, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
              stderr=subprocess.STDOUT, env=os.environ ):
    """
    Execute shell commands
    """
    try :    logging.debug( "Executing command ... " + cmd )
    except : pass
    cmd_to_exec = subprocess.Popen(  cmd ,
                                  shell=True ,
                                  stdin=subprocess.PIPE,
                                  stdout=subprocess.PIPE,
                                  stderr=subprocess.PIPE,
                                  env=env
                                  )
    if not nohup :
        out , err =  cmd_to_exec.communicate()
    else :
        out = err = ''
    return out , err


def exec_cmd_popen( cmd ):
    import popen2
    try :     logging.debug( "Executing command ... " + cmd )
    except :  pass
    p3     = popen2.Popen3( "%s" % cmd )
    output = p3.fromchild.read().strip()
    code   = p3.wait()
    return code, output
