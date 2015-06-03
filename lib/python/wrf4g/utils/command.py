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
    logging.debug( "Executing command ... " + cmd )
    cmd_to_exec = subprocess.Popen( cmd, shell = True, stdin = subprocess.PIPE,
                                    stdout = subprocess.PIPE, stderr=  subprocess.PIPE,
                                    env = env )
    if not nohup :
        out , err =  cmd_to_exec.communicate()
    else :
        out = err = ''
    return out , err

def exec_cmd_popen( cmd ):
    logging.debug( "Executing command ... " + cmd )
    try:
        import subprocess
        p      = subprocess.Popen( cmd, shell = True, stdout = subprocess.PIPE,
                                   stderr = subprocess.PIPE, close_fds = True )
        output = p.stdout.read().strip()
        code   = p.wait()
    except ImportError:
        import popen2
        p      = popen2.Popen3( "%s" % cmd )
        output = p.fromchild.read().strip()
        code   = p.wait()
    return code, output
