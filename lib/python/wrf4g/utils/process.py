import os
import subprocess

__version__  = '2.0.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

def process_is_runnig( pid ):
    """
    Check is a process is running given a file
    """
    try:
        with open( pid , 'r' ) as f:
            lines = f.readlines()
        os.kill( int( lines[0].strip() ) , 0 )
    except :
        return False
    else:
        return True

def exec_cmd( cmd, nohup=False, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
              stderr=subprocess.STDOUT, env=os.environ ):
    """
    Execute shell commands
    """
    logger.debug( "Executing command ... " + cmd )
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

