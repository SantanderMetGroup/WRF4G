import os
import sys
import logging

from os.path             import dirname, abspath, join, exists
from wrf4g               import WRF4G_DEPLOYMENT_DIR, WRF4G_DIR
from wrf4g.utils.command import exec_cmd

__version__  = '2.1.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

DRM4G_BIN = join( WRF4G_DEPLOYMENT_DIR, 'opt', 'drm4g', 'bin' )
    
class GWJob( object ):
    """
    Class to use DRM4G 
    """
    def create_template(self,name,directory,arguments,np=1,req='',environ='',inputsandbox='',outputsandbox=''):
        """
        Create template
        """
        ftemplate = join( directory,  name + ".gw" )
        f = open(ftemplate,'w')
        template="""NAME = %s
EXECUTABLE   = /usr/bin/env python 
ARGUMENTS    = ./bin/pilot_wrf.py %s
INPUT_FILES  = %s
OUTPUT_FILES = %s
REQUIREMENTS = %s
ENVIRONMENT  = %s
NP           = %d""" % (name,arguments,inputsandbox,outputsandbox,req,environ,np)
        f.write(template)
        f.close()
        return ftemplate
    
    def submit(self, dep = None, priority = 0, type_dep = "afterok", file_template = "job.gw" ):
        depend = "-d %s -r %s" % ( dep, type_dep ) if dep != None else ''
        cmd = "%s/gwsubmit -p %d -v %s -t %s" % (DRM4G_BIN, priority, 
                                                  depend, file_template )
        code, out = exec_cmd( cmd )
        logging.debug( out )
        if code :
            raise Exception( out )
        return out[ 8: ]
   
    def history(self, job_id ):
        cmd = '%s/gwhistory %s' % ( DRM4G_BIN , job_id )
        code, out = exec_cmd( cmd )
        logging.info( out )

    def log(self, job_id ):
        directory = join( WRF4G_DIR ,
                          'var' ,
                          '%d00-%d99' % ( int(int(float( job_id ))/100) , int(int(float( job_id ))/100) ) ,
                          job_id ,
                          'job.log' )
        if not exists( directory ) :
            raise Exception( 'There is not a log available for this job.')
        cmd = 'cat %s' % ( directory )
        code, out = exec_cmd( cmd )
        logging.info( out )
 
    def list(self, job_id = None ):
        cmd = '%s/gwps -o Jsetxjh '  % ( DRM4G_BIN )  
        if job_id :
            cmd = cmd + job_id 
        code, out = exec_cmd( cmd )
        logging.info( out )

    def kill(self, job_id, hard = False ):
        cmd = "%s/gwkill %s %s" % ( DRM4G_BIN, '-9' if hard else '', str( job_id ) )
        code, out = exec_cmd( cmd )
        if code :
            logging.info( out )
                
    def set_priority(self, job_id, priority ):
        cmd = "%s/gwkill -9 -p %d %s" % (DRM4G_BIN, priority, str(job_id ) )
        code, out = exec_cmd( cmd )
        logging.info( out )

