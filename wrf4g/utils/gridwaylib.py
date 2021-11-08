#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

import os
import sys
import logging
from os.path             import dirname, abspath, join, exists
from wrf4g               import WRF4G_DEPLOYMENT_DIR, WRF4G_DIR
from wrf4g.utils.command import exec_cmd
from drm4g               import DRM4G_DIR

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
EXECUTABLE   = /usr/bin/env python3
ARGUMENTS    = ./bin/wrf_wrapper.py %s
WRAPPER      = %s/etc/wrf4g_wrapper.sh
INPUT_FILES  = %s
OUTPUT_FILES = %s
REQUIREMENTS = %s
ENVIRONMENT  = %s
RESCHEDULE_ON_FAILURE = no
NP           = %d""" % (name,arguments,WRF4G_DIR,inputsandbox,outputsandbox,req,environ,np)
        f.write(template)
        f.close()
        return ftemplate
    
    def submit(self, dep = None, priority = 0, type_dep = "afterok", file_template = "job.gw" ):
        depend = "-d %s -r %s" % ( dep, type_dep ) if dep != None else '-o '
        cmd = "GW_LOCATION=%s gwsubmit -p %d -v %s -t %s" % ( DRM4G_DIR, priority, 
                                                  depend, file_template )
        code, out = exec_cmd( cmd )
        logging.debug( out )
        if code :
            raise Exception( out )
        return out[ 8: ]
   
    def history(self, job_id ):
        cmd = 'GW_LOCATION=%s gwhistory %s' % ( DRM4G_DIR, job_id )
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
        cmd = 'GW_LOCATION=%s gwps -o Jsetxjh ' % DRM4G_DIR
        if job_id :
            cmd = cmd + job_id 
        code, out = exec_cmd( cmd )
        logging.info( out )

    def release(self, job_id ):
        cmd = "GW_LOCATION=%s gwkill -l %s" % ( DRM4G_DIR, str( job_id ) )
        code, out = exec_cmd( cmd )
        if code :
            logging.info( out )

    def kill(self, job_id, hard = False ):
        cmd = "GW_LOCATION=%s gwkill %s %s" % ( DRM4G_DIR, '-9' if hard else '', str( job_id ) )
        code, out = exec_cmd( cmd )
        if code :
            logging.info( out )
                
    def set_priority(self, job_id, priority ):
        cmd = "GW_LOCATION=%s gwkill -p %d %s" % ( DRM4G_DIR, priority, str(job_id ) )
        code, out = exec_cmd( cmd )
        logging.info( out )

