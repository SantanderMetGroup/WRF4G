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
import re
import glob
import time
import copy
import tarfile
import shutil
import logging
import fortran_namelist as fn
import re
from fortran_namelist       import coerce_value_list
from sqlalchemy             import and_, or_
from os.path                import ( exists, expandvars, 
                                     expanduser, isdir, 
                                     join, abspath )
from datetime               import datetime
from wrf4g                  import WRF4G_DIR, WRF4G_DEPLOYMENT_DIR
from wrf4g.config           import get_conf, save_json
from wrf4g.utils            import Enumerate, dict_compare 
from wrf4g.utils.archive    import extract
from wrf4g.utils.time       import ( datetime2datewrf, Calendar, 
                                     datewrf2datetime, timedelta_total_seconds )
from wrf4g.utils.file       import validate_name, edit_file
from wrf4g.utils.command    import exec_cmd
from wrf4g.utils.vcplib     import VCPURL
from wrf4g.utils.gridwaylib import GWJob


class Experiment(object):
    """ 
    Manage WRF4G experiments
    """
    dryrun = False
    
    def run(self, rerun = False, rea_pattern = False, rea_status = False, priority = 0 ):
        """
        Run the realizations of this experiment
        n_chunk is the number of chunks to run. 
        If there are not n_chunk, run every chunk since the last one finished 
        """
        #Check if the experiment have some realization to run
        #list of realizations of the experiment
        l_realizations  = self._filter_realizations( rea_pattern, rea_status )
        if not ( l_realizations ):
            logging.warn( 'There are not realizations to run.' )
        else:
            #if there are realizations to run
            for rea in l_realizations :
                logging.info( "---> Submitting Realization %s" % rea.name )
                #Run every realization
                rea.dryrun = self.dryrun
                if rerun :
                   first_chunk_run = 1
                else :
                   first_chunk_run = None
                rea.run( first_chunk_run = first_chunk_run, rerun = rerun, priority = priority )    

    def run_wps(self, rerun = False, rea_pattern = False, rea_status = False, priority = 0 ):
        """
        Run the realizations of this experiment
        n_chunk is the number of chunks to run. 
        If there are not n_chunk, run every chunk since the last one finished 
        """
        #Check if the experiment have some realization to run
        #list of realizations of the experiment
        l_realizations  = self._filter_realizations( rea_pattern, rea_status )
        if not ( l_realizations ):
            logging.warn( 'There are not realizations to run.' )
        else:
            #if there are realizations to run
            for rea in l_realizations :
                logging.info( "---> Submitting Realization %s" % rea.name )
                #Run every realization
                rea.dryrun = self.dryrun
                if rerun :
                   first_chunk_run = 1
                else :
                   first_chunk_run = None
                rea.run_wps( rerun = rerun, priority = priority )

    def edit(self):
        """
        Edit experiment.wrf4g file.
        """
        edit_file( join( self.home_directory, 'experiment.wrf4g' )  )

    def _copy_namelist_template( self, namelist_template ):
        """
        Copy the namelist from the template directory 
        """
        logging.info( "Preparing namelist %s version ... " % namelist_template )

        if not namelist_template.startswith("/"):
            if re.match("^\d+.\d+.\d+",namelist_template):
                namelist_template = join( WRF4G_DIR, 'etc', 'templates', 'namelist',
                                  'namelist.input-%s' % namelist_template )    
            else:
                raise Exception( "namelist version: %s is not an accepted value. It should point to a file or to a WRF version"  %namelist_template)
           
        namelist_input    = join( self.home_directory, 'namelist.input' )
        try :
            logging.debug( "Copying '%s' to '%s'" % ( namelist_template, namelist_input ) )
            shutil.copyfile( namelist_template, namelist_input )
        except :
            raise Exception( "There is not a namelist template for WRF '%s'"
                             "(File namelist.input does not exist)" % namelist_template )
        return namelist_input

    def check_db(self, name, start_date, end_date, cfg) :
        """ 
        Check if there is a realization with the same no reconfigurable field. 
        If there is not a realization with the same no reconfigurable fields => error
        If there is a realization with the same reconfigurable fields, update data
        """
        #Check if exists the realization
        try :
            rea = self.realization.filter_by( name = name ).one()
        except Exception :
            return None
        else :
            #Check if there is a realization with the same no reconfigurable fields
            if rea.cfg[ 'calendar' ] == cfg[ 'calendar' ] and rea.end_date != end_date :
                logging.debug( '\t\tUpdating realization on the database...' )
                rea.end_date = end_date
                rea.status   = Realization.Status.PREPARED
                return rea
            elif rea.end_date == end_date and rea.cfg[ 'calendar' ] == cfg[ 'calendar' ] :
                rea.cfg = cfg
                return rea
            else :                                 
                #if rea does not exist (no realization with the same no reconfigurable fields)
                raise Exception( "\t\tRealization with the same name and " 
                                 "no reconfigurable fields already exists" ) 

    def prepare(self, update = False, directory = './' ):
        """
        Prepare all realizations and chunks needed to submit a WRF4G experiment.
        """
        if update :
            directory = self.home_directory
        # Read experiment.wrf4g file
        self.cfg = get_conf( directory )
        if self.name != self.cfg[ 'default'][ 'name' ] :
            raise Exception( "ERROR: experiment.wrf4g file has a different experiment name." )
        
        # Update experiment variables
        self.name           = self.cfg[ 'default' ][ 'name' ]
        self.home_directory = abspath( directory ) 
   
        if not self.dryrun :
            exp_sub_dir = join( WRF4G_DIR, 'var', 'submission', self.name )
            if not isdir( exp_sub_dir ) :
                try:
                    logging.debug( "Creating '%s' directory" % exp_sub_dir )
                    os.makedirs( exp_sub_dir )
                except Exception :
                    raise Exception( "Couldn't be created '%s' directory" % exp_sub_dir )
          
        # Cycle to create a realization per combination
        for section in sorted( list( self.cfg.keys() ) ) :
            if section.startswith( "ensemble" ) :
                try :
                    # Copy the namelist from the template directory 
                    namelist_input = self._copy_namelist_template( self.cfg[ section ][ 'namelist_template' ] )
                    self.update_namelist( namelist_input, section )
                    self.cycle_time( namelist_input, section )
                except KeyError as err :
                    logging.error( "%s is a mandatory variable."
                                   " Please add this variable to experiment.wrf4g file" % str(err) )

        if not self.dryrun :
            # Copy configure files before submission
            self._copy_experiment_files( exp_sub_dir  )
            # Create software bundles to use on the WN
            self._create_wrf4g_bundles( exp_sub_dir  )
    
    def get_status(self, rea_pattern = False, rea_status = False, showchunks= False ):
        """ 
        Show information about realizations of the experiment for example:
        
        Realization Status   Chunks     Comp.Res      Run.Sta     JID ext      %
        testc       Finished    3/3     mycomputer   Finished       0   0 100.00
        
        * Realization: Realization name. It is taken from the field experiment_name in experiment.wrf4g.
        * Status: It can be take the following values: Prepared, Submitted, Running, Failed and Done).
        * Chunks [Chunk running/Total Chunks]: A realization is split into chunks. Each chunk is sent as a job.
        * Computer resource: Resource (cluster) where the job is running.
        * Run.Sta: Job status in the WN (Downloading data, running ungrib, real, wrf, ...)
        * JID: Job identifier.
        * ext: Exit Code. If exit code is different from 0, there has been an error. 
        * % : percentage of simulation finished.
        """
        #list of realization of the experiment
        l_realizations = self._filter_realizations( rea_pattern, rea_status )
        #Header of the information
        if not ( l_realizations ):
            raise Exception ( 'There are not realizations to check.' )
        if showchunks:
            Realization.status_chunk_header()
        else:
            Realization.status_header()

        for rea in l_realizations.order_by( Realization.name ) :
            #Print information of each realization
            rea.get_status(showchunks)
    
    def cancel(self, rea_pattern = False, rea_status = False, hard = False ):
        """
        Delete jobs which status is running or submitted 
        """
        #list of realization of the experiment
        l_realizations  = self._filter_realizations( rea_pattern, rea_status )
        if not ( l_realizations ):
            logging.info( 'There are not realizations to cancel.' )
        else :
            logging.info( 'Canceling Experiment %s' % self.name )
            for rea in l_realizations :
                rea.dryrun = self.dryrun
                rea.cancel( hard )
 
    def release(self):
        """
        Check created job to be release
        """
        l_realizations  = self._filter_realizations( False, Realization.Status.SUBMITTED )
        for rea in l_realizations :
            rea.release( ) 

    def release_wps(self):
        """
        Check created job to be release
        """
        l_realizations  = self._filter_realizations( False, Realization.wps_only_Status.WPS_SUBMITTED, wps_only=True )
        for rea in l_realizations :
            rea.release_wps( ) 

    def statistics(self, rea_pattern = False ):
        """
        Get statistics from the database 
        """
        #list of realization of the experiment
        l_realizations  = self._filter_realizations( rea_pattern, False )
        logging.info( "Job ID;Realization Name;Chunk ID;Resource Name;Execution Time (s);"
                      "Waiting Time (s);REAL Execution Time (s);WRF Execution Time (s)" )
        for rea in l_realizations :
            rea.statistics( )

    def set_priority(self, rea_pattern = False, priority = 0 ):
        """
        Setting priority to jobs 
        """
        #list of realization of the experiment
        l_realizations  = self._filter_realizations( rea_pattern, False )
        if not ( l_realizations ):
            logging.info( 'There are not realizations to set priority.' )
        else :
            logging.info( 'Setting priority Experiment %s' % self.name )
            for rea in l_realizations :
                rea.dryrun = self.dryrun
                rea.set_priority( priority )

    def delete(self):
        """
        Delete the experiment, its realizations and chunks 
        """
        # Delete the local submission directory
        local_exp_dir = join( WRF4G_DIR , 'var' , 'submission' , self.name )
        logging.debug( "Deleting '%s' directory" % local_exp_dir )
        if not self.dryrun :
            if exists( local_exp_dir ) : 
                shutil.rmtree( local_exp_dir )

    @staticmethod 
    def create_files(name, template, force, directory):
        """
        Create the files needed to establish a WRF4G experiment
        """
        validate_name( name )
        template_dir = join( WRF4G_DIR , 'etc' , 'templates' , 'experiments',  template )
        if not exists(template_dir):
            raise Exception( "'%s' template does not exist" % template )
        exp_dir = expandvars( expanduser( os.getcwd() if directory == './' else directory ) )
        if not exists( exp_dir ):
            raise Exception("'%s' does not exist" % exp_dir )
        exp_dir_config = join( exp_dir, name )
        if exists( exp_dir_config ) and not force :
            raise Exception("'%s' already exists" % exp_dir_config )
        elif exists( exp_dir_config ) and force :
            shutil.rmtree( exp_dir_config )
        logging.debug( "Creating '%s' directory" % exp_dir_config )
        shutil.copytree( join( WRF4G_DIR , 'etc' , 'templates' , 'experiments',  template ),
                         exp_dir_config )
        dest_path = join( exp_dir_config , 'experiment.wrf4g' )
        with open( dest_path , 'r') as f :
            data = ''.join( f.readlines( ) )
        data_updated = data % {
                               'WRF4G_EXPERIMENT_HOME' : exp_dir_config ,
                               'WRF4G_DEPLOYMENT_DIR'  : WRF4G_DEPLOYMENT_DIR ,
                               'WRF4G_DIR'             : WRF4G_DIR ,
                               'exp_name'              : name ,
                               }
        with open(dest_path, 'w') as f :
            f.writelines( data_updated )
 
    def update_namelist( self, namelist_input, section ):
        """
        Update namelist values
        """
        nmli = fn.WrfNamelist( namelist_input )
        logging.debug( "Updating parameter 'max_dom' in the namelist" )
        nmli.setValue( "max_dom", int( self.cfg[ section ][ 'max_dom' ] ) )
        max_dom = single = False
        
        # If namelist_values is empty, do not modify namelist variables
        if (len(self.cfg[ section ][ 'namelist_values']) != 0):
            for mnl_variable, mnl_values in self.cfg[ section ][ 'namelist_values' ].items() :
                # Update the namelist per each combination
                logging.debug( "Updating parameter '%s' in the namelist" % mnl_variable )
                # Modify the namelist with the parameters available in the namelist description
                if mnl_variable.startswith( "max_dom:" ) :
                    mnl_variable = mnl_variable[ 8: ]
                    max_dom      = True
                elif mnl_variable.startswith( "single:" ) :
                    mnl_variable = mnl_variable[ 7: ]
                    single       = True
                if '.' in mnl_variable :
                    nml_section, val = mnl_variable.split( '.' )
                else :
                    nml_section, val = "",  mnl_variable
                if max_dom and not val in nmli.MAX_DOM_VARIABLES :
                    nmli.MAX_DOM_VARIABLES.extend( val  )
                if single and val in nmli.MAX_DOM_VARIABLES :
                    nmli.MAX_DOM_VARIABLES.remove( val  )
                try :
                    nmli.setValue( val, coerce_value_list( mnl_values.strip( ',' ).split( ',' ) ), nml_section )
                except IndexError:
                    raise Exception( "'%s' does not have values for all namelist combinations." % mnl_variable )
                except Exception as err:
                    raise Exception( err )
        nmli.trimMaxDom()
        nmli.extendMaxDomVariables()
        if nmli.wrfCheck() :
            raise Exception( "Please review 'namelist_values' variable." )
        if not self.dryrun :
            nmli.overWriteNamelist()

    def _update_member( self, realization_start_date ) :
        """
        If the member is not indicated the initialized month will be the 
        month of the realization
        """
        runtime = []
        for elem in range( self.cfg[ section ] [ 'preprocessor_optargs' ] [ 'member' ].count( ',' ) ) :
            runtime.append( str( realization_start_date.month ) )
        self.cfg[ section ] [ 'preprocessor_optargs' ] [ 'runtime' ] = ','.join( runtime )

    def cycle_time( self, namelist_input, section ) :
        """
        Clycle realization time
        """
        realization_name = self.name + '-' + section.split( "/" )[ 1 ]
        for ( start_date, end_date, simult_interval,
              simult_length, chunk_size, restart_interval ) in self.cfg[ section ][ 'date_time' ] :
            # Update restart_interval in the namelist 
            logging.debug( "Updating parameter 'restart_interval' in the namelist" )
            nmli = fn.WrfNamelist( namelist_input )
            nmli.setValue( "restart_interval", restart_interval )
            if not self.dryrun :
                nmli.overWriteNamelist()
            # Define which calendar is going to be used
            exp_calendar   = Calendar( self.cfg[ section ][ 'calendar' ] )
            rea_start_date = start_date
            while rea_start_date < end_date :
                rea_end_date = exp_calendar.add( rea_start_date, simult_length )
                if rea_end_date > end_date :
                    rea_end_date = end_date
                rea_name = "%s-%s" % ( realization_name, rea_start_date.strftime( "%Y%m%dT%H%M%S" ) )
                logging.info( "---> Realization %s: start date %s end date %s" % (
                               rea_name, rea_start_date, rea_end_date ) )
                # Check realization on the database
                rea = self.check_db( name = rea_name, start_date = rea_start_date, end_date = rea_end_date,
                                     cfg = self.cfg[ section ] )
                # Create chunks only if end date has been modified
                if rea:
                    rea.cycle_chunks()
                elif not rea:
                    # If there is not runtime we have to add the start month of the realization
                    if ( 'preprocessor_optargs' in  self.cfg[ section ] ) and \
                       ( 'member' in self.cfg[ section ] [ 'preprocessor_optargs' ] ) and \
                       (  not 'runtime' in self.cfg[ section ] [ 'preprocessor_optargs' ] ) :
                        self._update_member( rea_start_date )
                    # Create a realization 
                    rea = Realization( )
                    rea.name             = rea_name
                    rea.start_date       = rea_start_date
                    rea.end_date         = rea_end_date
                    rea.chunk_size       = chunk_size
                    rea.current_date     = rea_start_date
                    rea.status           = Realization.Status.PREPARED
                    rea.current_chunk    = 1
                    rea.cfg              = self.cfg[ section ]
                    # Add realization to the experiment 
                    self.realization.append( rea )
                    # Create chunk for the realization
                    rea.cycle_chunks()
                # Check storage
                if not self.dryrun :
                    # Default section will be the current section
                    realization_cfg = dict()
                    realization_cfg[ 'ensemble/default' ] = copy.deepcopy( self.cfg[ section ] )
                    for key, val in self.cfg[ section ].items() :
                        if key.startswith( 'resource' ) :
                            realization_cfg[ key ] = copy.deepcopy( val )
                    save_json( realization_cfg, self.home_directory, "realization.json" )
                    rea._prepare_sub_files()
                rea_start_date = exp_calendar.add( rea_start_date, simult_interval )

    def _copy_experiment_files(self, exp_sub_dir ):
        """
        Copy configure files before submission.
        """    
        for file in [ join( WRF4G_DIR, "etc", "db.conf" ),
                      join( self.home_directory, "experiment.wrf4g" ) ] :
            if not exists( expandvars( file ) ) :
                raise Exception( "'%s' is not available" % file )
            else :
                shutil.copy( expandvars( file ) , exp_sub_dir )

    def _create_wrf4g_bundles(self, exp_sub_dir):
        """
        Create bundles with the necessary software to run WRF on worker nodes.
        """
        # WRF4G bundle
        logging.debug( "Create a WRF4G software bundle to use on the worker node..." )
        wrf4g_package = join ( exp_sub_dir , "WRF4G.tar.gz" )
        if exists( wrf4g_package  ):
            logging.debug( "Removing '%s' package" % wrf4g_package )
            os.remove( wrf4g_package )
        current_path = os.getcwd()
        try :
            tar = tarfile.open( wrf4g_package, "w:gz" )
            # Add wn/bin
            tar.add('%s/data/wn/bin' % (WRF4G_DEPLOYMENT_DIR),arcname='bin')
            # Add python packages to lib/python
            for package in [ 'sqlalchemy','dateutil','wrf4g','fortran_namelist','drm4g']:
                ipackage = __import__(package)
                tar.add(os.path.dirname(ipackage.__file__),arcname='lib/python/%s' % (package) )
            for module in ['six']:
                imodule = __import__(module)
                tar.add('%s/%s.py' %(os.path.dirname(imodule.__file__),module), arcname='lib/python/%s' % ('%s.py' %(module)))
        except Exception as err:
            logging.warn( err )
        finally :
            tar.close()
        # wrf4g_files bundle
        wrf4g_files_dir = join( self.home_directory, 'wrf4g_files' )
        if isdir( wrf4g_files_dir ):
            logging.debug( "Create a wrf4g_files.tar.gz bundle to use on the worker node..." )
            wrf4g_files_package = join ( exp_sub_dir , "wrf4g_files.tar.gz" )
            if exists( wrf4g_files_package ):
                logging.debug( "Removing '%s' package" % wrf4g_files_package )
                os.remove( wrf4g_files_package )
            tar = tarfile.open( wrf4g_files_package , "w:gz" )
            os.chdir( wrf4g_files_dir )
            for elem in os.listdir('.') :
                tar.add( elem )
            tar.close()
        os.chdir( current_path )

    def _filter_realizations(self, pattern, status, wps_only=False):
        """
        Filter realizations from the experiment
        """
        l_realizations = self.realization
        if pattern :
            l_realizations = l_realizations.\
                             filter( Realization.name.like( pattern.replace( '*', '%' )\
                                                                   .replace( '?', '_' ) ) ) 
        if status and not wps_only:
            l_realizations = l_realizations.\
                             filter_by( status = status )
        elif status and wps_only:
            l_realizations = l_realizations.\
                             filter_by( wps_only_status = status )
        return l_realizations
    
class Realization( object ):
    """
    A class to mange WRF4G realizations
    """
    dryrun = False

    # Status = Enumerate( 'PREPARED', 'SUBMITTED','WPS_SUBMITTED', 'RUNNING',
    #                     'PENDING', 'FAILED', 'FINISHED','WPS_FINISHED' )

    Status = Enumerate( 'PREPARED', 'SUBMITTED', 'RUNNING',
                         'PENDING', 'FAILED', 'FINISHED')
    
    wps_only_Status = Enumerate( 'PREPARED', 'WPS_SUBMITTED', 'RUNNING',
                         'PENDING', 'FAILED', 'WPS_FINISHED')
        
    def run(self, first_chunk_run = None , last_chunk_run = None, rerun = False, priority = 0 ):
        """ 
        Run n_chunk of the realization.
        If n_chunk=0 run every chunk of the realization which haven't finished yet
        else run (n_chunk) chunks since the last one finished
        """
        first_chunk_run = int( first_chunk_run ) if first_chunk_run else None 
        last_chunk_run  = int( last_chunk_run  ) if last_chunk_run  else None
        #Check the status of the realization
        if self.status == Realization.Status.FINISHED and not rerun:
            logging.warn( "\tRealization '%s' already finished." % self.name )
        elif ( self.status == Realization.Status.SUBMITTED or 
               self.status == Realization.Status.RUNNING ) and not rerun :
            logging.warn( "\tRealization '%s' has been submitted." % self.name )
        elif first_chunk_run and first_chunk_run < 0 :
            logging.error( "\tERROR: The first chunk to run is '%d'." % first_chunk_run ) 
        elif last_chunk_run and last_chunk_run  < 0 :
            logging.error( "\tERROR: The last chunk to run is '%d'." % last_chunk_run )
        elif ( last_chunk_run and first_chunk_run ) and last_chunk_run < first_chunk_run :
            logging.error( "\tERROR: The last chunk to run is greater than the fist one." )
        elif last_chunk_run and last_chunk_run > self.nchunks :
            logging.error( "\tERROR: The last chunk does not exist." )
        elif first_chunk_run and first_chunk_run > self.nchunks :
            logging.error( "\tERROR: The first chunk does not exist." )
        else :
            # search first chunk to run
            if rerun and first_chunk_run :
                ch                 = self.chunk.filter( Chunk.chunk_id == first_chunk_run ).one()
                self.restart       = ch.start_date
                self.current_date  = ch.start_date
                self.current_chunk = first_chunk_run
            elif rerun and not first_chunk_run :
                self.restart       = None
                first_chunk_run    = self.current_chunk = 1
            else :
                #search first chunk to run
                if not self.restart : # run every chunks of the realization
                    first_chunk_run = 1
                else:
                    #search chunk with end_date>restart and start_date<restart
                    try:
                        first_chunk  = self.chunk.filter( and_( Chunk.start_date <= self.restart, 
                                                                Chunk.end_date   >= self.restart ) 
                                                         ).all()[ -1 ]
                    except : 
                        raise Exception( 'There are not chunks to run.' )
                    else:
                        if first_chunk_run and first_chunk.chunk_id != first_chunk_run :
                            raise Exception( 'Use the option --rerun.' )
                        else : 
                            first_chunk_run = self.current_chunk = first_chunk.chunk_id
            #search last chunk to run
            if not last_chunk_run :
                #run every chunk
                #Search last chunk of the realization
                last_chunk_run = self.nchunks
            else:
                #search last chunk
                last_chunk_run = last_chunk_run
            #Search chunks to run
            l_chunks = self.chunk.filter( and_( Chunk.chunk_id >= first_chunk_run, 
                                                Chunk.chunk_id <= last_chunk_run )
                                        ).all()
            #run chunks
            for index, chunk in enumerate( l_chunks ) :
                #print data of chunks
                logging.info( '\t---> Submitting Chunk %d %s %s' % ( chunk.chunk_id, 
                                                              datetime2datewrf(chunk.start_date), 
                                                              datetime2datewrf(chunk.end_date) ) )
                if not self.dryrun :
                    chunk.run( index, rerun, priority,mode='wrf' )
            if not self.dryrun :
                # Update realizaiton status
                self.status = Realization.Status.SUBMITTED
            
    
    def run_wps(self, first_chunk_run = None , last_chunk_run = None, rerun = False, priority = 0 ):
        """ 
        Run n_chunk of the realization.
        If n_chunk=0 run every chunk of the realization which haven't finished yet
        else run (n_chunk) chunks since the last one finished
        """
        first_chunk_run = int( first_chunk_run ) if first_chunk_run else None 
        last_chunk_run  = int( last_chunk_run  ) if last_chunk_run  else None
        #Check the status of the realization
        #if self.status == Realization.Status.WPS_FINISHED and not rerun:
        #    logging.warn( "\tWPS step of Realization '%s' already finished." % self.name )
        if self.status == Realization.Status.FINISHED and not rerun:
            logging.warn( "\tRealization '%s' already finished." % self.name )
        elif ( self.status == Realization.Status.SUBMITTED or 
               self.status == Realization.Status.RUNNING ) and not rerun :
            logging.warn( "\tRealization '%s' has been submitted." % self.name )
        elif first_chunk_run and first_chunk_run < 0 :
            logging.error( "\tERROR: The first chunk to run is '%d'." % first_chunk_run ) 
        elif last_chunk_run and last_chunk_run  < 0 :
            logging.error( "\tERROR: The last chunk to run is '%d'." % last_chunk_run )
        elif ( last_chunk_run and first_chunk_run ) and last_chunk_run < first_chunk_run :
            logging.error( "\tERROR: The last chunk to run is greater than the fist one." )
        elif last_chunk_run and last_chunk_run > self.nchunks :
            logging.error( "\tERROR: The last chunk does not exist." )
        elif first_chunk_run and first_chunk_run > self.nchunks :
            logging.error( "\tERROR: The first chunk does not exist." )
        else :
            # search first chunk to run
            if not first_chunk_run :
                first_chunk_run = 1
            
            #search last chunk to run
            if not last_chunk_run :
                #run every chunk
                #Search last chunk of the realization
                last_chunk_run = self.nchunks

            #Search chunks to run
            l_chunks = self.chunk.filter( and_( Chunk.chunk_id >= first_chunk_run, 
                                                Chunk.chunk_id <= last_chunk_run )
                                        ).all()
            #run chunks
            for index, chunk in enumerate( l_chunks ) :
                #print data of chunks
                logging.info( '\t---> Submitting Chunk %d %s %s' % ( chunk.chunk_id, 
                                                              datetime2datewrf(chunk.start_date), 
                                                              datetime2datewrf(chunk.end_date) ) )
                if not self.dryrun :
                    chunk.run( index, rerun, priority, mode='wps-only' )
            if not self.dryrun :
                # Update realizaiton status
                self.wps_only_status = Realization.wps_only_Status.WPS_SUBMITTED
    
    def _prepare_sub_files(self):
        """
        Prepare the files needed to submit the realization. 
        """
        rea_submission_path = join( WRF4G_DIR, 'var', 'submission', self.experiment.name, self.name )
        if not isdir( rea_submission_path ) :
            try :
                os.makedirs( rea_submission_path )
            except Exception :
                raise Exception( "Couldn't be created '%s' directory" % rea_submission_path )
        for file_name in [ join( self.experiment.home_directory, "namelist.input" ),
                           join( self.experiment.home_directory, "realization.json" ) ] :
            if not exists( file_name ) :
                raise Exception( "'%s' is not available" % file_name )
            else :
                shutil.copy( file_name , rea_submission_path )

    def check_db(self, rea_id, chunk_start_date, chunk_end_date, chunk_id ):
        """ 
        Check if there is a chunk with the same no reconfigurable field. 
        If there is not a chunk with the same no reconfigurable fields => error,.
        If there is a chunk with the same reconfigurable fields, update data.
        """
        #Check if there is a chunk with the same fields
        try:
            ch = self.chunk.filter( Chunk.rea_id     == rea_id,
                                    Chunk.chunk_id   == chunk_id,
                                    Chunk.start_date == chunk_start_date,
                                    Chunk.end_date   == chunk_end_date,
                                   ).one()
        except Exception :
            #There will be an exception for the last chunk of a realization.
            #This chunk will be able to modify 
            if self.nchunks == chunk_id :
                try :
                    ch2 = self.chunk.filter( Chunk.rea_id     == rea_id,
                                             Chunk.chunk_id   == chunk_id,
                                             Chunk.start_date == chunk_start_date,
                                             Chunk.end_date   != chunk_end_date,
                                           ).one()
                except Exception :
                    return None
                else :
                    logging.debug( '\t\t\tUpdating chunk on the database...' )
                    ch2.end_date = chunk_end_date
                    return ch2
            else :
                return None
        else :
            #if ch exists 
            return ch

    def cycle_chunks(self):
        """
        Create chunks the needed for a realization 
        """
        # Define which calendar is going to be used
        exp_calendar = Calendar( self.cfg[ 'calendar' ] )
        chunk_id = 1
        chunk_start_date = self.start_date
        while chunk_start_date < self.end_date :
            chunk_end_date = exp_calendar.add( chunk_start_date, self.chunk_size )
            if chunk_end_date > self.end_date :
                chunk_end_date = self.end_date
            # Check chunk on the database
            ch = self.check_db( rea_id           = self.id, 
                                chunk_start_date = chunk_start_date, 
                                chunk_end_date   = chunk_end_date,
                                chunk_id         = chunk_id
                                )
            if not ch :
                logging.info( "\t\t---> Chunk %d %s %s" %( chunk_id,
                                                       datetime2datewrf(chunk_start_date),
                                                       datetime2datewrf(chunk_end_date) ) )
                # Create Chunk
                ch = Chunk( )
                ch.rea_id     = self.id
                ch.start_date = chunk_start_date
                ch.end_date   = chunk_end_date
                ch.wps        = 0
                ch.chunk_id   = chunk_id
                ch.status     = Chunk.Status.PREPARED
                # Add realization to the experiment 
                self.chunk.append( ch )
            chunk_start_date = chunk_end_date 
            chunk_id         = chunk_id + 1
        # Set the number of chunks of a relaization    
        self.nchunks = chunk_id - 1

    @staticmethod 
    def status_header(): 
        logging.info( '%-60s %-10s %-10s %-16s %-10s %6s %-3s %6s'% (
                        'REALIZATION','STATUS','CHUNKS','RESOURCE','RUN STATUS',
                        'JID', 'EXT','%' ) )
    @staticmethod 
    def status_chunk_header(): 
        logging.info( '%-60s %-10s %-20s %-20s %-10s %16s'% (
                        'REALIZATION','CHUNK_ID','START','END','WPS','STATUS' ) )
 
    def get_status(self,showchunks=0):
        """ 
        Show information about the realization for example:
            Realization Status    Chunks     Comp.Res       Run.Sta     JID   ext      %
            testc       Finished     3/3   mycomputer      Finished       0     0 100.00
            
        * Realization: Realization name. It is taken from the field experiment_name in experiment.wrf4g.
        * Status: It can be take the following values: Prepared, Submitted, Running, Failed and Done).
        * Chunks [Chunk running/Total Chunks]: A realization is split into chunks. Each chunk is sent as a job.
        * Computer resource: Resource (cluster) where the job is running.
        * Run.Sta: Job status in the WN (Downloading data, running ungrib, real, wrf, ...)
        * JID: Job identifier
        * ext: Exit Code. If exit code is different from 0, there has been an error.
        * % : percentage of simulation finished.
        """                
        #Select parameters:job_status,rea_status,resource,exitcode,nchunks
        #Last job of current chunk

        if not showchunks:
            resource, exitcode, gw_job = '-', '-', '-'
            status = Realization.Status.PREPARED
            if self.status != Realization.Status.PREPARED :
                ch = self.chunk.filter_by( chunk_id = self.current_chunk ).one()
                try :
                    last_job = ch.job[ -1 ]
                except :
                    pass
                else :
                    resource = last_job.resource
                    exitcode = last_job.exitcode
                    status   = last_job.status
                    gw_job   = last_job.gw_job
            chunk_distribution = '%d/%d' % ( 0 if not self.current_chunk else self.current_chunk, self.nchunks )
            #Format chunks run / chunks total
            runt   = int( self.current_date.strftime("%s") ) - int( self.start_date.strftime("%s") ) 
            totalt = int( self.end_date.strftime("%s") )     - int( self.start_date.strftime("%s") )
            #Percentage
            per = runt * 100.0 / totalt
            #Print output
            logging.info( "%-60s %-10.10s %-10.10s %-16.16s %-10.10s %6.6s %-3.3s %6.2f" % (
                        self.name, self.status, chunk_distribution, resource, status, 
                        gw_job, exitcode, per ) )

        else:
            for ch in self.chunk:
                logging.info( "%-60s %-10s %-20s %-20s %-10s %16s"% (
                    self.name, ch.chunk_id, ch.start_date, ch.end_date, ch.wps, ch.status ) )


    def information( self ) :
        """
        Get information about all realization values.
        """
        logging.info( "Start date: %s" % self.start_date )
        logging.info( "End date: %s" % self.end_date  )
        logging.info( "Current date: %s" % self.current_date )
        logging.info( "Chunk size: %s" % self.chunk_size )
        logging.info( "Number of chunks: %s" % self.nchunks )
        logging.info( "Status: %s" % self.status )
        logging.info( "Configuration: " )
        for key, val in self.cfg.items() :
           if 'date_time' in key : 
               continue
           elif 'app' in key :
               logging.info( "\t%s: %s" % ( key, val.replace( '\n', '\n\t\t' ) ) )
           elif type( val ) == dict :
               logging.info( "\t%s" % key )
               for key2, val2 in val.items() :
                   logging.info( "\t\t%s: %s" % ( key2, val2 ) )
           elif type( val ) == list or type( val ) == tuple :
               logging.info( "\t%s: %s" % ( key, ' '.join( val ) ) )
           else :
               if 'app' in key : val
               logging.info( "\t%s: %s" % ( key, val ) )
  
    def get_log( self, chunk_id , directory ) :
        """
        Search and unpack log files. 
        """
        tar_path      = join( WRF4G_DIR, 'var', 'submission', self.experiment.name, self.name )
        all_tar_files = glob.glob( join( tar_path, 'log_%s_*.tar.gz' % chunk_id ) ) 
        if not all_tar_files :
            raise Exception( 'There is not a log available for this chunk.' )
        for tar_file in all_tar_files :
            logging.info( "Unpacking %s file in the %s directory" % ( tar_file, directory ) )
            extract( tar_file, expandvars( expanduser( directory ) ) )

    def cancel(self, hard = False ):
        """
        Delete chunks which status is running or submitted.
        """
        l_chunks = self.chunk.filter( or_( Chunk.status == Chunk.Status.SUBMITTED,
                                           Chunk.status == Chunk.Status.PENDING,
                                           Chunk.status == Chunk.Status.FAILED,
                                           Chunk.status == Chunk.Status.RUNNING ) 
                                    ).all()
        logging.info( '---> Canceling Realization %s' % self.name )
        if not ( l_chunks ):
            logging.info( '\tThere are not chunks to cancel.' )
        else :
            for chunk in l_chunks :
                chunk.dryrun = self.dryrun
                chunk.cancel( hard )

    def release(self):
        """
        Check created job to be release
        """
        current_ch = self.chunk.filter( and_( Chunk.chunk_id == self.current_chunk,
                                              Chunk.status   == Chunk.Status.SUBMITTED )
                                      ).first()
        if current_ch :
            l_jobs     = current_ch.job.filter( Job.status == Job.Status.SUBMITTED )
            try :
                job = l_jobs[ -1 ]
            except :
                pass
            else :
                logging.debug( "Releasing job %s" % job.gw_job )
                GWJob().release( job.gw_job  )
                job.set_status( Job.Status.RELEASED )
    
    def release_wps(self):
        """
        Check created job to be release
        """
        current_ch = self.chunk.filter(Chunk.status==Chunk.Status.WPS_SUBMITTED).all()
        for chunk in current_ch:
            l_jobs     = chunk.job.filter( Job.status == Job.Status.SUBMITTED )
            try :
                job = l_jobs[ -1 ]
            except :
                pass
            else :
                logging.debug( "Releasing job %s" % job.gw_job )
                GWJob().release( job.gw_job  )
                job.set_status( Job.Status.RELEASED )


    def statistics(self):
        """
        Get statistics from jobs.
        """
        for chunk in self.chunk.all() :
            if chunk.job.all() [ -1 ].status == Job.Status.FINISHED :
                finished_job   = chunk.job.filter_by( status = Job.Status.FINISHED ) [ -1 ]
                date_SUBMITTED = finished_job.events.filter_by( job_status = Job.Status.SUBMITTED ).one().timestamp
                date_RUNNING   = finished_job.events.filter_by( job_status = Job.Status.RUNNING ).one().timestamp
                date_REAL      = finished_job.events.filter_by( job_status = Job.Status.REAL ).one().timestamp
                date_WRF       = finished_job.events.filter_by( job_status = Job.Status.WRF ).one().timestamp
                date_FINISHED  = finished_job.events.filter_by( job_status = Job.Status.FINISHED ).one().timestamp
                logging.info( "%d;%d;%s;%s;%d;%d;%d;%d" % ( finished_job.gw_job, chunk.chunk_id, self.name, finished_job.resource,
                                                        timedelta_total_seconds( date_FINISHED - date_RUNNING ), 
                                                        timedelta_total_seconds( date_RUNNING  - date_SUBMITTED ),
                                                        timedelta_total_seconds( date_WRF      - date_REAL ),
                                                        timedelta_total_seconds( date_FINISHED - date_WRF ) ) )

    def set_priority(self, priority = 0 ):
        """
        Setting priority to chunks which status is submitted.
        """
        if priority < 0 or priority > 20 :
            raise Exception( "'%d' priority is out of the range [0, 20]" % priority )      
        l_chunks = self.chunk.filter_by( status = Chunk.Status.SUBMITTED ).all()
        logging.info( '---> Setting priority Realization %s' % self.name )
        if not ( l_chunks ):
            logging.info( '\tThere are not chunks to set priority.' )
        else :
            for chunk in l_chunks :
                chunk.dryrun = self.dryrun
                chunk.set_priority( priority )
   
    def get_restart(self):
        """
        Get restart date.
        """
        logging.info( datetime2datewrf( self.restart ) )
 
    def set_restart(self, restart_date ):
        """
        Setting restart date.
        """
        try :
            datetime_restart_date = datewrf2datetime( restart_date ) 
        except :
            raise Exception( "ERROR: restart date is malformed" )
        else :
            logging.info( '---> Setting restart date %s' % datetime2datewrf( datetime_restart_date ) )
            self.restart = datetime_restart_date
 
class Chunk( object ):
    """ 
    A class to manage WRF4G chunks
    """
    dryrun = False 

    Status = Enumerate( 'PREPARED', 'SUBMITTED','WPS_SUBMITTED', 'RUNNING',
                        'PENDING', 'FAILED', 'FINISHED','WPS_FINISHED' )
 
    #METHODS
    def run (self, index, rerun = False, priority = 0, mode='wrf' ):
        """ 
        Run a chunk is run a drm4g job
        """
        #Send a gridway's job and save data in table Job
        gw_job        = GWJob()
        # create template
        rea_name      = self.realization.name
        exp_name      = self.realization.experiment.name
        exp_path      = join( WRF4G_DIR, 'var', 'submission', exp_name )
        rea_path      = join( exp_path, rea_name )
        wrf4g_package = join( exp_path, "WRF4G.tar.gz" )
        if not exists(  wrf4g_package ) : 
            raise Exception( "'%s' file does not exist" % wrf4g_package )
        # files to add for the inputsandbox 
        inputsandbox  = "file://%s,"                  % wrf4g_package
        inputsandbox += "file://%s/db.conf,"          % exp_path
        inputsandbox += "file://%s/experiment.wrf4g," % exp_path
        inputsandbox += "file://%s/realization.json," % rea_path  
        inputsandbox += "file://%s/namelist.input"    % rea_path  
        # Add input file if it is exist
        input_files = join( exp_path, 'wrf4g_files.tar.gz' )
        if exists( input_files ):
            inputsandbox += ",file://%s" % ( input_files )
        # files to add for the outputsandbox
        outputsandbox = "log_%d_${JOB_ID}.tar.gz, events.pkl" % self.chunk_id
        arguments = '%s %s %d %s %s %d %s' % ( exp_name, rea_name, self.chunk_id,
                                            datetime2datewrf( self.start_date ),
                                            datetime2datewrf( self.end_date ),
                                            1 if rerun else 0,
                                            mode
                                             )
        # Create the job template
        file_template = gw_job.create_template( name          = rea_name,
                                                directory     = rea_path,
                                                arguments     = arguments,
                                                np            = int( self.realization.cfg.get( 'np', '1' ) ),
                                                req           = self.realization.cfg.get( 'requirements', '' ),
                                                environ       = self.realization.cfg.get( 'environment', '' ) ,
                                                inputsandbox  = inputsandbox,
                                                outputsandbox = outputsandbox )
        # Submit the template
        job = Job()  #create an object "job"
        time.sleep( 0.1 )
        # if the first chunk of the realization
        if index == 0 or mode == 'wps-only':
            job.gw_job    = gw_job.submit( priority = priority, file_template = file_template )
        else:
            # if the chunk is not the first of the realization, 
            # gwsubmit has an argument, gw_job of the job before
            chunk_before_id = self.chunk_id - 1
            chunk_before    = self.realization.chunk.\
                              filter( Chunk.chunk_id == chunk_before_id ).one()
            job_before      = chunk_before.job.order_by( Job.id )[-1]
            id_job_before   = job_before.id          
            gw_job_before   = job_before.gw_job
            job.gw_job      = gw_job.submit( dep = gw_job_before, priority = priority,
                                             file_template = file_template )
        job.chunk_id = self.chunk_id
        job.run( rerun ) 
        self.job.append( job )

        # Update realization status
        if mode=='wrf':
            self.status = Chunk.Status.SUBMITTED
        else:
            self.status=Chunk.Status.WPS_SUBMITTED

    def cancel(self, hard = False ):
        """
        Delete jobs
        """
        logging.info('\t---> Canceling Chunk %d %s %s' % ( self.chunk_id,
                                                           datetime2datewrf(self.start_date),
                                                           datetime2datewrf(self.end_date) ) )
        l_jobs = self.job.filter( and_( Job.status != Job.Status.PREPARED, 
                                        Job.status != Job.Status.FINISHED,
                                        Job.status != Job.Status.FAILED,
                                        Job.status != Job.Status.CANCEL ) 
                                ).all()
        if not ( l_jobs ):
            logging.info( '\t\tThere are not jobs to cancel.' )
        else :
            for job in l_jobs :
                job.dryrun = self.dryrun
                job.cancel( hard )

    def set_priority(self, priority = 0 ):
        """
        Setting priority to jobs
        """
        logging.info('\t---> Setting priority Chunk %d %s %s' % ( self.chunk_id,
                                                                  datetime2datewrf(self.start_date),
                                                                  datetime2datewrf(self.end_date) ) )
        l_jobs = self.job.filter_by( status = Job.Status.SUBMITTED ).all()
        if not ( l_jobs ):
            logging.info( '\t\tThere are not jobs to set priority.' )
        else :
            for job in l_jobs :
                job.dryrun = self.dryrun
                job.set_priority( priority )
 
class JobCodeError():
    LOG_PATH             = 1
    COPY_APP             = 2
    APP_ERROR            = 3
    SOURCE_SCRIPT        = 4
    JOB_SHOULD_NOT_RUN   = 5
    LOCAL_PATH           = 6
    BINARY               = 7
    COPY_RST_FILE        = 8
    RESTART_MISMATCH     = 9
    COPY_FILE            = 10
    COPY_REAL_FILE       = 11
    COPY_BOUND           = 12
    NAMELIST_FAILED      = 13
    PREPROCESSOR_FAILED  = 14
    LINK_GRIB_FAILED     = 15
    UNGRIB_FAILED        = 16
    UNGRIB_PROCESSOR_FAILED  = 31
    METGRID_FAILED       = 17
    REAL_FAILED          = 18
    COPY_UPLOAD_WPS      = 19
    WRF_FAILED           = 20
    POSTPROCESSOR_FAILED = 21
    COPY_OUTPUT_FILE     = 22
   
class Job( object ):
    """
    A class to manage WRF4G jobs
    """
    dryrun = False

    Status = Enumerate( 'UNKNOWN', 'PREPARED', 'SUBMITTED','WPS_SUBMITTED', 'RELEASED', 'RUNNING', 'PENDING', 
                        'CANCEL', 'FAILED', 'FINISHED','WPS_FINISHED', 'CREATE_OUTPUT_PATH', 
                        'CONF_APP', 'DOWN_RESTART', 'DOWN_WPS', 'DOWN_BOUND', 'UNGRIB', 
                        'METGRID', 'REAL', 'UPLOAD_WPS', 'ICBCPROCESOR', 'WRF' )

    CodeError = JobCodeError()

    def set_status(self, status, timestamp = False ):
        """ 
        Save the status of the job and if it is a jobstatus of CHUNK STATUS and REALIZATION STATUS,
        change the status of the Chunk and the Realization and add an event. 
        """
        #Save job's status
        self.status = status
        if status == Job.Status.CANCEL :
            self.chunk.status = Chunk.Status.PREPARED
            self.chunk.realization.status = Realization.Status.PREPARED
        #if it is an status of the CHUNK STATUS 
        elif status in Chunk.Status and status != Chunk.Status.SUBMITTED :
            self.chunk.status = status
            #if it is an status of the REALIZATION STATUS 
            if status in Realization.Status and status != Realization.Status.SUBMITTED :
                # Last chunk has finished
                if ( status == Job.Status.FINISHED and \
                     self.chunk.chunk_id == self.chunk.realization.nchunks ) :
                    self.chunk.realization.status = Realization.Status.FINISHED
                # Finished chunk other than last
                elif ( status == Job.Status.FINISHED and \
                       self.chunk.realization.status != Realization.Status.FINISHED and \
                       self.chunk.realization.status != Realization.Status.FAILED ) :
                    self.chunk.realization.current_chunk = self.chunk.chunk_id + 1                    
                    self.chunk.realization.status = Realization.Status.SUBMITTED
                elif status != Job.Status.FINISHED :
                    self.chunk.realization.status = status
        #Add event
        events            = Events()
        events.job_status = status
        if not timestamp :
            events.timestamp  = datetime.utcnow()
        else :
            events.timestamp  = timestamp
        self.events.append( events ) 
        
    def run(self, rerun = False):
        """ 
        Send a gridway's job and save data in table Job
        id_chunk is an instance of Chunk
        """
        if not self.gw_restarted :
            self.gw_restarted = 0
        #Insert values default
        self.resource = '-'
        self.exitcode = '-'
        # Update status
        self.set_status( Job.Status.SUBMITTED ) 
   
    def cancel(self, hard = False ):
        """
        Delete a job
        """
        logging.info('\t\t---> Canceling Job %d' % self.gw_job ) 
        if not self.dryrun :
            GWJob().kill( self.gw_job, hard )
            self.set_status( Job.Status.CANCEL )

    def set_priority(self, priority = 0 ):
        """
        Set job priority
        """
        logging.info('\t\t---> Setting priority Job %d' % self.gw_job )
        if not self.dryrun :
            GWJob().set_priority( self.gw_job, priority )

class Events( object ) :
    """
    A class to manage job events
    """
    pass


