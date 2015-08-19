from __future__              import with_statement
import sys
import re
import time
import threading
import logging

from os.path                 import join, dirname
from string                  import Template
from Queue                   import Queue
from drm4g.utils.rsl2        import Rsl2Parser
from drm4g.utils.list        import List 
from drm4g.core.configure    import Configuration
from drm4g.utils.dynamic     import ThreadPool
from drm4g.utils.message     import Send

from wrf4g.db                import get_session
from wrf4g.core              import Job

__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: em_mad.py 2352 2015-02-24 10:23:57Z carlos $"

class GwEmMad (object):
    """
    Execution manager MAD 

    GridWay uses a Middleware Access Driver (MAD) module to submit,
    control and monitor the execution of jobs.

    The format to send a request to the Execution MAD, through its 
    standard input, is:    
    OPERATION JID HOST/JM RSL

	Where:

    -OPERATION: Can be one of the following:
        -INIT: Initializes the MAD (i.e. INIT - - -).
        -SUBMIT: Submits a job(i.e. SUBMIT JID HOST/JM RSL).
        -POLL: Polls a job to obtain its state (i.e. POLL JID - -).
	-CANCEL: Cancels a job (i.e. CANCEL JID - -).
	-FINALIZE:Finalizes the MAD (i.e. FINALIZE - - -).
    -JID: Is a job identifier, chosen by GridWay.
    -HOST: If the operation is SUBMIT, it specifies the resource contact 
        to submit the job. Otherwise it is ignored.
    -JM: If the operation is SUBMIT, it specifies the job manager to submit 
        the job. Otherwise it is ignored.
    -RSL: If the operation is SUBMIT, it specifies the resource specification 
        to submit the job. Otherwise it is ignored.

    The format to receive a response from the MAD, through its standard output, is:

    OPERATION JID RESULT INFO

         Where:

    -OPERATION: Is the operation specified in the request that originated 
        the response or CALLBACK, in the case of an asynchronous notification 
        of a state change.
    -JID: It is the job identifier, as provided in the submission request.
    -RESULT: It is the result of the operation. Could be SUCCESS or FAILURE
    -INFO: If RESULT is FAILURE, it contains the cause of failure. Otherwise, 
        if OPERATION is POLL or CALLBACK,it contains the state of the job.
    """
    logger  = logging.getLogger(__name__)
    message = Send()

    def __init__(self):
        self._callback_interval = 30 #seconds
        self._max_thread        = 10
        self._min_thread        = 3
        self._job_list          = List()
        self._configure         = None  
        self._communicators     = dict()
        self._lock              = threading.Lock()
	        
    def do_INIT(self, args):
        """
        Initializes the MAD (i.e. INIT - - -)
        @param args : arguments of operation
        @type args : string
        """
        out = 'INIT - SUCCESS -'
        self.message.stdout( out )
        self.logger.debug( out )
  
    def do_SUBMIT(self, args):
        """
        Submits a job(i.e. SUBMIT JID HOST/JM RSL).
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, JID, HOST_JM, RSL = args.split()
        try:
            HOST, JM = HOST_JM.rsplit('/',1)
            # Init Job class
            job , communicator = self._update_resource( HOST )
            job.Communicator   = communicator
            # Parse rsl
            rsl                = Rsl2Parser(RSL).parser()
            
            if job.resfeatures.has_key( 'project' ) :
                rsl['project']      = job.resfeatures[ 'project' ]
            
            if job.resfeatures.has_key( 'parallel_env' ) :
                rsl['parallel_env'] = job.resfeatures[ 'parallel_env' ]
                
            if job.resfeatures.has_key( 'local_scratch' ) :
                rsl['environment']['WRF4G_LOCALSCP'] = job.resfeatures[ 'local_scratch' ]
             
            if job.resfeatures.has_key( 'vo' ) :
                _ , host                    = HOST.split('::')
                job.resfeatures['host']     = host
                job.resfeatures['jm']       = JM
                job.resfeatures['env_file'] = join( dirname(RSL) , "job.env" )
                job.resfeatures['queue']    = rsl[ 'queue' ]
            # Update remote directories
            ABS_REMOTE_JOBS_DIR   = job.get_abs_directory( job.resfeatures[ 'scratch' ] )
            for key in [ "stdout" , "stderr" , "executable" ] :
                rsl[key] = join( ABS_REMOTE_JOBS_DIR , rsl[key] )
            # Create and copy wrapper_drm4g file 
            gw_restarted  = RSL.split( '.' )[ -1 ]
            local_file    = join ( dirname ( RSL ) , "wrapper_drm4g.%s" % gw_restarted )
            remote_file   = join ( dirname ( rsl[ 'executable' ] ) , 'wrapper_drm4g' )
            job.createWrapper( local_file , job.jobTemplate( rsl ) )
            job.copyWrapper( local_file , remote_file )
            # Execute wrapper_drm4g 
            job.JobId = job.jobSubmit( remote_file )
            # Connect with the database to update resource and gw_restarted
            try :
                session                = get_session()
                query_job              = session.query(Job).filter( Job.gw_job == JID ).order_by( Job.id ).all()[-1]
                query_job.resource     = HOST
                query_job.gw_restarted = gw_restarted
                session.commit()
            except Exception , err :
                session.rollback()
                logger.error( str( err ) )
            finally:
                session.close()
            self._job_list.put( JID , job )
            out = 'SUBMIT %s SUCCESS %s:%s' % ( JID , HOST , job.JobId )
        except Exception, err:
            out = 'SUBMIT %s FAILURE %s' % ( JID , str( err ) )
        self.message.stdout(out)
        self.logger.debug(out , exc_info=1 )

    def do_FINALIZE(self, args):
        """
        Finalizes the MAD (i.e. FINALIZE - - -).
        @param args : arguments of operation
        @type args : string
        """
        out = 'FINALIZE - SUCCESS -'
        self.message.stdout( out )
        self.logger.debug( out ) 
        sys.exit( 0 )    
    
    def do_POLL(self, args):
        """
        Polls a job to obtain its state (i.e. POLL JID - -).
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, JID, HOST_JM, RSL = args.split()
        try:
            if self._job_list.has_key( JID ) :
                job    = self._job_list.get( JID )
                status = job.getStatus( )
                out = 'POLL %s SUCCESS %s' % ( JID , status )
            else:
                out = 'POLL %s FAILURE Job not submitted' % (JID) 
        except Exception, err:
            out = 'POLL %s FAILURE %s' % ( JID , str( err ) )
        self.message.stdout( out )
        self.logger.debug( out )
        
    def do_RECOVER(self, args):
        """
        Polls a job to obtain its state (i.e. RECOVER JID - -).
        @param args : arguments of operation
        @type args : string 
        """
        OPERATION, JID, HOST_JM, RSL = args.split()
        try:
            HOST, remote_job_id = HOST_JM.split( ':' , 1 )
            job , communicator  = self._update_resource( HOST )            
            job.Communicator    = communicator
            job.JobId           = remote_job_id
            job.refreshJobStatus( )
            self._job_list.put( JID , job )
            out = 'RECOVER %s SUCCESS %s' % ( JID , job.getStatus( ) )
        except Exception, err:
            out = 'RECOVER %s FAILURE %s' % ( JID , str( err ) )    
        self.message.stdout(out)
        self.logger.debug(out , exc_info=1 )
            
    def do_CALLBACK(self):
        """
        Show the state of the job
        """
        states = {
                  "PENDING"  : "PENDING" ,
                  "ACTIVE"   : "RUNNING" ,
                  "DONE"     : "FINISHED" ,
                  "FAILED"   : "FAILED" ,
                 } 
        avoid_states = ( 'CANCEL', 'CREATE_OUTPUT_PATH', 'CONF_APP',
                         'DOWN_RESTART', 'DOWN_WPS', 'DOWN_BOUND', 'UNGRIB', 'METGRID', 
                         'REAL', 'UPLOAD_WPS', 'ICBCPROCESOR', 'WRF', 'FINISHED', 'FAILED' ) 
        while True:
            time.sleep( self._callback_interval )
            self.logger.debug( "CALLBACK new iteration ..." )
            for JID , job  in self._job_list.items( ):
                try:
                    oldStatus = job.getStatus( )
                    job.refreshJobStatus( )
                    newStatus = job.getStatus( )
                    if oldStatus != newStatus :
                        if newStatus != 'DONE' :
                            # Connect with the database to update the status of the job
                            try :
                                session = get_session()
                                q_job   = session.query( Job ).\
                                          filter( Job.gw_job == JID ).\
                                          order_by( Job.id ).all()[-1]
                                self.logger.info( states[ newStatus ]  )
                                if not q_job.status in avoid_states :
                                    q_job.set_status( states[ newStatus ] )
                                    session.commit()
                            except Exception , err :
                                session.rollback()
                                self.logger.error( str( err ) )
                            finally :
                                session.close()
                        if ( newStatus == 'DONE' or newStatus == 'FAILED' ) :
                            self._job_list.delete(JID)
                        time.sleep ( 0.1 )
                        out = 'CALLBACK %s SUCCESS %s' % ( JID, newStatus )
                        self.message.stdout( out )
                        self.logger.debug( out )
                except Exception, err:
                    out = 'CALLBACK %s FAILURE %s' % ( JID , str( err ) )
                    self.message.stdout( out )
                    self.logger.debug( out, exc_info=1 )
        
    def do_CANCEL(self, args):
        """
        Cancels a job (i.e. CANCEL JID - -).
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, JID, HOST_JM, RSL = args.split()
        try:
            if self._job_list.has_key(JID):
                self._job_list.get(JID).jobCancel()
                out = 'CANCEL %s SUCCESS -' % (JID)
            else:
                out = 'CANCEL %s FAILURE Job not submitted' % (JID)
        except Exception, e:
            out = 'CANCEL %s FAILURE %s' % (JID, str(e))    
        self.message.stdout(out)
        self.logger.debug(out)
        
    methods = {'INIT'    : do_INIT,
               'SUBMIT'  : do_SUBMIT,
               'POLL'    : do_POLL,
               'RECOVER' : do_RECOVER,
               'CANCEL'  : do_CANCEL,
               'FINALIZE': do_FINALIZE}

    def processLine(self):
        """
        Choose the OPERATION through the command line
        """
        try:
            worker = threading.Thread(target = self.do_CALLBACK, )
            worker.setDaemon(True)
            worker.start()
            self._configure = Configuration()
            pool = ThreadPool(self._min_thread, self._max_thread)
            while True:
                input = sys.stdin.readline().split()
                self.logger.debug(' '.join(input))
                OPERATION = input[0].upper()
                if len(input) == 4 and self.methods.has_key(OPERATION):
                    if OPERATION == 'FINALIZE' or OPERATION == 'INIT' or OPERATION == 'SUBMIT' \
                        or OPERATION == 'RECOVER':
                        self.methods[OPERATION](self, ' '.join(input))
                    else:
                        pool.add_task(self.methods[OPERATION], self, ' '.join(input))    
                else:
                    out = 'WRONG COMMAND'
                    self.message.stdout(out)
                    self.logger.debug(out)
        except Exception, err:
            self.logger.warning( str( err ) )
    
    def _update_resource(self, host):
        with self._lock :
            if not self._configure.check_update() or not self._configure.resources : 
                self._configure.load()
                errors = self._configure.check()
                if errors :
                    self.logger.error ( ' '.join( errors ) )
                    raise Exception ( ' '.join( errors ) )
            for resname, resdict in self._configure.resources.iteritems() :
                if '::' in host :
                    _resname , _ = host.split( '::' )
                    if resname != _resname :
                        continue
                elif resname != host : 
                    continue
                if not self._communicators.has_key( resname ) :
                    self._communicators[ resname ] = self._configure.make_communicators()[resname]
                job          = self._configure.make_resources()[ resname ]['Job']
                communicator = self._communicators[ resname ]
                return job , communicator

  
