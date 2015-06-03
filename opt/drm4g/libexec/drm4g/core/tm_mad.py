from __future__            import with_statement
import sys
import threading
import os
import re
import time
import logging
from drm4g.utils.url       import urlparse
from drm4g.utils.dynamic   import ThreadPool
from drm4g.core.configure  import Configuration
from drm4g.utils.message   import Send

from wrf4g.db              import get_session
from wrf4g.core            import Job

__version__  = '2.3.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: tm_mad.py 2352 2015-02-24 10:23:57Z carlos $"


class GwTmMad (object):
    """
    Transfer manager MAD
    
    The File Transfer Driver interfaces with Grid Data Management Services
    and is responsible for file staging, remote working directory set-up 
    and remote host clean up.
   
    The format to send a request to the Transfer MAD, through its standard
    input, is:
    
    OPERATION JID TID EXE_MODE SRC_URL DST_URL
   
    Where:
   
    -OPERATION: Can be one of the following:
        -INIT: Initializes the MAD, JID should be max number of jobs.
        -START: Init transfer associated with job JID
        -END: Finish transfer associated with job JID
        -MKDIR: Creates directory SRC_URL
        -RMDIR: Removes directory SRC_URL
        -CP: start a copy of SRC_URL  to DST_URL, with identification TID,
            and associated with job JID.
        -FINALIZE: Finalizes the MAD.
    -JID: Is a job identifier, chosen by GridWay.
    -TID: Transfer identifier, only relevant for command CP.
    -EXE_MODE: If equal to 'X' file will be given execution permissions,
        only relevant for command CP.
   
    The format to receive a response from the MAD, through its standard
    output, is:
    
    OPERATION JID TID RESULT INFO
   
    Where:
   
    -OPERATION: Is the operation specified in the request that originated
        the response or CALLBACK, in the case of an asynchronous notification 
        of a state change.
    -JID: It is the job identifier, as provided in the START request.
    -TID: It is the transfer identifier, as provided in the CP request.
    -RESULT: It is the result of the operation. Could be SUCCESS or FAILURE.
    -INFO: If RESULT is FAILURE, it contains the cause of failure.
    
    """
    
    logger  = logging.getLogger(__name__)
    message = Send()
    
    def __init__(self):
        self._max_thread   = 30
        self._min_thread   = 5
        self._lock         = threading.Lock()
        self._communicator = dict ()
        self._configure    = None
    
    def do_INIT(self, args):
        """
        INIT: Initializes the MAD, JID should be max number of jobs.
        (i.e. INIT JID - - - -)
        @param args : arguments of operation
        @type args : string 
        """
        out = 'INIT - - SUCCESS -'
        self.message.stdout( out )
        self.logger.debug( out )

    def do_START(self, args):
        """
        START: Init transfer associated with job JID.(i.e. START JID - - - -)
        @param args : arguments of operation
        @type args : string 
        """
        out = 'START %s - SUCCESS -' % ( args.split( ) [ 1 ] )    
        self.message.stdout( out )
        self.logger.debug( out )
        
    def do_END(self, args):
        """
        END: Finish transfer associated with job JID .(i.e. END JID - - - -)
        @param args : arguments of operation
        @type args : string 
        """
        out = 'END %s - SUCCESS -' % ( args.split( ) [ 1 ] )
        self.message.stdout( out )
        self.logger.debug( out )
  
    def do_FINALIZE(self, args):
        """
        Finalizes the MAD (i.e. FINALIZE - - - - -)
        @param args : arguments of operation
        @type args : string
        """
        out = 'FINALIZE %s - SUCCESS -' % ( args.split( ) [ 1 ] )
        self.message.stdout( out )
        self.logger.debug( out )
        sys.exit( 0 )
 
    def do_MKDIR(self, args):
        """
        MKDIR: Creates directory SRC_URL (i.e. MKDIR JID - - SRC_URL -)
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, JID, TID, EXE_MODE, SRC_URL, DST_URL = args.split()
        try:
            com = self._update_com( urlparse( SRC_URL ).host )
            com.rmDirectory( SRC_URL )
            com.mkDirectory( SRC_URL )
            out = 'MKDIR %s - SUCCESS -' % ( JID )
        except Exception, err :
            out = 'MKDIR %s - FAILURE %s' % ( JID , str( err ) )
        self.message.stdout( out )
        self.logger.debug( out , exc_info=1 )
        
    def do_RMDIR(self, args):
        """
        RMDIR: Removes directory SRC_URL (i.e. RMDIR JID - - SRC_URL -)
        @param args : arguments of operation
        @type args : string 
        """
        OPERATION, JID, TID, EXE_MODE, SRC_URL, DST_URL = args.split()
        try:
            com = self._update_com( urlparse( SRC_URL ).host )
            if not com.checkOutLock( SRC_URL ) :            
                com.rmDirectory( SRC_URL )
            out = 'RMDIR %s - SUCCESS -' % ( JID )
        except Exception , err :
            out = 'RMDIR %s - FAILURE %s' % ( JID , str( err ) )
        self.message.stdout( out )        
        self.logger.debug( out, exc_info=1 )
    
    def do_CP(self, args):
        """
        CP: start a copy of SRC_URL  to DST_URL, with identification TID,
        and associated with job JID.(i.e. CP JID TID - SRC_URL DST_URL)
        @param args : arguments of operation
        @type args : string 
        """
        OPERATION, JID, TID, EXE_MODE, SRC_URL, DST_URL = args.split()
        if 'file:' in SRC_URL:
            url = DST_URL
        else:
            url = SRC_URL
        try:
            com = self._update_com( urlparse( url ).host )
            com.copy( SRC_URL , DST_URL , EXE_MODE )
            out = 'CP %s %s SUCCESS -' % ( JID , TID )
        except Exception, err :
            self.logger.warning( 'Error copying from %s to %s : %s' %( SRC_URL , DST_URL, str( err ) ) )
            time.sleep( 60 )
            try:
                self.logger.debug( 'Copying again from %s to %s' % ( SRC_URL , DST_URL ) )
                com = self._update_com( urlparse( SRC_URL ).host )
                com.copy( SRC_URL , DST_URL , EXE_MODE )
                out = 'CP %s %s SUCCESS -' % (JID, TID)
            except Exception, err :
                out = 'CP %s %s FAILURE %s' % ( JID , TID , str( err ) ) 
        finally:
            try:
                if 'stdout.wrapper' in DST_URL : 
                    # Connect with the database to update the exitcode
                    with open( DST_URL[7:], 'r') as file :
                        lines = file.readlines()  
                    all_lines = ''.join( lines )
                    re_exit_status = re.compile( "EXIT_STATUS=(-?\d*)" )
                    mo = re_exit_status.search(all_lines)
                    if mo :
                        try :
                            session   = get_session()
                            query_job = session.query(Job).\
                                        filter( Job.gw_job == JID ).\
                                        order_by( Job.id ).all()[-1] 
                            if query_job.exitcode == '-' :
                                exit_code = mo.group( 1 ) 
                                query_job.exitcode = exit_code
                                if exit_code == '0' :
                                    query_job.chunk.realization.current_date = query_job.chunk.realization.end_date
                                session.commit()
                        except Exception , err :
                            session.rollback()
                            self.logger.error( str( err ) )
                        finally:
                            session.close()  
            except Exception, err :
                self.logger.warning( err )
        self.message.stdout( out )
        self.logger.debug(out)
        
    methods = {'INIT'    : do_INIT,
               'START'   : do_START,
               'END'     : do_END,
               'MKDIR'   : do_MKDIR,
               'RMDIR'   : do_RMDIR,
               'CP'      : do_CP,
               'FINALIZE': do_FINALIZE}

    def processLine(self):
        """
        Choose the OPERATION through the command line
        """
        try:
            pool = ThreadPool( self._min_thread , self._max_thread )
            self._configure = Configuration()
            while True:
                input = sys.stdin.readline().split()
                self.logger.debug(' '.join(input))
                OPERATION = input[0].upper()
                if len(input) == 6 and self.methods.has_key(OPERATION):
                    if OPERATION == 'FINALIZE' or OPERATION == 'INIT':
                        self.methods[OPERATION](self, ' '.join(input))
                    else: pool.add_task(self.methods[OPERATION], self,' '.join(input))
                else:
                    out = 'WRONG COMMAND'
                    self.message.stdout(out)
                    self.logger.debug(out)
        except Exception , err : 
            self.logger.warning( str ( err ) , exc_info=1 )
    
    def _update_com(self, host):
        with self._lock :
            if self._configure.check_update() or not self._configure.resources :
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
                if not self._communicator.has_key( resname ): 
                    self._communicator[ resname ] = self._configure.make_communicators()[resname]
                return self._communicator[ resname ]
