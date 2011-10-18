import sys
import threading
import os
import re
import time
from drm4g.utils.url import urlparse
from drm4g.utils.dynamic import ThreadPool
from drm4g.utils.logger import *
from drm4g.utils.message import Send
from drm4g.core.configure import hostparse
from drm4g.global_settings import COMMUNICATOR, Debug
from drm4g.utils.importlib import import_module

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: tm_mad.py 1162 2011-09-09 07:42:58Z carlos $"


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
    
    logger = get_logger('drm4g.core.tm_mad')
    message = Send()
    
    def __init__(self):
        self._max_thread = 120
        self._min_thread = 5
        self._com_list   = { }
  
    def do_INIT(self, args):
        """
        INIT: Initializes the MAD, JID should be max number of jobs.
        (i.e. INIT JID - - - -)
        @param args : arguments of operation
        @type args : string 
        """
        try:
            for key, val in hostparse().items():
                com = getattr(import_module(COMMUNICATOR[val.SCHEME]), 'Communicator')()
                com.hostName = val.HOST
                com.userName = val.USERNAME
                com.workDirectory = val.GW_RUNDIR
                com.connect()
                self._com_list[key] = com
            out = 'INIT - - SUCCESS -'
        except Exception, e:
            out = 'INIT - - FAILURE %s' % (str(e))
        self.message.stdout(out)
        self.logger.log(DEBUG, '--> ' + out)

    def do_START(self, args):
        """
        START: Init transfer associated with job JID.(i.e. START JID - - - -)
        @param args : arguments of operation
        @type args : string 
        """
        out = 'START %s - SUCCESS -' % (args.split()[1])
        self.message.stdout(out)
        self.logger.log(DEBUG, '--> ' + out)
        
    def do_END(self, args):
        """
        END: Finish transfer associated with job JID .(i.e. END JID - - - -)
        @param args : arguments of operation
        @type args : string 
        """
        out = 'END %s - SUCCESS -' % (args.split()[1])
        self.message.stdout(out)
        self.logger.log(DEBUG, '--> ' + out)
  
    def do_FINALIZE(self, args):
        """
        Finalizes the MAD (i.e. FINALIZE - - - - -)
        @param args : arguments of operation
        @type args : string
        """
        out = 'FINALIZE %s - SUCCESS -' % (args.split()[1])
        self.message.stdout(out)
        self.logger.log(DEBUG, '--> ' + out)
        sys.exit(0)
 
    def do_MKDIR(self, args):
        """
        MKDIR: Creates directory SRC_URL (i.e. MKDIR JID - - SRC_URL -)
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, JID, TID, EXE_MODE, SRC_URL, DST_URL = args.split()
        try:
            self._com_list[urlparse(SRC_URL).host].mkDirectory(SRC_URL)
            out = 'MKDIR %s - SUCCESS -' % (JID)
        except Exception, e:
            out = 'MKDIR %s - FAILURE %s' % (JID, str(e))
        self.message.stdout(out)
        self.logger.log(DEBUG, '--> ' + out)
        
    def do_RMDIR(self, args):
        """
        RMDIR: Removes directory SRC_URL (i.e. RMDIR JID - - SRC_URL -)
        @param args : arguments of operation
        @type args : string 
        """
        OPERATION, JID, TID, EXE_MODE, SRC_URL, DST_URL = args.split()
        try:
            if not Debug: 
                self._com_list[urlparse(SRC_URL).host].rmDirectory(SRC_URL)
            out = 'RMDIR %s - SUCCESS -' % (JID)
        except Exception, e:
            out = 'RMDIR %s - FAILURE %s' % (JID, str(e))
        self.message.stdout(out)        
        self.logger.log(DEBUG, '--> ' + out)
    
    def do_CP(self, args):
        """
        CP: start a copy of SRC_URL  to DST_URL, with identification TID,
        and associated with job JID.(i.e. CP JID TID - SRC_URL DST_URL)
        @param args : arguments of operation
        @type args : string 
        """
        OPERATION, JID, TID, EXE_MODE, SRC_URL, DST_URL = args.split()
        try:
            if 'file:' in SRC_URL:
                url = DST_URL
            else:
                url = SRC_URL
            self._com_list[urlparse(url).host].copy(SRC_URL, DST_URL, EXE_MODE)
            out = 'CP %s %s SUCCESS -' % (JID, TID)
        except Exception, e:
            out = 'CP %s %s FAILURE %s' % (JID, TID, str(e))    
        self.message.stdout(out)
        self.logger.log(DEBUG, '--> ' + out)
        
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
            pool = ThreadPool(self._min_thread, self._max_thread)
            while True:
                input = sys.stdin.readline().split()
                self.logger.log(DEBUG, '<-- ' + ' '.join(input))
                OPERATION = input[0].upper()
                if len(input) == 6 and self.methods.has_key(OPERATION):
                    if OPERATION == 'FINALIZE' or OPERATION == 'INIT':
                        self.methods[OPERATION](self, ' '.join(input))
                    else: pool.add_task(self.methods[OPERATION], self,' '.join(input))
                else:
                    self.message.stdout('WRONG COMMAND')
                    self.logger.log(DEBUG, '--> WRONG COMMAND')
        except Exception, e: 
            self.logger.log(DEBUG, '--> ' + str(e))
