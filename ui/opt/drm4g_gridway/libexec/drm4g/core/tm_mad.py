import sys
import threading
import os
import re
import time
import logging
from drm4g.utils.url import urlparse
from drm4g.utils.dynamic import ThreadPool
from drm4g.core.configure import readHostList, parserHost, CheckConfigFile
from drm4g.utils.message import Send
from drm4g.global_settings import COMMUNICATOR
from drm4g.utils.importlib import import_module
import traceback

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: tm_mad.py 1788 2013-03-26 16:55:06Z carlos $"


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
        self._max_thread       = 100
        self._min_thread       = 5
        self._config_file_time = None
        self._lock             = threading.Lock()
        self._com_list         = { }
  
    def do_INIT(self, args):
        """
        INIT: Initializes the MAD, JID should be max number of jobs.
        (i.e. INIT JID - - - -)
        @param args : arguments of operation
        @type args : string 
        """
        out = 'INIT - - SUCCESS -'
        self.message.stdout(out)
        self.logger.debug(out)

    def do_START(self, args):
        """
        START: Init transfer associated with job JID.(i.e. START JID - - - -)
        @param args : arguments of operation
        @type args : string 
        """
        out = 'START %s - SUCCESS -' % (args.split()[1])    
        self.message.stdout(out)
        self.logger.debug(out)
        
    def do_END(self, args):
        """
        END: Finish transfer associated with job JID .(i.e. END JID - - - -)
        @param args : arguments of operation
        @type args : string 
        """
        out = 'END %s - SUCCESS -' % (args.split()[1])
        self.message.stdout(out)
        self.logger.debug(out)
  
    def do_FINALIZE(self, args):
        """
        Finalizes the MAD (i.e. FINALIZE - - - - -)
        @param args : arguments of operation
        @type args : string
        """
        out = 'FINALIZE %s - SUCCESS -' % (args.split()[1])
        self.message.stdout(out)
        self.logger.debug(out)
        sys.exit(0)
 
    def do_MKDIR(self, args):
        """
        MKDIR: Creates directory SRC_URL (i.e. MKDIR JID - - SRC_URL -)
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, JID, TID, EXE_MODE, SRC_URL, DST_URL = args.split()
        try:
            com = self._updateCom(urlparse(SRC_URL).host)
            com.rmDirectory(SRC_URL)
            com.mkDirectory(SRC_URL)
            out = 'MKDIR %s - SUCCESS -' % (JID)
        except Exception, e:
            out = 'MKDIR %s - FAILURE %s' % (JID, str(e))
            exc_type, exc_obj, exc_tb = sys.exc_info()
            fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
            out1 = ', '.join([str(exc_type), fname, str(exc_tb.tb_lineno)])
        self.message.stdout(out)
        self.logger.debug(out1)
        
    def do_RMDIR(self, args):
        """
        RMDIR: Removes directory SRC_URL (i.e. RMDIR JID - - SRC_URL -)
        @param args : arguments of operation
        @type args : string 
        """
        OPERATION, JID, TID, EXE_MODE, SRC_URL, DST_URL = args.split()
        try:
            com = self._updateCom(urlparse(SRC_URL).host)            
            com.rmDirectory(SRC_URL)
            out = 'RMDIR %s - SUCCESS -' % (JID)
        except Exception, e:
            out = 'RMDIR %s - FAILURE %s' % (JID, str(e))
            exc_type, exc_obj, exc_tb = sys.exc_info()
            fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
            out1 = ', '.join([str(exc_type), fname, str(exc_tb.tb_lineno)])
        self.message.stdout(out)        
        self.logger.debug(out1)
    
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
            com = self._updateCom(urlparse(url).host)
            com.copy(SRC_URL, DST_URL, EXE_MODE)
            out = 'CP %s %s SUCCESS -' % (JID, TID)
        except Exception, e:
            exc_type, exc_obj, exc_tb = sys.exc_info()
            fname = os.path.split(exc_tb.tb_frame.f_code.co_filename)[1]
            out1 = ', '.join([str(exc_type), fname, str(exc_tb.tb_lineno)])
            self.logger.warning(out1)
            self.logger.warning('Error copying from %s to %s : %s' %(SRC_URL, DST_URL, str(e)))
            time.sleep(60)
            try:
                self.logger.debug('Copying again from %s to %s' % (SRC_URL, DST_URL))
                com = self._updateCom(urlparse(SRC_URL).host)
                com.copy(SRC_URL, DST_URL, EXE_MODE)
                out = 'CP %s %s SUCCESS -' % (JID, TID)
            except Exception, e:
                out = 'CP %s %s FAILURE %s' % (JID, TID, str(e))   
        self.message.stdout(out)
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
            pool = ThreadPool(self._min_thread, self._max_thread)
            self._config_file_time = CheckConfigFile()
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
        except Exception, e: 
            self.logger.warning(str(e))
    
    def _updateCom(self, host):
        self._lock.acquire()
        try:
            if self._config_file_time.test() or not self._com_list or not self._com_list.has_key(host):
                hostList = readHostList()
                hostConf = parserHost(host, hostList[host])
                self.logger.warning(hostConf.HOST)
                if not self._com_list.has_key(host):
                    communicator = self._createCom(hostConf)
                else:
                    oldHostConf, oldCommunicator = self._com_list[host]
                    if hostConf.com_attrs() != oldHostConf.com_attrs():
                        oldCommunicator.close()
                        communicator = self._createCom(hostConf)
                    else:
                        communicator = self._com_list[host][1]
                self._com_list[host] = (hostConf, communicator)
                return communicator
            else:
                self.logger.warning(str(self._com_list.get(host)[1])) 
                return self._com_list[host][1]
        finally:
            self._lock.release() 
            
    def _createCom(self, hostConf):
        try:
            com               = getattr(import_module(COMMUNICATOR[hostConf.SCHEME]), 'Communicator')()
            com.hostName      = hostConf.HOST
            com.userName      = hostConf.USERNAME
            com.workDirectory = hostConf.TEMP_DIR
            com.keyFile       = hostConf.SSH_KEY_FILE
            com.port          = hostConf.PORT
            com.connect()
            return com
        except Exception, e:
            out = "It couldn't be connected to %s : %s" %(hostConf.HOST, str(e))
            self.logger.warning(out)
            raise Exception(out)
