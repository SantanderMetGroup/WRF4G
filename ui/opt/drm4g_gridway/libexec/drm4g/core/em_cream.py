import threading
import time
import sys
import os
import logging
from drm4g.utils.list import List 
from drm4g.utils.message import Send
from drm4g.global_settings import COMMUNICATOR, RESOURCE_MANAGER
from drm4g.utils.importlib import import_module
from drm4g.utils.dynamic import ThreadPool

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: em_cream.py 1331 2011-12-09 11:28:11Z carlos $"

GW_LOCATION = os.environ['GW_LOCATION']

class GwEmMad (object):
    
    logger = logging.getLogger(__name__)
    message = Send()

    def __init__(self):
        self._callback_interval = 200 #seconds
        self._max_thread        = 50
        self._min_thread        = 5
        self._JID_list          = List()
        self._resource_module 	= None
        self._com               = None
        self._pool_sema         = threading.Semaphore(15)
	        
    def do_INIT(self, args):
        """
        Initializes the MAD (i.e. INIT - - -)
        @param args : arguments of operation
        @type args : string
        """
        try:
            self._com = getattr(import_module(COMMUNICATOR['local']), 'Communicator')()
            self._resource_module = import_module(RESOURCE_MANAGER['cream'])
            out = 'INIT - SUCCESS -'
        except Exception, e:
            out = 'INIT - FAILURE %s' % (str(e))
        self.message.stdout(out)
        self.logger.debug(out)
    
    def do_SUBMIT(self, args):
        """
        Submits a job(i.e. SUBMIT JID HOST/JM RSL).
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, JID, HOST_JM, RSL = args.split()
        try:
            self._pool_sema.acquire()
            try:
                job = getattr(self._resource_module, 'Job')()
                job.Communicator = self._com
                job.jobSubmit(HOST_JM, RSL)
                self._JID_list.put(JID, job)
                out = 'SUBMIT %s SUCCESS %s' % (JID, job.JobId)
            except Exception, e:
                out = 'SUBMIT %s FAILURE %s' % (JID, str(e))
        finally:
            self._pool_sema.release()
        self.message.stdout(out)
        self.logger.debug(out)
        
    def do_RECOVER(self, args):
        """
        Polls a job to obtain its state (i.e. RECOVER JID - -).
        @param args : arguments of operation
        @type args : string 
        """
        OPERATION, JID, HOST_JM, RSL = args.split()
        self._pool_sema.acquire()
        try:
            try:
                job = getattr(self._resource_module, 'Job')()
                job.Communicator = self._com
                job.JobId = HOST_JM
                job.refreshJobStatus()
                self._JID_list.put(JID, job)
                out = 'RECOVER %s SUCCESS %s' % (JID, job.Status)
            except Exception, e:
                out = 'RECOVER %s FAILURE %s' % (JID, str(e)) 
        finally:
            self._pool_sema.release()   
        self.message.stdout(out)
        self.logger.debug(out)
       
    def do_CANCEL(self, args):
        """
        Cancels a job (i.e. CANCEL JID - -).
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, JID, HOST_JM, RSL = args.split()
        self._pool_sema.acquire()
        try:
            try:
                if self._JID_list.has_key(JID):
                    self._JID_list.get(JID).jobCancel()
                    out = 'CANCEL %s SUCCESS -' % (JID)
                else:
                    out = 'CANCEL %s FAILURE Job not submited' % (JID)
            except Exception, e:
                out = 'CANCEL %s FAILURE %s' % (JID, str(e))
        finally:
            self._pool_sema.release()
        self.message.stdout(out)
        self.logger.debug(out)

    def do_CALLBACK(self):
        """
        Show the state of the job
        """
        while True:
            time.sleep(self._callback_interval)
            for JID, job  in self._JID_list.items():
                try:
                    oldStatus = job.Status
                    job.refreshJobStatus()
                    newStatus = job.Status
                    if oldStatus != newStatus:
                        if newStatus == 'DONE' or newStatus == 'FAILED':
                            self._JID_list.delete(JID)
                        out = 'CALLBACK %s SUCCESS %s' % (JID, newStatus)
                        self.message.stdout(out)
                        self.logger.debug(out)
                except Exception, e:
                    out = 'CALLBACK %s FAILURE %s' % (JID, str(e))
                    self.message.stdout(out)
                    self.logger.debug(out)
                time.sleep(0.1)

    def do_FINALIZE(self, args):
        """
        Finalizes the MAD (i.e. FINALIZE - - -).
        @param args : arguments of operation
        @type args : string
        """
        out = 'FINALIZE - SUCCESS -'
        self.message.stdout(out)
        self.logger.debug(out)
        sys.exit(0)

    def do_POLL(self, args):
        """
        Polls a job to obtain its state (i.e. POLL JID - -).
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, JID, HOST_JM, RSL = args.split()
        try:
            if self._JID_list.has_key(JID):
                status = self._JID_list.get(JID).Status
                out = 'POLL %s SUCCESS %s' % (JID, status)
            else:
                out = 'POLL %s FAILURE Job not submited' % (JID)
        except Exception, e:
            out = 'POLL %s FAILURE %s' % (JID, str(e))
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
            worker.setDaemon(True); worker.start()
            pool = ThreadPool(self._min_thread, self._max_thread)
            while True:
                input = sys.stdin.readline().split()
                self.logger.log(' '.join(input))
                OPERATION = input[0].upper()
                if len(input) == 4 and self.methods.has_key(OPERATION):
                    if OPERATION == 'FINALIZE' or OPERATION == 'INIT':
                        self.methods[OPERATION](self, ' '.join(input))
                    else:
                        pool.add_task(self.methods[OPERATION], self, ' '.join(input))
                else:
                    self.message.stdout('WRONG COMMAND')
                    self.logger.debug('WRONG COMMAND')
        except Exception, e:
            self.logger.debug(str(e)) 
