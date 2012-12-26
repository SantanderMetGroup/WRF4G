import sys
from Queue import Queue
import re
import time
import os
import threading
import logging
from string import Template
from drm4g.utils.rsl2 import Rsl2Parser
from drm4g.utils.list import List 
from drm4g.core.configure import readHostList, parserHost
from drm4g.utils.dynamic import ThreadPool
from drm4g.utils.message import Send
from drm4g.global_settings import COMMUNICATOR, RESOURCE_MANAGER
from drm4g.utils.importlib import import_module

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: em_mad.py 1357 2012-01-10 19:59:38Z carlos $"

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
    logger = logging.getLogger(__name__)
    message = Send()

    def __init__(self):
	self._callback_interval    = 30 #seconds
	self._max_thread           = 100
	self._min_thread           = 5
        self._JID_list             = List()
        self._host_list_conf       = { } 
        self._com_list             = { }
        self._resource_module_list = { }
	        
    def do_INIT(self, args):
	"""
	Initializes the MAD (i.e. INIT - - -)
	@param args : arguments of operation
        @type args : string
	"""
	out = 'INIT - SUCCESS -'
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
            HOST, JM = HOST_JM.rsplit('/',1)
 
            # Init ResourceManager class
            if not self._resource_module_list.has_key(HOST):
                self._create_com(HOST) 
            job = getattr(self._resource_module_list[HOST], 'Job')()
            job.Communicator = self._com_list[HOST]

            # Parse rsl
            rsl_var = Rsl2Parser(RSL).parser()
            hostConf = self._host_list_conf[HOST]
            rsl_var['environment']['GW_RUNDIR'] = hostConf.GW_RUNDIR
            if hostConf.GW_LOCALDIR:
                rsl_var['environment']['GW_LOCALDIR'] = hostConf.GW_LOCALDIR
            if hostConf.PROJECT:
                rsl_var['PROJECT'] = hostConf.PROJECT
            rsl_var['mpi'] = hostConf.MPI_TAG
            rsl_wrapper_directory = rsl_var.setdefault('directory',rsl_var['executable'].split('/')[0])
            for k in "stdout", "stderr", "directory", "executable":
                rsl_var[k] = "%s/%s" % (hostConf.GW_RUNDIR, rsl_var[k])

            # Create and copy wrapper_drm4g 
            local_wrapper_directory  = '%s/wrapper_drm4g.%s' % (RSL.rsplit('/',1)[0] , RSL.split('.')[-1])
            remote_wrapper_directory = '%s/.wrapper_drm4g' % (rsl_wrapper_directory)
            string_template = job.jobTemplate(rsl_var)
            job.createWrapper(local_wrapper_directory, string_template)
            job.copyWrapper(local_wrapper_directory, remote_wrapper_directory)

            # Execute wrapper_drm4g 
            pathScript = Template('$directory/.wrapper_drm4g').safe_substitute(rsl_var)
            job.JobId = job.jobSubmit(pathScript)
            self._JID_list.put(JID, job)
            out = 'SUBMIT %s SUCCESS %s:%s' % (JID, HOST, job.JobId)
        except Exception, e:
            out = 'SUBMIT %s FAILURE %s' % (JID, str(e))
        self.message.stdout(out)
        self.logger.debug(out)

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
        
    def do_RECOVER(self, args):
        """
        Polls a job to obtain its state (i.e. RECOVER JID - -).
        @param args : arguments of operation
        @type args : string 
        """
        OPERATION, JID, HOST_JM, RSL = args.split()
        try:
            host, remoteJobId = HOST_JM.split(':')
            if not self._resource_module_list.has_key(host):
                self._create_com(host)
            job = getattr(self._resource_module_list[host], 'Job')()
            job.Communicator = self._com_list[host]
            job.JobId = remoteJobId
            job.refreshJobStatus()
            self._JID_list.put(JID, job)
            out = 'RECOVER %s SUCCESS %s' % (JID, job.Status)
        except Exception, e:
            out = 'RECOVER %s FAILURE %s' % (JID, str(e))    
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
        
    def do_CANCEL(self, args):
        """
        Cancels a job (i.e. CANCEL JID - -).
        @param args : arguments of operation
        @type args : string
        """
        OPERATION, JID, HOST_JM, RSL = args.split()
        try:
            if self._JID_list.has_key(JID):
                self._JID_list.get(JID).jobCancel()
                out = 'CANCEL %s SUCCESS -' % (JID)
            else:
                out = 'CANCEL %s FAILURE Job not submited' % (JID)
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
            worker.setDaemon(True); worker.start()
            pool = ThreadPool(self._min_thread, self._max_thread)
            while True:
                input = sys.stdin.readline().split()
                self.logger.debug(' '.join(input))
                OPERATION = input[0].upper()
                if len(input) == 4 and self.methods.has_key(OPERATION):
                    if OPERATION == 'FINALIZE' or OPERATION == 'INIT':
                        self.methods[OPERATION](self, ' '.join(input))
                    else:
                        pool.add_task(self.methods[OPERATION], self, ' '.join(input))    
                else:
                    self.message.stdout('WRONG COMMAND')
                    self.logger.debug(out)
        except Exception, e:
            self.logger.warning(str(e))

    def _create_com(self, host):
        hostList = readHostList()
        for hostname, url in hostList.items():
            if hostname == host:
                try:
                    hostConf = parserHost(hostname, url)
                    self._host_list_conf[hostname] = hostConf
                    com = getattr(import_module(COMMUNICATOR[hostConf.SCHEME]), 'Communicator')()
                    com.hostName = hostConf.HOST
                    com.userName = hostConf.USERNAME
                    com.workDirectory = hostConf.GW_RUNDIR
                    com.connect()
                except:
                    out = "It couldn't be connected to %s" %(host)
                    self.logger.warning(out)
                    raise out
                else:
                    self._com_list[hostname] = com
                    if hostConf.GW_RUNDIR == r'~':
                        out, err = com.execCommand('echo $HOME')
                        if err:
                            out = "Couldn't obtain home directory : %s" % (' '.join(err.split('\n')))
                            self.logger.warning(out)
                            raise out
                        self._host_list_conf[hostname].GW_RUNDIR = out.strip('\n')
                    self._resource_module_list[hostname] = import_module(RESOURCE_MANAGER[hostConf.LRMS_TYPE])
 
