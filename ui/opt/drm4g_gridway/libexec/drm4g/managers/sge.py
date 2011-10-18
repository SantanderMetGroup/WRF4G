import drm4g.managers
import re
import xml.dom.minidom
from string import Template
from drm4g.managers import sec_to_H_M_S

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: sge.py 1122 2011-08-22 07:56:18Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
QCONF = 'qconf'
QHOST = 'qhost'
QSUB  = 'qsub'  
QSTAT = 'qstat' 
QDEL  = 'qdel'  

class Resource (drm4g.managers.Resource):

    host_properties = {
        'LRMS_NAME'    : 'SGE',
        'LRMS_TYPE'    : 'SGE',
        }
    
    queue_default = {
        'QUEUE_NAME'           : 'default',
        'QUEUE_NODECOUNT'      : 0,
        'QUEUE_FREENODECOUNT'  : 0,
        'QUEUE_MAXTIME'        : 0,
        'QUEUE_MAXCPUTIME'     : 0,
        'QUEUE_MAXCOUNT'       : 0,
        'QUEUE_MAXRUNNINGJOBS' : 0,
        'QUEUE_MAXJOBSINQUEUE' : 0,
        'QUEUE_STATUS'         : '0',
        'QUEUE_DISPATCHTYPE'   : 'batch',
        'QUEUE_PRIORITY'       : 'NULL',
        }

    def dynamicNodes(self):
        out, err = self.Communicator.execCommand('%s -xml' % (QHOST))
        if err: 
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        out_parser = xml.dom.minidom.parseString(out)
        slots = [elem.getElementsByTagName('hostvalue')[1].firstChild.data \
            for elem in out_parser.getElementsByTagName('qhost')[0].getElementsByTagName('host')]
        self.total_cpu = sum([int(elem) for elem in slots if elem != '-']) 
        out, err = self.Communicator.execCommand('%s -s r -xml' % (QSTAT))
        if err: 
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        out_parser = xml.dom.minidom.parseString(out) 
        usedCpu = sum ([int(elem.firstChild.data) for elem in out_parser.getElementsByTagName('slots')])
        self.free_cpu = self.total_cpu - usedCpu

    def queues(self, Host):
        self.queue_default['QUEUE_NODECOUNT']     = self.total_cpu
        self.queue_default['QUEUE_FREENODECOUNT'] = self.free_cpu
        if Host.PROJECT and not Host.QUEUE_NAME:
            return self._queues_string([self.queue_default])
        else:
            out, err = self.Communicator.execCommand('%s -sql' % (QCONF))
            #output line --> Queue Memory CPU_Time Walltime Node Run Que Lm State
            if err:
                raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
            queues = []
            for queue_name in out.strip('\n').split():
                if queue_name == Host.QUEUE_NAME or not Host.QUEUE_NAME:
                    queue = self.queue_default.copy()
                    queue['QUEUE_NAME'] = queue_name
                    out, err = self.Communicator.execCommand('%s -sq %s' % (QCONF, queue_name))
                    if err:
                        raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
                    re_Walltime = re.compile(r'h_rt\s*(\d+):\d+:\d+')
                    if re_Walltime.search(out):
                        queue['QUEUE_MAXTIME'] = int(re_Walltime.search(out).group(1)) * 60
                    re_Cputime = re.compile(r'h_cpu\s*(\d+):\d+:\d+')
                    if re_Cputime.search(out):
                        queue['QUEUE_MAXTIME'] = int(re_Cputime.search(out).group(1)) * 60
                    queues.append(queue)
            return self._queues_string(queues)

class Job (drm4g.managers.Job):

    #sge job status <--> GridWay job status
    states_sge = {
        'd'  : 'FAILED',   #An error occurred with the job
        'Eqw': 'FAILED',   #An error occurred with the job
        'Ew' : 'FAILED',   #An error occurred with the job
        'h'  : 'PENDING',  #Job is hold
        'r'  : 'ACTIVE',   #Job is running
        'R'  : 'FAILED',   #Job is restarted
        'Rr' : 'FAILED',   #Job is restarted
        's'  : 'SUSPENDED',#Job is suspended
        'S'  : 'SUSPENDED',#Job is suspended
        't'  : 'PENDING',  #Job is transfering
        'T'  : 'PENDING',  #Job is Threshold 
        'w'  : 'PENDING',  #Job is waiting	
        'qw' : 'PENDING',  #Job is waiting
        }

    def jobSubmit(self, path_script):
        out, err = self.Communicator.execCommand('%s %s' % (QSUB, path_script))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))
        re_job  = re.compile(r'^Your job (\d*) .*').search(out)
        return re_job.group(1)

    def jobStatus(self):
        out, err = self.Communicator.execCommand('%s | grep %s' % (QSTAT, self.JobId))
        if not out:
            return 'DONE'
        else:
            state = out.split()[4]
            return self.states_sge.setdefault(state, 'UNKNOWN')
    
    def jobCancel(self):
        out, err = self.Communicator.execCommand('%s %s' % (QDEL, self.JobId))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '#$ -N JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        if parameters.has_key('PROJECT'): 
            args += '#$ -P $PROJECT\n'
        if parameters.has_key('queue') and parameters.has_key('queue') != self.queue_default['QUEUE_NAME']:
            args += '#$ -q $queue\n'
        args += '#$ -o $stdout\n'
        args += '#$ -e $stderr\n'
        if parameters.has_key('maxWallTime'): 
            args += '#$ -l h_rt=%s\n' % (sec_to_H_M_S(parameters['maxWallTime']))
        if parameters.has_key('maxCpuTime'): 
            args += '#$ -l cput=%s\n' % (sec_to_H_M_S(parameters['maxCpuTime']))
        if parameters.has_key('maxMemory'): 
            args += '#$ -l mem_free=%sM\n' % (parameters['maxMemory'])
        args += '#$ -l num_proc=$count\n'
        args += '#$ -v %s\n' % (','.join(['%s=%s' %(k, v) for k, v in parameters['environment'].items()]))
        args += 'cd $directory\n'
        args += '$executable\n'
        return Template(args).safe_substitute(parameters)

   
            
        
