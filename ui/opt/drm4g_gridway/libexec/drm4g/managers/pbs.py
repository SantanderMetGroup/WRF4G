import drm4g.managers 
from string import Template
from drm4g.managers import sec_to_H_M_S
import re
import xml.dom.minidom
from drm4g.utils.logger import *

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: pbs.py 1173 2011-10-03 07:34:32Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
PBSNODES = 'pbsnodes' #pbsnodes - pbs node manipulation
QSUB     = 'qsub'     #qsub - submit pbs job
QSTAT    = 'qstat'    #qstat - show status of pbs batch jobs
QDEL     = 'qdel'     #qdel - delete pbs batch job

class Resource (drm4g.managers.Resource):

    host_properties = {
        'LRMS_NAME'    : 'PBS',
        'LRMS_TYPE'    : 'PBS',
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
        out, err = self.Communicator.execCommand('%s -x' % (PBSNODES))
        if err: 
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        out_parser = xml.dom.minidom.parseString(out)
        self.total_cpu  = sum([int(elem.getElementsByTagName('np')[0].firstChild.data) \
            for elem in out_parser.getElementsByTagName('Node')])
        auxCpu = ','.join([elem.getElementsByTagName('jobs')[0].firstChild.data \
            for elem in out_parser.getElementsByTagName('Node') \
                if elem.getElementsByTagName('jobs')]).count(',')
        if auxCpu != 0 : self.free_cpu = self.total_cpu - (auxCpu + 1)
        else : self.free_cpu = self.total_cpu - auxCpu

    def queues(self, Host):
        out, err = self.Communicator.execCommand('%s -q' % (QSTAT))
        #output line --> Queue Memory CPU_Time Walltime Node Run Que Lm State
        if err:
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        self.queue_default['QUEUE_NODECOUNT']     = self.total_cpu
        self.queue_default['QUEUE_FREENODECOUNT'] = self.free_cpu
        queues = []
        for values in out.split('\n')[5:-3]:
            queue_name, _, CPU_Time, Walltime, _, _, _, Lm = values.split()[0:8]
            if queue_name == Host.QUEUE_NAME or not Host.QUEUE_NAME:
                queue = self.queue_default.copy()
                queue['QUEUE_NAME'] = queue_name
                Time = re.compile(r'(\d+):\d+:\d+')
                if CPU_Time != '--':
                    queue['QUEUE_MAXCPUTIME'] = int(Time.search(CPU_Time).group(1)) * 60
                if Walltime != '--':
                    queue['QUEUE_MAXTIME'] = int(Time.search(CPU_Time).group(1)) * 60
                if Lm != '--': queue['QUEUE_MAXRUNNINGJOBS'] = Lm
                queues.append(queue)
        return self._queues_string(queues)

class Job (drm4g.managers.Job):
   
    #pbs job status <--> GridWay job status
    states_pbs = {'E': 'ACTIVE',    #Job is exiting after having run.
                  'H': 'SUSPENDED', #Job is held.
                  'Q': 'PENDING',   #Job is queued, eligable to run or routed.
                  'R': 'ACTIVE',    #Job is running.
                  'T': 'PENDING',   #Job is being moved to new location.
                  'W': 'PENDING',   #Job is waiting for its execution time to be reached.
                  'S': 'SUSPENDED', #Job is suspend.
                  'C': 'DONE',	    #Job finalize.
                }

    def jobSubmit(self, path_script):
        out, err = self.Communicator.execCommand('%s %s' % (QSUB, path_script))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))
        return out.strip() #job_id

    def jobStatus(self):
        out, err = self.Communicator.execCommand('%s %s -x' % (QSTAT, self.JobId))
        if 'qstat: Unknown Job Id' in err :
            return 'DONE'
        elif err:
            self.logger.log(DEBUG, 'JID: ' + self.JobId + ' '.join(err.split('\n')))
            return 'UNKNOWN'
        else:
            out_parser = xml.dom.minidom.parseString(out)
            state = out_parser.getElementsByTagName('job_state')[0].firstChild.data
            return self.states_pbs.setdefault(state, 'UNKNOWN')
    
    def jobCancel(self):
        out, err = self.Communicator.execCommand('%s %s' % (QDEL, self.JobId))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '#PBS -N JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        args += '#PBS -q $queue\n'
        args += '#PBS -o $stdout\n'
        args += '#PBS -e $stderr\n'
        if parameters.has_key('maxWallTime'): 
            args += '#PBS -l walltime=%s\n' % (sec_to_H_M_S(parameters['maxWallTime']))
        if parameters.has_key('maxCpuTime'): 
            args += '#PBS -l cput=%s\n' % (sec_to_H_M_S(parameters['maxCpuTime']))
        if parameters.has_key('maxMemory'):
            args += '#PBS -l mem=%smb\n' % (parameters['maxMemory'])
        if parameters.has_key('tasksPerNode'):
            args += '#PBS -l nodes=$count:ppn=$tasksPerNode\n'
        else:
            args += '#PBS -l nodes=$count\n'
        args += '#PBS -v %s\n' % (','.join(['%s=%s' %(k, v) for k, v in parameters['environment'].items()]))
        args += 'cd $directory\n'
        args += '$executable\n'
        return Template(args).safe_substitute(parameters)


