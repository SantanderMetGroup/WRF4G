import drm4g.managers 
from string import Template
import re

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: pbs.py 1766 2013-02-14 10:20:21Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
PBSNODES = 'LANG=POSIX pbsnodes' #pbsnodes - pbs node manipulation
QSUB     = 'LANG=POSIX qsub'     #qsub - submit pbs job
QSTAT    = 'LANG=POSIX qstat'    #qstat - show status of pbs batch jobs
QDEL     = 'LANG=POSIX qdel'     #qdel - delete pbs batch job

class Resource (drm4g.managers.Resource):

    def lrmsProperties(self):
        return ('PBS', 'PBS') 

    def queueProperties(self, queueName):
        queue              = drm4g.managers.Queue()
        queue.Name         = queueName
        queue.DispatchType = 'batch'
        queue.Nodes        = self.TotalCpu
        queue.FreeNodes    = self.FreeCpu
        out, err = self.Communicator.execCommand('%s -q %s' % (QSTAT, queueName))
        #output line --> Queue Memory CPU_Time Walltime Node Run Que Lm State
        try:
            queueName, _, cpuTime, wallTime, _, _, _, lm = val.split()[0:8]
        except:
            pass
        else:
            reTime = re.compile(r'(\d+):(\d+):\d+')
            if cpuTime != '--':
                try:
                    hours, minutes   = reTime.search(cpuTime).groups()
                    queue.MaxCpuTime = str(int(hours) * 60 + int(minutes))
                except:
                    pass
            if wallTime != '--':
                try:
                    hours, minutes   = reTime.search(wallTime).groups()
                    queue.MaxTime    = str(int(hours) * 60 + int(minutes))
                except:
                    pass
            if lm != '--':
                try:
                    queue.MaxRunningJobs = lm
                except: 
                    pass
        return queue

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
    
    def jobSubmit(self, pathScript):
        out, err = self.Communicator.execCommand('%s %s' % (QSUB, pathScript))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))
        return out.strip() #job_id

    def jobStatus(self):
        out, err = self.Communicator.execCommand('%s %s' % (QSTAT, self.JobId))
        if 'Unknown Job Id' in err :
            return 'DONE'
        elif err:
            return 'UNKNOWN'
        else:
            state = out.split()[-2]
            return self.states_pbs.setdefault(state, 'UNKNOWN')
    
    def jobCancel(self):
        out, err = self.Communicator.execCommand('%s %s' % (QDEL, self.JobId))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '#PBS -N JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        if parameters['PROJECT']:
            args += '#PBS -P $PROJECT\n'
        args += '#PBS -q $queue\n'
        args += '#PBS -o $stdout\n'
        args += '#PBS -e $stderr\n'
        if parameters.has_key('maxWallTime'): 
            args += '#PBS -l walltime=$maxWallTime\n' 
        if parameters.has_key('maxCpuTime'): 
            args += '#PBS -l cput=$maxCpuTime\n' 
        if parameters.has_key('maxMemory'):
            args += '#PBS -l vmem=$maxMemoryMB\n'
        if parameters.has_key('ppn'):
            node_count = int(parameters['count']) / int(parameters['ppn'])
            if node_count == 0:
                node_count = 1
            args += '#PBS -l nodes=%d:ppn=$ppn\n' % (node_count)
        else:
            args += '#PBS -l nodes=$count\n'
        args += '#PBS -v %s\n' % (','.join(['%s=%s' %(k, v) for k, v in parameters['environment'].items()]))
        args += 'cd $directory\n'
        args += '$executable\n'
        return Template(args).safe_substitute(parameters)


