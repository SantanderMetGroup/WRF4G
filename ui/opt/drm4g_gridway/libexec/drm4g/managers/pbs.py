import drm4g.managers 
from string import Template
from drm4g.managers import sec_to_H_M_S
import re

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id: pbs.py 1362 2012-01-17 12:14:28Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
PBSNODES = 'LANG=POSIX pbsnodes' #pbsnodes - pbs node manipulation
QSUB     = 'LANG=POSIX qsub'     #qsub - submit pbs job
QSTAT    = 'LANG=POSIX qstat'    #qstat - show status of pbs batch jobs
QDEL     = 'LANG=POSIX qdel'     #qdel - delete pbs batch job

class Resource (drm4g.managers.Resource):

    def lrmsProperties(self):
        return ('PBS', 'PBS') 
 
    def dynamicNodes(self):
        out, err = self.Communicator.execCommand("%s | egrep 'np' | awk -F = '{print $2}'" % (PBSNODES))
        if err: 
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        total_cpu  = sum([int(elem) for elem in out.split()])
        out, err = self.Communicator.execCommand("%s | egrep ' jobs' | awk -F = '{print $2}'" % (PBSNODES))
        if err:
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        busy_cpu = out.count('/')
        free_cpu = total_cpu - busy_cpu  
        return (str(total_cpu), str(free_cpu))

    def queuesProperties(self, searchQueue, project):
        out, err = self.Communicator.execCommand('%s -q' % (QSTAT))
        #output line --> Queue Memory CPU_Time Walltime Node Run Que Lm State
        if err:
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        queues = []
        for val in out.split('\n')[5:]:
            try:
                queueName, _, cpuTime, wallTime, _, _, _, lm = val.split()[0:8]
            except:
                pass
            else:    
                if (queueName == searchQueue) or not searchQueue:
                    queue              = drm4g.managers.Queue()
                    queue.Name         = queueName
                    queue.Nodes        = self.TotalCpu
                    queue.FreeNodes    = self.FreeCpu
                    queue.DispatchType = 'batch'
                    time = re.compile(r'(\d+):(\d+):\d+')
                    if cpuTime != '--':
                        try:
                            hours, minutes   = time.search(cpuTime).groups()
                            queue.MaxCpuTime = str(int(hours) * 60 + int(minutes))
                        except: pass
                    if wallTime != '--':
                        try:
                            hours, minutes   = time.search(wallTime).groups()
                            queue.MaxTime    = str(int(hours) * 60 + int(minutes))
                        except: pass
                    if lm != '--':
                        try: 
                            queue.MaxRunningJobs = lm
                        except: pass
                    queues.append(queue)
        return queues

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
        if parameters.has_key('PROJECT'):
            args += '#PBS -P $PROJECT\n'
        args += '#PBS -q $queue\n'
        args += '#PBS -o $stdout\n'
        args += '#PBS -e $stderr\n'
        if parameters.has_key('maxWallTime'): 
            args += '#PBS -l walltime=%s\n' % (sec_to_H_M_S(parameters['maxWallTime']))
        if parameters.has_key('maxCpuTime'): 
            args += '#PBS -l cput=%s\n' % (sec_to_H_M_S(parameters['maxCpuTime']))
        if parameters.has_key('maxMemory'):
            args += '#PBS -l vmem=%sMB\n' % (parameters['maxMemory'])
        if parameters.has_key('tasksPerNode'):
            args += '#PBS -l nodes=%d:ppn=$tasksPerNode\n' % (int(parameters['count']) / int(parameters['tasksPerNode']))
        else:
            args += '#PBS -l nodes=$count\n'
        args += '#PBS -v %s\n' % (','.join(['%s=%s' %(k, v) for k, v in parameters['environment'].items()]))
        args += 'cd $directory\n'
        if parameters['jobType'] == "mpi":
            args += 'mpiexec -np $count $executable\n'
        else:
            args += '$executable\n'
        return Template(args).safe_substitute(parameters)


