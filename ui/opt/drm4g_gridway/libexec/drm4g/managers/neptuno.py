import drm4g.managers 
from string import Template
from drm4g.managers import sec_to_H_M_S
import re

__version__ = '0.1'
__author__  = 'Carlos Blanco'
__revision__ = "$Id$"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
SBATCH = 'sbatch'   #submit a job
SQUEUE = 'squeue'   #show status of jobs
QDEL   = 'qdel'     #delete a job

class Resource (drm4g.managers.Resource):

    def lrmsProperties(self):
        return ('NEPTUNO', 'NEPTUNO')

    def dynamicNodes(self):
        out, err = self.Communicator.execCommand('%s -h | wc -l' % (SQUEUE))
        if err:
            raise drm4g.managers.ResourceException(' '.join(err.split('\n')))
        busy_cpu = int(out)
        free_cpu = self.MAX_RESOURCES - busy_cpu
        return (str(self.MAX_RESOURCES), str(free_cpu))

    def queuesProperties(self, searchQueue, project):
        queue              = drm4g.managers.Queue()
        queue.Name         = 'default'
        queue.Nodes        = self.TotalCpu
        queue.FreeNodes    = self.FreeCpu
        queue.DispatchType = 'batch'
        return [queue]

class Job (drm4g.managers.Job):
   
    #job status <--> GridWay job status
    states_NEPTUNO = {'CANCELLED': 'DONE', 
                  'COMPLETED' : 'DONE', 
                  'COMPLETING': 'ACTIVE',  
                  'RUNNING'   : 'ACTIVE',  
                  'NODE_FAIL' : 'FAILED',  
                  'FAILED'    : 'FAILED',
                  'PENDING'   : 'PENDING',  
                  'SUSPENDED' : 'SUSPENDED',
                  'TIMEOUT'   : 'FAILED',
                }
    
    def jobSubmit(self, path_script):
        out, err = self.Communicator.execCommand('%s %s' % (SBATCH, path_script))
        re_job_id = re.compile(r'Submitted batch job (\d*)').search(out)
        if re_job_id:
            return re_job_id.group(1)
        else:
            raise drm4g.managers.JobException(' '.join(err.split('\n')))        

    def jobStatus(self):
        out, err = self.Communicator.execCommand('%s -h -o %T -j' % (SQUEUE, self.JobId))
        if err:
            return 'UNKNOWN'
        elif not out:
            return 'DONE'
        else:
            out_parser = xml.dom.minidom.parseString(out)
            state = out.split()[3]
            return self.states_mn.setdefault(state, 'UNKNOWN')
    
    def jobCancel(self):
        out, err = self.Communicator.execCommand('%s %s' % (QDEL, self.JobId))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '#MOAB -N JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        args += '#MOAB -o $stdout\n'
        args += '#MOAB -e $stderr\n'
        if parameters.has_key('maxWallTime'): 
            args += '#MOAB -l walltime=%s\n' % (sec_to_H_M_S(parameters['maxWallTime']))
        if parameters.has_key('maxMemory'):
            args += '#MOAB -l pmem=%sMB\n' % (parameters['maxMemory'])
        if parameters.has_key('tasksPerNode'):
            args += '#MOAB -l nodes=%d:ppn=$tasksPerNode\n' % (int(parameters['count']) / int(parameters['tasksPerNode']))
        else:
            args += '#MOAB -l nodes=$count\n'
        args += '#MOAB -v %s\n' % (','.join(['%s=%s' %(k, v) for k, v in parameters['environment'].items()]))
        args += 'cd $directory\n'
        if parameters['jobType'] == "mpi":
            args += 'mpiexec -np $count $executable\n'
        else:
            args += '$executable\n'
        return Template(args).safe_substitute(parameters)


