import re
import drm4g.managers 
from string import Template

__version__  = '2.4.1'
__author__   = 'Carlos Blanco'
__revision__ = "$Id: loadleveler.py 2811 2015-09-22 11:33:32Z carlos $"

# The programs needed by these utilities. If they are not in a location
# accessible by PATH, specify their location here.
LLCLASS  = 'llclass'    #show class information
LLSUBMIT = 'llsubmit'   #submit ajob
LLQ      = 'llq'        #show jobs' status
LLCANCEL = 'llcancel'   #delete ajob

class Resource (drm4g.managers.Resource): 
    pass

class Job (drm4g.managers.Job):
   
    #loadleveler job status <--> GridWay job status
    states_loadleveler = {'CA': 'DONE',
                  'CK': 'ACTIVE',
                  'C' : 'PENDING',
                  'CP': 'ACTIVE',
                  'D' : 'PENDING',
                  'I' : 'PENDING',
                  'NQ': 'SUSPENDED',
                  'NR': 'SUSPENDED',
                  'P' : 'ACTIVE',
                  'E' : 'ACTIVE',
                  'EP': 'ACTIVE',
                  'X' : 'DONE',
                  'XP': 'SUSPENDED',
                  'RM': 'DONE',
                  'RP': 'SUSPENDED',
                  'MP': 'ACTIVE',
                  'R' : 'ACTIVE', 
                  'ST': 'ACTIVE',
                  'S' : 'PENDING',
                  'TX': 'PENDING', 
                  'HS': 'PENDING',  
                  'H' : 'PENDING',
                  'V' : 'DONE',
                  'VP': 'PENDING',
                }
    re_submit=re.compile(r"The job \"(\S+)\" has been submitted")

    def jobSubmit(self, pathScript):
        out, err = self.Communicator.execCommand('%s %s' % (LLSUBMIT, pathScript))
        job_id = self.re_submit.search(out).group(1)
        return job_id

    def jobStatus(self):
        command = LLQ + ' -f %st ' + self.JobId
        out, err = self.Communicator.execCommand(command)
        if "There is currently no job status to report" in out :
            return 'DONE'
        else:
            status = out.split('\n')[2].strip()
            return self.states_loadleveler.setdefault(status, 'UNKNOWN')
    
    def jobCancel(self):
        out, err = self.Communicator.execCommand('%s %s' % (LLCANCEL, self.JobId))
        if err: 
            raise drm4g.managers.JobException(' '.join(err.split('\n')))

    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '#@ job_name = JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        if parameters['queue'] != 'default':
            args += '#@ class    = $queue\n'
        args += '#@ output   = $stdout\n'
        args += '#@ error    = $stderr\n'
        if int(parameters['count']) > 1 :
            args += '#@ job_type  = parallel\n'
        else:
            args += '#@ job_type  = serial\n'
        args += '#@ node = $count\n'
        if 'maxWallTime' in parameters: 
            args += '#@ wall_clock_limit = $maxWallTime\n'
        if 'maxCpuTime' in parameters:
            args += '#@ job_cpu_limit = $maxCpuTime\n' 
        if 'maxMemory' in parameters :
            args += '#@ resources = ConsumableMemory($maxMemory)\n'
        if 'ppn' in parameters:
            args += '#@ tasks_per_node = $ppn'
        if 'project' in parameters:
            args += '#@ account_no = $project\n'
        args += '#@ queue\n'
        args += ''.join(['export %s=%s\n' % (k, v) for k, v in list(parameters['environment'].items())]) 
        args += '\n'
        args += '$executable\n'
        return Template(args).safe_substitute(parameters)

