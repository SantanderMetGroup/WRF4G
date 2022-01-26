from drm4g.managers.slurm import *

class Job(drm4g.managers.slurm.Job):
    def jobTemplate(self, parameters):
        args  = '#!/bin/bash\n'
        args += '#SBATCH --job-name=JID_%s\n' % (parameters['environment']['GW_JOB_ID'])
        args += '#SBATCH --output=$stdout\n'
        args += '#SBATCH --error=$stderr\n'
        if parameters['queue'] != 'default':
            args += '#SBATCH -p $queue\n'
        if 'maxWallTime' in parameters :
            args += '#SBATCH --time=%s\n' % (parameters['maxWallTime'])
        if 'maxMemory' in parameters :
            args += '#SBATCH --mem=%s\n' % (parameters['maxMemory'])
        if 'ppn' in parameters :
            args += '#SBATCH --ntasks-per-node=$ppn\n'
        if 'nodes' in parameters :
            args += '#SBATCH --nodes=$nodes\n'
        args += '#SBATCH --ntasks=$count\n'
        args += '#SBATCH --mail-type=begin\n'
        args += '#SBATCH --mail-type=end\n'
        args += '#SBATCH --mail-user=milovacj@unican.es\n'
        args += ''.join(['export %s=%s\n' % (k, v) for k, v in list(parameters['environment'].items())])
        args += '\n'
        args += '$executable\n'
        return Template(args).safe_substitute(parameters)
