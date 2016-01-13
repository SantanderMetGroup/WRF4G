__version__  = '2.2.0'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

class ParallelEnvironment():
    """
    Class to invoke parallel environment for executing 
    programs on processor nodes.
    """
    class Poe():
        launcher = 'poe'
        npernode = '-tasks_per_node 1'
        ppn      = '-tasks_per_node'
        np       = '-procs'
    
    class Srun():
        launcher = 'srun'
        npernode = '--ntasks-per-node 1'
        ppn      = '--ntasks-per-node'
        np       = '--ntasks'
    
    class MpiRun():
        launcher = 'mpirun'
        npernode = '-npernode'
        ppn      = '-pernode'
        np       = '-np'
 
    class Dummy():
        pass

    launcher_map = {
        'POE'   : Poe,
        'SRUN'  : Srun,
        'MPIRUN': MpiRun,
        'DUMMY' : Dummy
    }

