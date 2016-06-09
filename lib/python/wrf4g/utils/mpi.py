__version__  = '2.2.2'
__author__   = 'Carlos Blanco'
__revision__ = "$Id$"

class ParallelEnvironment():
    """
    Class to invoke parallel environment for executing 
    programs on processor nodes.
    """
    class Poe():
        launcher = 'poe'
        pernode = '-tasks_per_node 1'
        ppn      = '-tasks_per_node'
        np       = '-procs'
    
    class Srun():
        launcher = 'srun'
        pernode = '--ntasks-per-node 1'
        ppn      = '--ntasks-per-node'
        np       = '--ntasks'
    
    class MpiRun():
        launcher = 'mpirun'
        pernode  = '-pernode'
        ppn      = '-npernode'
        np       = '-np'
 
    class Dummy():
        pass

    launcher_map = {
        'POE'   : Poe,
        'SRUN'  : Srun,
        'MPIRUN': MpiRun,
        'DUMMY' : Dummy
    }

