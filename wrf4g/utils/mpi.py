#
# Copyright 2016 Universidad de Cantabria
#
# Licensed under the EUPL, Version 1.1 only (the
# "Licence");
# You may not use this work except in compliance with the
# Licence.
# You may obtain a copy of the Licence at:
#
# http://ec.europa.eu/idabc/eupl
#
# Unless required by applicable law or agreed to in
# writing, software distributed under the Licence is
# distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
# express or implied.
# See the Licence for the specific language governing
# permissions and limitations under the Licence.
#

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

