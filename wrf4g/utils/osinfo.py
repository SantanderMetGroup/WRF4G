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

import os
import socket
from os.path              import exists, join, isfile

def get_hostname():
    """
    Hostname info 
    """
    return socket.gethostname()

def os_release():
    """
    Get OS release
    """
    release = ''
    for file in [ 'debian_version' , 'lsb-release' , 'redhat-release' ] :
        file_name = join( '/etc', file ) 
        if isfile( file_name ):
            f = open( file_name, 'r' )
            release = f.read().strip()
            f.close()
    return release
            
def cpu_info():
    """
    Get CPU information
    """
    model_name     = ''
    number_of_cpus = 0
    file_name = '/proc/cpuinfo'
    if exists( file_name ) :
        f = open( file_name, 'r' )
        cpu = f.readlines()
        f.close()
        number_of_cpus = 0
        for line in cpu :
            if line.startswith( 'model name' ): 
                model_name = line.rstrip('\n').split(':')[1]
            elif line.find( 'processor' ) == 0:
                number_of_cpus = number_of_cpus + 1
    return model_name, number_of_cpus

def mem_info():
    """
    Get memory information
    """
    total_mem = 0
    file_name = '/proc/meminfo' 
    if exists( file_name ):
        f = open( file_name, 'r' )
        mem = f.readlines()
        f.close()
        for line in mem :
            if line.find( 'MemTotal:' ) == 0:
                total_mem = int( line.split()[1] ) / 1024
    return total_mem 

def disk_space_check( directory ):
    """
    Disk space check
    """
    fs = os.statvfs( directory )
    disk_space = fs[4] * fs[0] / 1024 / 1024 / 1024 
    return disk_space
