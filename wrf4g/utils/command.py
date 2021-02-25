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
import logging
import subprocess
from distutils import spawn


def cls():
    os.system("clear")


def which(command):
    """
    Locate commands
    """
    return spawn.find_executable(command)


def exec_cmd_advance(
    cmd,
    nohup=False,
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.STDOUT,
    env=os.environ,
):
    """
    Execute shell commands
    """
    logging.debug("Executing command ... " + cmd)
    cmd_to_exec = subprocess.Popen(
        cmd,
        shell=True,
        stdin=stdin,
        stdout=stdout,
        stderr=stderr,
        env=env,
        universal_newlines=True,
    )
    if not nohup:
        out, err = cmd_to_exec.communicate()
    else:
        out = err = ""
    return out, err


def exec_cmd(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=os.environ):
    logging.debug("Executing command ... " + cmd)
    p = subprocess.Popen(
        cmd,
        shell=True,
        stdout=stdout,
        stderr=stderr,
        env=env,
        universal_newlines=True
    )
    out, err = p.communicate()
    output = out.strip() + err.strip()
    return p.returncode, output


def os_stat(ifile):
    return os.stat(ifile)
