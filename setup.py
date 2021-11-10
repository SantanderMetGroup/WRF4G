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

__author__   = 'Valvanuz Fernández, Jesus Fernandez, Carlos Blanco and Antonio S. Cofiño'

from setuptools import setup
from setuptools import find_packages
from setuptools.command.install import install
import glob
import sys
import os
from io import open

# Get every file under search_dir, including subdirectories. Needs to be improved
def get_conf_files(search_dir='data'):
    if os.path.exists('./wrf4g'):
        os.chdir('./wrf4g')
    directory_list = glob.glob(search_dir)
    search_dir += '/*'
    file_list = []
    if directory_list:
        for f in directory_list:
            if os.path.isfile(f):
                file_list.append(f)
        file_list += get_conf_files(search_dir)
    if not os.path.exists('./wrf4g'):
        os.chdir('..')
    return file_list

this_directory = os.path.abspath(os.path.dirname(__file__))
with open(os.path.join(this_directory, 'README.md'), encoding='utf-8') as f:
    long_description = f.read()

bin_scripts = glob.glob(os.path.join('bin', '*'))

def get_version_and_cmdclass(package_path):
    """Load version.py module without importing the whole package.

    Template code from miniver
    """
    import os
    from importlib.util import module_from_spec, spec_from_file_location

    spec = spec_from_file_location("version", os.path.join(package_path, "_version.py"))
    module = module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.__version__, module.cmdclass


version, cmdclass = get_version_and_cmdclass("wrf4g")

setup(
    name='wrf4g',
    packages=find_packages(),
    package_data={'wrf4g': get_conf_files()},
    python_requires='>=3.5',
    version=version,
    author='Santander Meteorology Group',
    author_email='meteo@unican.es',
    url='https://meteo.unican.es/trac/wiki/WRF4G2.0',
    license='European Union Public License 1.1',
    description='WRF for Grid (WRF4G) is a framework for the execution and monitoring of the WRF Modelling System.',
    long_description=long_description,
    long_description_content_type="text/markdown",
    classifiers=[
        "Intended Audience :: Science/Research",
        "Programming Language :: Python",
        "Topic :: Scientific/Engineering",
        "Topic :: Office/Business :: Scheduling",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
    ],
    install_requires=['drm4g', 'sqlalchemy', 'six', 'python-dateutil','requests'],
    scripts=bin_scripts,
    cmdclass=cmdclass,    
)
