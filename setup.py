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

__version__  = '2.3.0'
__author__   = 'Jesus Fernandez and Carlos Blanco'
__revision__ = "$Id$"

from setuptools import setup
from setuptools import find_packages
from setuptools.command.install import install
from os import path
import subprocess
import urllib2
import tarfile
import glob
import sys
import ast
import os

#To ensure a script runs with a minimal version requirement of the Python interpreter
#assert sys.version_info >= (2,5)
if (sys.version_info[0]==2 and sys.version_info<=(2,5)) or (sys.version_info[0]==3 and sys.version_info<(3,3)):
    exit( 'The version number of Python has to be >= 2.6 or >= 3.3' )

try: 
    input = raw_input
except NameError:
    pass

try:
    from urllib.request import urlopen # Python 3 - not verified
except ImportError:
    from urllib2 import urlopen # Python 2


here = path.abspath(path.dirname(__file__))
python_ver=sys.version[:3]
user_shell=os.environ['SHELL']
lib_dir=''
path_dir=''

if 'bash' in user_shell:
    user_shell='.bashrc'
else:
    user_shell='.profile'

def get_conf_files(search_dir='etc'):
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
        print os.getcwd()
        os.chdir('..')
        print os.getcwd()
    return file_list

def get_long_description():
    readme_file = 'README'
    if not os.path.isfile(readme_file):
        return ''
    # Try to transform the README from Markdown to reStructuredText.
    try:
        import pandoc
        pandoc.core.PANDOC_PATH = 'pandoc'
        doc = pandoc.Document()
        doc.markdown = open(readme_file).read()
        description = doc.rst
    except Exception:
        description = open(readme_file).read()
    return description

def yes_no_choice( message,  default = 'y') :
    """
    Ask for Yes/No questions
    """
    choices = 'y/n' if default.lower() in ( 'y', 'yes' ) else 'y/N'
    values = ( 'y', 'yes', 'n', 'no' )
    choice = ''
    while not choice.strip().lower() in values:
        choice = input("{} ({}) \n".format(message, choices))
    return choice.strip().lower()


class Builder(object):

    export_dir=sys.prefix
    prefix_directory=''
    arguments=str(sys.argv)
    arguments=ast.literal_eval(arguments) #convert from string to list

    def call(self, cmd):
        return subprocess.call(cmd, shell=True)

    def prefix_option(self):
        #Going through the whole list since the options can be defined in different ways (--prefix=dir> or --prefix <dir>)
        #Which is why I'm not using self.arguments.index('--prefix') to find it, since it doesn't check if it's a substring
        #Could also do it with a while and make it stop if it finds '--prefix' or '--home'
        for i in range(len(self.arguments)):
            option=self.arguments[i]
            #folder name can't contain '--prefix' or '--home'
            if '--prefix' in option or '--home' in option:
                #I'm working under the impression that the path passed on to prefix has to be an absolute path
                #for the moment, if you use a relative path, gridway's binary files will be copied to a directory relative to where ./gridway-5.8 is
                if '=' in option:
                    self.export_dir=option[option.find('=')+1:]
                    self.prefix_directory='--prefix '+self.export_dir
                else:
                    self.export_dir=self.arguments[i+1]
                    self.prefix_directory='--prefix '+self.export_dir

                global lib_dir
                global path_dir
                if '--prefix' in option:
                    lib_dir=os.path.join(self.export_dir,'lib/python{}/site-packages'.format(python_ver))
                elif '--home' in option:
                    lib_dir=os.path.join(self.export_dir,'lib/python')

                path_dir=os.path.join(self.export_dir,'bin')

                try:
                    os.makedirs(lib_dir)
                except OSError:
                    print('\nDirectory {} already exists'.format(lib_dir))

                message="\nWe are about to modify your {} file.\n" \
                    "If we don't you'll have to define the environment variables PATH and PYTHONPATH" \
                    " or access your installation directory everytime you wish to execute the WRF4G.\n" \
                    "Should we proceed?".format(user_shell)

                ans=yes_no_choice(message)
                if ans[0]=='y':
                    line_exists=False
                    home=os.path.expanduser('~') #to ensure that it will find $HOME directory in all platforms
                    python_export_line='export PYTHONPATH={}:$PYTHONPATH'.format(lib_dir)
                    path_export_line='export PATH={}:$PATH'.format(path_dir)

                    with open('{}/{}'.format(home,user_shell),'r') as f:
                        for i in f.readlines():
                            if python_export_line in i:
                                line_exists=True

                    if not line_exists :
                        with open('{}/{}'.format(home,user_shell),'a') as f:
                            f.write('\n'+python_export_line+'\n'+path_export_line+'\n')

    def download_repository(self):
        response = urlopen('https://meteo.unican.es/work/WRF4G/repository.tar.gz')
        tar_file = response.read()
        #the disadvantage of this method is that the entire file is loaded into ram before being saved to disk
        with open('repository.tar.gz','wb') as output:
            output.write(tar_file)

    def repository_files(self, members):
        for tarinfo in members:
            if "repository" in tarinfo.name:
                yield tarinfo

    def extract_repository(self):
        self.download_repository()
        with tarfile.open('repository.tar.gz', 'r') as tar:
            repository_path = path.dirname( path.dirname( os.environ.get('_', '/usr/bin/python') ) )
            tar.extractall(path=repository_path, members=self.repository_files(tar))
        os.remove('repository.tar.gz')

    def build(self):
        self.prefix_option()
        self.extract_repository()


class build_wrapper(install):
    def run(self):
        Builder().build()
        install.run(self)

bin_scripts= glob.glob(os.path.join('bin', '*'))
bin_scripts.append('LICENSE')

setup(
    name='wrf4g',
    packages=find_packages(),
    package_data={'wrf4g': get_conf_files()},
    version='2.3.0',
    author='Meteorology Group UC',
    author_email='josecarlos.blanco@unican.es',
    url='https://meteo.unican.es/trac/wiki/WRF4G2.0',
    license='European Union Public License 1.1',
    description='WRF for Grid (WRF4G) is a framework for the execution and monitoring of the WRF Modelling System.',
    long_description = get_long_description(),
    classifiers=[
        "Intended Audience :: Science/Research",
        "Programming Language :: Python",
        "Topic :: Scientific/Engineering",
        "Topic :: Office/Business :: Scheduling",
        "Programming Language :: Python :: 2.6",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3.3",
        "Programming Language :: Python :: 3.4",
        "Programming Language :: Python :: 3.5",
        "Programming Language :: Python :: 3.6",
    ],
    install_requires=['drm4g', 'sqlalchemy', 'six', 'python-dateutil'],
    scripts=bin_scripts,
    cmdclass={
        'install': build_wrapper,
    },
)

if lib_dir:
    print('\n\033[93mTo finish with the installation, you have to add the following paths to your $PYTHONPATH and $PATH:\e[0m\n' \
        '    export PYTHONPATH={}:$PYTHONPATH\n' \
        '    export PATH={}:$PATH\033[0m'.format(lib_dir,path_dir))
