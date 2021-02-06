WRF4G
=====

WRF4G can be installed in any Linux Computer. It provides the services needed to prepare, run and monitor experiments and it can manage many computing resources and use them at the same time to run different simulations of a WRF experiment.

Although in the following examples we will use the same computer to manage the experiments and to run them, usually scientists use a computer to prepare and manage the experiments (where WRF4G has to be installed) and different computing resources to run them (PBS or SGE Clusters, HPC infrastructures, stand-alone servers). We call User Interface (UI) to the computer where WRF4G is installed and Computing Resources (CR) to the nodes where a WRF experiment can run. In the following experiments the Computer where the UI role is installed will act as CR.

Required Software
-----------------

WRF4G needs the following software preinstalled both in the UI and the CR:

 * x86_64 Linux
 * Python, version >= 2.6 and >= 3.3

Platform Notes
--------------

WRF4G has been run only in 64bits platforms. Currently has been tested under the following O.S.:

 * Ubuntu 16.04, 18.04, 20.04 : No known issues.
 * Centos 6 and 7: No known issues.

Installation
------------
WRF4G can be downloaded from github and installed using the following code:

```bash
git clone https://github.com/SantanderMetGroup/WRF4G.git
cd WRF4G
pip install .
```
It is advisable to use conda enviroments to avoid python packages dependecy problems. 

```bash
conda create -n wrf4g-py36 python=3.6
conda activate wrf4g-py36
```

QuickStart
------------

Example for deploy WRF4G and run a simple test experiment

```
wrf4g start
wrf4g resource
wrf4g exp test define --from-template=single
wrf4g exp test create --dir test
wrf4g exp test submit
wrf4g exp test1 submit --rerun
```

Usage
-----
Check https://meteo.unican.es/trac/wiki/WRF4G2.0 for detailed documentation.
