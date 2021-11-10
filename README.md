# WRF4G

WRF4G is a framework for executing and monitoring weather and climate experiments with the WRF Modeling System (see this presentation) for an introduction to WRF). It provides a flexible and easy way of designing complex experiments involving many simulations (multiple start/end dates, multi-ensemble simulations, long climate runs and so on). The monitor allows a precise control of the experiment's state, where broken simulations are automatically detected and relaunched at the next submission.

Given a list of computing resources that the user can access, WRF4G submits the experiment to them according to the experiment needs. Users can configure different (Distributed Computing Infrastructures (DCIs) such as HPC, Grid and Cloud resources. The output files are going to be stored depending on the resources used to run the simulations.

WRF4G can be installed in any Linux Computer. It provides the services needed to prepare, run and monitor experiments and it can manage many computing resources and use them at the same time to run different simulations of a WRF experiment.

## Requirements and Installation

WRF4G can be installed in any 64-bit linux operating systems. Before installing WRF4G, make sure that the **_gcc_ compiler, _python3(>3.5)_ and _pip3_** are present in your environment.

WRF4G has been tested under the following operating systems:

* Ubuntu 16.04, 18.04, 20.04 : Issues not known.
* Centos 7: It is necesary to upgrade the default version of pip3 to the most recent. To do that you can run the following command: ```sudo pip3 install --upgrade pip```

The recommended way to install WRF4G is by using the command ```pip3 install wrf4g```

More information about WRF4G installation can be found in the [WRF4G wiki][wrf4g_install_wiki].

## QuickStart

Example for deploy WRF4G and run a simple test experiment

```bash
wrf4g start
wrf4g resource
wrf4g exp test define --from-template=single
wrf4g exp test create --dir test
wrf4g exp test submit
wrf4g exp test1 submit --rerun
```

## Usage
Check the [WRF4G wiki][wrf4g_wiki] for detailed documentation.

[wrf4g_install_wiki]: https://github.com/SantanderMetGroup/WRF4G/wiki/Installation
[wrf4g_wiki]: https://github.com/SantanderMetGroup/WRF4G/wiki/
