#!/bin/bash
echo "Compilation of netCDF diagnostic computation...."
source /software/ScientificLinux/4.6/etc/bashrc
f90=pgf90
options='-L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree'

basemodules='module_constants'
modules='module_diagnostic module_tools'
main=diagnostics_computation

objmodules=''
for module in ${basemodules}
do
  ${f90} ${module}.f90 ${options} -c ${module}.o
  echo "${f90} ${module}.f90 ${options} -c ${module}.o"
  objmodules=${objmodules}' '${module}.o
### end basic modules
done

for module in ${modules}
do
  ${f90} ${module}.f90 ${options} -c ${module}.o ${objmodules}.o
  echo "${f90} ${module}.f90 ${options} -c ${module}.o ${basemodule}.o"
  objmodules=${objmodules}' '${module}.o
### end modules
done

${f90} ${main}.f90 ${options} -o ${main} ${objmodules}
echo "${f90} ${main}.f90 ${options} -o ${main} ${objmodules}"
