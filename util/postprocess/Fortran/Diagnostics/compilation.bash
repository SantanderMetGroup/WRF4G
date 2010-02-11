#!/bin/bash 
leave_last() {
# Funciton to give a word without its last 'ncar' characters
  word=$1
  ncar=$2
  Lword=`expr length ${word}`
  Lword1=`expr ${Lword} - ${ncar}`
  result=`expr substr ${word} 1 ${Lword1}`
  echo ${result}
}
#######    #######    #######    #######    #######    #######    #######    ########
    #######    #######    #######    #######    #######    #######    ########

echo "Compilation of netCDF diagnostic computation...."
rootsh=`pwd`
source /software/ScientificLinux/4.6/etc/bashrc
f90=pgf90
options='-L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree'

basemodules='module_constants'
modules='module_gen_tools module_calc_tools'
diagnosticspath=diagnostics
modulel='module_list_diagnostics' 
modulec='module_com_diagnostics'

main=diagnostics_computation

basicmodules=''
for module in ${basemodules}
do
  ${f90} ${module}.f90 ${options} -c ${module}.o
  echo "${f90} ${module}.f90 ${options} -c ${module}.o"
  basicmodules=${objmodules}' '${rootsh}/${module}.o
### end basic modules
done

objmodules=''
for module in ${modules}
do
  ${f90} ${module}.f90 ${options} -c ${module}.o ${basicmodules}
  echo "${f90} ${module}.f90 ${options} -c ${module}.o ${basicmodules}"
  objmodules=${objmodules}' '${rootsh}/${module}.o
### end modules
done

cd ${rootsh}/${diagnosticspath}
diagmodules=''
for module0 in mod*.f90
do
  module=`leave_last ${module0} 4`
  ${f90} ${module}.f90 ${options} -I${rootsh} -c ${module}.o ${basicmodules} ${objmodules}
  echo "${f90} ${module}.f90 ${options} -I${rootsh} -c ${module}.o ${basicmodules} ${objmodules}"
  diagmodules=${diagmodules}' '${rootsh}/${diagnosticspath}/${module}.o
### diagnostics compilation
done

cd ${rootsh}

${f90} ${modulel}.f90 ${options} -I${rootsh}/${diagnosticspath} -c ${modulel}.o ${diagmodules} 
echo "${f90} ${modulel}.f90 ${options} -I${rootsh}/${diagnosticspath} -c ${modulel}.o ${diagmodules}"

${f90} ${modulec}.f90 ${options} -I${rootsh}/${diagnosticspath} -c ${modulec}.o ${modulel}.o
echo "${f90} ${modulec}.f90 ${options} -I${rootsh}/${diagnosticspath} -c ${modulec}.o ${modulel}.o"

${f90} ${main}.f90 ${options} -I${rootsh}/${diagnosticspath} -o ${main} ${objmodules} ${modulec}.o
echo "${f90} ${main}.f90 ${options} -I${rootsh}/${diagnosticspath} -o ${main} ${objmodules} ${modulec}.o"
