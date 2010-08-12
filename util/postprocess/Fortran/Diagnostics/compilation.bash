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

rootsh=`pwd`
if [ $HOSTNAME = 'oceano.macc.unican.es' ]
then
  source /software/ScientificLinux/4.6/etc/bashrc
  f90=pgf90
  options='-L/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/lib -lnetcdf -lm -I/software/ScientificLinux/4.6/netcdf/3.6.3/pgf716_gcc/include -Mfree'

elif [ $HOSTNAME = 'trueno' ]
then
  f90=gfortran
  options='-L/home/lluis/bin/netcdf-4.0.1/lib -lnetcdf -I/home/lluis/bin/netcdf-4.0.1/include'

elif test $HOSTNAME = 'mar.macc.unican.es'
then
  f90=gfortran
  options='-L/software/CentOS/5.2/netcdf/4.1.1/gcc-gfortran4.1.2/lib -lnetcdf -I/software/CentOS/5.2/netcdf/4.1.1/gcc-gfortran4.1.2/include'
fi
echo "Compilation of netCDF diagnostic computation in '"$HOSTNAME"'...."

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

basemodules='module_constants'
modules='module_gen_tools module_nc_tools module_calc_tools'
diagnosticspath='diagnostics'
modulel='module_list_diagnostics' 
modulec='module_com_diagnostics'

main=diagnostics_computation

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

rm *.o *.mod

echo "Compilation of basic modules..."
basicmodules=''
for module in ${basemodules}
do
  echo "${f90} ${module}.f90 ${options} -c "
  ${f90} ${module}.f90 ${options} -c 
  basicmodules=${basicmodules}' '${rootsh}/${module}.o
### end basic modules
done

echo "Compilation of generic modules..."
objmodules=''
for module in ${modules}
do
  echo "${f90} ${module}.f90 ${options} -c "
# ${basicmodules}"
  ${f90} ${module}.f90 ${options} -c 
#  ${f90} ${module}.f90 ${options} -o ${module}.out

# ${basicmodules}
#  objmodules=${objmodules}' '${rootsh}/${module}.o
  objmodules=${objmodules}' '${module}.o

### end modules
done

echo "Compilation of diagnostic modules..."
cd ${rootsh}/${diagnosticspath}
diagmodules=''
for module0 in mod*.f90
do
  module=`leave_last ${module0} 4`
  echo "${f90} ${module}.f90 ${options} -I${rootsh} -c "
#${module}.o ${basicmodules} ${objmodules}"
  ${f90} ${module}.f90 ${options} -I${rootsh} -c 
#${basicmodules} ${objmodules}
  diagmodules=${diagmodules}' '${rootsh}/${diagnosticspath}/${module}.o
#  diagmodules=${diagmodules}' '${module}.o
### diagnostics compilation
done

cd ${rootsh}

echo "Compilation of main program..."

echo "${f90} ${modulel}.f90 ${options} -I${rootsh} -I${rootsh}/${diagnosticspath} -c "
${f90} ${modulel}.f90 ${options} -I${rootsh} -I${rootsh}/${diagnosticspath} -c 

echo "${f90} ${modulec}.f90 ${options} -I${rootsh} -I${rootsh}/${diagnosticspath} -c "
${f90} ${modulec}.f90 ${options} -I${rootsh} -I${rootsh}/${diagnosticspath} -c 

echo "${f90} ${main}.f90 -o ${main} -I${rootsh} -I${rootsh}/${diagnosticspath} ${basicmodules} ${objmodules} ${diagmodules} ${modulel}.o ${modulec}.o ${options}"
${f90} ${main}.f90 -o ${main} -I${rootsh} -I${rootsh}/${diagnosticspath} ${basicmodules} ${objmodules} ${diagmodules} ${modulel}.o ${modulec}.o ${options}
