#!/bin/bash

alpha=(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z)
i1=0
i2=0
i3=0

first_loop=0
if  [ ${#} -eq 2 ]  && [ ${2} == "." ];then
   first_loop=1
fi

if [ ${#} -eq 1  -o $first_loop -eq 1 ] ; then

   rm -f GRIBFILE.??? >& /dev/null

   for f in $1* ; do
      
      ln -sf ${f} GRIBFILE.${alpha[$i3]}${alpha[$i2]}${alpha[$i1]}
      let i1++
   
      if [ $i1 -gt 25 ]; then
         i1=0
         let i2++
        if [ $i2 -gt 25 ]; then
           i2=0
           let i3++
           if [ $i3 -gt 25 ]; then
              echo "RAN OUT OF GRIB FILE SUFFIXES!"
           fi
        fi
      fi
   
   done

elif [ ${#} -gt 1 ]; then

   rm -f GRIBFILE.??? >& /dev/null

   for f in $* ; do
      if [ $f != "." ]; then
         ln -sf ${f} GRIBFILE.${alpha[$i3]}${alpha[$i2]}${alpha[$i1]}
         let i1++
   
         if [ $i1 -gt 25 ]; then
            i1=0
            let i2++
            if [ $i2 -gt 25 ]; then
               i2=0
               let i3++
               if [ $i3 -gt 25 ]; then
                  echo "RAN OUT OF GRIB FILE SUFFIXES!"
               fi
            fi
         fi
      fi
   
   done
elif [ ${#} -eq 0 ]; then
   echo " " 
   echo " " 
   echo "   Please provide some GRIB data to link"
   echo "   usage: $0 path_to_grib_data/grib_data_root"
   echo " " 
   echo " " 
fi

