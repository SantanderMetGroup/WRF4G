

i=0
ls wrfout_d01* | while read f
do
  if test $i -eq 0; then
    expsize=$(stat -c %s $f)
    idate=$(echo $f | sed -e "s/^.*\([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\).*$/\1/")
  fi
  fildate=$(echo $f | sed -e "s/^.*\([0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\).*$/\1/")
  expdate=$(date '+%Y%m%d' -d "$idate $i days")
  filsize=$(stat -c %s $f)
  echo $fildate/$filsize $expdate/$expsize 
  if test $expsize -ne $filsize; then
    echo "$f: The file size ($filsize) does not match the expected size ($expsize)"
    exit
  fi
  if test $expdate -ne $fildate; then
    echo "Missing file for date $expdate"
    exit
  fi
  let i++
done
