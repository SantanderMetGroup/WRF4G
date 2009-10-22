if test ${#} -ne 3; then
  echo "Usage: xxxxxx d0X insize outsize"
  exit
fi

dom=$1
original_size=$2
expected_size=$3

for file in wrfout_${dom}*.nc; do
  test "$(stat -c %s ${file})" -eq "$original_size" || continue
  echo "Processing $file"
  ncks -O -v U,V,PH,PHB,T,P,PB,QVAPOR,Q2,T2,TH2,PSFC,U10,V10,RAINC,RAINNC,SNOWNC,SWDOWN,Times ${file} tmp.nc
  ncap2 -O -s 'PRES=P+PB;GEOP=PH+PHB' tmp.nc tmp2.nc
  ncks -O -x -v P,PB,PH,PHB tmp2.nc ${file}.part
  rm -f tmp.nc tmp2.nc
  if test "$(stat -c %s ${file}.part)" -eq "$expected_size"; then
    mv ${file}.part ${file}
  fi
done
