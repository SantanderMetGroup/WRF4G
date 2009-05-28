
BASEEXP="dpccc"
domain_name="Cantabria25a5"
max_dom=2

export "PATH=/oceano/gmeteo/WORK/chus/wrf4g/ui/scripts:$PATH"

for ncore in 4 8 16 24 32 64
do
  case $ncore in
    4)
      NUMBER_OF_NODES=1
      PROCESSES_PER_NODE=4
      multiphysics_combinations="-1,-1/2,2/4,1/1,4"
      ;;
    8)
      NUMBER_OF_NODES=1
      PROCESSES_PER_NODE=8
      multiphysics_combinations="-1,-1/2,4/4,2/1,8/8,1"
      ;;
    16)
      NUMBER_OF_NODES=2
      PROCESSES_PER_NODE=8
      multiphysics_combinations="-1,-1/2,8/8,2/4,4"
      ;;
    24)
      NUMBER_OF_NODES=3
      PROCESSES_PER_NODE=8
      multiphysics_combinations="-1,-1/3,8/8,3/6,4/4,6"
      ;;
    32)
      NUMBER_OF_NODES=4
      PROCESSES_PER_NODE=8
      multiphysics_combinations="-1,-1/4,8/8,4"
      ;;
    64)
      NUMBER_OF_NODES=8
      PROCESSES_PER_NODE=8
      multiphysics_combinations="-1,-1"
      ;;
  esac
  pncore=$(printf "%02d" $ncore)
  cat << __EOF | cat wrf4g.conf.in - > wrf4g.conf
    NUMBER_OF_NODES=${NUMBER_OF_NODES}
    PROCESSES_PER_NODE=${PROCESSES_PER_NODE}
__EOF
  cat << __EOF | cat - wrf.input.in > wrf.input
    experiment_name="${BASEEXP}${pncore}"
    domain_name="${domain_name}"
    max_dom=${max_dom}
    multiphysics_combinations="${multiphysics_combinations=}" 
__EOF
  #wrf4g_submitter.sh
done
