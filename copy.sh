#!/bin/bash

E3SM_REFROOT=/global/u1/t/tienyiao/climate_models/E3SM
cwd=`pwd`

target_dirs=(
    components/mpas-seaice
    components/mpas-source/src/core_seaice
)


for target_dir in "${target_dirs[@]}"; do

    src_dir=$E3SM_REFROOT/${target_dir}
    dst_dir=$cwd/${target_dir}
    mkdir -p $dst_dir
    cp -r $src_dir $dst_dir

done
