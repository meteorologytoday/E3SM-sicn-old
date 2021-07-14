#!/bin/bash

ref_file=/global/cfs/cdirs/e3sm/inputdata/ocn/mpas-o/oEC60to30v3/oEC60to30v3.monthlySurfaceForcingBGC.171120.nc
mesh_file=/global/cfs/cdirs/e3sm/inputdata/ocn/mpas-o/oEC60to30v3/oEC60to30v3_60layer.170905.nc
output_file=mpas_seaice_forcing_1.nc

ncks -O -v xtimeOrig,xtime $ref_file $output_file
ncks -A -v latCell,lonCell $mesh_file $output_file
ncap2 -s 'sit_target=array(0.0, 0.0, /$Time, $nCells/)'                  \
      -s 'where(latCell>(40.0 * (atan(1)/45.0) ) ) sit_target = 1.0' \
      -O $output_file $output_file




