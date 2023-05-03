#!/bin/bash
export mod_path='/home/rb13801/tools/my_code/fortran/modules/'
export netcdf_inc='/home/rb13801/local/include'
export netcdf_lib='/home/rb13801/local/lib'
gfortran -c -O3 $mod_path'mod_time.f90' 
gfortran -c -O3 $mod_path'mod_stats.f90' 
gfortran -c -O3 $mod_path'mod_spatial.f90' 
gfortran -c -O3 $mod_path'mod_netcdf.f90' -I$netcdf_inc -L$netcdf_lib -lnetcdff
gfortran -c -O3 mod_drifter.f90 
gfortran -c -O3 drifters_wind.f90 
gfortran *.o -O3 -I$netcdf_inc -L$netcdf_lib -lnetcdff
rm -f *.o *.mod
