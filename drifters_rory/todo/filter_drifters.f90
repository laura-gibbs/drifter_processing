program filter_drifters

  use iso_fortran_env, only : int32, real32
  use mod_drifter, only: drftr_rm_slip, &
                         drftr_rm_intertial, &       
                         drftr_rm_geo

  implicit none

  integer(int32) :: i, j
  integer(int32) ::  drft_cnt, drft_cnt_1, drft_cnt_2
  integer(int32) ::  id, id_1, id_2
  integer(int32) ::  Nlocs, Nlocs_1, Nlocs_2
  character(len=128) :: path0, pin1, pin2, pout
  real(real32), allocatable :: drift_vel(:, :)
  real(real32), allocatable :: drift_vel_wind(:, :)
  real(real32), allocatable :: drift_wind(:, :)
  real(real32), allocatable :: drift_geo(:, :)
 !----------------------------------------------------------

 !----------------------------------------------------------
  path0 = '/home/rb13801/rdsf/data/'
  pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'
  pin1 = 'data/drifters/'
  pin2 = 'data/corrections/'
  pout = 'data/drifters/'
 !----------------------------------------------------------


 ! Read in the drifters and filter
 ! Note: still need to remove drouge off drifters
 !       drift_vel(k, :) = [1.0 * n0, drec % lon, drec % lat, drec % ve, drec % vn]
 !----------------------------------------------------------
  open(21, file = trim(pin1)//'drifters.dat', form = 'unformatted')
  open(22, file = trim(pin2)//'drifters_wind.dat', form = 'unformatted')
  open(23, file = trim(pin2)//'drifters_geo.dat', form = 'unformatted')

  open(31, file = trim(pout)//'drifters_filtered.dat', form = 'unformatted')

  read(21) drft_cnt
  read(22) drft_cnt_1
  read(23) drft_cnt_2
  if ((drft_cnt /= drft_cnt_1).or.(drft_cnt /= drft_cnt_2)) &
    stop 'error: no. of drifters does not match!'
  write(31) drft_cnt

  do i = 1, drft_cnt

    print *, 'processing drifter', i, 'of', drft_cnt

    read(21) id, Nlocs
    read(22) id_1, Nlocs_1
    read(23) id_2, Nlocs_2
    if ((id /= id_1).or.(id /= id_2)) &
      stop 'error: drifter IDs do not match!'
    if ((Nlocs /= Nlocs_1).or.(Nlocs /= Nlocs_2)) &
      stop 'error: no. of drifter locations do not match!'
    write(31) id, Nlocs

    allocate(drift_vel(Nlocs, 5))
    allocate(drift_wind(Nlocs, 5))
    allocate(drift_geo(Nlocs, 5))
    allocate(drift_vel_wind(Nlocs, 7))

    read(21)drift_vel
    read(22)drift_wind
    read(23)drift_geo

   ! 1. Remove the wind-slip:
   !    - for this we need the wind at the drifter locations
    drift_vel = drftr_rm_slip(drift_vel, drift_wind) 

   ! 2. Apply low-pass filter to remove inertial motion: 
   !    - need to calculate intertial period
    drift_vel = drftr_rm_intertial(drift_vel)

   ! 3. Remove geostrophic time-var component:
   !    - for this we need to time-val geo currents at 
   !      drifter locations and times. Guess we need to 
   !      interpolate in time as well. But how to do
   !      this for drifters prior to 1993?
    drift_vel = drftr_rm_geo(drift_vel, drift_geo)

   ! 4. Append wind vels to drifter vels
   ! (useful later)
    do j = 1, Nlocs
      drift_vel_wind(j, 1:5) = drift_vel(j, :)
      drift_vel_wind(j, 6:7) = drift_wind(j, 4:5)
    end do

    write(31)drift_vel_wind

    deallocate(drift_vel)
    deallocate(drift_wind)
    deallocate(drift_geo)
    deallocate(drift_vel_wind)

  end do
  close(21)
  close(22)
  close(23)
  close(31)
 !----------------------------------------------------------

end program filter_drifters
