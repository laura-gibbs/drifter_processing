program drifters_wind

  use iso_fortran_env, only : int32, real32
  use mod_time, only : days_in_mnth, days_in_period
  use mod_spatial, only : bilin_cart_to_points, intrp_bilin
  use mod_netcdf, only: read_nc, read_nc_var_1d, read_nc_var_3d_sub
  use mod_drifter

  implicit none

  real(real32), parameter :: flg = -1.9e19

  integer(int32) :: timestep_hrs = 6 

  integer(int32) :: i, j, k, h, d, y
  integer(int32) :: n, n0, NN, tstep, hour_count
  integer(int32) :: id, i0
  integer(int32) :: strt(3), cnt(3)
  integer(int32) :: Nlocs, drft_cnt
  integer(int32) :: Nlocs_tstep_max
  integer(int32) :: year_min, year_max
  character(len=128) :: path0, pin1, pin2, pout
  character(len=128) :: fn_u, fn_v, fn_sfx
  real(real32), allocatable :: drift_vel(:, :)
  integer(int32), allocatable :: drft_loc_cnt_6hr(:) 
  real(real32), allocatable :: coords(:, :, :)
  real(real32), allocatable :: lon(:), lat(:)
  real(real32), allocatable :: time(:)
  real(real32), allocatable :: wind_u(:, :, :)
  real(real32), allocatable :: wind_v(:, :, :)
  real(real32), allocatable :: qw(:, :, :)
  real(real32), allocatable :: msk(:, :), tmp(:, :)
  real(real32), allocatable :: wind_coords(:, :, :)
  real(real32), allocatable :: drift_wind(:, :)
  logical :: have_meta 
 !----------------------------------------------------------

 !----------------------------------------------------------
  path0 = '/home/rb13801/rdsf/data/'
  !pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'
  pin1 = trim(path0)//'bin/drifters/svp/meta/'
  pin2 = trim(path0)//'src/reanalysis/era/era5/wind_10m/6hr/'
  pout = trim(path0)//'bin/drifters/svp/drifters/'
 !----------------------------------------------------------
 
 ! Read drifter locations grouped by timestep
 !----------------------------------------------------------
  open(21, file = trim(pin1)//'drifter_coords_timestep.dat', form = 'unformatted')
  read(21) Nlocs_tstep_max, year_min, year_max, NN
  print *, Nlocs_tstep_max, year_min, year_max, NN
  allocate(drft_loc_cnt_6hr(NN))
  allocate(coords(Nlocs_tstep_max, 3, NN))
  read(21) drft_loc_cnt_6hr
  read(21) coords
  close(21)
 !----------------------------------------------------------
 
 ! Initiate hour count.  
 !----------------------------------------------------------
  hour_count = (24 * days_in_period([1, 1, 1900], [31, 12, year_min - 1])) &
              - timestep_hrs
  print *, hour_count + timestep_hrs
  !stop
 !----------------------------------------------------------

 ! First we interpolate the wind fields to the 
 ! drifter locations and store in an array matching coords
 !----------------------------------------------------------
  have_meta = .FALSE.
  allocate(wind_coords(Nlocs_tstep_max, 2, NN))
  tstep = 62824  !0
  hour_count = 1069434
  !do y = year_min, year_max
  do y = 2022, 2022
    
    fn_sfx = era5_wind_fn_sfx(y)

    fn_u = trim(pin2)//'era5_wind_u10m_'//trim(fn_sfx)//'.nc'  
    fn_v = trim(pin2)//'era5_wind_v10m_'//trim(fn_sfx)//'.nc'  
    print *, trim(fn_u)
    print *, trim(fn_v)
    !call read_nc(fn_v)
    !stop

    if (.NOT.have_meta) then
      lon = read_nc_var_1d(fn_u, ['longitude'])
      lat = read_nc_var_1d(fn_u, ['latitude'])
      cnt = [size(lon), size(lat), 1]
      have_meta = .TRUE.
    end if
    time = read_nc_var_1d(fn_u, ['time'])
    print *, size(lon), size(lat), size(time)

    do d = 1, sum(days_in_mnth(y))

      do h = 1, 4

        tstep = tstep + 1 ! index into coords array

        hour_count = hour_count + timestep_hrs

        Nlocs = drft_loc_cnt_6hr(tstep)

        print *, y, d, h, tstep, hour_count, Nlocs

        if (Nlocs > 0) then

         ! determine time index in netcdf file
          print *, hour_count
          n0 = minloc(abs(time - hour_count), 1)
          print *, n0
          print *, time(n0)
          !stop
          if (time(n0) /= hour_count) &
            stop 'time indexing error!'
          strt = [1, 1, n0]

         ! read in the time slice for u and v
          print *, strt
          wind_u = read_nc_var_3d_sub(fn_u, ['u10'], strt, cnt)
          !wind_v = read_nc_var_3d_sub(fn_v, ['v10'], strt, cnt)

         ! call interpolation routine
          !print *, Nlocs
          !qw = bilin_cart_to_points(lon, lat, coords(1:Nlocs, 2:3, n))

          !allocate(msk(1:Nlocs, 1))
          !msk = 0.0

          !tmp = intrp_bilin(wind_u(:, :, 1), qw, msk, flg)
          !wind_coords(1:Nlocs, 1, n0) = tmp(:, 1)

          !tmp = intrp_bilin(wind_v(:, :, 1), qw, msk, flg)
          !wind_coords(1:Nlocs, 2, n0) = tmp(:, 1)

          !deallocate(msk, tmp)

        end if

      end do
    end do
  end do
 !----------------------------------------------------------

 ! Next we will regroup and store the interpolated data 
 ! in a format matching the drifter data grouped by drifter 
 !       drift_vel(k, :) = [1.0 * n0, drec % lon, drec % lat, drec % ve, drec % vn]
 !----------------------------------------------------------
  open(21, file = trim(pin1)//'drifters.dat', form = 'unformatted')
  open(31, file = trim(pout)//'drifters_wind.dat', form = 'unformatted')

  read(21) drft_cnt
  write(31) drft_cnt

  do i = 1, drft_cnt

    print *, 'processing drifter', i, 'of', drft_cnt

    read(21) id, Nlocs
    write(31) id, Nlocs

    allocate(drift_vel(Nlocs, 5))
    allocate(drift_wind(Nlocs, 5))

    read(21)drift_vel

    drift_wind = drift_vel

    do k = 1, Nlocs

      ! Need to check n0 is referenced to consistent year
      n0 = drift_vel(k, 1) ! 3rd dimension of coords array

      i0 = minloc(id - coords(:, 1, n0), 1) 

      drift_wind(k, 4:5) = wind_coords(i0, :, n0)
     
    end do 

    write(31)drift_wind

    deallocate(drift_vel)
    deallocate(drift_wind)

  end do
  close(21)
  close(31)
 !----------------------------------------------------------

 !----------------------------------------------------------
  deallocate(wind_coords)
 !----------------------------------------------------------

end program drifters_wind
