program ekman_params_refine

  use iso_fortran_env, only : int32, real32
  use mod_drifter, only : wind2ekman, check_tile, &
                                  fix_ek_params

  implicit none

  integer(int32), parameter :: tile_size = 5.0
  integer(int32), parameter :: Ntiles_lon = 360.0 / tile_size
  integer(int32), parameter :: Ntiles_lat = 180.0 / tile_size

  real(real32), parameter :: flg = -1.9e19
  real(real32), parameter :: flg0p1 = flg * 0.1

 ! Input / Output
  real(real32) :: ek_params(Ntiles_lon, Ntiles_lat, 2)

 ! Local
  integer(int32) :: i, j
  real(real32) :: wind_ref(1, 2)
  real(real32) :: mask(Ntiles_lon, Ntiles_lat)
  real(real32) :: tile_lon(Ntiles_lon)
  real(real32) :: tile_lat(Ntiles_lat)
  real(real32), allocatable :: lon(:, :), lat(:, :)
  real(real32) :: lat_list(1)
  real(real32) :: vel_ek_list(1, 2)
  real(real32) :: vel_ek(Ntiles_lon, Ntiles_lat, 2)
  logical :: outlier(Ntiles_lon, Ntiles_lat), outliers
  character(len=128) :: path0, pin1, pin2, pout
 !----------------------------------------------------------

 !----------------------------------------------------------
  path0 = '/home/rb13801/rdsf/data/'
  pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'
  pin1 = 'data/drifters/'
  pout = 'data/drifters/'
 !----------------------------------------------------------

 !----------------------------------------------------------
  open(21, file = trim(pin1)//'ekman_params_0.dat', form = 'unformatted')
  read(21) ek_params
  close(21)
 !----------------------------------------------------------

 !----------------------------------------------------------
  print *, 'create a mask that inlcudes land as -1.9e19 &
            and tiles with insufficent values as 1.9e19'
  stop
 !----------------------------------------------------------

 !----------------------------------------------------------
  tile_lon = [(tile_size * (i - 0.5), i = 1, Ntiles_lon)]
  tile_lat = [(tile_size * (j - 0.5) - 90.0, j = 1, Ntiles_lat)]
 !----------------------------------------------------------

 ! Compute Ekman transport for each cell
 !----------------------------------------------------------
  wind_ref(1, :) = [1.0, 0.0]
  vel_ek = 0.0
  do i = 1, Ntiles_lon
    do j = 1, Ntiles_lat
      if (abs(mask(i, j)) < flg0p1) then
         lat_list = tile_lat(j)
         vel_ek_list = wind2ekman(wind_ref, ek_params(i, j, :), lat_list) 
         vel_ek(i, j, :) = vel_ek_list(1, :)
      end if
    end do
  end do
 !----------------------------------------------------------

 !----------------------------------------------------------
  do j = 1, Ntiles_lat
    lon(:, j) = tile_lon
  end do
  do i = 1, Ntiles_lon
    lat(i, :) = tile_lat
  end do
  outliers = .TRUE.
  do while(outliers)
    outliers = .FALSE.
    do i = 1, Ntiles_lon
      do j = 1, Ntiles_lat
        if (abs(mask(i, j)) < flg0p1) then
          outlier(i, j) = check_tile(vel_ek, lon, lat, i, j, flg)
          if (outlier(i, j)) then
            outliers = .TRUE.
            mask(i, j) = -flg
          end if
        end if
      end do
    end do
    if (outliers) then
      ek_params_new = ek_params
      vel_ek_new = vel_ek
      mask_new = mask
      do i = 1, Ntiles_lon
        do j = 1, Ntiles_lat
          if ((abs(mask(i, j)) < flg0p1) &
                       .and.(outlier(i, j))) then
            ek_params_new(i, j, :) = &
                       fix_ek_params(vel_ek, ek_params, mask lon, lat, i, j, flg)
            vel_ek_list = wind2ekman(wind_ref, ek_params(i, j, :), lat_list) 
            vel_ek_new(i, j, :) = vel_ek_list(1, :)
            mask_new(i, j) = 0.0
          end if
        end do
        ek_params = ek_params_new 
        vel_ek = ek_params_new 
        mask = mask_new
      end do
    end if
  end do
 !----------------------------------------------------------
 
 !----------------------------------------------------------
  open(31, file = trim(pout)//'ekman_params.dat', form = 'unformatted')
  write(31) ek_params
  close(31)
 !----------------------------------------------------------

end program ekman_params_refine
