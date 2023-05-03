program tile_drifters

  use iso_fortran_env, only : int32, real32

  implicit none

  real(real32), parameter :: tile_size = 5.0

  integer(int32), parameter :: Ntiles_lon = 360.0 / tile_size
  integer(int32), parameter :: Ntiles_lat = 180.0 / tile_size

  integer(int32) :: i, j, m, n
  integer(int32) :: i0, j0
  integer(int32) ::  drft_cnt
  integer(int32) ::  id, Nlocs, Nobs_max
  character(len=128) :: path0, pin1, pout
  real(real32), allocatable :: drift_vel(:, :)
  real(real32) :: tile_lon(Ntiles_lon)
  real(real32) :: tile_lat(Ntiles_lat)
  real(real32) :: drift_lon, drift_lat
  integer(int32) :: tile_cnt(Ntiles_lon, Ntiles_lat)
  real(real32), allocatable :: tile_vel(:, :)
 !----------------------------------------------------------

 !----------------------------------------------------------
  path0 = '/home/rb13801/rdsf/data/'
  pin1 = trim(path0)//'src/obs/ocean/drifters/SVP/'
  pin1 = 'data/drifters/'
  pout = 'data/drifters/'
 !----------------------------------------------------------

 !----------------------------------------------------------
  tile_lon = [(tile_size * (i - 0.5), i = 1, Ntiles_lon)]
  tile_lat = [(tile_size * (j - 0.5) - 90.0, j = 1, Ntiles_lat)]
 !----------------------------------------------------------

 ! Find maximum obs per tile
 !----------------------------------------------------------
  tile_cnt = 0
  open(21, file = trim(pin1)//'drifters_filtered.dat', form = 'unformatted')

  read(21) drft_cnt

  do i = 1, drft_cnt

    !print *, 'processing drifter', i, 'of', drft_cnt

    read(21) id, Nlocs

    allocate(drift_vel(Nlocs, 7))

    read(21)drift_vel

    do j = 1, Nlocs
      drift_lon = drift_vel(n, 2)
      if (drift_lon < 0.0) drift_lon = drift_lon + 360.0 
      drift_lat = drift_vel(n, 3)
      i0 = minloc(abs(tile_lon - drift_lon), 1)
      j0 = minloc(abs(tile_lat - drift_lat), 1)
      tile_cnt(i0, j0) = tile_cnt(i0, j0) + 1
    end do

    deallocate(drift_vel)

  end do
  close(21)
  Nobs_max =  maxval(maxval(tile_cnt, 2), 1)
  print *, 'maximum drifter obs per tile:', Nobs_max
 !----------------------------------------------------------

 ! Group the drifter records by tile
 !----------------------------------------------------------
  allocate(tile_vel(Nobs_max, 7))
  tile_cnt = 0
  open(31, file = trim(pout)//'drifters_filtered_tiled.dat', form = 'unformatted')
  do i = 1, Ntiles_lon
    do j = 1, Ntiles_lat
    !do j = Ntiles_lat, Ntiles_lat
      open(21, file = trim(pin1)//'drifters_filtered.dat', form = 'unformatted')
      read(21) drft_cnt
      do m = 1, drft_cnt
        read(21) id, Nlocs
        allocate(drift_vel(Nlocs, 7))
        read(21)drift_vel
        do n = 1, Nlocs
          drift_lon = drift_vel(n, 2)
          if (drift_lon < 0.0) drift_lon = drift_lon + 360.0 
          drift_lat = drift_vel(n, 3)
          if ((minloc(abs(tile_lon - drift_lon), 1) == i).and.&
              (minloc(abs(tile_lat - drift_lat), 1) == j)) then
            tile_cnt(i, j) = tile_cnt(i, j) + 1
            tile_vel(tile_cnt(i, j), :) = drift_vel(n, :)
          end if
        end do
        deallocate(drift_vel)
      end do
      close(21)
      print *, i, j, tile_lon(i), tile_lat(j), tile_cnt(i, j)
      !stop
      write(31) i, j, tile_cnt(i, j)
      if (tile_cnt(i, j) > 0) then
        write(31) tile_vel(1:tile_cnt(i, j), :)
      end if
    end do
  end do
  close(31) 
 !----------------------------------------------------------

 !----------------------------------------------------------
  open(31, file = trim(pout)//'drifters_tile_count.dat', form = 'unformatted')
  write(31) tile_cnt
  close(31)
 !----------------------------------------------------------

end program tile_drifters
