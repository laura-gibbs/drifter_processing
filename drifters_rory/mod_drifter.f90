module mod_drifter

  use iso_fortran_env, only : int32, int64, real32
  use mod_time, only : days_in_mnth
  use mod_stats, only : mean, stndrd_dev, wghtd_mean
  use mod_spatial, only : check_periodic, spherical_distance

  implicit none

  private

  public :: read_drift_rec, &
            drifter_index, &
            era5_wind_fn_sfx, &
            drftr_rm_slip, &
            drftr_rm_intertial, &
            drftr_rm_geo, &
            ekman_params_tile, &
            wind2ekman, &
            check_tile, &
            fix_ek_params

  type, public :: drift_record
    integer(int64) :: id
    integer(int32) :: mnth, yr
    real(real32) :: day, lat, lon, temp, ve, vn, spd 
    real(real32) :: latv, lonv, tempv
  end type drift_record

contains

 !----------------------------------------------------------
  !pure function fix_ek_params(ek_params, vel_ek, mask, lon, lat, i0, j0, flg) result(ek_params_new)
  function fix_ek_params(ek_params, vel_ek, mask, lon, lat, i0, j0, flg) result(ek_params_new)

   ! Checks a tile to see if it is an outlier 
   ! by comparison to its neighbours.

   !----------------------------------------------
    real(real32), intent(in) :: ek_params(:, :, :)
    real(real32), intent(in) :: vel_ek(:, :, :)
    real(real32), intent(in) :: mask(:, :), flg
    real(real32), intent(in) :: lon(:, :), lat(:, :)
    integer(int32), intent(in) :: i0, j0
    real(real32) :: ek_params_new(2) ! result
    integer(int32), parameter :: Nvals_req = 8
    integer(int32) :: i, j, n
    real(real32), allocatable :: neighbours(:, :)
    real(real32), allocatable :: bvals(:)
    real(real32), allocatable :: vvals(:, :)
    real(real32), allocatable :: wghts(:)
    real(real32) :: v_mn(2)
   !----------------------------------------------

   ! Extract neighbours
   !----------------------------------------------
    allocate(bvals(Nvals_req))
    allocate(vvals(Nvals_req, 2))
    neighbours = get_good_neighbours(mask, lon, lat, i0, j0, flg, Nvals_req)
    do n = 1, Nvals_req
      i = neighbours(n, 1)
      j = neighbours(n, 2)
      bvals(n) = ek_params(i, j, 1)
      vvals(n, :) = vel_ek(i, j, :)
    end do
   !----------------------------------------------

   ! Compute the corrected parameters
   !----------------------------------------------
    wghts = 1.0 / neighbours(:, 3)

    ek_params_new(1) = wghtd_mean(bvals, wghts, flg)

    do i = 1, 2
      v_mn(i) = wghtd_mean(vvals(:, i), wghts, flg)
    end do
    stop 'check atan2 argument order'
    ek_params_new(2) = atan2(v_mn(2), v_mn(1))
   !----------------------------------------------

  end function fix_ek_params
 !----------------------------------------------------------

 !----------------------------------------------------------
  !pure function check_tile(field, lon, lat, i0, j0, flg) result(outlier)
  function check_tile(field, lon, lat, i0, j0, flg) result(outlier)

   ! Checks a tile to see if it is an outlier 
   ! by comparison to its neighbours.

   !----------------------------------------------
    real(real32), intent(in) :: field(:, :, :), flg
    real(real32), intent(in) :: lon(:, :), lat(:, :)
    integer(int32), intent(in) :: i0, j0
    logical :: outlier ! result
    integer(int32), parameter :: Nvals_req = 8
    integer(int32) :: i, j, n
    real(real32), allocatable :: mask(:, :)
    real(real32), allocatable :: neighbours(:, :)
    real(real32), allocatable :: vals(:, :)
    real(real32) :: mn(2), sd(2)
   !----------------------------------------------

   ! Create mask
   !----------------------------------------------
    allocate(mask(size(field, 1), size(field, 2)))
    mask = 0.0
    where (abs(field(:, :, 1)) > flg * 0.1) mask = flg
   !----------------------------------------------

   ! Extract neighbours
   !----------------------------------------------
    allocate(vals(Nvals_req, size(field, 3)))
    neighbours = get_good_neighbours(mask, lon, lat, i0, j0, flg, Nvals_req)
    do n = 1, Nvals_req
      i = neighbours(n, 1)
      j = neighbours(n, 2)
      vals(n, :) = field(i, j, :)
    end do
   !----------------------------------------------

   !----------------------------------------------
   do i = 1, 2
     mn(i) = mean(vals(:, i), flg)
     sd(i) = stndrd_dev(vals(:, i), flg)
    end do
   !----------------------------------------------

   !---------------------------------------------- 
    outlier = .FALSE.
    do i = 1, 2
      if (sd(i) < abs(field(i0, j0, i) - mn(i)) / 2.5) then 
        outlier = .TRUE.
      end if
    end do
   !----------------------------------------------

  end function check_tile
 !----------------------------------------------------------

 !----------------------------------------------------------
  !pure function get_good_neighbours(mask, lon, lat, i0, j0, flg, Nvals_req) result(neighbours)
  function get_good_neighbours(mask, lon, lat, i0, j0, flg, Nvals_req) result(neighbours)

   ! Gets a minimum number of "good" neighbours of a given point.
   ! Searches concentric "squares" around point until minimum
   ! is reached. Then takes the closest x points.

   !----------------------------------------------
    real(real32), intent(in) :: mask(:, :), flg
    real(real32), intent(in) :: lon(:, :), lat(:, :)
    integer(int32), intent(in) :: i0, j0, Nvals_req
    real(real32), allocatable :: neighbours(:, :) ! result
    integer(int32) :: i, j, n, r, cnt, Nvals_max 
    real(real32), allocatable :: tmp(:, :), tmp2(:, :)
   !----------------------------------------------

   ! Extract neighbours
   !----------------------------------------------
    cnt = 0
    r = 0
    do while(cnt < Nvals_req)
      r = r + 1
      tmp = get_shell(lon, lat, i0, j0, r)
      do n = 1, size(tmp, 1)
        i = tmp(n, 1)
        j = tmp(n, 2)
        if (mask(i, j) > flg * 0.1) then
          cnt = cnt + 1
          tmp2(cnt, :) = tmp(n, :)
        end if
      end do
      deallocate(tmp)
    end do
    Nvals_max = cnt
    tmp = tmp2(1:Nvals_max, :)
   !----------------------------------------------

   ! Sort by distance
   !----------------------------------------------
    tmp2 = sort_bubble(tmp, 3)  
   !----------------------------------------------

   !----------------------------------------------
    neighbours = tmp2(1:Nvals_req, :)
   !----------------------------------------------

  end function get_good_neighbours
 !----------------------------------------------------------

 !----------------------------------------------------------
  pure function sort_bubble(x, sort_col) result(y)

    real(real32), intent(in) :: x(:, :) 
    integer(int32), intent(in) :: sort_col
    real(real32), allocatable :: y(:, :) ! result
    integer(int32) :: i, Nvals
    real(real32), allocatable :: tmp1(:, :)
    real(real32) :: tmp2(2, 2)
    logical :: have_swapped

    Nvals = size(x, 1)
    allocate(tmp1(Nvals, 2))
    tmp1(:, 1) = x(:, sort_col)
    tmp1(:, 2) = [(i, i = 1, Nvals)]
    have_swapped = .TRUE.
    do while(have_swapped)
      have_swapped = .FALSE.
      do i = 2, Nvals
        if (tmp1(i - 1, 1) > tmp1(i, 1)) then
          tmp2(1, :) = tmp1(i, :)
          tmp2(2, :) = tmp1(i - 1, :)
          tmp1(i - 1:i, :) = tmp2
          have_swapped = .TRUE.
        end if
      end do
    end do
    do i = 1, Nvals
      y(i, :) = x(int(tmp1(i, 2)), :)
    end do

  end function sort_bubble
 !----------------------------------------------------------

 !----------------------------------------------------------
  !pure function get_shell(lon, lat, i0, j0, r) result(shell)
  function get_shell(lon, lat, i0, j0, r) result(shell)

   ! Extracts a "square" shell of points surrounding a
   ! given point at a given "radius" from the point.

   ! At the moment assumes periodic longitude!

   !----------------------------------------------
    real(real32), intent(in) :: lon(:, :)
    real(real32), intent(in) :: lat(:, :)
    integer(int32), intent(in) :: i0, j0, r
    real(real32), allocatable :: shell(:, :) 
    real(real32), parameter :: flg = -1.9e19
    integer(int32) :: II, JJ, NN
    integer(int32) :: i, j, k, pnt_cnt
    integer(int32) :: i_tmp, i1, i2, j1, j2
    real(real32) :: ln0, lt0
    logical :: periodic
   !----------------------------------------------

   !----------------------------------------------
    II = size(lon, 1)
    JJ = size(lon, 2)

    if((II /= size(lat, 1)).or.(JJ /= size(lat, 2))) &
      stop 'error lon/lat dimensions must agree!'
   !----------------------------------------------

   !----------------------------------------------
    ln0 = lon(i0, j0)
    lt0 = lat(i0, j0)
   !----------------------------------------------

   !----------------------------------------------
    allocate(shell(8 * r, 3))
    shell = flg
   !----------------------------------------------

   !----------------------------------------------
    periodic = check_periodic(lon(:, 1))
   !----------------------------------------------

   !----------------------------------------------
    if (periodic) then
      i1 = i0 - r
      i2 = i0 + r
    else
      i1 = max(1, i0 - r)
      i2 = min(JJ, i0 + r)
    end if
    j1 = max(1, j0 - r)
    j2 = min(JJ, j0 + r)
   !----------------------------------------------

   !----------------------------------------------
    pnt_cnt = 0
   !----------------------------------------------

   ! South face
   !----------------------------------------------
    if (j0 - r > 0) then    
      do i_tmp = i1, i2
        i = i_tmp
        if (i < 1) i = i + II
        if (i > II) i = i - II
        pnt_cnt = pnt_cnt + 1
        shell(pnt_cnt, 1:2) = [i, j]
        shell(pnt_cnt, 3) = spherical_distance(ln0, lt0, &
                                       & lon(i, j), lat(i, j))
      end do
    end if
   !----------------------------------------------

   ! East face
   !----------------------------------------------
    i = i2
    if (i > II) i = i - II
    do j = j1, j2
      pnt_cnt = pnt_cnt + 1
      shell(pnt_cnt, 1:2) = [i, j]
    end do
   !----------------------------------------------

   ! North face
   !----------------------------------------------
    if (j0 + r <= JJ) then    
      do i_tmp = i2, i1, -1
        i = i_tmp
        if (i < 1) i = i + II
        if (i > II) i = i - II
        pnt_cnt = pnt_cnt + 1
        shell(pnt_cnt, 1:2) = [i, j]
      end do
    end if
   !----------------------------------------------

   ! West face
   !----------------------------------------------
    i = i1
    if (i > II) i = i - II
    do j = j2, j1, -1
      pnt_cnt = pnt_cnt + 1
      shell(pnt_cnt, 1:2) = [i, j]
    end do
   !----------------------------------------------

   ! Compute distances to points
   !----------------------------------------------
    do k = 1, pnt_cnt 
      i = shell(k, 1)
      j = shell(k, 2)
      shell(k, 3) = spherical_distance(ln0, lt0, &
                                       lon(i, j), lat(i, j))
    end do
   !----------------------------------------------

  end function get_shell
 !----------------------------------------------------------

 !----------------------------------------------------------
  !pure function wind2ekman(vel_wind, ek_params, lat) result(vel_ek)
  function wind2ekman(vel_wind, ek_params, lat) result(vel_ek)

   ! Computes a set of ekman transport vectors given
   ! a set of 10 m wind vectors and Ekman parameters.

   !----------------------------------------------
    real(real32), intent(in) :: vel_wind(:, :)
    real(real32), intent(in) :: ek_params(2)
    real(real32), intent(in) :: lat(:)
    real(real32), allocatable :: vel_ek(:, :) ! result
    integer(int32) :: n, Nvecs
    real(real32) :: b, theta, stress_mag
    real(real32), allocatable :: stress(:, :) 
    real(real32), allocatable :: f(:) 
    complex(real32) :: stress_cmplx, vel_ek_cmplx
   !----------------------------------------------

   !----------------------------------------------
    Nvecs = size(vel_wind, 1)

    b = ek_params(1)
    theta = ek_params(1)
    stress = wind2stress(vel_wind)
    f = coriolis(lat)
    do n = 1, Nvecs
      stress_mag = sqrt(stress(n, 1)**2 + stress(n, 2)**2)
      stress(n, :) = stress(n, :) / sqrt(f(n) * stress_mag)
      stress_cmplx = complex(stress(n, 1), stress(n, 2)) 
      vel_ek_cmplx = b * stress_cmplx * complex(cos(theta), sin(theta))
      vel_ek(n, :) = [real(vel_ek_cmplx), aimag(vel_ek_cmplx)]
    end do
   !----------------------------------------------

  end function wind2ekman
 !----------------------------------------------------------

!=================================================================

 !----------------------------------------------------------
  !pure function ekman_params_tile(dvel) result(ek_params)
  function ekman_params_tile(dvel) result(ek_params)

   ! Finds the Ekman parameters

    real(real32), intent(in) :: dvel(:, :)
    integer(int32) :: n, Nvecs
    real(real32), allocatable :: stress(:, :) 
    real(real32) :: stress_mag 
    real(real32), allocatable :: f(:) 
    complex(real32), allocatable :: stress_anom(:) 
    complex(real32), allocatable :: dvel_anom(:) 
    complex(real32) :: a 
    real(real32) :: ek_params(2) ! result

    Nvecs = size(dvel, 1)

   ! Compute the wind-stress anomalies
   !----------------------------------------------
    stop 'probably need to screen for very &
               small or zero denominator. And also &
               screen drifter velocities.'
    stress = wind2stress(dvel(:, 6:7))
    f = coriolis(dvel(:, 3))
    do n = 1, Nvecs
      stress_mag = sqrt(stress(n, 1)**2 + stress(n, 2)**2)
      stress(n, :) = stress(n, :) / sqrt(f(n) * stress_mag)
      stress_anom(n) = complex(stress(n, 1), stress(n, 2))
    end do
    stress_anom = stress_anom - sum(stress_anom) / Nvecs
   !----------------------------------------------

   ! Compute drifter velocity anomalies
   ! (may need to screen based on above)
   !----------------------------------------------
    do n = 1, Nvecs
      dvel_anom(n) = complex(dvel(n, 4), dvel(n, 5))
    end do
    dvel_anom = dvel_anom - sum(dvel_anom) / Nvecs
   !----------------------------------------------

   !----------------------------------------------
    a = lin_reg_cmplx(dvel_anom, stress_anom)
    ek_params(1) = sqrt(real(a)**2 + aimag(a)**2)
    ek_params(2) = atan2(aimag(a), real(a))
   !----------------------------------------------

  end function ekman_params_tile
 !----------------------------------------------------------

 !----------------------------------------------------------
  !pure function lin_reg_cmplx(y0, x0) result(a)
  function lin_reg_cmplx(y0, x0) result(a)
    complex(real32), intent(in) :: x0(:), y0(:)
    complex(real32) :: a ! result
    integer(int32) :: Nvals
    complex(real32), allocatable :: x(:), y(:)
    complex(real32), allocatable :: xs(:)
    if (size(x0) /= size(y0)) then
      stop 'error: vector lengths do not match!'
    else
      Nvals = size(x0)
    end if
    x = x0 - sum(x0) / Nvals
    y = y0 - sum(y0) / Nvals
    xs = conjg(x)
    a = sum(xs * y) / sum(x * xs)
  end function lin_reg_cmplx
 !----------------------------------------------------------

 !----------------------------------------------------------
  !pure function coriolis(deg) result(f)
  function coriolis(deg) result(f)
    real(real32), parameter :: pi = 4.0 * atan(1.0)
    real(real32), parameter :: omega = 7.21e-5
    real(real32), intent(in) :: deg(:)
    real(real32), allocatable :: f(:)
    stop 'check omega'
    f = 2.0 * omega * sin(deg * pi / 180.0)
  end function coriolis
 !----------------------------------------------------------

 !----------------------------------------------------------
  pure function deg2rad(deg) result(rad)
    real(real32), parameter :: pi = 4.0 * atan(1.0)
    real(real32), intent(in) :: deg(:)
    real(real32), allocatable :: rad(:)
    rad = deg * pi / 180.0
  end function deg2rad
 !----------------------------------------------------------

 !----------------------------------------------------------
  !pure function wind2stress(vel) result(stress)
  function wind2stress(vel) result(stress)

   ! Computes a set of wind-stress vectors from
   ! a set of 10 m wind vectors.

    real(real32), parameter :: rho = 1.22 !air density: 1.22 kg/m3
    real(real32), parameter :: cd1 = 2.70e-3
    real(real32), parameter :: cd2 = 1.42e-4
    real(real32), parameter :: cd3 = 7.64e-5

    real(real32), intent(in) :: vel(:, :)
    real(real32), allocatable :: stress(:, :) ! result
    integer(int32) :: n, Nvecs
    real(real32) :: wind_speed, cd

    Nvecs = size(vel, 1)

    stress(1:Nvecs, 1:2) = 0.0

    do n = 1, Nvecs
      wind_speed = sqrt(vel(n, 1)**2 + vel(n, 2)**2)
      if (wind_speed /= 0.0) then
        stop 'check this!'
        cd = cd1 / wind_speed + cd2 + cd3 * wind_speed
        stress(n, :) = cd * rho * vel(n, :) * wind_speed
      end if
    end do

  end function wind2stress
 !----------------------------------------------------------

 !----------------------------------------------------------
  pure function drftr_rm_slip(drift_vel, drift_wind) result(drift_vel_cor)

   ! Applies a wind-slip correction to a drifter

    real(real32), intent(in) :: drift_vel(:, :)
    real(real32), intent(in) :: drift_wind(:, :)
    real(real32), allocatable :: drift_vel_cor(:, :) ! result

    drift_vel_cor = drift_vel

  end function drftr_rm_slip
 !----------------------------------------------------------

 !----------------------------------------------------------
  pure function drftr_rm_intertial(drift_vel) result(drift_vel_cor)

   ! Removes intertial motion from a drifter

    real(real32), intent(in) :: drift_vel(:, :)
    real(real32), allocatable :: drift_vel_cor(:, :) ! result

    drift_vel_cor = drift_vel

  end function drftr_rm_intertial
 !----------------------------------------------------------

 !----------------------------------------------------------
  pure function drftr_rm_geo(drift_vel, drift_geo) result(drift_vel_cor)

   ! Removes time-dependent geostrophic motion from a drifter

    real(real32), intent(in) :: drift_vel(:, :)
    real(real32), intent(in) :: drift_geo(:, :)
    real(real32), allocatable :: drift_vel_cor(:, :) ! result

    drift_vel_cor = drift_vel

  end function drftr_rm_geo
 !----------------------------------------------------------

 !----------------------------------------------------------
  function read_drift_rec(funit, file_no) result(drec)

   ! Reads a record from the original drifter files.
    
    integer(int32), intent(in) :: funit, file_no
    
    type(drift_record) :: drec
  
    character(len=128) :: frmt 

    if (file_no < 4) then
      frmt = '(I8, I5, f7.3, I6, 6f10.3, 3e13.2)'
    else
      frmt = '(I15, I5, f7.3, I5, 6f10.3, 3e13.2)'
    end if

    read(funit, frmt) drec % id, & 
                  drec % mnth, drec % day, drec % yr, &
                  drec % lat, drec % lon, &
                  drec % temp, &
                  drec % ve, drec % vn, drec % spd, &
                  drec % latv, drec % lonv, drec % tempv
    !print *, drec

  end function read_drift_rec
 !----------------------------------------------------------

 !----------------------------------------------------------
  pure function drifter_index(drec, yr_ref, timestep) result(n0)

   ! Computes an index given the beginning of yr_ref
   ! and counting in steps of X hours.

    type(drift_record), intent(in) :: drec
    integer(int32), intent(in) :: yr_ref
    integer(int32), intent(in) :: timestep
    integer(int32) :: n0 ! result
    integer(int32) :: m, n, y
    integer(int32) :: ints_per_day
    integer(int32) :: days(12)

    ints_per_day = 24.0 / timestep

    n0 = 0
    do y = yr_ref, drec % yr - 1
      n0 = n0 + sum(days_in_mnth(y))
    end do
    days = days_in_mnth(drec % yr)
    if (drec % mnth > 1) then 
      do m = 1, drec % mnth - 1
        n0 = n0 + days(m)
      end do
    end if
    n0 = ints_per_day * n0 + 1
    n0 = n0 + ints_per_day * drec % day

  end function drifter_index
 !----------------------------------------------------------

 !----------------------------------------------------------
  !pure function era5_wind_fn_sfx(yr) result(fn_sfx)
  function era5_wind_fn_sfx(yr) result(fn_sfx)

   ! Generates the time-range dependent filename suffix for
   ! the ERA5 wind fields. This is arbitrary, depending on 
   ! the way I grouped the data to download and how I named
   ! the files.

    integer(int32), intent(in) :: yr
    character(len=9) :: fn_sfx ! result

    if ((1979 <= yr).and.(yr <= 1985))then
      fn_sfx = '1979-1985'
    elseif ((1986 <= yr).and.(yr <= 1990)) then
      fn_sfx = '1986-1990'
    elseif ((1991 <= yr).and.(yr <= 1995)) then
      fn_sfx = '1991-1995'
    elseif ((1996 <= yr).and.(yr <= 2000)) then
      fn_sfx = '1996-2000'
    elseif ((2001 <= yr).and.(yr <= 2005)) then
      fn_sfx = '2001-2005'
    elseif ((2006 <= yr).and.(yr <= 2010)) then
      fn_sfx = '2006-2010'
    elseif ((2011 <= yr).and.(yr <= 2015)) then
      fn_sfx = '2011-2015'
    elseif ((2016 <= yr).and.(yr <= 2020)) then
      fn_sfx = '2016-2020'
    elseif (2021 == yr) then
      fn_sfx = '2021-2021'
    elseif (2022 == yr) then
      fn_sfx = '2022-2022'
    else
      stop 'cannot form era filename for this year!'
    end if

  end function era5_wind_fn_sfx
 !----------------------------------------------------------

end module mod_drifter  
