subroutine ellipsoid_grid ( n, r, c, ng, xyz )

!*****************************************************************************80
!
!! ELLIPSOID_GRID generates the grid points inside an ellipsoid.
!
!  Discussion:
!
!    The ellipsoid is specified as
!
!      ( ( X - C1 ) / R1 )^2 
!    + ( ( Y - C2 ) / R2 )^2 
!    + ( ( Z - C3 ) / R3 )^2 = 1
!
!    The user supplies a number N.  There will be N+1 grid points along
!    the shortest axis.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of subintervals.
!
!    Input, real ( kind = rk ) R(3), the half axis lengths.
!
!    Input, real ( kind = rk ) C(3), the center of the ellipsoid.
!
!    Input, integer NG, the number of grid points.
!
!    Output, real ( kind = rk ) XYZ(3,NG), the grid point coordinates.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer ng

  real ( kind = rk ) c(3)
  real ( kind = rk ) h
  integer i
  integer i4_ceiling
  integer j
  integer k
  integer m
  integer n
  integer ng2
  integer ni
  integer nj
  integer nk
  integer np
  real ( kind = rk ) p(3,8)
  real ( kind = rk ) r(3)
  real ( kind = rk ) x
  real ( kind = rk ) xyz(3,ng)
  real ( kind = rk ) y
  real ( kind = rk ) z

  if ( r(1) == minval ( r(1:3) ) ) then
    h = 2.0D+00 * r(1) / real ( 2 * n + 1, kind = rk )
    ni = n
    nj = i4_ceiling ( r(2) / r(1) ) * n
    nk = i4_ceiling ( r(3) / r(1) ) * n
  else if ( r(2) == minval ( r(1:3) ) ) then
    h = 2.0D+00 * r(2) / real ( 2 * n + 1, kind = rk )
    nj = n
    ni = i4_ceiling ( r(1) / r(2) ) * n
    nk = i4_ceiling ( r(3) / r(2) ) * n
  else
    h = 2.0D+00 * r(3) / real ( 2 * n + 1, kind = rk )
    nk = n
    ni = i4_ceiling ( r(1) / r(3) ) * n
    nj = i4_ceiling ( r(2) / r(3) ) * n
  end if

  ng2 = 0

  do k = 0, nk
    z = c(3) + real ( k, kind = rk ) * h
    do j = 0, nj
      y = c(2) + real ( j, kind = rk ) * h
      do i = 0, ni
        x = c(1) + real ( i, kind = rk ) * h
!
!  If we have left the ellipsoid, the I loop is completed.
!
        if ( 1.0D+00 < ( ( x - c(1) ) / r(1) ) ** 2 &
                     + ( ( y - c(2) ) / r(2) ) ** 2 &
                     + ( ( z - c(3) ) / r(3) ) ** 2 ) then
          exit
        end if
!
!  At least one point is generated, but more possible by symmetry.
!
        np = 1
        p(1,np) = x
        p(2,np) = y
        p(3,np) = z

        if ( 0 < i ) then
          do m = 1, np
            p(1,m+np) = 2.0D+00 * c(1) - p(1,m)
            p(2,m+np) = p(2,m)
            p(3,m+np) = p(3,m)
          end do
          np = 2 * np
        end if

        if ( 0 < j ) then
          do m = 1, np
            p(1,m+np) = p(1,m)
            p(2,m+np) = 2.0D+00 * c(2) - p(2,m)
            p(3,m+np) = p(3,m)
          end do
          np = 2 * np
        end if

        if ( 0 < k ) then
          do m = 1, np
            p(1,m+np) = p(1,m)
            p(2,m+np) = p(2,m)
            p(3,m+np) = 2.0D+00 * c(3) - p(3,m)
          end do
          np = 2 * np
        end if

        xyz(1:3,ng2+1:ng2+np) = p(1:3,1:np)
        ng2 = ng2 + np

      end do
    end do
  end do

  return
end
subroutine ellipsoid_grid_count ( n, r, c, ng )

!*****************************************************************************80
!
!! ELLIPSOID_GRID_COUNT counts the grid points inside an ellipsoid.
!
!  Discussion:
!
!    The ellipsoid is specified as
!
!      ( ( X - C1 ) / R1 )^2 
!    + ( ( Y - C2 ) / R2 )^2 
!    + ( ( Z - C3 ) / R3 )^2 = 1
!
!    The user supplies a number N.  There will be N+1 grid points along
!    the shortest axis.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of subintervals.
!
!    Input, real ( kind = rk ) R(3), the half axis lengths.
!
!    Input, real ( kind = rk ) C(3), the center of the ellipsoid.
!
!    Output, integer NG, the number of grid points.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) c(3)
  real ( kind = rk ) h
  integer i
  integer i4_ceiling
  integer j
  integer k
  integer n
  integer ng
  integer ni
  integer nj
  integer nk
  integer np
  real ( kind = rk ) r(3)
  real ( kind = rk ) x
  real ( kind = rk ) y
  real ( kind = rk ) z

  if ( r(1) == minval ( r(1:3) ) ) then
    h = 2.0D+00 * r(1) / real ( 2 * n + 1, kind = rk )
    ni = n
    nj = i4_ceiling ( r(2) / r(1) ) * n
    nk = i4_ceiling ( r(3) / r(1) ) * n
  else if ( r(2) == minval ( r(1:3) ) ) then
    h = 2.0D+00 * r(2) / real ( 2 * n + 1, kind = rk )
    nj = n
    ni = i4_ceiling ( r(1) / r(2) ) * n
    nk = i4_ceiling ( r(3) / r(2) ) * n
  else
    h = 2.0D+00 * r(3) / real ( 2 * n + 1, kind = rk )
    nk = n
    ni = i4_ceiling ( r(1) / r(3) ) * n
    nj = i4_ceiling ( r(2) / r(3) ) * n
  end if

  ng = 0

  do k = 0, nk
    z = c(3) + real ( k, kind = rk ) * h
    do j = 0, nj
      y = c(2) + real ( j, kind = rk ) * h
      do i = 0, ni
        x = c(1) + real ( i, kind = rk ) * h
!
!  If we have left the ellipsoid, the I loop is completed.
!
        if ( 1.0D+00 < ( ( x - c(1) ) / r(1) ) ** 2 &
                     + ( ( y - c(2) ) / r(2) ) ** 2 &
                     + ( ( z - c(3) ) / r(3) ) ** 2 ) then
          exit
        end if
!
!  At least one point is generated, but more possible by symmetry.
!
        np = 1

        if ( 0 < i ) then
          np = 2 * np
        end if

        if ( 0 < j ) then
          np = 2 * np
        end if

        if ( 0 < k ) then
          np = 2 * np
        end if

        ng = ng + np

      end do
    end do
  end do

  return
end
subroutine get_unit ( iunit )

!*****************************************************************************80
!
!! GET_UNIT returns a free FORTRAN unit number.
!
!  Discussion:
!
!    A "free" FORTRAN unit number is a value between 1 and 99 which
!    is not currently associated with an I/O device.  A free FORTRAN unit
!    number is needed in order to open a file with the OPEN command.
!
!    If IUNIT = 0, then no free FORTRAN unit could be found, although
!    all 99 units were checked (except for units 5, 6 and 9, which
!    are commonly reserved for console I/O).
!
!    Otherwise, IUNIT is a value between 1 and 99, representing a
!    free FORTRAN unit.  Note that GET_UNIT assumes that units 5 and 6
!    are special, and will never return those values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    26 October 2008
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Output, integer IUNIT, the free unit number.
!
  implicit none

  integer i
  integer ios
  integer iunit
  logical lopen

  iunit = 0

  do i = 1, 99

    if ( i /= 5 .and. i /= 6 .and. i /= 9 ) then

      inquire ( unit = i, opened = lopen, iostat = ios )

      if ( ios == 0 ) then
        if ( .not. lopen ) then
          iunit = i
          return
        end if
      end if

    end if

  end do

  return
end
function i4_ceiling ( r )

!*****************************************************************************80
!
!! I4_CEILING rounds an R8 "up" (towards +oo) to the next I4.
!
!  Example:
!
!    R     Value
!
!   -1.1  -1
!   -1.0  -1
!   -0.9   0
!    0.0   0
!    5.0   5
!    5.1   6
!    5.9   6
!    6.0   6
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    10 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) R, the value to be rounded up.
!
!    Output, integer I4_CEILING, the rounded value.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer i4_ceiling
  real ( kind = rk ) r
  integer value

  value = int ( r )
  if ( real ( value, kind = rk ) < r ) then
    value = value + 1
  end if

  i4_ceiling = value

  return
end
subroutine r83vec_print_part ( n, a, max_print, title )

!*****************************************************************************80
!
!! R83VEC_PRINT_PART prints "part" of an R83VEC.
!
!  Discussion:
!
!    The user specifies MAX_PRINT, the maximum number of lines to print.
!
!    If N, the size of the vector, is no more than MAX_PRINT, then
!    the entire vector is printed, one entry per line.
!
!    Otherwise, if possible, the first MAX_PRINT-2 entries are printed,
!    followed by a line of periods suggesting an omission,
!    and the last entry.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    09 November 2011
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, integer N, the number of entries of the vector.
!
!    Input, real ( kind = rk ) A(3,N), the vector to be printed.
!
!    Input, integer MAX_PRINT, the maximum number of lines
!    to print.
!
!    Input, character ( len = * ) TITLE, a title.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer n

  real ( kind = rk ) a(3,n)
  integer i
  integer max_print
  character ( len = * )  title

  if ( max_print <= 0 ) then
    return
  end if

  if ( n <= 0 ) then
    return
  end if

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) trim ( title )
  write ( *, '(a)' ) ' '

  if ( n <= max_print ) then

    do i = 1, n
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do

  else if ( 3 <= max_print ) then

    do i = 1, max_print - 2
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do
    write ( *, '(a)' ) &
      '  ........  ..............  ..............  ..............'
    i = n
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)

  else

    do i = 1, max_print - 1
      write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6)' ) i, ':', a(1:3,i)
    end do
    i = max_print
    write ( *, '(2x,i8,a,1x,g14.6,2x,g14.6,2x,g14.6,2x,a)' ) i, ':', a(1:3,i), &
      '...more entries...'

  end if

  return
end
subroutine r8mat_write ( output_filename, m, n, table )

!*****************************************************************************80
!
!! R8MAT_WRITE writes an R8MAT file.
!
!  Discussion:
!
!    An R8MAT is an array of R8 values.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    31 May 2009
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, character ( len = * ) OUTPUT_FILENAME, the output file name.
!
!    Input, integer M, the spatial dimension.
!
!    Input, integer N, the number of points.
!
!    Input, real ( kind = rk ) TABLE(M,N), the data.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer m
  integer n

  integer j
  character ( len = * ) output_filename
  integer output_status
  integer output_unit
  character ( len = 30 ) string
  real ( kind = rk ) table(m,n)
!
!  Open the file.
!
  call get_unit ( output_unit )

  open ( unit = output_unit, file = output_filename, &
    status = 'replace', iostat = output_status )

  if ( output_status /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'R8MAT_WRITE - Fatal error!'
    write ( *, '(a,i8)' ) '  Could not open the output file "' // &
      trim ( output_filename ) // '" on unit ', output_unit
    output_unit = -1
    stop
  end if
!
!  Create a format string.
!
!  For less precision in the output file, try:
!
!                                            '(', m, 'g', 14, '.', 6, ')'
!
  if ( 0 < m .and. 0 < n ) then

    write ( string, '(a1,i8,a1,i8,a1,i8,a1)' ) '(', m, 'g', 24, '.', 16, ')'
!
!  Write the data.
!
    do j = 1, n
      write ( output_unit, string ) table(1:m,j)
    end do

  end if
!
!  Close the file.
!
  close ( unit = output_unit )

  return
end
subroutine timestamp ( )

!*****************************************************************************80
!
!! TIMESTAMP prints the current YMDHMS date as a time stamp.
!
!  Example:
!
!    31 May 2001   9:45:54.872 AM
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    18 May 2013
!
!  Author:
!
!    John Burkardt
!
  implicit none

  character ( len = 8 ) ampm
  integer d
  integer h
  integer m
  integer mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer n
  integer s
  integer values(8)
  integer y

  call date_and_time ( values = values )

  y = values(1)
  m = values(2)
  d = values(3)
  h = values(5)
  n = values(6)
  s = values(7)
  mm = values(8)

  if ( h < 12 ) then
    ampm = 'AM'
  else if ( h == 12 ) then
    if ( n == 0 .and. s == 0 ) then
      ampm = 'Noon'
    else
      ampm = 'PM'
    end if
  else
    h = h - 12
    if ( h < 12 ) then
      ampm = 'PM'
    else if ( h == 12 ) then
      if ( n == 0 .and. s == 0 ) then
        ampm = 'Midnight'
      else
        ampm = 'AM'
      end if
    end if
  end if

  write ( *, '(i2,1x,a,1x,i4,2x,i2,a1,i2.2,a1,i2.2,a1,i3.3,1x,a)' ) &
    d, trim ( month(m) ), y, h, ':', n, ':', s, '.', mm, trim ( ampm )

  return
end
