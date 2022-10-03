program main

!*****************************************************************************80
!
!! MAIN is the main program for LORENZ_ODE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    14 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    None
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer ( kind = 4 ), parameter :: m = 3
  integer ( kind = 4 ), parameter :: n = 200000

  character ( len = 255 ) command_filename
  integer ( kind = 4 ) command_unit
  character ( len = 255 ) data_filename
  integer ( kind = 4 ) data_unit
  real ( kind = rk ) dt
  integer ( kind = 4 ) j
  external lorenz_rhs
  real ( kind = rk ) t(0:n)
  real ( kind = rk ) t_final
  real ( kind = rk ) x(m,0:n)

  call timestamp ( )
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LORENZ_ODE'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Compute solutions of the Lorenz system.'
  write ( *, '(a)' ) '  Write data to a file for use by gnuplot.'
!
!  Data
!
  t_final = 40.0D+00
  dt = t_final / real ( n, kind = rk )
!
!  Initial conditions.
!
  do j = 0, n
    t(j) = real ( j, kind = rk ) * t_final / real ( n, kind = rk )
  end do

  x(1:m,0) = (/ 8.0D+00, 1.0D+00, 1.0D+00 /)
!
!  Compute the approximate solution at equally spaced times.
!
  do j = 0, n - 1

    call rk4vec ( t(j), m, x(1:m,j), dt, lorenz_rhs, x(1:m,j+1) )

  end do
!
!  Create the data file.
!
  call get_unit ( data_unit )
  data_filename = 'lorenz_ode_data.txt'
  open ( unit = data_unit, file = data_filename, status = 'replace' )
  do j = 0, n, 50
    write ( data_unit, '(2x,g14.6,2x,g14.6,2x,g14.6,2x,g14.6)' ) t(j), x(1:m,j)
  end do
  close ( unit = data_unit )
  write ( *, '(a)' ) '  Created data file "' // trim ( data_filename ) // '".'
!
!  Create the command file.
!
  call get_unit ( command_unit )
  command_filename = 'lorenz_ode_commands.txt'
  open ( unit = command_unit, file = command_filename, status = 'replace' )
  write ( command_unit, '(a)' ) '# ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) '# Usage:'
  write ( command_unit, '(a)' ) '#  gnuplot < ' // trim ( command_filename )
  write ( command_unit, '(a)' ) '#'
  write ( command_unit, '(a)' ) 'set term png'
  write ( command_unit, '(a)' ) &
    'set output "xyz_time.png"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- T --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- X(T), Y(T), Z(T) --->"'
  write ( command_unit, '(a)' ) &
    'set title "X(T), Y(T), Z(T) versus Time"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'plot "' // trim ( data_filename ) // &
    '" using 1:2 lw 3 linecolor rgb "blue",' // &
    ' "" using 1:3 lw 3 linecolor rgb "red",' // &
    ' "" using 1:4 lw 3 linecolor rgb "green"'
  write ( command_unit, '(a)' ) &
    'set output "xyz_3d.png"'
  write ( command_unit, '(a)' ) 'set xlabel "<--- X(T) --->"'
  write ( command_unit, '(a)' ) 'set ylabel "<--- Y(T) --->"'
  write ( command_unit, '(a)' ) 'set zlabel "<--- Z(T) --->"'
  write ( command_unit, '(a)' ) &
    'set title "(X(T),Y(T),Z(T)) trajectory"'
  write ( command_unit, '(a)' ) 'set grid'
  write ( command_unit, '(a)' ) 'set style data lines'
  write ( command_unit, '(a)' ) 'splot "' // trim ( data_filename ) // &
    '" using 2:3:4 lw 1 linecolor rgb "blue"'
  close ( unit = command_unit )
  write ( *, '(a)' ) &
    '  Created command file "' // trim ( command_filename ) // '".'
!
!  Terminate.
!
  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'LORENZ_ODE:'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ''
  call timestamp ( )

  stop
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
!    Output, integer ( kind = 4 ) IUNIT, the free unit number.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer ( kind = 4 ) i
  integer ( kind = 4 ) ios
  integer ( kind = 4 ) iunit
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
subroutine lorenz_rhs ( t, m, x, dxdt )

!*****************************************************************************80
!
!! LORENZ_RHS evaluates the right hand side of the Lorenz ODE.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    08 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) T, the value of the independent variable.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension.
!
!    Input, real ( kind = rk ) X(M), the values of the dependent variables
!    at time T.
!
!    Output, real ( kind = rk ) DXDT(M), the values of the derivatives
!    of the dependent variables at time T.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer ( kind = 4 ) m

  real ( kind = rk ), parameter :: beta = 8.0D+00 / 3.0D+00
  real ( kind = rk ) dxdt(m)
  real ( kind = rk ), parameter :: rho = 28.0D+00
  real ( kind = rk ), parameter :: sigma = 10.0D+00
  real ( kind = rk ) t
  real ( kind = rk ) x(m)

  call r8_fake_use ( t )

  dxdt(1) = sigma * ( x(2) - x(1) )
  dxdt(2) = x(1) * ( rho - x(3) ) - x(2)
  dxdt(3) = x(1) * x(2) - beta * x(3)

  return
end
subroutine r8_fake_use ( x )

!*****************************************************************************80
!
!! r8_fake_use() pretends to use an R8 variable.
!
!  Discussion:
!
!    Some compilers will issue a warning if a variable is unused.
!    Sometimes there's a good reason to include a variable in a program,
!    but not to use it.  Calling this function with that variable as
!    the argument will shut the compiler up.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    21 April 2020
!
!  Author:
!
!    John Burkardt
!
!  Input:
!
!    real ( kind = rk ) X, the variable to be "used".
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) x

  if ( x /= x ) then
    write ( *, '(a)' ) '  r8_fake_use: variable is NAN.'
  end if

  return
end
subroutine rk4vec ( t0, m, u0, dt, f, u )

!*****************************************************************************80
!
!! RK4VEC takes one Runge-Kutta step for a vector system.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license. 
!
!  Modified:
!
!    08 October 2013
!
!  Author:
!
!    John Burkardt
!
!  Parameters:
!
!    Input, real ( kind = rk ) T0, the current time.
!
!    Input, integer ( kind = 4 ) M, the spatial dimension
!
!    Input, real ( kind = rk ) U0(N), the solution estimate at the current time.
!
!    Input, real ( kind = rk ) DT, the time step.
!
!    Input, external F, a subroutine of the form 
!      subroutine f ( t, m, u, uprime ) 
!    which evaluates the derivative UPRIME(1:N) given the time T and
!    solution vector U(1:N).
!
!    Output, real ( kind = rk ) U(M), the fourth-order Runge-Kutta solution 
!    estimate at time T0+DT.
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  integer ( kind = 4 ) m

  real ( kind = rk ) dt
  external f
  real ( kind = rk ) f0(m)
  real ( kind = rk ) f1(m)
  real ( kind = rk ) f2(m)
  real ( kind = rk ) f3(m)
  real ( kind = rk ) t0
  real ( kind = rk ) t1
  real ( kind = rk ) t2
  real ( kind = rk ) t3
  real ( kind = rk ) u(m)
  real ( kind = rk ) u0(m)
  real ( kind = rk ) u1(m)
  real ( kind = rk ) u2(m)
  real ( kind = rk ) u3(m)
!
!  Get four sample values of the derivative.
!
  call f ( t0, m, u0, f0 )

  t1 = t0 + dt / 2.0D+00
  u1(1:m) = u0(1:m) + dt * f0(1:m) / 2.0D+00
  call f ( t1, m, u1, f1 )

  t2 = t0 + dt / 2.0D+00
  u2(1:m) = u0(1:m) + dt * f1(1:m) / 2.0D+00
  call f ( t2, m, u2, f2 )

  t3 = t0 + dt
  u3(1:m) = u0(1:m) + dt * f2(1:m)
  call f ( t1, m, u1, f3 )
!
!  Combine them to estimate the solution at time T0 + DT.
!
  u(1:m) = u0(1:m) + dt * ( f0(1:m) + 2.0D+00 * f1(1:m) + 2.0D+00 * f2(1:m) &
    + f3(1:m) ) / 6.0D+00

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
!  Parameters:
!
!    None
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  character ( len = 8 ) ampm
  integer ( kind = 4 ) d
  integer ( kind = 4 ) h
  integer ( kind = 4 ) m
  integer ( kind = 4 ) mm
  character ( len = 9 ), parameter, dimension(12) :: month = (/ &
    'January  ', 'February ', 'March    ', 'April    ', &
    'May      ', 'June     ', 'July     ', 'August   ', &
    'September', 'October  ', 'November ', 'December ' /)
  integer ( kind = 4 ) n
  integer ( kind = 4 ) s
  integer ( kind = 4 ) values(8)
  integer ( kind = 4 ) y

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
