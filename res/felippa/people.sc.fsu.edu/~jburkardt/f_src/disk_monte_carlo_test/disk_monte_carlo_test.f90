program main

!*****************************************************************************80
!
!! disk_monte_carlo_test() tests disk_monte_carlo().
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) center(2)
  real ( kind = rk ) r

  call timestamp ( )
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DISK_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  FORTRAN90 version'
  write ( *, '(a)' ) '  Test the DISK_MONTE_CARLO library.'

  call disk_area_test ( )

  center = (/ 0.0D+00, 0.0D+00 /)
  r = 1.0D+00
  call disk_sample_test ( center, r )

  center = (/ 1.0D+00, 0.0D+00 /)
  r = 1.0D+00
  call disk_sample_test ( center, r )

  center = (/ 1.0D+00, 2.0D+00 /)
  r = 3.0D+00
  call disk_sample_test ( center, r )
!
!  Terminate.
!
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'DISK_MONTE_CARLO_TEST'
  write ( *, '(a)' ) '  Normal end of execution.'
  write ( *, '(a)' ) ' '
  call timestamp ( )

  stop 0
end
subroutine disk_area_test ( )

!*****************************************************************************80
!
!! DISK_AREA_TEST test DISK_AREA.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) area
  real ( kind = rk ) center(2)
  real ( kind = rk ) dat(3)
  real ( kind = rk ) disk_area
  integer i
  real ( kind = rk ) r
  integer seed

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) 'DISK_AREA_TEST'
  write ( *, '(a)' ) '  DISK_AREA computes the area of a disk with'
  write ( *, '(a)' ) '  center = (CX,CY) and radius R.'
  
  seed = 123456789

  write ( *, '(a)' ) ''
  write ( *, '(a)' ) '  (   CX        CY     )    R          Area'
  write ( *, '(a)' ) ''

  do i = 1, 10

    call random_number ( harvest = dat(1:3) )
    center(1) = 10.0D+00 * dat(1) - 5.0D+00
    center(2) = 10.0D+00 * dat(2) - 5.0D+00
    r = dat(3)
    area = disk_area ( center, r )
    write ( *, '(a,f9.6,a,f9.6,a,f9.6,2x,f9.6)' ) &
      '  (', center(1), ', ', center(2), ')  ', r, area
  end do

  return
end
subroutine disk_sample_test ( center, r )

!*****************************************************************************80
!
!! TEST01 uses DISK01_SAMPLE with an increasing number of points.
!
!  Licensing:
!
!    This code is distributed under the GNU LGPL license.
!
!  Modified:
!
!    05 July 2018
!
!  Author:
!
!    John Burkardt
!
  implicit none

  integer, parameter :: rk = kind ( 1.0D+00 )

  real ( kind = rk ) center(2)
  real ( kind = rk ) disk_area
  integer e(2)
  integer :: e_test(2,7) = reshape ( (/ &
    0, 0, &
    2, 0, &
    0, 2, &
    4, 0, &
    2, 2, &
    0, 4, &
    6, 0 /), (/ 2, 7 /) )
  integer j
  integer n
  real ( kind = rk ) r
  real ( kind = rk ) result
  real ( kind = rk ), allocatable :: value(:)
  real ( kind = rk ), allocatable :: x(:,:)

  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) 'TEST01'
  write ( *, '(a)' ) '  Use DISK01_SAMPLE to estimate integrals in the unit disk.'
  write ( *, '(a)' ) ' '
  write ( *, '(a)' ) '         N        1              X^2             Y^2' // &
    '             X^4             X^2Y^2           Y^4             X^6'
  write ( *, '(a)' ) ' '

  n = 1

  do while ( n <= 65536 )

    write ( *, '(2x,i8)', advance = 'no' ) n

    allocate ( value(1:n) )
    allocate ( x(1:2,1:n) )

    call disk_sample ( center, r, n, x )

    do j = 1, 7

      e(1:2) = e_test(1:2,j)

      call monomial_value ( 2, n, e, x, value )

      result = disk_area ( center, r ) * sum ( value(1:n) ) / real ( n, kind = rk )

      write ( *, '(2x,g14.6)', advance = 'no' ) result

    end do

    write ( *, '(a)' ) ''

    deallocate ( value )
    deallocate ( x )

    n = 2 * n

  end do

  if ( center(1) == 0.0D+00 .and. center(2) == 0.0D+00 .and. r == 1.0D+00 ) then

    write ( *, '(a)' ) ''
    write ( *, '(a)', advance = 'no' ) '     Exact'
    do j = 1, 7
      e(1:2) = e_test(1:2,j)
      call disk01_monomial_integral ( e, result )
      write ( *, '(2x,g14.6)', advance = 'no' ) result
    end do

    write ( *, '(a)' ) ''

  end if

  return
end