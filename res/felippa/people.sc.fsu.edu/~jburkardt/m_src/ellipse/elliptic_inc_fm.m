function value = elliptic_inc_fm ( phi, m )

%*****************************************************************************80
%
%% elliptic_inc_fm() evaluates the incomplete elliptic integral F(PHI,M).
%
%  Discussion:
%
%    The value is computed using Carlson elliptic integrals:
%
%      F(phi,m) = sin(phi) * RF ( cos^2 ( phi ), 1-m sin^2 ( phi ), 1 ).
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 June 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real PHI, M, the arguments.
%    0 <= PHI <= PI/2.
%    0 <= M <= 1 / sin^2 ( PHI )
%
%  Output:
%
%    real VALUE, the function value.
%
  sp = sin ( phi );
  cp = cos ( phi );

  x = cp ^ 2;
  y = 1.0 - m * sp^2;
  z = 1.0;
  errtol = 1.0E-03;

  [ value, ierr ] = rf ( x, y, z, errtol );
  if ( ierr ~= 0 )
    fprintf ( 1, 'ELLIPTIC_INC_FM - Fatal error!\n' );
    fprintf ( 1, '  RF returned IERR = %d\n', ierr );
    error ( 'ELLIPTIC_INC_FM - Fatal error!' );
    return
  end

  value = sp * value;

  return
end
