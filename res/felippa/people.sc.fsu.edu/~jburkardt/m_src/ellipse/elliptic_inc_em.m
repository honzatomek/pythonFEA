function value = elliptic_inc_em ( phi, m )

%*****************************************************************************80
%
%% elliptic_inc_em() evaluates the incomplete elliptic integral E(PHI,M).
%
%  Discussion:
%
%    The value is computed using Carlson elliptic integrals:
%
%      E(phi,m) = 
%                sin ( phi )   RF ( cos^2 ( phi ), 1-m sin^2 ( phi ), 1 ) 
%        - 1/3 m sin^3 ( phi ) RD ( cos^2 ( phi ), 1-m sin^2 ( phi ), 1 ).
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
%    real PHI, M, the argument.
%    0 <= PHI <= PI/2.
%    0 <= M sin^2(PHI) <= 1.
%
%  Output:
%
%    real VALUE, the function value.
%
  cp = cos ( phi );
  sp = sin ( phi );
  x = cp^2;
  y = 1.0 - m * sp^2;
  z = 1.0;
  errtol = 1.0E-03;

  [ value1, ierr ] = rf ( x, y, z, errtol );
  if ( ierr ~= 0 )
    fprintf ( 1, 'ELLIPTIC_INC_EM - Fatal error!\n' );
    fprintf ( 1, '  RF returned IERR = %d\n', ierr );
    error ( 'ELLIPTIC_INC_EM - Fatal error!' );
    return
  end

  [ value2, ierr ] = rd ( x, y, z, errtol );
  if ( ierr ~= 0 )
    fprintf ( 1, 'ELLIPTIC_INC_EM - Fatal error!\n' );
    fprintf ( 1, '  RD returned IERR = %d\n', ierr );
    error ( 'ELLIPTIC_INC_EM - Fatal error!' );
  end

  value = sp * value1 - m * sp^3 * value2 / 3.0;

  return
end
