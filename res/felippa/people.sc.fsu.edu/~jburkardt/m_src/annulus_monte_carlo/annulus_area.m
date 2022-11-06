function area = annulus_area ( center, r1, r2 )

%*****************************************************************************80
%
%% annulus_area() computes the area of a circular annulus in 2D.
%
%  Discussion:
%
%    A circular annulus with center (XC,YC), inner radius R1 and
%    outer radius R2, is the set of points (X,Y) so that
%
%      R1^2 <= (X-XC)^2 + (Y-YC)^2 <= R2^2
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 July 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real CENTER(2), the coordinates of the center.
%    This data is actually not necessary for area calculations.
%
%    real R1, R2, the inner and outer radii of the annulus.
%    0.0 <= R1 <= R2.
%
%  Output:
%
%    real AREA, the area of the annulus.
%
  if ( r1 < 0.0 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'ANNULUS_AREA - Fatal error!\n' );
    fprintf ( 1, '  Inner radius R1 < 0.0.\n' );
    error ( 'ANNULUS_AREA - Fatal error!' );
  end

  if ( r2 < r1 )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'ANNULUS_AREA - Fatal error!\n' );
    fprintf ( 1, '  Outer radius R1 < R1 = inner radius.\n' );
    error ( 'ANNULUS_AREA - Fatal error!' );
  end

  area = pi * ( r2 + r1 ) * ( r2 - r1 );

  return
end
