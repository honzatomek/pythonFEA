function value = hexagon01_area_2d ( )

%*****************************************************************************80
%
%% hexagon01_area_2d() returns the area of a unit regular hexagon in 2D.
%
%  Discussion:
%
%    The definition is given in terms of THETA, the angle in degrees of the
%    vector (X,Y).  The following six conditions apply, respectively,
%    between the bracketing values of THETA of 0, 60, 120, 180, 240,
%    300, and 360.
%
%                              0 <= Y <= - SQRT(3) * X + SQRT(3)
%                              0 <= Y <=                 SQRT(3)/2
%                              0 <= Y <=   SQRT(3) * X + SQRT(3)
%      - SQRT(3) * X - SQRT(3)   <= Y <= 0
%                    - SQRT(3)/2 <= Y <= 0
%        SQRT(3) * X - SQRT(3)   <= Y <= 0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    31 July 2018
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real VALUE, the area of the hexagon.
%
  value = 3.0 * sqrt ( 3.0 ) / 2.0;

  return
end