function [ x, angle ] = circle01_sample_ergodic ( n, angle )

%*****************************************************************************80
%
%% circle01_sample_ergodic() samples points on the circumference of the unit circle in 2D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    06 June 2017
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points.
%
%    real ANGLE, an angle between 0 and 2 Pi.
%
%  Output:
%
%    real X(2,N), the points.
%
%    real ANGLE, the next angle to use.
%
  r = 1.0;
  c = zeros ( 2, 1 );

  golden_ratio = ( 1.0 + sqrt ( 5.0 ) ) / 2.0;

  golden_angle = 2.0 * pi / golden_ratio ^ 2;

  x = zeros ( 2, n );

  for j = 1 : n
    x(1,j) = c(1) + r * cos ( angle );
    x(2,j) = c(2) + r * sin ( angle );
    angle = mod ( angle + golden_angle, 2.0 * pi ) ;
  end

  return
end
