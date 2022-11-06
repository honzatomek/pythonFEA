function [ a, b, theta, x0, y0 ] = ellipse_quadratic_to_standard ( ...
  A, B, C, D, E, F )

%*****************************************************************************80
%
%% ellipse_quadratic_to_standard() converts from quadratic to standard form.
%
%  Discussion:
%
%    Quadratic form of ellipse:
%
%      Ax^2 + Bxy + Cy^2 + Dx + Ey + F = 0
%      The determinant B^2 - 4AC must be nonpositive.
%
%    Standard form of an ellipse:
%
%      x^2   y^2
%      --- + --- = 1
%      a^2   b^2
%
%    Rotated ellipse (center at (x0,y0), major axis at angle theta:
%
%        (   ( x - x0 ) cos ( theta ) + ( y - y0 ) sin ( theta ) ) / a^2
%      + ( - ( x - x0 ) sin ( theta ) + ( y - y0 ) cos ( theta ) ) / b^2
%      = 1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    07 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real A, B, C, D, E, F: the parameters of the ellipse equation
%    in quadratic form.
%
%  Output:
%
%    real a, b, theta, x0, y0: the parameters of the ellipse equation
%    in standard form.
%
  f1 = 2.0 * ( A * E^2 + C * D^2 - B * D * E + ( B^2 - 4.0 * A * C ) * F );
  f2 = ( A + C );
  f3 = sqrt ( ( A - C )^2 + B^2 );
  a = - sqrt ( f1 * ( f2 + f3 ) ) / ( B^2 - 4.0 * A * C );
  b = - sqrt ( f1 * ( f2 - f3 ) ) / ( B^2 - 4.0 * A * C );

  if ( B == 0.0 ) 
    if ( A < C )
      theta = 0.0;
    else
      theta = pi / 2.0;
    end
  else
    arg = ( C - A - sqrt ( ( A - C )^2 + B^2 ) ) / B;
    theta = atan ( arg );
  end
  x0 = ( 2.0 * C * D - B * E ) / ( B^2 - 4.0 * A * C );
  y0 = ( 2.0 * A * E - B * D ) / ( B^2 - 4.0 * A * C );

  return
end

