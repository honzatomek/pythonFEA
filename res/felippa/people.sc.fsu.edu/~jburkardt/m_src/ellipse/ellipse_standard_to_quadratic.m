function [ A, B, C, D, E, F ] = ellipse_standard_to_quadratic ( ...
  a, b, theta, x0, y0 )

%*****************************************************************************80
%
%% ellipse_standard_to_quadratic() converts from standard to quadratic form.
%
%  Discussion:
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
%    Quadratic form of ellipse:
%
%      Ax^2 + Bxy + Cy^2 + Dx + Ey + F = 0
%      The determinant B^2 - 4AC must be nonpositive.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    06 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real a, b, theta, x0, y0: the parameters of the ellipse equation
%    in standard form.
%
%  Output:
%
%    real A, B, C, D, E, F: the parameters of the ellipse equation
%    in quadratic form.
%
  A = ( a * sin ( theta ) )^2 + ( b * cos ( theta ) )^2;
  B = 2.0 * ( b^2 - a^2 ) * sin ( theta ) * cos ( theta );
  C = ( a * cos ( theta ) )^2 + ( b * sin ( theta ) )^2;
  D = - 2.0 * A * x0 - B * y0;
  E = - B * x0 - 2.0 * C * y0;
  F = A * x0^2 + B * x0 * y0 + C * y0^2 - a^2 * b^2;

  return
end

