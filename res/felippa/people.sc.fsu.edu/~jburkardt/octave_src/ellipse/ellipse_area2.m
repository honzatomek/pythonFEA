function value = ellipse_area2 ( a, b, c, d )

%*****************************************************************************80
%
%% ellipse_area2() returns the area of an ellipse defined by an equation.
%
%  Discussion:
%
%    The ellipse is described by the formula
%      a x^2 + b xy + c y^2 = d
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 November 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real A, B, C, coefficients on the left hand side.
%
%    real D, the right hand side.
%
%  Output:
%
%    real VALUE, the area of the ellipse.
%
  value = 2.0 * d * d * pi / sqrt ( 4.0 * a * c - b * b );

  return
end
