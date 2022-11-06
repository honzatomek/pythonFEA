function result = circle_xy_sum ( func, xc, yc, r, norder, xtab, ytab, weight )

%*****************************************************************************80
%
%% circle_xy_sum() applies an XY quadrature rule inside a circle in 2D.
%
%  Integration region:
%
%    Points (X,Y) such that:
%
%      (X-XC)^2 + (Y-YC)^2 <= R^2.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    23 May 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    external FUNC, the name of the user supplied
%    function of two variables which is to be integrated,
%    of the form:
%      function value = func ( x, y )
%
%    real XC, YC, the coordinates of the center of
%    the circle.
%
%    real R, the radius of the circle.
%
%    integer NORDER, the order of the rule.  The rule is
%    assumed to be defined on the unit circle.
%
%    real XTAB(NORDER), YTAB(NORDER), the XY
%    coordinates of the abscissas of the quadrature rule for a unit circle.
%
%    real WEIGHT(NORDER), the weights of the rule.
%
%  Output:
%
%    real RESULT, the approximate integral of the function.
%
  quad = 0.0E+00;
  for i = 1 : norder
    x = xc + r * xtab(i);
    y = yc + r * ytab(i);
    quad = quad + weight(i) * feval ( func, x, y );
  end

  volume = circle_area_2d ( r );
  result = quad * volume;

  return
end
