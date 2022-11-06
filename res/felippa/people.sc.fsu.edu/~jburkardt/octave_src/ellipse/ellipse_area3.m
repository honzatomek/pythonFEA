function value = ellipse_area3 ( r1, r2 )

%*****************************************************************************80
%
%% ellipse_area3() returns the area of an ellipse in 2D.
%
%  Discussion:
%
%    The ellipse is defined as points (X,Y) such that
%
%      ( ( X - XC ) / R1 )^2 + ( ( Y - YC ) / R2 )^2 <= 1
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 November 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real R1, R2, the major and minor semi-axes.
%
%  Output:
%
%    real ELLIPSE_AREA3, the area of the ellipse.
%
  value = pi * r1 * r2;

  return
end
