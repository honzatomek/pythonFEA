function theta = lines_imp_angle_2d ( a1, b1, c1, a2, b2, c2 )

%*****************************************************************************80
%
%% lines_imp_angle_2d() finds the angle between two implicit lines in 2D.
%
%  Discussion:
%
%    The implicit form of a line in 2D is:
%
%      A * X + B * Y + C = 0
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Adrian Bowyer and John Woodwark,
%    A Programmer's Geometry,
%    Butterworths, 1983.
%
%  Input:
%
%    real A1, B1, C1, the implicit parameters of the 
%    first line.
%
%    real A2, B2, C2, the implicit parameters of the
%    second line.
%
%  Output:
%
%    real THETA, the angle between the two lines.
%
  pdotq = a1 * a2 + b1 * b2;
  pnorm = sqrt ( a1 * a1 + b1 * b1 );
  qnorm = sqrt ( a2 * a2 + b2 * b2 );

  theta = acos ( pdotq / ( pnorm * qnorm ) );

  return
end
