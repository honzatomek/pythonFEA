function fixed = p05_fixed_points ( m, fixed_num )

%*****************************************************************************80
%
%% p05_fixed_points returns the fixed points in problem 05.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2009
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Per-Olof Persson and Gilbert Strang,
%    A Simple Mesh Generator in MATLAB,
%    SIAM Review,
%    Volume 46, Number 2, June 2004, pages 329-345.
%
%  Input:
%
%    integer M, the spatial dimension.
%
%    integer FIXED_num, the number of fixed points.
%
%  Output:
%
%    real FIXED(M,FIXED_num), the fixed points.
%
  center1 = [  0.0, 0.0 ];
  center2 = [ -0.4, 0.0 ];
  r1 = 1.0;
  r2 = 0.55;

  fixed = [ ...
    center1(1) - r1,  center1(2); ...
    center2(1) - r2,  center2(2); ...
    center2(1) + r2,  center2(2); ...
    center1(1) + r1,  center1(2) ]';

  return
end