function fixed = p12_fixed_points ( m, fixed_num )

%*****************************************************************************80
%
%% p12_fixed_points returns the fixed points in problem 12.
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
  fixed = [    ...
    0.000,  0.000; ...
    0.500,  0.000; ...
    0.500,  0.250; ...
    0.625,  0.250; ...
    0.625,  0.000; ...
    1.000,  0.000; ...
    1.000,  1.000; ...
    0.625,  1.000; ...
    0.625,  0.375; ...
    0.500,  0.375; ...
    0.500,  1.000; ...
    0.000,  1.000 ]';

  return
end
