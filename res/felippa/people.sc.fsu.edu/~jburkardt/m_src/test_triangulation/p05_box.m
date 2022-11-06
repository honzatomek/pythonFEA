function [ lo, hi ] = p05_box ( m )

%*****************************************************************************80
%
%% p05_box returns a bounding box for problem 05.
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
%  Output:
%
%    real LO(M), HI(M), the low and high corners of the box.
%
  center1 = [  0.0, 0.0 ];
  center2 = [ -0.4, 0.0 ];
  r1 = 1.0;
  r2 = 0.55;

  lo(1:m) = [ center1(1) - r1, center1(2)      ];
  hi(1:m) = [ center1(1) + r1, center1(2) + r1 ];

  return
end
