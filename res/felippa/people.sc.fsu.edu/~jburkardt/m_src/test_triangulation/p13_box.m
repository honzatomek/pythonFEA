function [ lo, hi ] = p13_box ( m )

%*****************************************************************************80
%
%% p13_box returns a bounding box for problem 13.
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
  lo(1:m) = [    0.0,    0.0 ];
  hi(1:m) = [ +100.0, +100.0 ];

  return
end
