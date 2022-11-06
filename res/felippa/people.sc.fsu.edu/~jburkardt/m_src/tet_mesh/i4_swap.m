function [ x, y ] = i4_swap ( x, y )

%*****************************************************************************80
%
%% i4_swap() swaps two I4's.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 February 2004
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer X, Y, two values to interchange.
%
%  Output:
%
%    integer X, Y, the interchanged values.
%
  z = x;
  x = y;
  y = z;

  return
end
