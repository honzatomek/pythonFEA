function [ x, y ] = r8_swap ( x, y )

%*****************************************************************************80
%
%% r8_swap() swaps two R8's.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    22 October 2005
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real X, Y, two values to interchange.
%
%  Output:
%
%    real X, Y, the interchanged values.
%
  z = y;
  y = x;
  x = z;

  return
end
