function x = cube01_sample ( n )

%*****************************************************************************80
%
%% cube01_sample() samples points in the unit cube in 3D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    18 January 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(3,N), the points.
%
  x = rand ( 3, n );

  return
end
