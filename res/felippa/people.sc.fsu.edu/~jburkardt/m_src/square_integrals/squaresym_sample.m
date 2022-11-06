function x = squaresym_sample ( n )

%*****************************************************************************80
%
%% squaresym_sample() samples points in the symmetric unit square in 2D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    03 February 2021
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
%    real X(2,N), the points.
%
  x = - 1.0 + 2.0 * rand ( 2, n );

  return
end
