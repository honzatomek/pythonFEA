function x = square01_sample ( n )

%*****************************************************************************80
%
%% square01_sample() samples points in the unit square in 2D.
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
%    real X(2,N), the points.
%
  x = rand ( 2, n );

  return
end
