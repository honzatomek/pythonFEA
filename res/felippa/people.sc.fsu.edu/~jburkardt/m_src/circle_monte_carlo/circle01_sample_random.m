function x = circle01_sample_random ( n )

%*****************************************************************************80
%
%% circle01_sample_random() samples points on the circumference of the unit circle in 2D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    28 November 2021
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
  r = 1.0;
  c = zeros ( 2, 1 );

  theta = rand ( 1, n );

  x = zeros ( 2, n );

  x(1,1:n) = c(1) + r * cos ( 2.0 * pi * theta(1:n)' );
  x(2,1:n) = c(2) + r * sin ( 2.0 * pi * theta(1:n)' );

  return
end
