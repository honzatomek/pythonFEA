function x = normal_circular ( n, d )

%*****************************************************************************80
%
%% normal_circular() creates circularly normal points.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 April 2022
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964, page 936.
%
%  Input:
%
%    integer N, the number of points.
%
%    integer D, the dimension of the space, which must be 2.
%
%  Output:
%
%    real X(N,D), the points.
%

%
%  The angle varies uniformly from 0 to 2 pi.
%
  t = 2.0 * pi * rand ( n, 1 );
%
%  The radius is normally distributed.
%
  r = randn ( n, 1 );

  x(1:n,1) = r(1:n) .* cos ( t(1:n) );
  x(1:n,2) = r(1:n) .* sin ( t(1:n) );

  return
end
