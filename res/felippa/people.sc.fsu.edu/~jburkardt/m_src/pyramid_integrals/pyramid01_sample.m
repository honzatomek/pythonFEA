function x = pyramid01_sample ( n )

%*****************************************************************************80
%
%% pyramid01_sample(): sample the unit pyramid.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 January 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of samples desired.
%
%  Output:
%
%    real X(3,N), the sample values.
%
  one_third = 1.0 / 3.0;

  x = rand ( 3, n );

  x(3,1:n) = 1.0 - x(3,1:n) .^ one_third;
  x(2,1:n) = ( 1.0 - x(3,1:n) ) .* ( 2.0 * x(2,1:n) - 1.0 );
  x(1,1:n) = ( 1.0 - x(3,1:n) ) .* ( 2.0 * x(1,1:n) - 1.0 );

  return
end
