function [ x, shift ] = line01_sample_ergodic ( n, shift )

%*****************************************************************************80
%
%% line01_sample_ergodic() samples the unit line in 1D.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    07 June 2017
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points.
%
%    real SHIFT, a value between 0 and 1.
%
%  Output:
%
%    real X(N), the points.
%
%    real SHIFT, the shift value.
%
  golden = ( 1.0 + sqrt ( 5.0 ) ) / 2.0;

  x = zeros ( n, 1 );

  shift = mod ( shift, 1.0 );

  for j = 1 : n
    x(j) = shift;
    shift = mod ( shift + golden, 1.0 );
  end


  return
end
