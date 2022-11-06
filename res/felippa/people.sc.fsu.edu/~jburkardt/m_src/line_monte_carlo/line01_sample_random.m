function x = line01_sample_random ( n )

%*****************************************************************************80
%
%% line01_sample_random() samples the unit line in 1D.
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
%  Output:
%
%    Output, real X(N), the points.
%
  x = rand ( n, 1 );

  return
end
