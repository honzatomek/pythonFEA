function x = ball01_sample ( n )

%*****************************************************************************80
%
%% ball01_sample() uniformly samples the unit ball.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    03 September 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Russell Cheng,
%    Random Variate Generation,
%    in Handbook of Simulation,
%    edited by Jerry Banks,
%    Wiley, 1998, pages 168.
%
%    Reuven Rubinstein,
%    Monte Carlo Optimization, Simulation, and Sensitivity 
%    of Queueing Networks,
%    Krieger, 1992,
%    ISBN: 0894647644,
%    LC: QA298.R79.
%
%  Input:
%
%    integer N, the number of points.
%
%  Output:
%
%    real X(3,N), the points.
%
  x = randn ( 3, n );
  norm = ones ( 1, 3 ) * ( x.^2 );
  norm = sqrt ( norm );
  for i = 1 : 3
    x(i,1:n) = x(i,1:n) ./ norm(1:n);
  end

  for j = 1 : n
    r = rand ( 1, 1 );
    x(1:3,j) = r ^ ( 1.0 / 3.0 ) * x(1:3,j);
  end

  return
end
