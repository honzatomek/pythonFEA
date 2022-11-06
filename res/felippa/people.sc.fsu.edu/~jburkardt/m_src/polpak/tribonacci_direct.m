function t = tribonacci_direct ( n )

%*****************************************************************************80
%
%% tribonacci_direct() computes the N-th Tribonacci number directly.
%
%  Example:
%
%     N   T
%    --  --
%     1   0
%     2   0
%     3   1
%     4   1
%     5   2
%     6   4
%     7   7
%     8  13
%     9  24
%    10  44
%    11  81
%    12 149
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    09 May 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the index of the number to compute.
%    N should be positive.
%
%  Output:
%
%    integer T, the value of the N-th number.
%
  [ alpha, beta, gamma ] = tribonacci_roots ( );

  if ( n <= 0 )
    t = 0;
  else
    t = round ...
    ( ...
        alpha^n / ( - alpha^2 + 4.0 * alpha - 1.0 ) ...
      + beta^n  / ( - beta^2  + 4.0 * beta  - 1.0 ) ...
      + gamma^n / ( - gamma^2 + 4.0 * gamma - 1.0 ) ...
    );
  end
 
  return
end
