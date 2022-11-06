function value = harmonic_estimate ( n )

%*****************************************************************************80
%
%% harmonic_estimate() estimates the Nth harmonic number.
%
%  Discussion:
%
%    H(N) = Sum ( 1 <= I <= N ) 1 / I
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 May 2022
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N: the index of the harmonic number.
%
%  Output:
%
%    real VALUE: the estimated value of the harmonic number.
%
  value = log ( n ) + euler_mascheroni ( ) ...
    + 0.5 / n ...
    - 1.0 / 12.0 / n^2 ...
    + 1.0 / 120.0 / n^4;

  return
end
