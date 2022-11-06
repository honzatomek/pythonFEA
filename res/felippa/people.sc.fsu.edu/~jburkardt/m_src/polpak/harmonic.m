function value = harmonic ( n )

%*****************************************************************************80
%
%% harmonic() computes the Nth harmonic number.
%
%  Discussion:
%
%    H(N) = Sum ( 1 <= I <=N ) 1 / I
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    21 May 2022
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
%    real VALUE: the value of the harmonic number.
%
  value = 0.0;

  for i = 1 : n
    value = value + 1.0 / i;
  end

  return
end
