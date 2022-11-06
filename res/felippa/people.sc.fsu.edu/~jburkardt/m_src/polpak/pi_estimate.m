function value = pi_estimate ( n )

%*****************************************************************************80
%
%% pi_estimate() estimates Pi(n), the number of primes less than or equal to n. 
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 May 2022
%
%  Input:
%
%    integer N: the argument.
%
%  Output:
%
%    real value: the estimate for Pi(n).
%
  if ( n == 0 )
    value = 0.0;
  else
    value = n / log ( n );
  end

  return
end
