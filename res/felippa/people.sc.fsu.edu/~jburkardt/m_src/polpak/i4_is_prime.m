function value = i4_is_prime ( n )

%*****************************************************************************80
%
%% i4_is_prime() reports whether an integer is prime.
%
%  Discussion:
%
%    A simple, unoptimized sieve of Eratosthenes is used to
%    check whether N can be divided by any integer between 2
%    and SQRT(N).
%
%    Note that negative numbers, 0 and 1 are not considered prime.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 June 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the integer to be tested.
%
%  Output:
%
%    logical VALUE, is true if N is prime.
%
  if ( n <= 0 )
    value = false;
    return
  end

  if ( n == 1 )
    value = false;
    return
  end

  if ( n <= 3 )
    value = true;
    return
  end

  nhi = floor ( sqrt ( n ) );

  for i = 2 : nhi
    if ( mod ( n, i ) == 0 )
      value = false;
      return
    end
  end

  value = true;

  return
end
