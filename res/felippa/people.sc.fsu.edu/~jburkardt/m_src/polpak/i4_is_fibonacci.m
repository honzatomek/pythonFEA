function value = i4_is_fibonacci ( i4 )

%*****************************************************************************80
%
%% i4_is_fibonacci() reports whether an integer is a Fibonacci number.
%
%  Discussion:
%
%    The positive integer i4 is a Fibonacci number if and only if
%    5*I4^2+4 or 5*I4^2-4 is a perfect square.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 December 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer I4, the number to investigate.
%
%  Output:
%
%    logical VALUE, is true if I4 is a Fibonacci number.
%
  value = false;
%
%  The number must be an integer.
%
  if ( i4 ~= floor ( i4 ) )
    return
  end
%
%  The number must be positive.
%
  if ( i4 <= 0 )
    return;
  end

  t1 = 5 * i4 ^ 2 + 4;
  t2 = sqrt ( t1 );
  t3 = floor ( t2 );
  if ( t3 * t3 == t1 )
    value = true;
    return
  end

  t1 = 5 * i4 ^ 2 - 4;
  t2 = sqrt ( t1 );
  t3 = floor ( t2 );
  if ( t3 * t3 == t1 )
    value = true;
    return
  end

  return
end

