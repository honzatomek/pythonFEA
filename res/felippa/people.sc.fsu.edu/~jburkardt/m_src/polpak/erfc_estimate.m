function value = erfc_estimate ( x, n )

%*****************************************************************************80
%
%% erfc_estimate() estimates erfc(x) by an asymptotic expansion. 
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    29 May 2022
%
%  Input:
%
%    real X: the argument.
%
%    integer N: the number of terms to use in the asymptotic expansion.
%
%  Output:
%
%    real value: the estimate for erfc(x).
%
  if ( x == 0.0 )
    value = 0.0;
  else
    value = 1.0;
    for i = 1 : n
      value = value + r8_mop ( i ) * i4_factorial2 ( 2 * i - 1 ) / ( 2.0 * x^2 ) ^ i;
    end
    value = value / sqrt ( pi ) / x / exp ( x^2 );
  end

  return
end
