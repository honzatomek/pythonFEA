function value = i4_sign ( i )

%*****************************************************************************80
%
%% i4_sign() returns the sign of an I4.
%
%  Discussion:
%
%    The value is +1 if the number is positive or zero, and it is -1 otherwise.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    28 September 2014
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer I, the number whose sign is desired.
%
%  Output:
%
%    integer VALUE, the sign of I.
%
  if ( i < 0 )
    value = -1;
  else
    value = +1;
  end

  return
end
