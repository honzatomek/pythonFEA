function check = gray_code_check ( n, t )

%*****************************************************************************80
%
%% gray_code_check() checks a Gray code element.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 January 2011
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of digits in each element.
%    N must be positive.
%
%    integer T(N), an element of the Gray code.
%    Each entry T(I) is either 0 or 1.
%
%  Output:
%
%    integer CHECK, error flag.
%    1, T represents a Gray code.
%    0, T does not represent a Gray code.
%
  check = 1;

  if ( n < 1 )
    check = 0;
    return
  end

  for i = 1 : n

    if ( t(i) ~= 0 & t(i) ~= 1 )
      check = 0;
      return
    end

  end

  return
end
