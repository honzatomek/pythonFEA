function ncode = pruefer_enum ( n )

%*****************************************************************************80
%
%% pruefer_enum() enumerates the Pruefer codes on N-2 digits.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 December 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of digits in the code, plus 2.
%    N must be at least 3.
%
%  Output:
%
%    integer NCODE, the number of distinct elements.
%
  if ( n < 3 )
    ncode = 1;
  else
    ncode = n ^ ( n - 2 );
  end

  return
end
