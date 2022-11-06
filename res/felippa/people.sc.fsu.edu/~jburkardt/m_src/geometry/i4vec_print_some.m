function i4vec_print_some ( n, a, i_lo, i_hi, label )

%*****************************************************************************80
%
%% i4vec_print_some() prints "some" of an I4VEC.
%
%  Discussion:
%
%    An I4VEC is a vector of I4 values.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    31 July 2018
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the dimension of the vector.
%
%    integer A(N), the vector to be printed.
%
%    integer I_LO, I_HI, the first and last indices to print.
%    The routine expects 1 <= I_LO <= I_HI <= N.
%
%    string LABEL, a title.
%
  if ( 0 < length ( label ) )
    fprintf ( 1, '\n' );
    fprintf ( 1, '%s\n', label );
  end

  fprintf ( 1, '\n' );

  for i = max ( 1, i_lo ) : min ( n, i_hi )
    fprintf ( 1, '  %8d: %12d\n', i, a(i) );
  end

  return
end
