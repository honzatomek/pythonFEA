function r8vec3_print ( a1, a2, a3, label )

%*****************************************************************************80
%
%% r8vec3_print() prints an R8VEC3.
%
%  Discussion:
%
%    An R8VEC3 is a dataset consisting of N triples of real values, stored
%    as separate vectors A1, A2, A3.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 June 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real A1(N), A2(N), A3(N), the vectors to be printed.
%
%    string LABEL, a title.
%
  if ( 0 < length ( label ) )
    fprintf ( 1, '\n' );
    fprintf ( 1, '%s\n', label );
  end
  fprintf ( 1, '\n' );

  n = length ( a1 );

  for i = 1 : n
    fprintf ( 1, '  %6d:   %12g  %12g  %12g\n', i, a1(i), a2(i), a3(i) );
  end

  return
end
