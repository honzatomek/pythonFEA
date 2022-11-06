function hermite_probabilist_set_test ( )

%*****************************************************************************80
%
%% hermite_probabilist_set_test() tests hermite_probabilist_set().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    17 June 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'HERMITE_PROBABILIST_SET_TEST\n' );
  fprintf ( 1, '  HERMITE_PROBABILIST_SET sets a Hermite quadrature rule;\n' );
  fprintf ( 1, '  The integration interval is ( -oo, +oo ).\n' );
  fprintf ( 1, '  The weight is exp ( - x * x / 2 ) / sqrt ( 2 * pi ).\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  for n = 1 : 10

    [ x, w ] = hermite_probabilist_set ( n );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
