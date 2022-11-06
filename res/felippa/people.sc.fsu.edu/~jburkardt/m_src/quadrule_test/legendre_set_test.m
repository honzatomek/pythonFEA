function legendre_set_test ( )

%*****************************************************************************80
%
%% legendre_set_test() tests legendre_set().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    18 June 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'LEGENDRE_SET_TEST\n' );
  fprintf ( 1, '  LEGENDRE_SET sets a Legendre quadrature rule.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         I            X                  W\n' );

  for n = 1 : 10

    [ x, w ] = legendre_set ( n );

    fprintf ( 1, '\n' );
    for i = 1 : n
      fprintf ( 1, '  %8d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
