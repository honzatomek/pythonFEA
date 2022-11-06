function laguerre_1_set_test ( )

%*****************************************************************************80
%
%% laguerre_1_set_test() tests laguerre_1_set().
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
  fprintf ( 1, 'LAGUERRE_1_SET_TEST\n' );
  fprintf ( 1, '  LAGUERRE_1_SET sets a Laguerre rule.\n' );
  fprintf ( 1, '  The density function is rho(x)=1.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         I            X                  W\n' );

  for n = 1 : 10

    [ x, w ] = laguerre_1_set ( n );

    fprintf ( 1, '\n' );
    for i = 1 : n
      fprintf ( 1, '  %8d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end
  return
end
