function laguerre_set_test ( )

%*****************************************************************************80
%
%% laguerre_set_test() tests laguerre_set().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    10 June 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'LAGUERRE_SET_TEST\n' );
  fprintf ( 1, '  LAGUERRE_SET sets a Laguerre rule.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         I            X                  W\n' );

  for n = 1 : 10

    [ x, w ] = laguerre_set ( n );

    fprintf ( 1, '\n' );
    for i = 1 : n
      fprintf ( 1, '  %8d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
