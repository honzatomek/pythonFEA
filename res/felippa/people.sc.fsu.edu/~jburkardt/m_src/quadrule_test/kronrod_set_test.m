function kronrod_set_test ( )

%*****************************************************************************80
%
%% kronrod_set_test() tests kronrod_set().
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
  nl_test = [ 7, 10, 15, 20 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'KRONROD_SET_TEST\n' );
  fprintf ( 1, '  KRONROD_SET sets up a Kronrod quadrature rule;\n' );
  fprintf ( 1, '  This is used following a lower order Legendre rule.\n' );

  for test = 1 : 4

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Legendre/Kronrod quadrature pair #%d\n', test );
    fprintf ( 1, '                W                         X\n' );
    fprintf ( 1, '\n' );

    nl = nl_test(test);
    [ xl, wl ] = legendre_set ( nl );

    for i = 1 : nl
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, wl(i), xl(i) );
    end

    fprintf ( 1, '\n' );

    nk = 2 * nl + 1;
    [ xk, wk ] = kronrod_set ( nk );

    for i = 1 : nk
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, wk(i), xk(i) );
    end

  end

  return
end
