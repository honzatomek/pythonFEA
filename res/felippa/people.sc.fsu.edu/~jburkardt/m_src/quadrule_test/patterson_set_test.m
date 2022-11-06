function patterson_set_test ( )

%*****************************************************************************80
%
%% patterson_set_test() tests patterson_set().
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
  n_test = [ 1, 3, 7, 15 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'PATTERSON_SET_TEST\n' );
  fprintf ( 1, '  PATTERSON_SET sets a Patterson quadrature rule;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  for j = 1 : 4

    n = n_test(j);

    [ x, w ] = patterson_set ( n );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %12g  %12g\n', i, x(i), w(i) );
    end

  end

  return
end
