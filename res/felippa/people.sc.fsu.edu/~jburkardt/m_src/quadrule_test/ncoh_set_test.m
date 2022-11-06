function ncoh_set_test ( )

%*****************************************************************************80
%
%% ncoh_set_test() tests ncoh_set().
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
  fprintf ( 1, 'NCOH_SET_TEST\n' );
  fprintf ( 1, '  NCOH_SET sets up a Newton-Cotes Open Half quadrature rule;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );
  fprintf ( 1, '\n' );

  for n = 1 : 10

    [ x, w ] = ncoh_set ( n );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
