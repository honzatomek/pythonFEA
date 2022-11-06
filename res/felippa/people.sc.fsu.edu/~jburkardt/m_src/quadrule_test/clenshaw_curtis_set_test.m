function clenshaw_curtis_set_test ( )

%*****************************************************************************80
%
%% clenshaw_curtis_set_test() tests clenshaw_curtis_set().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    23 June 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CLENSHAW_CURTIS_SET_TEST\n' );
  fprintf ( 1, '  CLENSHAW_CURTIS_SET sets up a Clenshaw-Curtis rule;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  for n = 1 : 10

    [ x, w ] = clenshaw_curtis_set ( n );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
