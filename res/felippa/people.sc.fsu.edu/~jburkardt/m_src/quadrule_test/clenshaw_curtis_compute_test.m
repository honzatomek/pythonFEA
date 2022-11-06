function clenshaw_curtis_compute_test ( )

%*****************************************************************************80
%
%% clenshaw_curtis_compute_test() tests clenshaw_curtis_compute().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    03 April 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CLENSHAW_CURTIS_COMPUTE_TEST\n' );
  fprintf ( 1, '  CLENSHAW_CURTIS_COMPUTE computes\n' );
  fprintf ( 1, '  a Clenshaw-Curtis quadrature rule over [-1,1]\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  for order = 1 : 10

    [ x, w ] = clenshaw_curtis_compute ( order );

    fprintf ( 1, '\n' );

    for i = 1 : order
      fprintf ( 1, '  %2d  %24.16f  %24.16f\n', i, x(i), w(i) );
    end

  end

  return
end
