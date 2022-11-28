function chebyshev1_set_test ( )

%*****************************************************************************80
%
%% chebyshev1_set_test() tests chebyshev1_set().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    11 June 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CHEBYSHEV1_SET_TEST\n' );
  fprintf ( 1, '  CHEBYSHEV1_SET sets\n' );
  fprintf ( 1, '  a Chebyshev Type 1 quadrature rule over [-1,1]\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  for n = 1 : 10

    [ x, w ] = chebyshev1_set ( n );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end