function nc_compute_weights_test ( )

%*****************************************************************************80
%
%% nc_compute_weights_test() tests nc_compute_weights().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    20 June 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'NC_COMPUTE_WEIGHTS_TEST\n' );
  fprintf ( 1, '  NC_COMPUTE_WEIGHTS computes weights for \n' );
  fprintf ( 1, '  a Newton-Cotes quadrature rule;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  x_min = 0.0;
  x_max = 1.0;

  for n = 1 : 10

    x = linspace ( x_min, x_max, n );

    w = nc_compute_weights ( n, x_min, x_max, x );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
