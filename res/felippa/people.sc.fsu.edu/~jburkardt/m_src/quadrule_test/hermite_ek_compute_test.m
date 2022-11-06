function hermite_ek_compute_test ( )

%*****************************************************************************80
%
%% hermite_ek_compute_test() tests hermite_ek_compute().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    12 June 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'HERMITE_EK_COMPUTE_TEST\n' );
  fprintf ( 1, '  HERMITE_EK_COMPUTE computes a Hermite quadrature rule\n' );
  fprintf ( 1, '  using the Elhay-Kautsky algorithm.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  for n = 1 : 10

    [ x, w ] = hermite_ek_compute ( n );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
