function laguerre_ss_compute_test ( )

%*****************************************************************************80
%
%% laguerre_ss_compute_test() tests laguerre_ss_compute().
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
  fprintf ( 1, 'LAGUERRE_SS_COMPUTE_TEST\n' );
  fprintf ( 1, '  LAGUERRE_SS_COMPUTE computes a Laguerre quadrature rule\n' );
  fprintf ( 1, '  using the Stroud-Secrest algorithm.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  for n = 1 : 10

    [ x, w ] = laguerre_ss_compute ( n );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
