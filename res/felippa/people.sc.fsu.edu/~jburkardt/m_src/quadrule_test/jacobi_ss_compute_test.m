function jacobi_ss_compute_test ( )

%*****************************************************************************80
%
%% jacobi_ss_compute_test() tests jacobi_ss_compute().
%
%  Discussion:
%
%    Compare with tabular values on page 178 of Stroud and Secrest.
%
%     In particular,
%
%             X              W
%
%     1  -0.9833999115   0.4615276287E-03
%     2  -0.9447138932   0.2732249104E-02
%     3  -0.8849310847   0.8045830455E-02
%    ..  .............   ................
%    19   0.9656375637   0.7613987785E-01
%    20   0.9934477866   0.3348337670E-01
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    15 June 2015
%
%  Author:
%
%    John Burkardt
%
  alpha = 1.5;
  beta = 0.5;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'JACOBI_SS_COMPUTE_TEST\n' );
  fprintf ( 1, '  JACOBI_SS_COMPUTE sets up Gauss-Jacobi quadrature;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  ALPHA = %f\n', alpha );
  fprintf ( 1, '  BETA =  %f\n', beta );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Index       X             W\n' );

  for n = 1 : 10

    [ x, w ] = jacobi_ss_compute ( n, alpha, beta );

    fprintf ( 1, '\n' );

    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
