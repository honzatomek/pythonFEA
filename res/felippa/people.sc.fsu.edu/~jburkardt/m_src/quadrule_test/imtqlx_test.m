function imtqlx_test ( )

%*****************************************************************************80
%
%% imtqlx_test() tests imtqlx().
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
%    John Burkardt.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'IMTQLX_TEST\n' );
  fprintf ( 1, '  IMTQLX takes a symmetric tridiagonal matrix A\n' );
  fprintf ( 1, '  and computes its eigenvalues LAM.\n' );
  fprintf ( 1, '  It also accepts a vector Z and computes Q''*Z,\n' );
  fprintf ( 1, '  where Q is the matrix that diagonalizes A.\n' );

  n = 5;
  d = zeros ( n, 1 );
  d(1:n) = 2.0;
  e = zeros ( n, 1 );
  e(1:n-1) = -1.0;
  e(n) = 0.0;
  z = ones ( n, 1 );

  [ lam, qtz ] = imtqlx ( n, d, e, z );

  r8vec_print ( n, lam, '  Computed eigenvalues:' );

  lam2 = zeros ( n, 1 );
  for i = 1 : n
    angle = i * pi / ( 2 * ( n + 1 ) );
    lam2(i) = 4.0 * ( sin ( angle ) )^2;
  end

  r8vec_print ( n, lam2, '  Exact eigenvalues:' );

  r8vec_print ( n, z, '  Vector Z:' );
  r8vec_print ( n, qtz, '  Vector Q''*Z:' );

  return
end
