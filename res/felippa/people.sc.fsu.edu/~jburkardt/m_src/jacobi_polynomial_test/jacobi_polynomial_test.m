function jacobi_polynomial_test ( )

%*****************************************************************************80
%
%% jacobi_polynomial_test() tests jacobi_polynomial().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 February 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../jacobi_polynomial' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'jacobi_polynomial_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test jacobi_polynomial().\n' );

  jacobi_polynomial_test01 ( );
  jacobi_polynomial_test02 ( );
  jacobi_polynomial_test03 ( );
  jacobi_polynomial_test04 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'jacobi_polynomial_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../jacobi_polynomial' );

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp() prints the current YMDHMS date as a timestamp.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  t = now;
  c = datevec ( t );
  s = datestr ( c, 0 );
  fprintf ( 1, '%s\n', s );

  return
end

