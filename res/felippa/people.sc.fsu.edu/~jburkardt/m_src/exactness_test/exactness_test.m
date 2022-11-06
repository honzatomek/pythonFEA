function exactness_test ( )

%*****************************************************************************80
%
%% exactness_test() tests exactness().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 January 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../exactness' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'EXACTNESS_TEST\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test EXACTNESS.\n' );

  chebyshev1_exactness_test ( );
  chebyshev2_exactness_test ( );
  chebyshev3_exactness_test ( );
  clenshaw_curtis_exactness_test ( );
  fejer1_exactness_test ( );
  fejer2_exactness_test ( );
  gegenbauer_exactness_test ( );
  hermite_exactness_test ( );
  hermite_1_exactness_test ( );
  laguerre_exactness_test ( );
  laguerre_1_exactness_test ( );
  legendre_exactness_test ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'EXACTNESS_TEST\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../exactness' );

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

