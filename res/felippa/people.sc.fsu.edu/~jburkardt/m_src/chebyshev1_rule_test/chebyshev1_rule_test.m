function chebyshev1_rule_test ( )

%*****************************************************************************80
%
%% chebyshev1_rule_test() tests chebyshev1_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 December 2018
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../chebyshev1_rule' )

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CHEBYSHEV1_RULE_TEST\n' );
  fprintf ( 1, '  MATLAB/Octave version %s.\n', version ( ) );
  fprintf ( 1, '  Test CHEBYSHEV1_RULE.\n' );

  n = 5;
  a = -1.0;
  b = +1.0;
  filename = 'cheby1_o5';

  chebyshev1_rule ( n, a, b, filename );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'CHEBYSHEV1_RULE_TEST\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../chebyshev1_rule' )

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

