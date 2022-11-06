function sphere_lebedev_rule_test ( )

%*****************************************************************************80
%
%% sphere_lebedev_rule_test() tests sphere_lebedev_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    30 January 2019
%
%  Author:
%
%    John Burkardt
%
  addpath ( '../sphere_lebedev_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_lebedev_rule_test():\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test sphere_lebedev_rule().\n' );

  sphere_lebedev_rule_test01 ( );
  sphere_lebedev_rule_test02 ( );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'sphere_lebedev_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( );

  rmpath ( '../sphere_lebedev_rule' );

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

