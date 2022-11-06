function annulus_rule_compute_test ( )

%*****************************************************************************80
%
%% annulus_rule_compute_test() tests annulus_rule_compute().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 July 2018
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ANNULUS_RULE_COMPUTE_TEST:\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test ANNULUS_RULE_COMPUTE.\n' );

  center = [ 0.0, 0.0 ];
  r1 = 0.5;
  r2 = 1.0;
  nr = 3;
  nt = 12;

  [ w, x, y ] = annulus_rule_compute ( center, r1, r2, nr, nt );

  r8vec3_print ( nr * nt, w, x, y, '  W, X, Y for annulus quadrature:' );
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ANNULUS_RULE_COMPUTE_TEST:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
