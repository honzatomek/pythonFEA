function ellipse_eccentricity_test ( )

%*****************************************************************************80
%
%% ellipse_eccentricity_test() tests ellipse_eccentricity().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 March 2021
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_eccentricity_test():\n' );
  fprintf ( 1, '  ellipse_eccentricity() computes the eccentricity of an ellipse.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '      A      B      Ecc\n' );
  fprintf ( 1, '\n' );
  a = 1.0;
  n = 10;
  for i = 0 : n
    b = i / n;
    e = ellipse_eccentricity ( a, b );
    fprintf ( 1, '  %5.1f  %5.1f  %10.6f\n', a, b, e );
  end
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'ellipse_eccentricity_test\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
