function hyperball01_volume_pnorm_test ( )

%*****************************************************************************80
%
%% hyperball01_volume_pnorm_test() tests hyperball01_volume_pnorm().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 April 2019
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' )
  fprintf ( 1, 'hyperball01_volume_pnorm_test\n' )
  fprintf ( 1, '  MATLAB version\n' )
  fprintf ( 1, '  hyperball01_volume_pnorm returns the volume of the unit hyperball\n' )
  fprintf ( 1, '  in M dimensions under the P_norm.\n' )
%
%  Compare with L2 norm function.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Compare, for P = 2:\n' );
  fprintf ( 1, '  V1 = hyperball01_volume_pnorm ( m, p )\n' );
  fprintf ( 1, '  V2 = hyperball01_volume ( m )\n' );
  fprintf ( 1, '\n' )
  fprintf ( 1, '   M    P         V1        V2\n' )
  fprintf ( 1, '\n' )

  p = 2.0;

  for m = 1 : 10
    value1 = hyperball01_volume_pnorm ( m, p );
    value2 = hyperball01_volume ( m );
    fprintf ( 1, '  %2d  %8.6f  %8.6f  %8.6f\n', m, p, value1, value2 );
  end
%
%  Let P vary.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Compute volume for fixed M, a range of P values:\n' );
  fprintf ( 1, '\n' )
  fprintf ( 1, '   M     P          V\n' )
  fprintf ( 1, '\n' )

  m = 3;
  for p = 1 : 20
    value = hyperball01_volume_pnorm ( m, p );
    fprintf ( 1, '  %2d  %9.6f  %9.6f\n', m, p, value );
  end
  p = Inf;
  value = hyperball01_volume_pnorm ( m, p );
  fprintf ( 1, '  %2d  %9.6f  %9.6f\n', m, p, value );
%
%  Let M vary.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Compute volume for fixed P, a range of M values:\n' );
  fprintf ( 1, '\n' )
  fprintf ( 1, '   M     P          V\n' )
  fprintf ( 1, '\n' )

  p = 3.0;
  for m = 1 : 25
    value = hyperball01_volume_pnorm ( m, p );
    fprintf ( 1, '  %2d  %9.6f  %9.6f\n', m, p, value );
  end
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'hyperball01_volume_pnorm_test\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end

