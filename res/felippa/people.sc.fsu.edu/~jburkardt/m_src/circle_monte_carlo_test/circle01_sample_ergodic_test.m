function circle01_sample_ergodic_test ( )

%*****************************************************************************80
%
%% circle01_sample_ergodic_test() tests circle01_sample_ergodic().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    06 June 2017
%
%  Author:
%
%    John Burkardt
%
  e_test = [ ...
    0, 0; ...
    2, 0; ...
    0, 2; ...
    4, 0; ...
    2, 2; ...
    0, 4; ...
    6, 0 ]';


  fprintf ( 1, '\n' );
  fprintf ( 1, 'CIRCLE01_SAMPLE_ERGODIC_TEST\n' );
  fprintf ( 1, '  CIRCLE01_SAMPLE_ERGODIC ergodically samples the unit circle.\n' );
  fprintf ( 1, '  Use it to estimate integrals.\n' );


  fprintf ( 1, '\n' );
  fprintf ( 1, '         N           1              X^2             Y^2' );
  fprintf ( 1, '             X^4           X^2Y^2             Y^4           X^6\n' );
  fprintf ( 1, '\n' );

  n = 1;

  while ( n <= 65536 )

    angle = 0.0;
    [ x, angle ] = circle01_sample_ergodic ( n, angle );

    fprintf ( 1, '  %8d', n );

    for j = 1 : 7

      e(1:2) = e_test(1:2,j);

      value = monomial_value ( 2, n, e, x );

      result = circle01_length ( ) * sum ( value(1:n) ) / n;

      fprintf ( 1, '  %14.6g', result );

    end

    fprintf ( 1, '\n' );

    n = 2 * n;

  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '     Exact' );

  for j = 1 : 7

    e(1:2) = e_test(1:2,j);

    result = circle01_monomial_integral ( e );
    fprintf ( 1, '  %14.6g', result );

  end

  fprintf ( 1, '\n' );

  return
end
