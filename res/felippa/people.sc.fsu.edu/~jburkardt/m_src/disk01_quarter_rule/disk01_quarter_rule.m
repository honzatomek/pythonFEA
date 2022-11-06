function disk01_quarter_rule ( n, d )

%*****************************************************************************80
%
%% disk01_quarter_rule() computes a quadrature rule for the unit quarter disk.
%
%  Discussion:
%
%    The integration region is 
%
%      X^2 + Y^2 <= 1.
%      0 <= X, 0 <= Y.
%
%    The number of degrees of freedom is DOF = 3 * N.  This should be
%    greater than or equal to ( D + 1 ) * ( D + 2 ) / 2.
%  
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    09 January 2019
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer N, the number of points.
%
%    integer D, the total degree precision of the rule.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'DISK01_QUARTER_RULE\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Determine a quadrature rule for the unit quarter disk.\n' );

  dof = 3 * n;
  f_num = ( ( d + 1 ) * ( d + 2 ) / 2 );

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Seeking a rule using %d points\n', n );
  fprintf ( 1, '  The degree of precision will be %d\n', d );
  fprintf ( 1, '  The number of degrees of freedom is %d\n', dof );
  fprintf ( 1, '  The number of monomial integrals to match is %d\n', f_num ); 

  if ( dof < f_num )
    fprintf ( 1, '\n' );
    fprintf ( 1, 'DISK01_QUARTER_RULE - Warning!\n' );
    fprintf ( 1, '  Number of degrees of freedom is lower than number of integrals to match.\n' );
    fprintf ( 1, '  A quadrature rule, if found, is unlikely to be exact.\n' );
  end
%
%  WXY0 contains the initial guess for the W's, X's, and Y's.
%
  wxy0 = zeros ( dof, 1 );
  wxy0(    1:  n) = disk01_quarter_area ( ) / n;
  angle = ( pi / 2.0 ) * ( 2 * ( 1 : n ) - 1 ) / ( 2 * n );
  wxy0(  n+1:2*n) = 0.5 * cos ( angle );
  wxy0(2*n+1:3*n) = 0.5 * sin ( angle );
%
%  Print the initial quess.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Initial guess for W, X, Y values:\n' );
  fprintf ( 1, '\n' );
  for i = 1 : n
    fprintf ( 1, '  %14.6g  %14.6g  %14.6g\n', wxy0(i), wxy0(i+n), wxy0(2*i+n) );
  end
%
%  Call FSOLVE to determine solution of nonlinear system defining the
%  quadrature rule.
%
  wxy = fsolve ( @disk01_quarter_f, wxy0 );
%
%  Print solution.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, '  FSOLVE solves for W, X, Y values:\n' );
  fprintf ( 1, '\n' );
  for i = 1 : n
    fprintf ( 1, '  %14.6g  %14.6g  %14.6g\n', wxy(i), wxy(i+n), wxy(2*i+n) );
  end

  f = disk01_quarter_f ( wxy );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Residuals for WXY:\n' );
  fprintf ( 1, '\n' );
  for i = 1 : f_num
    fprintf ( 1, '  %14.6g\n', f(i) );
  end
%
%  Test the quadrature rule.
%
  w = wxy(    1:  n);
  x = wxy(  n+1:2*n);
  y = wxy(2*n+1:3*n);

  fprintf ( 1, '\n' );
  fprintf ( 1, '  Monomial       Estimate           Exact       Error\n' );
  fprintf ( 1, '\n' );

  k = 0;
  for dsub = 0 : d
    for d1 = dsub : -1 : 0
      d2 = dsub - d1;
      k = k + 1;
      r = disk01_quarter_monomial_integral ( [ d1, d2 ] );
      f(k) = sum ( w(1:n) .* x(1:n) .^ d1 .* y(1:n) .^ d2 );
      fprintf ( 1, '  x^%d*y^%d  %14.6g  %14.6g  %14.6g\n', d1, d2, f(k), r, abs ( f(k) - r ) );
    end
  end
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'DISK01_QUARTER_RULE:\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end
function f = disk01_quarter_f ( wxy )

%*****************************************************************************80
%
%% disk01_quarter_f() evaluates the unit disk quadrature rule residual.
%
%  Discussion:
%
%    The integration region is 
%
%      X^2 + Y^2 <= 1.
%      0 <= X, 0 <= Y.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    05 May 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real WXY(3*N), the W, X and Y values of a quadrature rule
%    of desired precision D.
%
%  Output:
%
%    real F( D * ( D + 1 ) / 2 ), the residual of the quadrature equations:
%    F(1) = Q(1) - I(1)
%    F(2) = Q(x) - I(x)
%    F(3) = Q(y) - I(y)
%    F(4) = Q(x^2) - I(x^2)
%    F(5) = Q(xy) - I(xy)
%    F(6) = Q(y^2) - I(y^2) and so on.
%
  n = 3;
  dof = 3 * n;
  d = 2;

  w = wxy(    1:  n);
  x = wxy(  n+1:2*n);
  y = wxy(2*n+1:3*n);

  f_num = ( ( d + 1 ) * ( d + 2 ) ) / 2;
  f = zeros ( f_num, 1 );
  
  k = 0;
  for dsub = 0 : d
    for d1 = dsub : -1 : 0
      d2 = dsub - d1;
      k = k + 1;
      r = disk01_quarter_monomial_integral ( [ d1, d2 ] );
      f(k) = sum ( w(1:n) .* x(1:n) .^ d1 .* y(1:n) .^ d2 ) - r;
    end
  end

  return
end
 
