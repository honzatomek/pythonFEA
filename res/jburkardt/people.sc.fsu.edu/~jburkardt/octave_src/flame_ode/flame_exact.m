function y = flame_exact ( t )

%*****************************************************************************80
%
%% flame_exact evaluates the exact solution of the flame ODE.
%
%  Discussion:
%
%    1 equation.
%
%    Moler attributes this problem to Lawrence Shampine.
%
%    The equation describes the radius of a ball of flame that
%    begins, at time 0, at DELTA.
%
%      Y(0) = DELTA
%
%    The rate of fuel consumption is proportional to the volume, and
%    the rate of fuel intake is proportional to the area of the ball.
%    We take the constant of proportionality to be 1.
%
%      Y' = Y^2 - Y^3
%
%    The data is normalized so that Y = 1 is the equilibrium solution.
%
%    The computation is to be made from T = 0 to T = 2/DELTA.
%
%    For values of DELTA close to 1, such as 0.01, the equation is
%    not stiff.  But for DELTA = 0.0001, the equation can become
%    stiff as the solution approaches the equilibrium solution Y = 1,
%    and computed solutions may be wildly inaccurate or cautious
%    solvers may take very small timesteps.
%
%    The exact solution involves the Lambert W function, defined by
%
%      W(z) * exp ( W(z) ) = z
%
%    and if we set
%
%      A = ( 1 / DELTA - 1 )
%
%    then
%
%      Y(T) = 1 / ( W(A*exp(A-T)) + 1 )
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    10 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Cleve Moler,
%    Cleve's Corner: Stiff Differential Equations,
%    MATLAB News and Notes,
%    May 2003, pages 12-13.
%
%  Input:
%
%    real T(:): the times.
%
%  Output:
%
%    real Y(:), the exact solution values.
%
  [ t0, y0 ] = flame_parameters ( );

  a = ( 1.0 - y0 ) / y0;
  y = 1.0 ./ ( lambertw ( a .* exp ( a - ( t - t0 ) ) ) + 1.0 );

  return
end