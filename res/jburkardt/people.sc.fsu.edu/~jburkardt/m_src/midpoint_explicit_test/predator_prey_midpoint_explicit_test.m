function predator_prey_midpoint_explicit_test ( tspan, p0, n )

%*****************************************************************************80
%
%% predator_prey_midpoint_explicit_test: predator-prey ODE using midpoint_explicit().
%
%  Discussion:
%
%    The physical system under consideration is a pair of animal populations.
%
%    The PREY reproduce rapidly; for each animal alive at the beginning of the
%    year, two more will be born by the end of the year.  The prey do not have
%    a natural death rate; instead, they only die by being eaten by the predator.
%    Every prey animal has 1 chance in 1000 of being eaten in a given year by
%    a given predator.
%
%    The PREDATORS only die of starvation, but this happens very quickly.
%    If unfed, a predator will tend to starve in about 1/10 of a year.
%    On the other hand, the predator reproduction rate is dependent on
%    eating prey, and the chances of this depend on the number of available prey.
%
%    The resulting differential equations can be written:
%
%      PREY(0) = 5000         
%      PRED(0) =  100
%
%      d PREY / dT =    2 * PREY(T) - 0.001 * PREY(T) * PRED(T)
%      d PRED / dT = - 10 * PRED(T) + 0.002 * PREY(T) * PRED(T)
%
%    Here, the initial values (5000,100) are a somewhat arbitrary starting point.
%
%    The pair of ordinary differential equations that result have an interesting
%    behavior.  For certain choices of the interaction coefficients (such as
%    those given here), the populations of predator and prey will tend to 
%    a periodic oscillation.  The two populations will be out of phase; the number
%    of prey will rise, then after a delay, the predators will rise as the prey
%    begins to fall, causing the predator population to crash again.
%
%    There is a conserved quantity, which here would be:
%      E(r,f) = 0.002 r + 0.001 f - 10 ln(r) - 2 ln(f)
% 
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 April 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    George Lindfield, John Penny,
%    Numerical Methods Using MATLAB,
%    Second Edition,
%    Prentice Hall, 1999,
%    ISBN: 0-13-012641-1,
%    LC: QA297.P45.
%
%  Input:
%
%    real TSPAN = [ T0, TMAX ], the initial and final times.
%    A reasonable value might be [ 0, 5 ].
%
%    real P0 = [ PREY, PRED ], the initial number of prey and predators.
%    A reasonable value might be [ 5000, 100 ].
%
%    integer N: the number of time steps.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'predator_prey_midpoint_explicit_test\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  A pair of ordinary differential equations for a population\n' );
  fprintf ( 1, '  of predators and prey are solved using midpoint_fixed().\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  The exact solution shows periodic behavior, with a fixed\n' );
  fprintf ( 1, '  period and amplitude.\n' );

  f = @ predator_prey_deriv;
  [ t, pout ] = midpoint_explicit ( f, tspan, p0, n );
%
%  Plot the solution.
%
  plot ( pout(:,1), pout(:,2), 'linewidth', 2 );
  grid ( 'on' );
  xlabel ( 'Rabbits' );
  ylabel ( 'Foxes' );
  title ( 'midpoint\_explicit: Predator Prey Phase Plot' )
  filename = 'predator_prey_midpoint_explicit.png';
  print ( '-dpng', filename );
  fprintf ( 1, '  Graphics saved as "%s"\n', filename );

  fprintf ( 1, '\n' );
  fprintf ( 1, 'predator_prey_midpoint_explicit_test\n' );
  fprintf ( 1, '  Normal end of execution.\n' );

  return
end

