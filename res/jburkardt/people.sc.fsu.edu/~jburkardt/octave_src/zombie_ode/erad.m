function [] = erad ( a, b, ze, d, k, T, dt )

%*****************************************************************************80
%
%% erad is the main function in the zombie eradication simulation.
%
%  Discussion:
%
%    This is the main function in our numerical impulse analysis, used in
%    conjunction with erad_ode.m, which will simulate the eradication of
%    zombies. The impulses represent a coordinated attack against zombiekind
%    at specified times.
%
%  Modified:
%
%    22 February 2020
%
%  Author:
%
%    Philip Munz
%
%  Input: 
%
%    a - alpha value in model: "zombie destruction" rate
%
%    b - beta value in model: "new zombie" rate
%
%    ze - zeta value in model: zombie resurrection rate
%
%    d - delta value in model: background death rate
%
%    k - "kill" rate, used in the impulse
%
%    T - Stopping time
%
%    dt - time step for numerical solutions
%
  N = 1000;
%
%  We plan to break the solution into 4 parts with 4 impulses
%
  Ti = T/4; 
  n = Ti/dt;
  m = T/dt;
  s = zeros(1,n+1);
  z = zeros(1,n+1);
  r = zeros(1,n+1);
%
%  The solution vector for all zombie impulses and such
%
  sol = zeros(1,m+1); 
  t = zeros(1,m+1);
  s1 = N;
  z1 = 0;
  r1 = 0;

  sol1 = erad_ode(a,b,ze,d,Ti,dt,s1,z1,r1);
  sol1(n+1) = ( 1.0 - k ) * sol1(n+1); %347.7975;
  s1 = N-sol1(n+1);
  z1 = sol1(n+1);
  r1 = 0;

  sol2 = erad_ode(a,b,ze,d,Ti,dt,s1,z1,r1);
  sol2(n+1) = ( 1.0 - 2 * k ) * sol2(n+1);
  s1 = N-sol2(n+1);
  z1 = sol2(n+1);
  r1 = 0;

  sol3 = erad_ode(a,b,ze,d,Ti,dt,s1,z1,r1);
  sol3(n+1) = ( 1.0 - 3 * k ) * sol3(n+1);
  s1 = N-sol3(n+1);
  z1 = sol3(n+1);
  r1 = 0;

  sol4 = erad_ode(a,b,ze,d,Ti,dt,s1,z1,r1);
  sol4(n+1) = ( 1.0 - 4 * k ) * sol4(n+1);
  s1 = N-sol4(n+1);
  z1 = sol4(n+1);
  r1 = 0;

  t1 = 0:dt:Ti;
  t2 = Ti:dt:2*Ti;
  t3 = 2*Ti:dt:3*Ti;
  t4 = 3*Ti:dt:4*Ti;

  clf ( );
  hold ( 'on' );
  plot(t1,sol1,'r', 'linewidth', 3 )
  plot(t2,sol2,'r', 'linewidth', 3 )
  plot(t3,sol3,'r', 'linewidth', 3 )
  plot(t4,sol4,'r', 'linewidth', 3 )
  hold off
  grid ( 'on' );
%
%  Save plot in a file.
%
  filename = 'zombie_ode_test02.png';
  print ( '-dpng', filename );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Graphics saved as "%s".\n', filename );

  return
end
