function [z] = erad_ode(a,b,ze,d,Ti,dt,s1,z1,r1)

%*****************************************************************************80
%
%% erad_ode applies Euler's method to simulate the eradication plan.
%
%  Discussion:
%
%    This function will take as inputs, the initial value of the 3 classes.
%    It will then apply Eulers method to the problem and churn out a vector of
%    solutions over a predetermined period of time (the other input).
%
%  Author:
%
%    Philip Munz
%
%  Input: 
%
%    s1, z1, r1 - initial value of each ODE, either the
%    actual initial value or the value after the impulse.
%
%    Ti - Amount of time between inpulses.
%
%    real dt, the time step
%
%  Output:
%
%    z: the current zombie population.
%
  k = Ti/dt;
  s = zeros(1,k+1);
  z = zeros(1,k+1);
  r = zeros(1,k+1);
  t = 0:dt:Ti; 
  s(1) = s1;
  z(1) = z1;
  r(1) = r1;
  for i=1:k
    s(i+1) = s(i) + dt*(-b*s(i)*z(i));
    z(i+1) = z(i) + dt*(b*s(i)*z(i) -a*s(i)*z(i) +ze*r(i));
    r(i+1) = r(i) + dt*(a*s(i)*z(i) +d*s(i) - ze*r(i));
  end

  return
end

