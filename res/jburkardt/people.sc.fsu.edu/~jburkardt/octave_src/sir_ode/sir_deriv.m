function [ dSdt, dIdt, dRdt ] = sir_deriv ( t, S, I, R, beta, gamma )

%*****************************************************************************80
%
%% sir_deriv evaluates the derivative of the SIR ode.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    22 February 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T: the time.
%
%    real S, I, R, the susceptible, infected, and recovered populations.
%
%    real BETA, GAMMA, the infection and recovery rates.
%    0.02 and 0.005 might be typical.
%
%  Output:
%
%    real dSdt, dIdt, dRdt, the derivatives.
%
  P = S + I + R;
  dSdt = - beta * S * I / P;
  dIdt =   beta * S * I / P - gamma * I;
  dRdt =                      gamma * I;

  return
end

