function [ alpha, beta, gamma, delta, t0, y0, tstop ] = ...
  predator_prey_parameters ( alpha_user, beta_user, gamma_user, ...
  delta_user, t0_user, y0_user, tstop_user )

%*****************************************************************************80
%
%% predator_prey_parameters() returns parameters for predator_prey_ode().
%
%  Discussion:
%
%    If input values are specified, this resets the default parameters.
%    Otherwise, the output will be the current defaults.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    24 April 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real ALPHA_USER, BETA_USER, GAMMA_USER, DELTA_USER: the parameter values;
%
%    real T0_USER: the initial time.
%
%    real Y0_USER(2): the initial condition.
%
%    real TSTOP_USER: the final time.
%
%  Output:
%
%    real ALPHA, BETA, GAMMA, DELTA: the parameter values;
%
%    real T0: the initial time.
%
%    real Y0(2): the initial condition.
%
%    real TSTOP: the final time.
%
  persistent alpha_default;
  persistent beta_default;
  persistent gamma_default;
  persistent delta_default;
  persistent t0_default;
  persistent y0_default;
  persistent tstop_default;
%
%  Initialize defaults.
%
  if ( isempty ( alpha_default ) )
    alpha_default = 2.0;
  end

  if ( isempty ( beta_default ) )
    beta_default = 0.001;
  end

  if ( isempty ( gamma_default ) )
    gamma_default = 10.0;
  end

  if ( isempty ( delta_default ) )
    delta_default = 0.002;
  end

  if ( isempty ( t0_default ) )
    t0_default = 0.0;
  end

  if ( isempty ( y0_default ) )
    y0_default = [ 5000, 100 ];
  end

  if ( isempty ( tstop_default ) )
    tstop_default = 5.0;
  end
%
%  Update defaults if input was supplied.
%
  if ( 1 <= nargin )
    alpha_default = alpha_user;
  end

  if ( 2 <= nargin )
    beta_default = beta_user;
  end

  if ( 3 <= nargin )
    gamma_default = gamma_user;
  end

  if ( 4 <= nargin )
    delta_default = delta_user;
  end

  if ( 5 <= nargin )
    t0_default = t0_user;
  end

  if ( 6 <= nargin )
    y0_default = y0_user;
  end

  if ( 7 <= nargin )
    tstop_default = tstop_user;
  end
%
%  Return values.
%
  alpha = alpha_default;
  beta = beta_default;
  gamma = gamma_default;
  delta = delta_default;
  t0 = t0_default;
  y0 = y0_default;
  tstop = tstop_default;
  
  return
end

