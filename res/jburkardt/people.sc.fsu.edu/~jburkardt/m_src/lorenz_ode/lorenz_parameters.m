function [ beta, rho, sigma, t0, y0, tstop ] = lorenz_parameters ( ...
  beta_user, rho_user, sigma_user, t0_user, y0_user, tstop_user )

%*****************************************************************************80
%
%% lorenz_parameters() returns parameters for lorenz_ode().
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
%    23 April 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real BETA_USER, RHO_USER, SIGMA_USER: problem parameters.
%
%    real T0_USER: the initial time.
%
%    real Y0_USER(3): the initial condition.
%
%    real TSTOP_USER: the final time.
%
%  Output:
%
%    real BETA, RHO, SIGMA: problem parameters.
%
%    real T0: the initial time.
%
%    real Y0(3): the initial condition.
%
%    real TSTOP: the final time.
%
  persistent beta_default;
  persistent rho_default;
  persistent sigma_default;
  persistent t0_default;
  persistent y0_default;
  persistent tstop_default;
%
%  Initialize defaults.
%
  if ( isempty ( beta_default ) )
    beta_default = 8.0 / 3.0;
  end

  if ( isempty ( rho_default ) )
    rho_default = 28.0;
  end

  if ( isempty ( sigma_default ) )
    sigma_default = 10.0;
  end

  if ( isempty ( t0_default ) )
    t0_default = 0.0;
  end

  if ( isempty ( y0_default ) )
    y0_default = [ 8.0, 1.0, 1.0 ];
  end

  if ( isempty ( tstop_default ) )
    tstop_default = 40.0;
  end
%
%  Update defaults if input was supplied.
%
  if ( 1 <= nargin )
    beta_default = beta_user;
  end

  if ( 2 <= nargin )
    rho_default = rho_user;
  end

  if ( 3 <= nargin )
    sigma_default = sigma_user;
  end

  if ( 4 <= nargin )
    t0_default = t0_user;
  end

  if ( 5 <= nargin )
    y0_default = y0_user;
  end

  if ( 6 <= nargin )
    tstop_default = tstop_user;
  end
%
%  Return values.
%
  beta = beta_default;
  rho = rho_default;
  sigma = sigma_default;
  t0 = t0_default;
  y0 = y0_default;
  tstop = tstop_default;
  
  return
end

