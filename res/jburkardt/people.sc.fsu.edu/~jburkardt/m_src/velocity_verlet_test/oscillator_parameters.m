function [ alpha, beta, epsilon, t0, y0, tstop ] = oscillator_parameters ( ...
  alpha_user, beta_user, epsilon_user, t0_user, y0_user, tstop_user )

%*****************************************************************************80
%
%% oscillator_parameters() returns oscillator parameters.
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
%    real ALPHA_USER: the initial value of Y'
%
%    real BETA_USER: the initial value of Y/EPSILON
%
%    real EPSILON_USER: the period / ( 2*pi).
%
%    real T0_USER: the initial time.
%
%    real Y0(2)_USER: the initial condition.
%
%    real TSTOP_USER: the final time.
%
%  Output:
%
%    real ALPHA: the initial value of Y'
%
%    real BETA: the initial value of Y/EPSILON
%
%    real EPSILON: the period / (2*pi).
%
%    real T0: the initial time.
%
%    real Y0(2): the initial condition.
%
%    real TSTOP: the final time.
%
  persistent alpha_default;
  persistent beta_default;
  persistent epsilon_default;
  persistent t0_default;
  persistent y0_default;
  persistent tstop_default;
%
%  Initialize defaults.
%
  if ( isempty ( alpha_default ) )
    alpha_default = 0.0;
  end

  if ( isempty ( beta_default ) )
    beta_default = 1.0;
  end

  if ( isempty ( epsilon_default ) )
    epsilon_default = 0.1;
  end

  if ( isempty ( t0_default ) )
    t0_default = 0.0;
  end

  if ( isempty ( y0_default ) )
    y1 = beta_default * epsilon_default;
    y2 = alpha_default;
    y0_default = [ y1, y2 ];
  end

  if ( isempty ( tstop_default ) )
    tstop_default = 2.0 * pi;
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
    epsilon_default = epsilon_user;
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
  alpha = alpha_default;
  beta = beta_default;
  epsilon = epsilon_default;
  t0 = t0_default;
  y0 = y0_default;
  tstop = tstop_default;
  
  return
end

