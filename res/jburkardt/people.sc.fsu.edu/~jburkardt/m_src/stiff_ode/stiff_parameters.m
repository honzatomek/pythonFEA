function [ lambda, t0, y0, tstop ] = stiff_parameters ( lambda_user, ...
  t0_user, y0_user, tstop_user )

%*****************************************************************************80
%
%% stiff_parameters() returns parameters of stiff_ode().
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
%    26 April 2021
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real LAMBDA_USER: a parameter.
%
%    real T0_USER: the initial time.
%
%    real Y0_USER: the initial condition.
%
%    real TSTOP_USER: the final time.
%
%  Output:
%
%    real LAMBDA: a parameter.
%
%    real T0: the initial time.
%
%    real Y0: the initial condition.
%
%    real TSTOP: the final time.
%
  persistent lambda_default;
  persistent t0_default;
  persistent y0_default;
  persistent tstop_default;
%
%  Initialize defaults.
%
  if ( isempty ( lambda_default ) )
    lambda_default = 50.0;
  end

  if ( isempty ( t0_default ) )
    t0_default = 0.0;
  end

  if ( isempty ( y0_default ) )
    y0_default = 0.0;
  end

  if ( isempty ( tstop_default ) )
    tstop_default = 1.0;
  end
%
%  Update defaults if input was supplied.
%
  if ( 1 <= nargin )
    lambda_default = lambda_user;
  end

  if ( 2 <= nargin )
    t0_default = t0_user;
  end

  if ( 3 <= nargin )
    y0_default = y0_user;
  end

  if ( 4 <= nargin )
    tstop_default = tstop_user;
  end
%
%  Return values.
%
  lambda = lambda_default;
  t0 = t0_default;
  y0 = y0_default;
  tstop = tstop_default;
  return
end

