function [ t0, y0, tstop ] = flame_parameters ( t0_user, y0_user, tstop_user )

%*****************************************************************************80
%
%% flame_parameters() returns parameters for flame_ode().
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
%    real T0_USER: the initial time.
%
%    real Y0_USER: the initial condition at time T0.
%
%    real TSTOP_USER: the final time.
%
%  Output:
%
%    real T0: the initial time.
%
%    real Y0: the initial condition at time T0.
%
%    real TSTOP: the final time.
%
  persistent t0_default;
  persistent y0_default;
  persistent tstop_default;
%
%  Initialize defaults.
%
  if ( isempty ( t0_default ) )
    t0_default = 0.0;
  end

  if ( isempty ( y0_default ) )
    y0_default = 0.01;
  end

  if ( isempty ( tstop_default ) )
    tstop_default = 200.0;
  end
%
%  Update defaults if input was supplied.
%
  if ( 1 <= nargin )
    t0_default = t0_user;
  end

  if ( 2 <= nargin )
    y0_default = y0_user;
  end

  if ( 3 <= nargin )
    tstop_default = tstop_user;
  end
%
%  Return values.
%
  t0 = t0_default;
  y0 = y0_default;
  tstop = tstop_default;

  return
end

