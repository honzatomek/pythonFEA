function [ e, t0, y0, tstop ] = kepler_parameters ( e_user, t0_user, ...
  y0_user, tstop_user )

%*****************************************************************************80
%
%% kepler_parameters() returns parameters for the kepler ODE.
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
%    real E_USER: the orbital eccentricity.
%
%    real T0_USER: the initial time.
%
%    real Y0_USER[4]: the initial condition.
%
%    real TSTOP_USER: the final time.
%
%  Output:
%
%    real E: the orbital eccentricity.
%
%    real T0: the initial time.
%
%    real Y0[4]: the initial condition.
%
%    real TSTOP: the final time.
%
  persistent e_default;
  persistent t0_default;
  persistent y0_default;
  persistent tstop_default;
%
%  Initialize defaults.
%
  if ( isempty ( e_default ) )
    e_default = 0.6;
  end

  if ( isempty ( t0_default ) )
    t0_default = 0.0;
  end

  if ( isempty ( y0_default ) )
    y1 = 1.0 - e_default;
    y2 = 0.0;
    y3 = 0.0;
    y4 = sqrt ( ( 1.0 + e_default ) / ( 1.0 - e_default ) );
    y0_default = [ y1, y2, y3, y4 ];
  end

  if ( isempty ( tstop_default ) )
    tstop_default = 120.0;
  end
%
%  Update defaults if input was supplied.
%
  if ( 1 <= nargin )
    e_default = e_user;
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
  e = e_default;
  t0 = t0_default;
  y0 = y0_default;
  tstop = tstop_default;
  
  return
end

