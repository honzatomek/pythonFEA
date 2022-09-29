function y = flame_exact ( t )

%*****************************************************************************80
%
%% flame_exact() evaluates the exact solution for flame_ode().
%
%  Discussion:
%
%    I would prefer to call the MATLAB function "lambertw(x)" but
%    this requires access to the symbolic toolbox.
%
%    So instead, I call "lambert_w(x,0,0)", a version of TOMS743.
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
%  Reference:
%
%    Cleve Moler,
%    Cleve's Corner: Stiff Differential Equations,
%    MATLAB News and Notes,
%    May 2003, pages 12-13.
%
%  Input:
%
%    real T(:): the times.
%
%  Output:
%
%    real Y(:), the exact solution values.
%
  [ t0, y0, tstop ] = flame_parameters ( );

  a = ( 1.0 - y0 ) / y0;

  n = size ( t, 1 );
  y = zeros ( n, 1 );
  for i = 1 : n
    y(i) = 1.0 ./ ( lambert_w ( a .* exp ( a - ( t(i) - t0 ) ), 0, 0 ) + 1.0 );
  end

  return
end
