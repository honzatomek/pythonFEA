function h = sir_conserved ( y )

%*****************************************************************************80
%
%% sir_conserved returns a conserved quantity for the sir ODE.
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
%    real Y(3,:): the current solution.
%
%  Output:
%
%    real H(:): the value of the conserved quantity.
%
  h = sum ( y );

  return
end
