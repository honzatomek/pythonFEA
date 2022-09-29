function y = quasiperiodic_exact ( t )

%*****************************************************************************80
%
%% quasiperiodic_exact returns the exact solution for the quasiperiodic ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    23 July 2020
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real T(:): the current time.
%
%  Output:
%
%    real Y(4,:): the exact solution.
%
  p =   cos ( t ) +        cos ( pi * t );
  q = - sin ( t ) - pi   * sin ( pi * t );
  r = - cos ( t ) - pi^2 * cos ( pi * t );
  s =   sin ( t ) + pi^3 * sin ( pi * t );

  y = [ p, q, r, s ];
 
  return
end

