function h = henon_heiles_conserved ( w )

%*****************************************************************************80
%
%% henon_heiles_conserved evaluates conserved quantities for the Henon Heiles ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 November 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Michel Henon, Carl Heiles,
%    The applicability of the third integral of motion: 
%    Some numerical experiments,
%    The Astronomical Journal,
%    Volume 69, pages 73-79, 1964.
%
%  Input:
%
%    real W(N,4): the current coordinates.
%
%  Output:
%
%    real H(N): the conserved quantities.
%
  [ e, lam, t0, w0 ] = henon_heiles_parameters ( );

  x =  w(:,1);
  xp = w(:,2);
  y =  w(:,3);
  yp = w(:,4);

  h = 0.5 * ( xp.^2 + yp.^2 ) + 0.5 * ( x.^2 + y.^2 ) ...
    + lam * ( x.^2 .* y - y.^3 / 3.0 );

  return
end

