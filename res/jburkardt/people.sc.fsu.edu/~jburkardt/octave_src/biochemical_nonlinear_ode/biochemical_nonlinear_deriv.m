function dcnpddt = biochemical_nonlinear_deriv ( t, cnpd )

%*****************************************************************************80
%
%% biochemical_nonlinear_deriv: derivative of a nonlinear biochemical ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    13 Novmber 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Angela Martiradonna, Gianpiero Colonna, Fasma Diele,
%    GeCo: Geometric conservative nonstandard schemesfor biochemical systems,
%    Applied Numerical Mathematics,
%    2019.
%
%  Input:
%
%    real T, CNPD(4): the arguments of the derivative.
%
%  Output:
%
%    real DCNPDDT(4): the value of the derivative.
%
  [ a, b, kc, kn, rmax, e, t0, y0 ] = biochemical_nonlinear_parameters ( );

  c = cnpd(1);
  n = cnpd(2);
  p = cnpd(3);
  d = cnpd(4);

  S = [ -a,    0.0; ...
        -b,    0.0; ...
         1.0, -1.0; ...
         0.0,  1.0 ];

  r = [ rmax * c / ( kc + c ) * n * ( kn + n ) * p; ...
        e * p ];
  
  dcnpddt = S * r;

  return
end
