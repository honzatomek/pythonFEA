function dydt = robertson_deriv ( t, y )

%*****************************************************************************80
%
%% robertson_deriv evaluates the derivative of the Robertson ODE.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    05 August 2020
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Ernst Hairer, Gerhard Wanner,
%    Solving Ordinary Differential Equations II: 
%    Stiff and Differential-algebraic Problems,
%    Springer-Verlag, second revised edition, 1996.
%
%  Input:
%
%    real T, Y(3): the arguments of the derivative.
%
%  Output:
%
%    real DYDT(3): the value of the derivative.
%
  y1 = y(1);
  y2 = y(2);
  y3 = y(3);

  dydt = zeros(3,1);

  dydt(1) = - 0.04 * y1 + 10000.0 * y2 * y3;
  dydt(2) =   0.04 * y1 - 10000.0 * y2 * y3 - 30000000.0 * y2 * y2;
  dydt(3) =                                 + 30000000.0 * y2 * y2;  

  return
end
