function [ n_data, x, fx ] = gud_values ( n_data )

%*****************************************************************************80
%
%% gud_values() returns some values of the Gudermannian function.
%
%  Definition:
%
%    The Gudermannian function relates the hyperbolic and trigonomentric
%    functions.  For any argument X, there is a corresponding value
%    GAMMA so that
%
%      SINH(X) = TAN(GAMMA).
%
%    This value GAMMA(X) is called the Gudermannian of X and symbolized
%    GD(X).  The inverse Gudermannian function is given as input a value
%    GAMMA and computes the corresponding value X.
%
%    GD(X) = 2 * arctan ( exp ( X ) ) - PI / 2
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    30 May 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%    Daniel Zwillinger, editor,
%    CRC Standard Mathematical Tables and Formulae,
%    30th Edition,
%    CRC Press, 1996.
%
%  Input:
%
%    integer N_DATA.  The user sets N_DATA to 0 before the first call.
%
%  Output:
%
%    integer N_DATA.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 13;
  fx_vec = [ ...
    -1.301760336E+00,  -0.8657694832E+00, 0.0000000000E+00, ...
     0.09983374879E+00, 0.1986798470E+00, 0.4803810791E+00, ...
     0.8657694832E+00,  1.131728345E+00,  1.301760336E+00,  ...
     1.406993569E+00,   1.471304341E+00,  1.510419908E+00,  ...
     1.534169144E+00 ];
  x_vec = [ ...
    -2.0E+00, -1.0E+00,  0.0E+00, ...
     0.1E+00,  0.2E+00,  0.5E+00, ...
     1.0E+00,  1.5E+00,  2.0E+00, ...
     2.5E+00,  3.0E+00,  3.5E+00, ...
     4.0E+00  ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    x = 0.0E+00;
    fx = 0.0E+00;
  else
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
