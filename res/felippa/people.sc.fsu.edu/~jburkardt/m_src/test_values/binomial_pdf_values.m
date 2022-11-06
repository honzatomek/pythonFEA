function [ n_data, a, b, x, fx ] = binomial_pdf_values ( n_data )

%*****************************************************************************80
%
%% binomial_pdf_values() returns some values of the binomial PDF.
%
%  Discussion:
%
%    PDF(X)(A,B) is the probability of X successes in A trials,
%    given that the probability of success on a single trial is B.
%
%    In Mathematica, the function can be evaluated by:
%
%      Needs["Statistics`DiscreteDistributions`]
%      dist = BinomialDistribution [ n, p ]
%      PDF [ dist, x ]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 July 2015
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Milton Abramowitz and Irene Stegun,
%    Handbook of Mathematical Functions,
%    US Department of Commerce, 1964.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
%
%    Daniel Zwillinger,
%    CRC Standard Mathematical Tables and Formulae,
%    30th Edition, CRC Press, 1996, pages 651-652.
%
%  Input:
%
%    integer N_DATA.  The user sets N_DATA to 0 before the first call.  
%    Thereafter, it should simply be the value returned by the previous call.
%
%  Output:
%
%    integer N_DATA.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    integer A, a parameter of the function.
%
%    real B, a parameter of the function.
%
%    integer X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 10;

  a_vec = [ ...
     5, 12,  6, 13,  9, ...
     1,  2, 17,  6,  8 ];

  b_vec = [ ...
    0.8295092339327006; ...
    0.06611873491603133; ...
    0.0438289977791071; ...
    0.4495389603071763; ...
    0.7972869541062562; ...
    0.3507523379805466; ...
    0.8590968552798568; ...
    0.007512364073964213; ...
    0.1136640464424993; ...
    0.2671322702601793  ];

  fx_vec = [ ...
    0.3927408939646697; ...
    0.0006199968732461383; ...
    0.764211224733124; ...
    0.0004260353334364943; ...
    0.302948289145794; ...
    0.3507523379805466; ...
    0.01985369619202562; ...
    0.006854388879646552; ...
    0.000002156446446382985; ...
    0.0005691150511772053 ];

  x_vec = [ ...
     5, 5, 0, 0, 7, ...
     1, 0, 2, 6, 7 ];

  if ( n_data < 0 )
    n_data = 0;
  end
 
  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    a = 0;
    b = 0.0;
    x = 0;
    fx = 0.0;
  else
    a = a_vec(n_data);
    b = b_vec(n_data);
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
