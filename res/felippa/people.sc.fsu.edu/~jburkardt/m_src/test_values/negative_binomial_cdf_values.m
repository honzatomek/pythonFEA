function [ n_data, f, s, p, cdf ] = negative_binomial_cdf_values ( n_data )

%*****************************************************************************80
%
%% negative_binomial_cdf_values() returns values of the negative binomial CDF.
%
%  Discussion:
%
%    Assume that a coin has a probability P of coming up heads on
%    any one trial.  Suppose that we plan to flip the coin until we
%    achieve a total of S heads.  If we let F represent the number of
%    tails that occur in this process, then the value of F satisfies
%    a negative binomial PDF:
%
%      PDF(F,S,P) = Choose ( F from F+S-1 ) * P^S * (1-P)^F
%
%    The negative binomial CDF is the probability that there are F or
%    fewer failures upon the attainment of the S-th success.  Thus,
%
%      CDF(F,S,P) = sum ( 0 <= G <= F ) PDF(G,S,P)
%
%    In Mathematica, the function can be evaluated by:
%
%      Needs["Statistics`DiscreteDistributions`]
%      dist = NegativeBinomialDistribution [ s, p ]
%      CDF [ dist, f ]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    19 September 2004
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    F C Powell,
%    Statistical Tables for Sociology, Biology and Physical Sciences,
%    Cambridge University Press, 1982.
%
%    Stephen Wolfram,
%    The Mathematica Book,
%    Fourth Edition,
%    Wolfram Media / Cambridge University Press, 1999.
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
%    integer F, the maximum number of failures.
%
%    integer S, the number of successes.
%
%    real P, the probability of a success on one trial.
%
%    real CDF, the probability of at most F failures 
%    before the S-th success.
%
  n_max = 27;

  cdf_vec = [ ...
     0.6367187500000000E+00, ...
     0.3632812500000000E+00, ...
     0.1445312500000000E+00, ...
     0.5000000000000000E+00, ...
     0.2265625000000000E+00, ...
     0.6250000000000000E-01, ...
     0.3437500000000000E+00, ...
     0.1093750000000000E+00, ...
     0.1562500000000000E-01, ...
     0.1792000000000000E+00, ...
     0.4096000000000000E-01, ...
     0.4096000000000000E-02, ...
     0.7047000000000000E-01, ...
     0.1093500000000000E-01, ...
     0.7290000000000000E-03, ...
     0.9861587127990000E+00, ...
     0.9149749500510000E+00, ...
     0.7471846521450000E+00, ...
     0.8499053647030009E+00, ...
     0.5497160941090026E+00, ...
     0.2662040052146710E+00, ...
     0.6513215599000000E+00, ...
     0.2639010709000000E+00, ...
     0.7019082640000000E-01, ...
     0.1000000000000000E+01, ...
     0.1990000000000000E-01, ...
     0.1000000000000000E-03 ];

  f_vec = [ ...
     4,  3,  2, ...
     3,  2,  1, ...
     2,  1,  0, ...
     2,  1,  0, ...
     2,  1,  0, ...
    11, 10,  9, ...
    17, 16, 15, ...
     9,  8,  7, ...
     2,  1,  0 ];

  p_vec = [ ...
     0.50E+00, ...
     0.50E+00, ...
     0.50E+00, ...
     0.50E+00, ...
     0.50E+00, ...
     0.50E+00, ...
     0.50E+00, ...
     0.50E+00, ...
     0.50E+00, ...
     0.40E+00, ...
     0.40E+00, ...
     0.40E+00, ...
     0.30E+00, ...
     0.30E+00, ...
     0.30E+00, ...
     0.30E+00, ...
     0.30E+00, ...
     0.30E+00, ...
     0.10E+00, ...
     0.10E+00, ...
     0.10E+00, ...
     0.10E+00, ...
     0.10E+00, ...
     0.10E+00, ...
     0.10E-01, ...
     0.10E-01, ...
     0.10E-01 ];

  s_vec = [ ...
    4, 5, 6, ...
    4, 5, 6, ...
    4, 5, 6, ...
    4, 5, 6, ...
    4, 5, 6, ...
    1, 2, 3, ...
    1, 2, 3, ...
    1, 2, 3, ...
    0, 1, 2 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    f = 0;
    s = 0;
    p = 0.0;
    cdf = 0.0;
  else
    f = f_vec(n_data);
    s = s_vec(n_data);
    p = p_vec(n_data);
    cdf = cdf_vec(n_data);
  end

  return
end