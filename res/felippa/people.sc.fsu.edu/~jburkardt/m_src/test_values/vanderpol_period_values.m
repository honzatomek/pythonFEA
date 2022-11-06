function [ n_data, mu, p ] = vanderpol_period_values ( n_data )

%*****************************************************************************80
%
%% vanderpol_period_values() returns some values of the Van der Pol period.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 December 2021
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Mary Cartwright,
%    Van der Pol's equation for relaxation oscillations,
%    Contributions to the theory of nonlinear oscillations, II,
%    Annals of Mathematical Studies 29,
%    Princeton, 1952, pages 3-18.
%
%    John D Cook,
%    Calculating the period of Van der Pol Oscillators,
%    https://www.johndcook.com/blog/2019/12/26/van-der-pol-period/
%    26 December 2019.
%
%    Roger Grimshaw,
%    Nonlinear Ordinary Differential Equations,
%    CRC Press, 1991, pages 160-163.
%
%    Minoru Urabe,
%    Journal of Science of Hiroshima University,
%    Series A, Volume 24, Number 2, October 1960, pages 197-199.
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
%    real MU, the parameter.
%
%    real P, the period.
%
  n_max = 10;

  mu_vec = [ ...
       0.0, ...
       1.0, ...
       2.0, ...
       3.0, ...
       4.0, ...
       5.0, ...
       6.0, ...
       8.0, ...
      10.0, ...
      20.0 ];

  p_vec = [ ...
     6.283, ...
     6.687, ...
     7.6310, ...
     8.8613, ...
    10.2072, ...
    11.6055, ...
    13.0550, ...
    16.0740, ...
    19.1550, ...
    34.7103 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    mu = 0.0;
    p = 0.0;
  else
    mu = mu_vec(n_data);
    p = p_vec(n_data);
  end

  return
end
