function [ n_data, x, fx ] = normal_01_pdf_values ( n_data )

%*****************************************************************************80
%
%% normal_01_pdf_values() returns some values of the Normal 01 PDF.
%
%  Discussion:
%
%    In Mathematica, the function can be evaluated by:
%
%      Needs["Statistics`ContinuousDistributions`"]
%      dist = NormalDistribution [ 0, 1 ]
%      PDF [ dist, x ]
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    27 July 2015
%
%  Author:
%
%    John Burkardt
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
%    real X, the argument of the function.
%
%    real FX, the value of the function.
%
  n_max = 10;

  fx_vec = [ ...
    0.03155059887555709, ...
    0.0005094586261557538, ...
    0.01235886992552887, ...
    0.353192862601275, ...
    0.3171212685764107, ...
    0.0009653372813755943, ...
    0.06083856556197816, ...
    0.003066504313116445, ...
    0.0005116437388114821, ...
    0.2246444116615346 ];

  x_vec = [ ...
    -2.252653624140994, ...
     3.650540612071437, ...
     2.636073871461605, ...
     0.4935635421351536, ...
    -0.6775433481923101, ...
    -3.471050120671749, ...
    -1.939377660943641, ...
    -3.120345651740235, ...
    -3.649368017767143, ...
     1.0717256984193 ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    x = 0.0;
    fx = 0.0;
  else
    x = x_vec(n_data);
    fx = fx_vec(n_data);
  end

  return
end
