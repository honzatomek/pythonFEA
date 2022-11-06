function p41_param_set ( alpha )

%*****************************************************************************80
%
%% p41_param_set() sets the parameter values for problem 41.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    11 November 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    real ALPHA, the new value of the parameter.
%
  p41_param ( 'SET', 'ALPHA', alpha );

  return
end
