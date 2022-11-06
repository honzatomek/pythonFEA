function p42_param_set ( alpha )

%*****************************************************************************80
%
%% p42_param_set() sets the parameter values for problem 42.
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
  p42_param ( 'SET', 'ALPHA', alpha );

  return
end
