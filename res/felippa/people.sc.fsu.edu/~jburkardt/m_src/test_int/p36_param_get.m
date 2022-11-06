function alpha = p36_param_get ( )

%*****************************************************************************80
%
%% p36_param_get() returns the parameter values for problem 36.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    04 November 2009
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real ALPHA, the current value of the parameter.
%
  alpha = p36_param ( 'get', 'alpha', [] );

  return
end
