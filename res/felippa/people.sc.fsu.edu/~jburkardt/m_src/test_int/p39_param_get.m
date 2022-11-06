function alpha = p39_param_get ( )

%*****************************************************************************80
%
%% p39_param_get() returns the parameter values for problem 39.
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
%  Output:
%
%    real ALPHA, the current value of the parameter.
%
  alpha = p39_param ( 'GET', 'ALPHA', [] );

  return
end
