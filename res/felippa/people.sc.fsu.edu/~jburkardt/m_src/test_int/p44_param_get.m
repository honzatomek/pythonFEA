function alpha = p44_param_get ( )

%*****************************************************************************80
%
%% p44_param_get() returns the parameter values for problem 44.
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
  alpha = p44_param ( 'GET', 'ALPHA', [] );

  return
end
