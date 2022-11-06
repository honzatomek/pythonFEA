function alpha = p37_param_get ( )

%*****************************************************************************80
%
%% p37_param_get() returns the parameter values for problem 37.
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
  alpha = p37_param ( 'GET', 'ALPHA', [] );

  return
end
