function alpha = p45_param_get ( )

%*****************************************************************************80
%
%% p45_param_get() returns the parameter values for problem 45.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    16 March 2002
%
%  Author:
%
%    John Burkardt
%
%  Output:
%
%    real ALPHA, the current value of the parameter.
%
  alpha = p45_param ( 'GET', 'ALPHA', [] );

  return
end
