function value = p36_param ( action, name, value )

%*****************************************************************************80
%
%% p36_param() gets or sets the parameter values for problem 36.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 November 2009
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string ACTION.
%    'GET' to get the value.
%    'SET' to set the value.
%
%    string NAME, the name of the parameter.
%    'ALPHA' is the only option.
%
%    real VALUE.
%    If ACTION is 'SET', then the parameter value is set to VALUE.
%
%  Output:
%
%    real VALUE.
%    If the action is 'GET', then VALUE returns the current parameter value.
%
  persistent alpha;

  if ( s_eqi ( action, 'get' ) )
    if ( s_eqi ( name, 'alpha' ) )
      if ( isempty ( alpha ) )
        alpha = -0.9;
      end
      value = alpha;
    else
      fprintf ( 1, '\n' );
      fprintf ( 1, 'P36_param - Fatal error!\n' );
      fprintf ( 1, '  Unrecognized name.\n' );
      error ( 'P36_param - Fatal error!' );
    end
  elseif ( s_eqi ( action, 'set' ) )
    if ( s_eqi ( name, 'alpha' ) )
      alpha = value;
    else
      fprintf ( 1, '\n' );
      fprintf ( 1, 'P36_param - Fatal error!\n' );
      fprintf ( 1, '  Unrecognized name.\n' );
      error ( 'P36_param - Fatal error!' );
    end
  else
    fprintf ( 1, '\n' );
    fprintf ( 1, 'P36_param - Fatal error!\n' );
    fprintf ( 1, '  Unrecognized action.\n' );
    error ( 'P36_param - Fatal error!' );
  end

  return
end
