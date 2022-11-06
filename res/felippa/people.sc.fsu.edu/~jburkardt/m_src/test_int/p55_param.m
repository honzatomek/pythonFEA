function value = p55_param ( action, name, value )

%*****************************************************************************80
%
%% p55_param() sets or gets real scalar parameters for problem 55.
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
%    string ACTION.
%    'GET' to get the value.
%    'RANDOMIZE' to randomly set the value.
%    'SET' to set the value.
%
%    string NAME, the name of the parameter.
%    'C' is the coefficient.
%    'X0' is the base point.
%
%    real VALUE.
%    If ACTION is 'SET', then the parameter value is set to VALUE.
%
%  Output:
%
%    real VALUE.
%    If the action is 'GET', then VALUE returns the current parameter value.
%
  persistent c;
  persistent x0;

  if ( s_eqi ( action, 'get' ) )

    if ( s_eqi ( name, 'c' ) )
      if ( isempty ( c ) )
        c = 3.0;
      end
      value = c;
    elseif ( s_eqi ( name, 'x0' ) )
      if ( isempty ( x0 ) )
        x0 = 0.75;
      end
      value = x0;
    else
      fprintf ( 1, '\n' );
      fprintf ( 1, 'P55_param - Fatal error!\n' );
      fprintf ( 1, '  Unrecognized name.\n' );
      error ( 'P55_param - Fatal error!' );
    end

  elseif ( s_eqi ( action, 'random' ) )

    if ( s_eqi ( name, 'c' ) )
      c = rand ( );
    elseif ( s_eqi ( name, 'x0' ) )
      x0 = rand ( );
    else
      fprintf ( 1, '\n' );
      fprintf ( 1, 'P55_param - Fatal error!\n' );
      fprintf ( 1, '  Unrecognized name.\n' );
      error ( 'P554_param - Fatal error!' );
    end

  elseif ( s_eqi ( action, 'set' ) )

    if ( s_eqi ( name, 'c' ) )
      c = value;
    elseif ( s_eqi ( name, 'x0' ) )
      x0 = value;
    else
      fprintf ( 1, '\n' );
      fprintf ( 1, 'P55_param - Fatal error!\n' );
      fprintf ( 1, '  Unrecognized name.\n' );
      error ( 'P55_param - Fatal error!' );
    end

  else

    fprintf ( 1, '\n' );
    fprintf ( 1, 'P55_param - Fatal error!\n' );
    fprintf ( 1, '  Unrecognized action.\n' );
    error ( 'P55_param - Fatal error!' );

  end

  return
end
