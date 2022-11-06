function order = square_minimal_rule_order ( degree )

%*****************************************************************************80
%
%% square_minimal_rule_order() returns the order of a minimal square rule.
%
%  Licensing:
%
%    This code is distributed under the GNU GPL license.
%
%  Modified:
%
%    20 February 2018
%
%  Author:
%
%    John Burkardt.
%
%  Reference:
%
%    Mattia Festa, Alvise Sommariva,
%    Computing almost minimal formulas on the square,
%    Journal of Computational and Applied Mathematics,
%    Volume 17, Number 236, November 2012, pages 4296-4302.
%
%  Input:
%
%    integer DEGREE, the degree of the rule, between 0 and 55.
%
%  Output:
%
%    integer ORDER, the order of the rule.
%
  order_list = [ ...
    1,     1,    3,    4,    6, ...
    7,    10,   12,   16,   17, ...
   22,    24,   31,   33,   40, ...
   43,    52,   54,   64,   67, ...
   78,    81,   93,   96,  109, ...
  113,   127,  132,  146,  153, ...
  167,   172,  189,  197,  211, ...
  220,   238,  245,  265,  274, ...
  296,   303,  326,  331,  353, ...
  359,   387,  396,  417,  427, ...
  454,   462,  493,  498,  530, ...
  536 ];

  degree_max = square_minimal_rule_degree_max ( );

  if ( degree < 1 )
    order = 0;
  elseif ( degree_max < degree )
    order = 0;
  else
    order = order_list(degree+1);
  end

  return
end
