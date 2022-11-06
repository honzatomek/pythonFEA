function r = i4_dedekind_factor ( p, q )

%*****************************************************************************80
%
%% i4_dedekind_factor() computes a function needed for a Dedekind sum. 
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    17 July 2009
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Hans Rademacher, Emil Grosswald,
%    Dedekind Sums,
%    Mathematics Association of America, 1972,
%    LC: QA241.R2.
%
%  Input:
%
%    integer P, Q, two positive integers.
%
%  Output:
%
%    real R, the Dedekind factor of P / Q.
%
  if ( mod ( p, q ) == 0 )
    r = 0.0;
  else
    r = ( p / q ) - floor ( p / q ) - 0.5;
  end

  return
end
