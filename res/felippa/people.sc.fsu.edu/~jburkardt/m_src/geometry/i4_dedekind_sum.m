function s = i4_dedekind_sum ( p, q )

%*****************************************************************************80
%
%% i4_dedekind_sum() computes the Dedekind sum of two I4's.
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
%    real S, the Dedekind sum of P and Q.
%
  s = 0.0;

  for i = 1 : q
    s = s + i4_dedekind_factor ( i, q ) * i4_dedekind_factor ( p * i, q );
  end

  return
end
