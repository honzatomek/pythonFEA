function result = p00_halton ( prob, int_num )

%*****************************************************************************80
%
%% p00_halton() applies a Halton sequence rule to integrate a function.
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
%  Reference:
%
%    John Halton,
%    On the efficiency of certain quasi-random sequences of points
%    in evaluating multi-dimensional integrals,
%    Numerische Mathematik,
%    Volume 2, pages 84-90, 1960.
%
%  Input:
%
%    integer PROB, the problem index.
%
%    integer INT_NUM, the number of sample points.
%
%  Output:
%
%    real RESULT, the approximate integral.
%
  [ a, b ] = p00_lim ( prob );

  key = 1;
  base = 2;
  x = i4_to_halton_number_sequence ( key, base, int_num );
  x(1:int_num) = a + ( b - a ) * x(1:int_num);

  result = ( b - a ) * sum ( p00_fun ( prob, int_num, x(1:int_num) ) ) / int_num;

  return
end
