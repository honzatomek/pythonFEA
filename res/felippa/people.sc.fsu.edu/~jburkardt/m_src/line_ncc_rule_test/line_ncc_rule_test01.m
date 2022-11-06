function line_ncc_rule_test01 ( )

%*****************************************************************************80
%
%% line_ncc_rule_test01() computes and prints NCC rules.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    10 April 2014
%
%  Author:
%
%    John Burkardt
%
  a = -1.0;
  b = +1.0;

  fprintf ( 1, '\n' );
  fprintf ( 1, 'line_ncc_rule_test01():\n' );
  fprintf ( 1, '  line_ncc_rule() computes the closed Newton-Cotes rule\n' );
  fprintf ( 1, '  using N equally spaced points for an interval [A,B].\n' );

  for n = 1 : 12

    [ x, w ] = line_ncc_rule ( n, a, b );
    fprintf ( 1, '\n' );
    fprintf ( 1, '  Closed Newton-Cotes Rule #%d\n', n );
    fprintf ( 1, '   I       X(I)            W(I)\n' );
    fprintf ( 1, '\n' );
    for i = 1 : n
      fprintf ( 1, '  %2d  %14.6g  %14.6g\n', i, x(i), w(i) );
    end
    fprintf ( 1, '        Sum(|W)|) =  %14.6g\n', sum ( abs ( w(1:n) ) ) );

  end

  return
end
