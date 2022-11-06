function radau_set_test ( )

%*****************************************************************************80
%
%% radau_set_test() tests radau_set().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    09 June 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'RADAU_SET_TEST\n' );
  fprintf ( 1, '  RADAU_SET sets a Radau rule.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         I            X                  W\n' );

  for n = 4 : 3 : 12

    [ x, w ] = radau_set ( n );

    fprintf ( 1, '\n' );
    for i = 1 : n
      fprintf ( 1, '  %8d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
