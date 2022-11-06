function lobatto_compute_test ( )

%*****************************************************************************80
%
%% lobatto_compute_test() tests lobatto_compute().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license. 
%
%  Modified:
%
%    23 April 2015
%
%  Author:
%
%    John Burkardt
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'LOBATTO_COMPUTE_TEST\n' );
  fprintf ( 1, '  LOBATTO_COMPUTE computes a Lobatto rule;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '         I      X             W\n' );

  for n = 4 : 3 : 12

    [ x, w ] = lobatto_compute ( n );

    fprintf ( 1, '\n' );
    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
