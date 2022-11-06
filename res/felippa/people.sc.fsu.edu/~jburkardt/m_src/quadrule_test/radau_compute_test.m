function radau_compute_test ( )

%*****************************************************************************80
%
%% radau_compute_test() tests radau_compute().
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
  fprintf ( 1, 'RADAU_COMPUTE_TEST\n' );
  fprintf ( 1, '  RADAU_COMPUTE computes a Radau rule;\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1,'         I      X            W\n' );

  for n = 4 : 3 : 12

    [ x, w ] = radau_compute ( n );

    fprintf ( 1, '\n' );
    for i = 1 : n
      fprintf ( 1, '  %2d  %24.16g  %24.16g\n', i, x(i), w(i) );
    end

  end

  return
end
