function triangle_symq_rule_test ( )

%*****************************************************************************80
%
%% triangle_symq_rule_test() tests triangle_symq_rule().
%
%  Licensing:
%
%    This code is distributed under the GNU GPL license.
%
%  Modified:
%
%    07 April 2019
%
%  Author:
%
%    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
%    This MATLAB version by John Burkardt.
%
%  Reference:
%
%    Hong Xiao, Zydrunas Gimbutas,
%    A numerical algorithm for the construction of efficient quadrature
%    rules in two and higher dimensions,
%    Computers and Mathematics with Applications,
%    Volume 59, 2010, pages 663-676.
%
  addpath ( '../triangle_symq_rule' );

  timestamp ( );
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_symq_rule_test()\n' );
  fprintf ( 1, '  MATLAB/Octave version %s\n', version ( ) );
  fprintf ( 1, '  Test triangle_symq_rule()\n' );

  triangle_symq_rule_test01 ( )

  for itype = 0 : 2

    if ( itype == 0 )

      fprintf ( 1, '\n' );
      fprintf ( 1, '  Region is user-defined triangle.\n' );
      vert1(1) = 1.0;
      vert1(2) = 0.0;
      vert2(1) = 4.0;
      vert2(2) = 4.0;
      vert3(1) = 0.0;
      vert3(2) = 3.0;
      header = 'user08';
      degree = 8;

    elseif ( itype == 1 )

      fprintf ( 1, '\n' );
      fprintf ( 1, '  Region is standard equilateral triangle.\n' );
      vert1(1) = -1.0;
      vert1(2) = -1.0 / sqrt ( 3.0 );
      vert2(1) = +1.0;
      vert2(2) = -1.0 / sqrt ( 3.0 );
      vert3(1) =  0.0;
      vert3(2) =  2.0 / sqrt ( 3.0 );
      header = 'equi08';
      degree = 8;

    elseif ( itype == 2 )

      fprintf ( 1, '\n' );
      fprintf ( 1, '  Region is the simplex (0,0),(1,0),(0,1).\n' );
      vert1(1) = 0.0;
      vert1(2) = 0.0;
      vert2(1) = 1.0;
      vert2(2) = 0.0;
      vert3(1) = 0.0;
      vert3(2) = 1.0;
      header = 'simp08';
      degree = 8;

    end

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Triangle:\n' );
    fprintf ( 1, '\n' );
    fprintf ( 1, '  %10.4f  %10.4f\n', vert1(1:2) );
    fprintf ( 1, '  %10.4f  %10.4f\n', vert2(1:2) );
    fprintf ( 1, '  %10.4f  %10.4f\n', vert3(1:2) );
%
%  Determine the size of the rule.
%
    numnodes = rule_full_size ( degree );
%
%  Retrieve a rule and print it.
%
    triangle_symq_rule_test02 ( degree, numnodes, vert1, vert2, vert3 );
%
%  Get a rule, and write data files that gnuplot can use to plot the points.
%
    triangle_symq_rule_test03 ( degree, numnodes, vert1, vert2, vert3, header );

    triangle_symq_rule_test04 ( degree, numnodes, vert1, vert2, vert3, header );

    triangle_symq_rule_test05 ( degree, numnodes, vert1, vert2, vert3 );

  end
%
%  Terminate.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'triangle_symq_rule_test():\n' );
  fprintf ( 1, '  Normal end of execution.\n' );
  fprintf ( 1, '\n' );
  timestamp ( )

  rmpath ( '../triangle_symq_rule' );

  return
end
function timestamp ( )

%*****************************************************************************80
%
%% timestamp() prints the current YMDHMS date as a timestamp.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    14 February 2003
%
%  Author:
%
%    John Burkardt
%
  t = now;
  c = datevec ( t );
  s = datestr ( c, 0 );
  fprintf ( 1, '%s\n', s );

  return
end

