function simplex_gm_rule_test01 ( )

%*****************************************************************************80
%
%% simplex_gm_rule_test01() tests simplex_unit_to_general().
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    02 March 2008
%
%  Author:
%
%    John Burkardt
%
  m = 2;
  vertex_num = m + 1;
  n = 10;

  t = [
    1.0, 1.0;
    3.0, 1.0;
    2.0, 5.0 ]';

  t_unit = [
    0.0, 0.0;
    1.0, 0.0;
    0.0, 1.0 ]';

  fprintf ( 1, '\n' );
  fprintf ( 1, 'SIMPLEX_GM_RULE_TEST01():\n' );
  fprintf ( 1, '  SIMPLEX_UNIT_TO_GENERAL\n' );
  fprintf ( 1, '  maps points in the unit simplex to a general simplex.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Here we consider a simplex in 2D, a triangle.\n' );
  fprintf ( 1, '\n' );
  fprintf ( 1, '  The vertices of the general triangle are:\n' );
  fprintf ( 1, '\n' );
  for j = 1 : vertex_num
    for dim = 1 : m
      fprintf ( 1, '  %8f', t(dim,j) );
    end
    fprintf ( 1, '\n' );
  end

  fprintf ( 1, '\n' );
  fprintf ( 1, '   (  XSI     ETA )   ( X       Y  )\n' );
  fprintf ( 1, '\n' );

  phy_unit = simplex_unit_to_general ( m, m + 1, t, t_unit );

  for j = 1 : m + 1
    for dim = 1 : m
      fprintf ( 1, '  %8f', t_unit(dim,j) );
    end
    for dim = 1 : m
      fprintf ( 1, '  %8f', phy_unit(dim,j) );
    end
    fprintf ( 1, '\n' );
  end

  ref = simplex_unit_sample ( m, n );

  phy = simplex_unit_to_general ( m, n, t, ref );

  for j = 1 : n
    for dim = 1 : m
      fprintf ( 1, '  %8f', ref(dim,j) );
    end
    for dim = 1 : m
      fprintf ( 1, '  %8f', phy(dim,j) );
    end
    fprintf ( 1, '\n' );
  end

  return
end
