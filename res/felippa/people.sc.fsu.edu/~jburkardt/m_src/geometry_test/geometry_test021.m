function geometry_test021 ( )

%*****************************************************************************80
%
%% geometry_test021() tests direction_pert_3d().
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
  dim_num = 3;
  ntest = 3;

  vbase(1:dim_num) =  [ 1.0,  0.0, 0.0 ];
  sigma(1:ntest) = [ 0.99, 0.5, 0.1 ];

  fprintf ( 1, '\n' );
  fprintf ( 1, 'geometry_test021():\n' );
  fprintf ( 1, '  direction_pert_3d() perturbs a direction vector.\n' );

  r8vec_print ( dim_num, vbase, '  The base vector:' );

  for itest = 1 : ntest

    fprintf ( 1, '\n' );
    fprintf ( 1, '  Using Sigma = %f\n', sigma(itest) );
    fprintf ( 1, '\n' );

    for i = 1 : 20
      vran = direction_pert_3d ( sigma(itest), vbase );
      fprintf ( 1, '  %10f  %10f  %10f\n', vran(1:dim_num) );
    end

  end

  return
end
