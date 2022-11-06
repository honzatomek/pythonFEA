function [ n_data, z, fz ] = wright_omega_values ( n_data )

%*****************************************************************************80
%
%% wright_omega_values() returns values of the Wright Omega function.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    15 May 2016
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Robert Corless, David Jeffrey,
%    The Wright Omega Function,
%    in Artificial Intelligence, Automated Reasoning, and Symbolic Computation,
%    ed J Calmet, B Benhamou, O Caprotti, L Henocque, V Sorge,
%    Lecture Notes in Artificial Intelligence, volume 2385,
%    Springer, 2002, pages 76-89.
%
%  Input:
%
%    integer N_DATA.  The user sets N_DATA to 0 before the first call.  
%    Thereafter, it should simply be the value returned by the previous call.
%
%  Output:
%
%    integer N_DATA.  On each call, the routine increments N_DATA by 1, and
%    returns the corresponding data; when there is no more data, the
%    output value of N_DATA will be 0 again.
%
%    complex Z, the argument of the function.
%
%    complex FZ, the value of the function.
%
  n_max = 10;

  fz_vec = [ ...
    +0.5671432904097838  +0.0000000000000000 * i, ...
    +1.000000000000000   +0.0000000000000000 * i, ...
    +2.718281828459045   +0.0000000000000000 * i, ...
    -1.000000000000000   +0.0000000000000000 * i, ...
    -1.000000000000000   +0.0000000000000000 * i, ...
    -2.000000000000000   +0.0000000000000000 * i, ...
    -0.40637573995996    +0.0000000000000000 * i, ...
    +0.000000000000000   +1.0000000000000000 * i, ...
    -0.3181315052047641  +1.337235701430689 * i, ...
    +0.9372082083733697  +0.5054213160131512 * i ];

  z_vec = [ ...
     +0.000000000000000   +0.000000000000000 * i, ...
     +1.000000000000000   +0.000000000000000 * i, ...
     +3.718281828459045   +0.000000000000000 * i, ... 
     -1.000000000000000   +3.141592653589793 * i, ...
     -1.000000000000000   -3.141592653589793 * i, ...
     -1.306852819440055   +3.141592653589793 * i, ...
     -1.306852819440055   -3.141592653589793 * i, ...
     +0.000000000000000   +2.570796326794897 * i, ...
     +0.000000000000000   +3.141592653589793 * i, ...
     +1.000000000000000   +1.000000000000000 * i ];

  if ( n_data < 0 )
    n_data = 0;
  end

  n_data = n_data + 1;

  if ( n_max < n_data )
    n_data = 0;
    z = 0.0;
    fz = 0.0;
  else
    z = z_vec(n_data);
    fz = fz_vec(n_data);
  end

  return
end
