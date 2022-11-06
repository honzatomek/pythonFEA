function estimate = maple_area_estimate_qmc ( b, p_m, p_n, sample_num )

%*****************************************************************************80
%
%% maple_area_estimate_qmc() estimates area using a Quasi Monte Carlo sample.
%
%  Discussion:
%
%    As a set of MATLAB coordinates, the leaf lies inside the box
%    [0,P_M] x [0,P_N].
%
%    Generate SAMPLE_NUM Hammersley points (X,Y) within this region.
%
%    For each point (X,Y), determine if it is inside or outside the polygon.
%
%    The percentage of inside points gives the relative area of the leaf.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    20 August 2016
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    integer B(*,2), a list of pixels that form the boundary.
%
%    integer P_M, P_N, the number of rows and columns of pixels.
%
%    integer SAMPLE_NUM, the number of sample points.
%
%  Output:
%
%    real ESTIMATE, the relative area estimate.
%
  r = hammersley_sequence ( 0, sample_num - 1, 2, sample_num - 1 );

  x = p_m * r(1,:);
  y = p_n * r(2,:);
  inside = inpolygon ( x, y, b(:,1), b(:,2) ); 
  estimate = sum ( inside ) / sample_num;

  return
end
function r = hammersley_sequence ( i1, i2, m, n )

%*****************************************************************************80
%
%% hammersley_sequence() computes elements I1 through I2 of a Hammersley sequence.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    18 August 2016
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    John Hammersley,
%    On the efficiency of certain quasi-random sequences of points
%    in evaluating multi-dimensional integrals,
%    Numerische Mathematik,
%    Volume 2, pages 84-90, 1960.
%
%  Input:
%
%    integer I1, I2, the indices of the first and last elements
%    of the sequence.  0 <= I1, I2.
%
%    integer M, the spatial dimension.
%    1 <= M <= 100.
%
%    integer N, the "base" for the first component.
%    1 <= N.
%
%  Output:
%
%    real R(M,abs(I1-I2)+1), the elements of the sequence with 
%    indices I1 through I2.
%
 prime = [ ...
        2;    3;    5;    7;   11;   13;   17;   19;   23;   29; ...
       31;   37;   41;   43;   47;   53;   59;   61;   67;   71; ...
       73;   79;   83;   89;   97;  101;  103;  107;  109;  113; ...
      127;  131;  137;  139;  149;  151;  157;  163;  167;  173; ...
      179;  181;  191;  193;  197;  199;  211;  223;  227;  229; ...
      233;  239;  241;  251;  257;  263;  269;  271;  277;  281; ...
      283;  293;  307;  311;  313;  317;  331;  337;  347;  349; ...
      353;  359;  367;  373;  379;  383;  389;  397;  401;  409; ...
      419;  421;  431;  433;  439;  443;  449;  457;  461;  463; ...
      467;  479;  487;  491;  499;  503;  509;  521;  523;  541 ];

  if ( i1 <= i2 )
    i3 = +1;
  else
    i3 = -1;
  end

  l = abs ( i2 - i1 ) + 1;
  r = zeros ( m, l );
  k = 0;

  for i = i1 : i3 : i2

    t(1:m-1,1) = i;
%
%  Carry out the computation.
%
    prime_inv = zeros ( m - 1, 1 );
    for i4 = 1 : m - 1
      prime_inv(i4,1) = 1.0 / prime(i4,1);
    end

    k = k + 1;

    r(1,k) = mod ( i, n + 1 ) / n;

    while ( any ( t ~= 0 ) )
      for j = 1 : m - 1
        d = mod ( t(j), prime(j) );
        r(j+1,k) = r(j+1,k) + d * prime_inv(j);
        prime_inv(j) = prime_inv(j) / prime(j);
        t(j) = floor ( t(j) / prime(j) );
      end
    end

  end

  return
end

