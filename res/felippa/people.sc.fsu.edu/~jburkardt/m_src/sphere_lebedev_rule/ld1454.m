function [ x, y, z, w ] = ld1454 ( )

%*****************************************************************************80
%
%% ld1454() computes the 1454 point Lebedev angular grid.
%
%  Modified:
%
%    14 September 2010
%
%  Author:
%
%    Dmitri Laikov
%
%  Reference:
%
%    Vyacheslav Lebedev, Dmitri Laikov,
%    A quadrature formula for the sphere of the 131st
%    algebraic order of accuracy,
%    Russian Academy of Sciences Doklady Mathematics,
%    Volume 59, Number 3, 1999, pages 477-481.
%
%  Output:
%
%    real X(N), Y(N), Z(N), W(N), the coordinates
%    and weights of the points.
%
  n = 0;
  x = zeros(1454,1);
  y = zeros(1454,1);
  z = zeros(1454,1);
  w = zeros(1454,1);
  a = 0.0;
  b = 0.0;
  v = 0.7777160743261247E-04;
  [ n, x, y, z, w ] = gen_oh ( 1, n, a, b, v, x, y, z, w );
  v = 0.7557646413004701E-03;
  [ n, x, y, z, w ] = gen_oh ( 3, n, a, b, v, x, y, z, w );
  a = 0.3229290663413854E-01;
  v = 0.2841633806090617E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.8036733271462222E-01;
  v = 0.4374419127053555E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.1354289960531653;
  v = 0.5417174740872172E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.1938963861114426;
  v = 0.6148000891358593E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.2537343715011275;
  v = 0.6664394485800705E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.3135251434752570;
  v = 0.7025039356923220E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.3721558339375338;
  v = 0.7268511789249627E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.4286809575195696;
  v = 0.7422637534208629E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.4822510128282994;
  v = 0.7509545035841214E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.5320679333566263;
  v = 0.7548535057718401E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6172998195394274;
  v = 0.7554088969774001E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6510679849127481;
  v = 0.7553147174442808E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6777315251687360;
  v = 0.7564767653292297E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.6963109410648741;
  v = 0.7587991808518730E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.7058935009831749;
  v = 0.7608261832033027E-03;
  [ n, x, y, z, w ] = gen_oh ( 4, n, a, b, v, x, y, z, w );
  a = 0.9955546194091857;
  v = 0.4021680447874916E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.9734115901794209;
  v = 0.5804871793945964E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.9275693732388626;
  v = 0.6792151955945159E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.8568022422795103;
  v = 0.7336741211286294E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.7623495553719372;
  v = 0.7581866300989608E-03;
  [ n, x, y, z, w ] = gen_oh ( 5, n, a, b, v, x, y, z, w );
  a = 0.5707522908892223;
  b = 0.4387028039889501;
  v = 0.7538257859800743E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5196463388403083;
  b = 0.3858908414762617;
  v = 0.7483517247053123E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4646337531215351;
  b = 0.3301937372343854;
  v = 0.7371763661112059E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4063901697557691;
  b = 0.2725423573563777;
  v = 0.7183448895756934E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3456329466643087;
  b = 0.2139510237495250;
  v = 0.6895815529822191E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.2831395121050332;
  b = 0.1555922309786647;
  v = 0.6480105801792886E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.2197682022925330;
  b = 0.9892878979686097E-01;
  v = 0.5897558896594636E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.1564696098650355;
  b = 0.4598642910675510E-01;
  v = 0.5095708849247346E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6027356673721295;
  b = 0.3376625140173426;
  v = 0.7536906428909755E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5496032320255096;
  b = 0.2822301309727988;
  v = 0.7472505965575118E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4921707755234567;
  b = 0.2248632342592540;
  v = 0.7343017132279698E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4309422998598483;
  b = 0.1666224723456479;
  v = 0.7130871582177445E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.3664108182313672;
  b = 0.1086964901822169;
  v = 0.6817022032112776E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.2990189057758436;
  b = 0.5251989784120085E-01;
  v = 0.6380941145604121E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6268724013144998;
  b = 0.2297523657550023;
  v = 0.7550381377920310E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5707324144834607;
  b = 0.1723080607093800;
  v = 0.7478646640144802E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5096360901960365;
  b = 0.1140238465390513;
  v = 0.7335918720601220E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.4438729938312456;
  b = 0.5611522095882537E-01;
  v = 0.7110120527658118E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.6419978471082389;
  b = 0.1164174423140873;
  v = 0.7571363978689501E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  a = 0.5817218061802611;
  b = 0.5797589531445219E-01;
  v = 0.7489908329079234E-03;
  [ n, x, y, z, w ] = gen_oh ( 6, n, a, b, v, x, y, z, w );
  
  return
end
