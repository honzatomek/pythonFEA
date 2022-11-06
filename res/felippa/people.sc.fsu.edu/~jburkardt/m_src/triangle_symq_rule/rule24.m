function [ x, y, w ] = rule24 ( )

%*****************************************************************************80
%
%% rule24() returns the rule of degree 24.
%
%  Discussion:
%
%    Order 24 (112 pts)
%    1/6 data for 24-th order quadrature with 24 nodes.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    26 June 2014
%
%  Author:
%
%    Original FORTRAN77 version by Hong Xiao, Zydrunas Gimbutas.
%    This MATLAB version by John Burkardt.
%
%  Output:
%
%    real X(*), Y(*), the coordinates of the nodes.
%
%    real W(*), the weights.
%
  x = [ ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
     -0.34667319739036827052339346563820, ...
     -0.17228858629081128185759902607603, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
     -0.77534695461921828552255298716799, ...
     -0.11830295887885374209920762360797, ...
     -0.63345645296256978775220168640372, ...
     -0.45325106659208302305278781769339, ...
     -0.23690281718952127487435067945971, ...
     -0.34221988285107932085443071544544, ...
     -0.22606667538238729829778396624439, ...
     -0.44495329180516622777269400394083, ...
      0.00000000000000000000000000000000, ...
     -0.80452275594748579655555201318815, ...
      0.00000000000000000000000000000000, ...
     -0.54364142669910333813418299173702, ...
      0.00000000000000000000000000000000, ...
     -0.64253738555354065050641405694698, ...
      0.00000000000000000000000000000000, ...
     -0.91734214502720875776048093000479, ...
      0.00000000000000000000000000000000 ];
  y = [ ... ...
      0.00000000000000000000000000000000, ...
     -0.29638036437519615283072347461721, ...
      0.59226680488565760168026231308527, ...
     -0.28226548001723516079013789395034, ...
     -0.28331767767631024291018810948384, ...
      0.10127221547590450105942364741960E+01, ...
      0.11313827320245876948911869224423E+01, ...
     -0.51098115521172256268950056659125, ...
     -0.41686453554510700393115126619431, ...
     -0.50600932341283035759161511854566, ...
     -0.50881145127983229474141858425667, ...
     -0.51050575168543342224214849423552, ...
     -0.41361091237025662602034980078080, ...
     -0.56455388712044611888415763422386, ...
     -0.56428020831842462418864564259509, ...
     -0.56437817053789745666612468626219, ...
     -0.56481964637042001394869703493070, ...
      0.23937641179955802312096963113378, ...
     -0.41182463487840787556899445189961, ...
     -0.51019021318165292778033437868107, ...
     -0.56351478264510604651378681421503, ...
      0.82100884119022006576877128851229, ...
     -0.56336411329583398015443759191052, ...
     -0.14556014125612880230802044190807 ];
  w = [ ... ...
      0.27518427300594242408167672948018E-02, ...
      0.86272156924574954117622248939788E-02, ...
      0.68297766559224770708029514748518E-02, ...
      0.18615927197937334900149347500352E-01, ...
      0.20102296969937806928616412243917E-01, ...
      0.25227162946694301850912768013948E-02, ...
      0.40617630703098676858295809545372E-03, ...
      0.70624104072789811560052517607139E-02, ...
      0.19783032876181461967862187704075E-01, ...
      0.94811737732738431203731642290050E-02, ...
      0.11719477113696286147199954244934E-01, ...
      0.13091319693878567354029144297298E-01, ...
      0.18888756936770716280013255717068E-01, ...
      0.55829824091059095164775149600824E-02, ...
      0.53712600687036400647412622640952E-02, ...
      0.28580170714449744209873764706737E-02, ...
      0.34075951304887990048003455619043E-02, ...
      0.13502925079066951153738813940684E-01, ...
      0.15587004356588926954110113389875E-01, ...
      0.68123246685728385281460756274493E-02, ...
      0.48789848045465228958508578200054E-02, ...
      0.65983957168252596123510923456370E-02, ...
      0.23649160198439673642978641242145E-02, ...
      0.12499140851132809089730270870368E-01 ];

  return
end
