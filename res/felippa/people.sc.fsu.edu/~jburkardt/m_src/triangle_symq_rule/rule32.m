function [ x, y, w ] = rule32 ( )

%*****************************************************************************80
%
%% rule32() returns the rule of degree 32.
%
%  Discussion:
%
%    Order 32 (193 pts)
%    1/6 data for 32-th order quadrature with 39 nodes.
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
     -0.42746868613914916208067002398662, ...
     -0.91710704502811275874385926715291E-01, ...
      0.00000000000000000000000000000000, ...
     -0.48363645746700960769905958976308, ...
     -0.74677040622149170601230763257760, ...
     -0.33090034155997483681589019846520, ...
     -0.46023597931812041521733845612288, ...
     -0.55152849427095291026768125922720, ...
     -0.20577133469764029385252414280604, ...
     -0.20092303305428395370178245682808, ...
      0.00000000000000000000000000000000, ...
     -0.25879477956175550900580265426342, ...
      0.00000000000000000000000000000000, ...
     -0.72262398109805332027279578058226, ...
     -0.32997225542915476024532745217312, ...
     -0.40104907184189418029309583833701, ...
      0.00000000000000000000000000000000, ...
     -0.69151287469732085458587108553859, ...
     -0.81882407794345790149465089749751, ...
      0.00000000000000000000000000000000, ...
     -0.15979469018140228096803155997403, ...
      0.00000000000000000000000000000000, ...
     -0.59877821961446415276093759190966, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
     -0.42015173485047987871656798146767, ...
     -0.28990563943088228305314863137265, ...
     -0.84085334890089953008631910023907, ...
     -0.12723619924623591749401870286216, ...
     -0.12551628141173309089569225449278, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
     -0.55077303550427213809118028350091, ...
     -0.61718975394462310684988723266656, ...
     -0.90920085094674871327008780126851, ...
      0.00000000000000000000000000000000, ...
      0.00000000000000000000000000000000, ...
     -0.97068830122109422411832138489096 ];
  y = [ ... ...
      0.11496517991990804340276319918613E+01, ...
     -0.30218292328705052033455030789710, ...
     -0.55177404065234470341871573116633, ...
     -0.57267682745711810079098827470171, ...
     -0.48826894371900250566656288607256, ...
     -0.54347180509529604896284843807779, ...
     -0.55549427045965430240427224834972, ...
     -0.54086954371044273649778573751227, ...
     -0.37375893906115265347255148781801, ...
     -0.52660317145013860310284351646008, ...
     -0.57138712716648846304698735307588, ...
     -0.51211470939471719438254705852599, ...
     -0.42082435075243723269873387979224, ...
     -0.41206328125638506041335006800986, ...
     -0.48953938794088206965231133840212, ...
     -0.49430799576545804368134930045435, ...
     -0.57513903374312331695098163467832, ...
     -0.14894052019732994524801638469381, ...
     -0.56996729278416821373273663004497, ...
     -0.57067752268839624045481729672027, ...
      0.17429645803883591285042159190117, ...
     -0.22764895370246828181434158209954, ...
     -0.27832827065225098989964142553377, ...
     -0.46007242792992039354306838595294, ...
      0.10498449848071351612896202347394E+01, ...
      0.11103247160622636896577670049089E+01, ...
     -0.41181398695180711307526515295114, ...
     -0.31690029177033716996969306425727, ...
     -0.53517509064720514075419136628404, ...
     -0.35324390902713134915617189643158, ...
     -0.46783716452744596679002275357204, ...
      0.00000000000000000000000000000000, ...
      0.93988934616034742698090141489668, ...
     -0.56822819321283957200082177635155, ...
     -0.53204175653167234191507273763322, ...
     -0.56772912901378600226993106149821, ...
      0.36045825890517508775557027645869, ...
      0.80150480143647657259482305879194, ...
     -0.57627103176029262025065680355071 ];
  w = [ ... ...
      0.35089101065112855884820534496463E-04, ...
      0.10988190324830709992823329341192E-01, ...
      0.40017234188803658556418944416512E-02, ...
      0.93087707933178618554865620359439E-03, ...
      0.69568246166267704238682724201103E-02, ...
      0.32813277551329795738555421297416E-02, ...
      0.39753082327573825644781696272502E-02, ...
      0.47960047753629003693425154224810E-02, ...
      0.98456677416312611477210982996463E-02, ...
      0.60120289449877842202024432440538E-02, ...
      0.22178527178401095681968551311850E-02, ...
      0.36718564604057522445250408427746E-02, ...
      0.10803923534388294394797611307667E-01, ...
      0.55134227454329365414727793248372E-02, ...
      0.58771612493962009794350932546429E-02, ...
      0.81633895836356217774190986558429E-02, ...
      0.11698483763495028290383571359353E-02, ...
      0.83316003706962053904317473673468E-02, ...
      0.19546717234154615848859112275635E-02, ...
      0.14644039054791717956440739265680E-02, ...
      0.84773151751406951264947448004187E-02, ...
      0.15757826762525756648322149683424E-01, ...
      0.71597492934303095837773594696045E-02, ...
      0.77415633062502329911450279233184E-02, ...
      0.12234738959380913656810149160128E-02, ...
      0.52757528161206686084277006574669E-03, ...
      0.11119171725536887499648558701843E-01, ...
      0.13439031765939374980048747865869E-01, ...
      0.32769705348614632852685205970683E-02, ...
      0.13014005319508587872203236381061E-01, ...
      0.97509412664434843717560320628702E-02, ...
      0.28580896374081882633069151128591E-02, ...
      0.26165845261107117861016768586572E-02, ...
      0.25605447042338306730739080320977E-02, ...
      0.56075763180908099390771878498063E-02, ...
      0.13889507817898641123545037024783E-02, ...
      0.82488872017764297885488574625852E-02, ...
      0.43623899484098009158061336724647E-02, ...
      0.22384872276251377786582863606787E-03 ];

  return
end