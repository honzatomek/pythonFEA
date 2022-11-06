function [ x, y, w ] = rule30 ( )

%*****************************************************************************80
%
%% rule30() returns the rule of degree 30.
%
%  Discussion:
%
%    Order 30 (171 pts)
%    1/6 data for 30-th order quadrature with 34 nodes.
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
      -0.43405710781709278714048879994740, ...
       0.00000000000000000000000000000000, ...
      -0.13719609648588667050981701694674, ...
      -0.23002603213365866154430597150794, ...
       0.00000000000000000000000000000000, ...
      -0.35671356772393844272156480720059, ...
       0.00000000000000000000000000000000, ...
      -0.49401409080195115810253005052280, ...
      -0.37062552685345952318116499093046, ...
      -0.51430581999606514802110718905564, ...
       0.00000000000000000000000000000000, ...
       0.00000000000000000000000000000000, ...
      -0.54607380091083807654188300887860, ...
      -0.84275872174416357830668461312136, ...
       0.00000000000000000000000000000000, ...
      -0.57724024570193215242188127722161, ...
      -0.39821033011703771207712265483060, ...
      -0.30605985322806922570625655304903, ...
      -0.68776182141386689567302952570524, ...
      -0.19684758015531914348305332007873, ...
      -0.96321844715106605579144337343699E-01, ...
      -0.28286197991210025994286433769979, ...
       0.00000000000000000000000000000000, ...
      -0.10333424594611078538852107076057, ...
      -0.64379410387887037331359309442254, ...
       0.00000000000000000000000000000000, ...
      -0.89873358431890446012484882993312, ...
      -0.78478618004244445633333163168023, ...
      -0.72349108151471496801951044440950, ...
       0.00000000000000000000000000000000, ...
       0.00000000000000000000000000000000, ...
      -0.20239462350406048608674549751121, ...
       0.00000000000000000000000000000000, ...
      -0.94049820614395764193267866092540 ];
  y = [ ... ...
      -0.49449740552349402097090260436227, ...
       0.11432041379660233658011039218632E+01, ...
      -0.43937592123719222447451521408216, ...
      -0.47742236692181668651784560731673, ...
       0.90399516562083738727059247641795, ...
      -0.44352953384065130754596083984676, ...
       0.99134074534282262769308436316314, ...
      -0.53793159129250810586076585776398, ...
      -0.36290163137317815520540870562751, ...
      -0.37663681425566764191708385069014, ...
      -0.46660354209881582130470470379699, ...
       0.11107528506358136609304671750972E+01, ...
      -0.46213867523774957532303180078394, ...
      -0.56966207432086504530851133019367, ...
       0.73349519835786388112994533410054, ...
      -0.56927271273635792416671462689250, ...
      -0.56920325339091635438895039029322, ...
      -0.53373365021893670540255969319794, ...
      -0.46342190137736602703775671709799, ...
      -0.35889692861698105392115484048066, ...
      -0.23982388127083061470496201687854, ...
      -0.24652609751239687685958293412409, ...
       0.52281528171007302347717037989158, ...
      -0.52966101087647732119998666908062, ...
      -0.52874213045823877050213253058208, ...
      -0.10029941117116486849674700598946, ...
      -0.54980647624872101250977315077756, ...
      -0.53007527494669219480580008827439, ...
      -0.56749280180954759083517414571717, ...
      -0.35822245431797926788870429663662, ...
       0.21098212229083163080456906246771, ...
      -0.56840882187053609024013730511891, ...
      -0.56806418709095800771552792081267, ...
      -0.57642585867286344650886243834268 ];
  w = [ ... ...
       0.55469340852267197383357734501536E-02, ...
       0.11300463022066637667549395901160E-03, ...
       0.77976368535667454572847114089957E-02, ...
       0.78230337119887396150629609702565E-02, ...
       0.25018123919483940113671864156512E-02, ...
       0.90508836547186347263240388076065E-02, ...
       0.18717426158053290329737464178677E-02, ...
       0.53860315402766333154068593578253E-02, ...
       0.11677070021275261169640320523198E-01, ...
       0.99157266127682177040571302280702E-02, ...
       0.51222447690487278622934837026924E-02, ...
       0.55706132346425325140873159634701E-03, ...
       0.89545764756588636881737594179016E-02, ...
       0.15881019953772277632781829119128E-02, ...
       0.48608613509509536172532545379762E-02, ...
       0.25781213160755392530469248853581E-02, ...
       0.29933391441706776190700124360794E-02, ...
       0.70603223137458408522017573340119E-02, ...
       0.77254463220316942248431581666276E-02, ...
       0.14875615552650235020592865135842E-01, ...
       0.18841554220226669798776264579558E-01, ...
       0.17011603553515205522964322169417E-01, ...
       0.70757738603786203845878772981467E-02, ...
       0.82670460199627466892622746876773E-02, ...
       0.61880970884828196145627428599983E-02, ...
       0.10262805248776557218482365083790E-01, ...
       0.24090825302303188401346360674076E-02, ...
       0.50298776686404106640379168657878E-02, ...
       0.25267049096575867286260646250422E-02, ...
       0.81629221043416840383932440429541E-02, ...
       0.98143263368157515388390143296277E-02, ...
       0.34778149711369936288944433476055E-02, ...
       0.18367906196371196197648328999357E-02, ...
       0.44170301264357154992013311250048E-03 ];

  return
end
