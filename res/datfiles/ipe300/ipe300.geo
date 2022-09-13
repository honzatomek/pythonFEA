// Gmsh project created on Tue Feb  8 11:23:47 2022

// Geometry
h = 300;
b = 150;
tf = 10.7;
tw = 7.1;

Point(1) = {-b/2, -h/2, 0, 30.};
Point(2) = {-tw/2, -h/2, 0, 30.};
Point(3) = {tw/2, -h/2, 0, 30.};
Point(4) = {b/2, -h/2, 0, 30.};
Point(5) = {b/2, -h/2+tf, 0, 30.};
Point(6) = {tw/2, -h/2+tf, 0, 30.};
Point(7) = {tw/2, h/2-tf, 0, 30.};
Point(8) = {b/2, h/2-tf, 0, 30.};
Point(9) = {b/2, h/2, 0, 30.};
Point(10) = {tw/2, h/2, 0, 30.};
Point(11) = {-tw/2, h/2, 0, 30.};
Point(12) = {-b/2, h/2, 0, 30.};
Point(13) = {-b/2, h/2-tf, 0, 30.};
Point(14) = {-tw/2, h/2-tf, 0, 30.};
Point(15) = {-tw/2, -h/2+tf, 0, 30.};
Point(16) = {-b/2, -h/2+tf, 0, 30.};

Line(1) = {1, 2};
Line(2) = {2, 3};
Line(3) = {3, 4};
Line(4) = {4, 5};
Line(5) = {5, 6};
Line(6) = {6, 7};
Line(7) = {7, 8};
Line(8) = {8, 9};
Line(9) = {9, 10};
Line(10) = {10, 11};
Line(11) = {11, 12};
Line(12) = {12, 13};
Line(13) = {13, 14};
Line(14) = {14, 15};
Line(15) = {15, 16};
Line(16) = {16, 1};
Line(17) = {2, 15};
Line(18) = {3, 6};
Line(19) = {7, 10};
Line(20) = {14, 11};
Line(21) = {6, 15};
Line(22) = {7, 14};

Curve Loop(1) = {1, 17, 15, 16};
Curve Loop(2) = {2, 18, 21, -17};
Curve Loop(3) = {3, 4, 5, -18};
Curve Loop(4) = {21, -14, -22, -6};
Curve Loop(5) = {7, 8, 9, -19};
Curve Loop(6) = {22, 20, -10, -19};
Curve Loop(7) = {13, 20, 11, 12};

Plane Surface(1) = {1};
Plane Surface(2) = {2};
Plane Surface(3) = {3};
Plane Surface(4) = {4};
Plane Surface(5) = {5};
Plane Surface(6) = {6};
Plane Surface(7) = {7};

Extrude {0, 0, 5000} {
  Surface{1};
  Surface{2};
  Surface{3};
  Surface{4};
  Surface{5};
  Surface{6};
  Surface{7};
}

Coherence;

MeshSize {74, 65, 64, 44, 58, 48, 26, 17, 54, 50, 22, 18, 32, 28, 38, 34, 12, 13, 11, 14, 10, 7, 16, 1, 9, 8, 15, 2, 6, 3, 5, 4} = 50;

Transfinite Curve {170, 161, 38, 29, 144, 100, 126, 104, 34, 30, 56, 52, 122, 118, 78, 74} = 101 Using Progression 1;
Transfinite Curve {91, 93, 14, 6} = 16 Using Progression 1;
Transfinite Curve {156, 158, 13, 11, 112, 114, 7, 9, 24, 26, 1, 15, 68, 70, 3, 5} = 6 Using Progression 1;
Transfinite Curve {25, 27, 47, 69, 4, 18, 17, 16, 113, 115, 135, 159, 8, 20, 19, 12, 46, 48, 92, 136, 2, 22, 21, 10} = 2 Using Progression 1;
Transfinite Surface {176, 154, 44, 110, 66, 132, 88, 175, 171, 163, 43, 7, 145, 149, 105, 6, 131, 39, 31, 101, 109, 127, 119, 1, 35, 61, 53, 4, 57, 2, 123, 5, 83, 75, 79, 3};

Transfinite Volume {1, 2, 3, 4, 5, 6, 7};

Physical Volume("ipe300", 1) = {7, 6, 1, 4, 5, 2, 3};
Physical Surface("endA", 1) = {110, 154, 176, 132, 44, 66, 88};
Physical Surface("endB", 2) = {2, 3, 4, 5, 6, 7};

//+
Show "*";
