// Gmsh project created on Sun Sep 18 17:05:37 2022
SetFactory("OpenCASCADE");
//+
SetFactory("Built-in");
//+
SetFactory("Built-in");
//+
SetFactory("Built-in");
//+
SetFactory("OpenCASCADE");
//+
SetFactory("OpenCASCADE");
//+
SetFactory("Built-in");
//+
SetFactory("Built-in");
//+
Point(1) = {0, 0, 0, 100.0};
//+
Point(2) = {500, 0, 0, 100.0};
//+
Point(3) = {500, 500, 0, 100.0};
//+
Point(4) = {0, 500, 0, 100.0};
//+
Line(1) = {1, 2};
//+
Line(2) = {2, 3};
//+
Line(3) = {3, 4};
//+
Line(4) = {4, 1};
//+
Curve Loop(1) = {4, 1, 2, 3};
//+
Plane Surface(1) = {1};
//+
Compound Surface {1};
//+
Compound Surface {1};
