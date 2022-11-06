double ellipse_area1 ( double a[], double r );
double ellipse_area2 ( double a, double b, double c, double d );
double ellipse_area3 ( double r1, double r2 );
double ellipse_aspect_ratio ( double a, double b );
double ellipse_eccentricity ( double a, double b );
double ellipse_flattening ( double a, double b );
double ellipse_point_dist_2d ( double r1, double r2, double p[2] );
double *ellipse_point_near_2d ( double r1, double r2, double p[2] );
void ellipse_points_2d ( double pc[2], double r1, double r2, double psi, 
  int n, double p[] );
void ellipse_points_arc_2d ( double pc[2], double r1, double r2, double psi,
  double theta1, double theta2, int n, double p[] );
double r8_modp ( double x, double y );
double r8_sign ( double x );

