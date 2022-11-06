function [ b, p_m, p_n ] = maple_boundary ( filename )

%*****************************************************************************80
%
%% maple_boundary() extracts a boundary between black exterior and white interior.
%
%  Discussion:
%
%    If the image is not black and white, this function converts it to
%    a black and white version.  The intensity dividing level in im2bw()
%    is 0.5, but this can be adjusted to other values between 0 and 1 if
%    the initial result is not satisfactory.
%
%    It is important that the image is simple, having only one dividing
%    line between black and white.  As it turns out, this implies that the
%    image must have a black background that extends to the picture limits.
%    MATLAB will see any white pixels on the picture limits as being "next to"
%    phantom black pixels, and will generate a border there, which is
%    almost certainly not what you want.  Thus, this function can correctly
%    handle a white filled circle on a black background, but will get "confused"
%    by a black filled circle on a white background.   Moreover, a circle
%    drawn by a black line on a white background will produce THREE boundary
%    lines, only one of which will be returned by this function: the 
%    picture limits will be a boundary, and the black line of the circle
%    will generate two black/white boundaries, on the inside and on the
%    outside of the circle.  So don't do that.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    08 August 2015
%
%  Author:
%
%    John Burkardt
%
%  Input:
%
%    string FILENAME, the name of an image file to be processed.
%
%  Output:
%
%    integer B(*,2), a list of pixels that form the first black and
%    white boundary detected in the image.
%
%    integer P_M, P_N, the number of rows and columns of pixels.
%
  fprintf ( 1, '\n' );
  fprintf ( 1, 'maple_boundary():\n' );
  fprintf ( 1, '  Extract a polygonal boundary between a black exterior\n' );
  fprintf ( 1, '  and a white interior, from an image of a maple leaf.\n' );
 
  fprintf ( 1, '\n' );
  fprintf ( 1, '  Apply im=imread ( %s ) to extract image data from file\n', filename );

  im = imread ( filename );

  [ p_m, p_n, nc ] = size ( im );

  fprintf ( 1, '  size(im) = %d rows by %d columns using %d colors.\n', p_m, p_n, nc );
%
%  Convert the image to black and white, if necessary.
%
  if ( 1 < nc )

    im = im2bw ( im, 0.5 );
    fprintf ( 1, '  im=im2bw(im,0.5) converts image to black and white (1 color).\n' );

  end
%
%  We have to convert the image from B/W to W/B.
%
  im = imcomplement ( im );
  fprintf ( 1, '  im=imcomplement(im) converts image to black around white.\n' );
%
%  Actually, we can compute the EXACT relative area, by comparing the number
%  of white pixels to the total number of pixels.
%
  relative = sum ( sum ( im ) ) / p_m / p_n;
  fprintf ( 1, '  Exact relative area = %14.6f\n', relative );
%
%  Now might be a good time to display the converted image...
%
  imshow ( im );
  fprintf ( 1, '  imshow(im) displays the image.\n' );
%
%  BWBOUNDARIES traces the region boundaries in a black and white image.
%
  bcell = bwboundaries ( im );
  fprintf ( 1, '  bcell=bwboundaries(im) traces ALL the boundaries.\n' );
%
%  bcell is actually a cell array.
%  Assuming there is only one connected boundary between black and white 
%  regions, then we just need to extract the contents of the first cell
%  as a standard MATLAB array of Nx2 pixel coordinates.
%
  b = bcell{1};
  fprintf ( 1, '  b=bcell{1} extracts the first (and only, we hope) boundary.\n' );

  return
end
