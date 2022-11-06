function [ x, y ] = maple_boundary ( )

%*****************************************************************************80
%
%% maple_boundary() returns a list of points that outline a maple leaf.
%
%  Discussion:
%
%    The boundary points are given in counter clockwise order.  The
%    points satisfy the following range limits:
%
%       1 <= X <= 380
%      16 <= Y <= 378
%
%    The first point is repeated as the last point, so that plot(x,y) will
%    draw a closed curve.
%
%  Licensing:
%
%    This code is distributed under the GNU LGPL license.
%
%  Modified:
%
%    12 October 2017
%
%  Author:
%
%    John Burkardt
%
%  Reference:
%
%    Brian Hayes,
%    Quasirandom Ramblings,
%    American Scientist,
%    Volume 99, Number 4, July-August 2011, pages 282-287.
%
%  Output:
%
%    integer X(1776), Y(1776), the coordinates of points along the
%    boundary of the maple leaf.
%
x = [ ...
 288, 289, 288, 287, 287, 286, 285, 284, 284, 283, 282, 281, 280, 279, 279, 279, 278, 278, 277, 276,...
 276, 276, 276, 276, 275, 275, 275, 274, 273, 273, 273, 272, 272, 272, 271, 271, 270, 270, 270, 269,...
 268, 268, 268, 267, 267, 267, 266, 265, 265, 265, 265, 264, 263, 262, 262, 262, 261, 260, 260, 260,...
 259, 259, 258, 257, 257, 257, 257, 256, 255, 255, 254, 255, 256, 257, 258, 259, 259, 260, 261, 262,...
 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282,...
 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302,...
 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321, 322,...
 323, 324, 325, 326, 327, 328, 329, 330, 331, 332, 333, 334, 335, 335, 336, 337, 338, 339, 339, 339,...
 338, 337, 336, 336, 335, 334, 334, 333, 333, 333, 333, 334, 334, 335, 336, 336, 337, 338, 338, 338,...
 339, 340, 341, 341, 342, 343, 344, 345, 346, 347, 348, 349, 349, 350, 351, 352, 352, 352, 352, 352,...
 352, 353, 354, 354, 354, 355, 355, 356, 357, 358, 358, 359, 360, 361, 362, 363, 363, 364, 365, 366,...
 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376, 376, 377, 378, 379, 380, 380, 379, 378, 377,...
 376, 375, 374, 373, 372, 371, 370, 369, 368, 367, 366, 365, 364, 363, 363, 362, 361, 360, 359, 358,...
 358, 357, 356, 355, 355, 354, 353, 352, 351, 350, 349, 349, 349, 348, 347, 347, 347, 346, 346, 346,...
 346, 346, 346, 346, 346, 346, 347, 347, 348, 349, 348, 347, 346, 345, 344, 343, 342, 341, 341, 340,...
 339, 338, 337, 336, 335, 334, 333, 332, 331, 330, 329, 328, 327, 326, 325, 324, 323, 322, 321, 320,...
 319, 318, 317, 316, 315, 314, 313, 312, 311, 310, 309, 308, 307, 306, 305, 304, 303, 302, 301, 300,...
 299, 298, 297, 296, 295, 294, 293, 292, 291, 290, 289, 288, 287, 286, 285, 284, 283, 282, 281, 280,...
 279, 278, 277, 276, 275, 274, 273, 272, 271, 270, 269, 268, 269, 270, 270, 270, 271, 272, 273, 273,...
 274, 275, 275, 276, 277, 278, 279, 279, 279, 280, 281, 282, 283, 284, 284, 285, 286, 287, 287, 288,...
 289, 289, 290, 290, 291, 292, 292, 293, 294, 294, 295, 296, 297, 297, 298, 299, 300, 300, 301, 302,...
 303, 303, 304, 305, 306, 307, 308, 308, 309, 310, 311, 312, 313, 314, 315, 316, 317, 318, 319, 320,...
 320, 321, 320, 319, 318, 317, 316, 315, 314, 313, 312, 311, 311, 310, 309, 308, 308, 308, 308, 307,...
 306, 306, 306, 305, 305, 305, 305, 305, 305, 305, 305, 305, 305, 305, 306, 306, 306, 306, 306, 306,...
 306, 306, 306, 307, 308, 308, 308, 308, 308, 308, 308, 308, 308, 308, 308, 308, 308, 309, 309, 309,...
 309, 309, 309, 309, 309, 310, 311, 311, 311, 312, 313, 313, 314, 314, 314, 315, 316, 317, 317, 318,...
 319, 319, 320, 319, 318, 317, 316, 315, 314, 313, 313, 312, 311, 310, 310, 309, 308, 307, 306, 305,...
 304, 303, 302, 302, 301, 300, 299, 298, 297, 296, 295, 294, 293, 292, 291, 290, 289, 289, 289, 288,...
 287, 286, 286, 285, 284, 283, 282, 281, 280, 279, 278, 277, 276, 275, 274, 273, 272, 271, 270, 269,...
 268, 268, 267, 266, 265, 264, 263, 262, 262, 261, 260, 260, 260, 259, 259, 259, 259, 259, 259, 259,...
 260, 260, 260, 259, 258, 258, 257, 256, 256, 256, 255, 254, 254, 253, 253, 253, 252, 251, 250, 250,...
 249, 248, 248, 247, 246, 245, 244, 243, 243, 242, 241, 240, 239, 239, 238, 237, 236, 235, 234, 233,...
 232, 231, 230, 229, 228, 227, 226, 225, 224, 223, 222, 221, 220, 220, 219, 218, 217, 216, 215, 215,...
 214, 213, 212, 212, 211, 210, 209, 209, 208, 207, 207, 207, 206, 205, 204, 203, 202, 201, 201, 200,...
 199, 198, 197, 196, 195, 194, 193, 192, 191, 191, 190, 189, 188, 187, 186, 186, 186, 186, 186, 185,...
 185, 185, 184, 183, 183, 183, 182, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181, 181,...
 181, 181, 181, 181, 181, 181, 181, 181, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180,...
 180, 180, 180, 179, 178, 178, 178, 179, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180, 180,...
 180, 180, 180, 180, 180, 180, 180, 181, 181, 181, 181, 181, 180, 179, 178, 177, 176, 175, 174, 173,...
 172, 171, 170, 169, 168, 167, 166, 165, 164, 163, 163, 163, 162, 161, 161, 161, 160, 159, 159, 158,...
 158, 158, 157, 156, 156, 156, 155, 155, 154, 153, 153, 153, 152, 152, 151, 150, 150, 150, 150, 149,...
 148, 148, 147, 147, 147, 147, 146, 145, 145, 145, 145, 145, 144, 144, 144, 144, 144, 143, 142, 142,...
 142, 142, 141, 141, 141, 141, 142, 142, 142, 142, 142, 142, 141, 141, 141, 141, 141, 141, 141, 141,...
 141, 141, 140, 139, 139, 138, 137, 137, 137, 136, 136, 135, 134, 133, 133, 132, 131, 130, 130, 130,...
 129, 128, 127, 127, 126, 125, 124, 123, 122, 121, 120, 119, 118, 117, 117, 116, 115, 114, 113, 112,...
 111, 110, 109, 108, 107, 106, 105, 104, 103, 102, 101, 101, 101, 101, 101, 100, 100, 100, 100, 100,...
 99, 99, 98, 98, 98, 98, 98, 99, 99, 99, 99, 99, 99, 98, 98, 98, 98, 98, 98, 98,...
 98, 97, 96, 96, 96, 96, 96, 96, 96, 96, 96, 95, 95, 95, 95, 95, 95, 95, 95, 95,...
 95, 95, 94, 93, 93, 93, 93, 93, 93, 92, 92, 92, 92, 92, 92, 92, 91, 90, 90, 90,...
 90, 90, 89, 89, 89, 89, 88, 87, 87, 87, 87, 86, 85, 85, 85, 84, 84, 83, 82, 81,...
 80, 79, 79, 78, 77, 76, 75, 74, 74, 73, 72, 71, 70, 69, 68, 67, 66, 65, 64, 63,...
 62, 61, 60, 59, 58, 58, 57, 56, 55, 55, 54, 53, 52, 51, 50, 49, 49, 48, 47, 46,...
 45, 44, 43, 42, 41, 41, 40, 39, 38, 37, 36, 36, 35, 34, 33, 32, 31, 30, 29, 28,...
 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 17, 16, 15, 15, 14, 14, 13, 12, 11,...
 11, 11, 10, 9, 9, 9, 8, 8, 7, 7, 8, 9, 8, 8, 8, 8, 8, 8, 8, 9,...
 10, 10, 10, 10, 11, 11, 11, 11, 11, 12, 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,...
 13, 13, 13, 13, 13, 12, 11, 11, 10, 10, 10, 10, 10, 10, 11, 11, 11, 12, 13, 13,...
 13, 14, 14, 14, 15, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 17, 16, 16, 16, 15,...
 14, 14, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 1, 1, 2, 3,...
 4, 5, 5, 6, 7, 8, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,...
 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 30, 30, 31, 32, 32, 33, 34, 35, 35, 35,...
 36, 36, 37, 38, 38, 39, 40, 40, 41, 41, 42, 43, 43, 44, 45, 46, 46, 46, 46, 47,...
 48, 49, 49, 49, 50, 51, 51, 52, 52, 52, 53, 54, 54, 54, 54, 55, 55, 55, 55, 55,...
 55, 54, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42, 41, 40, 39, 38, 37,...
 36, 35, 34, 33, 32, 31, 30, 29, 28, 27, 27, 26, 25, 25, 24, 24, 24, 25, 26, 27,...
 27, 28, 29, 30, 30, 31, 32, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 43,...
 44, 45, 46, 47, 48, 49, 49, 50, 51, 52, 53, 54, 54, 55, 56, 57, 58, 59, 60, 61,...
 62, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 73, 74, 75, 76, 77, 78, 79,...
 79, 80, 81, 82, 82, 83, 84, 84, 85, 86, 87, 87, 88, 89, 90, 90, 91, 92, 93, 93,...
 94, 95, 95, 96, 97, 98, 98, 99, 100, 101, 102, 103, 104, 105, 106, 106, 105, 104, 104, 104,...
 103, 103, 103, 102, 101, 101, 101, 100, 100, 100, 99, 98, 98, 98, 97, 96, 96, 96, 95, 95,...
 95, 95, 94, 93, 93, 93, 93, 93, 92, 92, 92, 91, 90, 90, 90, 90, 90, 89, 89, 89,...
 88, 87, 87, 87, 87, 86, 85, 85, 85, 85, 84, 84, 84, 83, 82, 82, 82, 81, 81, 81,...
 80, 79, 79, 79, 78, 77, 77, 76, 76, 76, 76, 76, 75, 74, 74, 74, 74, 75, 76, 76,...
 77, 77, 78, 79, 79, 79, 80, 80, 80, 81, 82, 82, 82, 83, 84, 84, 84, 85, 85, 85,...
 85, 85, 86, 87, 87, 88, 88, 88, 88, 88, 88, 89, 90, 90, 90, 91, 91, 91, 92, 93,...
 93, 93, 93, 93, 94, 95, 95, 95, 95, 96, 96, 96, 96, 97, 98, 98, 98, 99, 99, 99,...
 99, 100, 101, 101, 101, 102, 103, 103, 103, 104, 104, 104, 104, 105, 106, 107, 107, 108, 109, 110,...
 111, 112, 113, 114, 114, 115, 116, 117, 118, 119, 120, 121, 122, 122, 123, 124, 125, 126, 127, 128,...
 128, 129, 130, 131, 132, 133, 133, 134, 135, 136, 137, 138, 139, 139, 140, 141, 142, 142, 143, 144,...
 145, 145, 146, 147, 148, 149, 149, 150, 151, 152, 152, 153, 153, 153, 154, 155, 155, 156, 157, 158,...
 158, 159, 160, 160, 160, 160, 161, 162, 163, 164, 164, 164, 164, 164, 165, 166, 166, 166, 167, 168,...
 169, 170, 170, 171, 172, 173, 174, 175, 176, 177, 177, 178, 179, 180, 181, 182, 183, 184, 185, 185,...
 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 201, 202, 203, 204,...
 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 215, 216, 217, 218, 218, 219, 220, 221, 222,...
 223, 224, 225, 226, 226, 227, 227, 228, 229, 229, 230, 230, 230, 231, 232, 233, 234, 235, 236, 237,...
 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 250, 251, 252, 253, 253, 254, 255,...
 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275,...
 275, 276, 277, 278, 279, 280, 280, 281, 282, 283, 284, 285, 285, 286, 287, 288 ];

y = [ ...
 16, 17, 17, 18, 19, 19, 20, 21, 22, 23, 24, 25, 25, 26, 27, 28, 29, 30, 30, 31,...
 32, 33, 34, 35, 36, 37, 38, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 49,...
 50, 51, 52, 53, 54, 55, 55, 56, 57, 58, 59, 60, 60, 61, 62, 63, 63, 64, 65, 66,...
 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 78, 79, 79, 79, 80, 81, 82, 83,...
 84, 84, 84, 85, 86, 86, 86, 86, 86, 86, 86, 87, 87, 87, 87, 87, 87, 87, 87, 87,...
 86, 86, 86, 86, 86, 86, 86, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87, 87,...
 88, 88, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89,...
 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 89, 88, 87, 87, 87, 87, 87, 88, 89,...
 90, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 105, 106, 107,...
 108, 109, 109, 110, 111, 111, 112, 112, 113, 114, 114, 114, 115, 116, 116, 116, 117, 118, 119, 120,...
 121, 122, 122, 123, 124, 125, 126, 127, 127, 128, 129, 130, 130, 130, 131, 131, 132, 133, 133, 133,...
 134, 135, 135, 135, 135, 135, 136, 136, 136, 136, 136, 137, 138, 138, 138, 138, 139, 139, 139, 139,...
 139, 139, 139, 139, 139, 140, 140, 141, 141, 141, 142, 142, 142, 143, 144, 144, 145, 145, 145, 146,...
 147, 148, 149, 150, 151, 152, 153, 153, 154, 155, 156, 157, 158, 158, 159, 160, 161, 162, 163, 164,...
 165, 166, 167, 168, 169, 170, 171, 172, 173, 173, 174, 174, 174, 174, 174, 174, 174, 174, 173, 172,...
 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 172, 173, 174, 174,...
 174, 174, 174, 174, 174, 174, 174, 174, 174, 174, 175, 175, 175, 175, 175, 175, 175, 175, 175, 175,...
 175, 175, 175, 175, 175, 175, 175, 176, 177, 177, 177, 177, 177, 177, 177, 177, 177, 177, 177, 177,...
 177, 178, 179, 179, 179, 179, 179, 180, 180, 181, 182, 183, 184, 184, 185, 186, 187, 187, 188, 189,...
 190, 190, 191, 192, 193, 194, 195, 196, 197, 198, 198, 199, 200, 201, 202, 203, 203, 204, 205, 206,...
 206, 207, 208, 209, 210, 210, 211, 212, 212, 213, 214, 214, 215, 216, 217, 218, 218, 219, 220, 220,...
 221, 222, 223, 223, 224, 225, 225, 226, 227, 228, 228, 229, 229, 229, 230, 230, 231, 231, 231, 231,...
 232, 233, 234, 234, 234, 234, 235, 235, 235, 236, 237, 238, 239, 239, 239, 240, 241, 242, 243, 243,...
 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260, 261, 262, 263,...
 264, 265, 266, 267, 267, 268, 269, 270, 271, 272, 273, 274, 275, 276, 277, 278, 279, 280, 281, 282,...
 283, 284, 285, 286, 287, 288, 288, 289, 290, 291, 291, 292, 293, 294, 295, 296, 296, 297, 298, 299,...
 299, 300, 301, 300, 300, 300, 300, 299, 299, 299, 298, 297, 297, 297, 296, 295, 295, 295, 294, 294,...
 294, 294, 294, 293, 292, 292, 292, 292, 292, 292, 292, 292, 292, 293, 294, 294, 295, 296, 297, 297,...
 297, 297, 296, 295, 295, 295, 294, 294, 294, 294, 293, 292, 292, 292, 292, 292, 292, 292, 292, 292,...
 293, 294, 294, 294, 295, 295, 295, 296, 297, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307,...
 308, 309, 310, 309, 308, 307, 306, 305, 304, 303, 302, 302, 301, 300, 299, 298, 297, 297, 296, 295,...
 294, 294, 293, 292, 292, 292, 291, 291, 290, 289, 288, 288, 287, 286, 285, 284, 283, 282, 281, 281,...
 281, 280, 279, 279, 278, 277, 277, 277, 277, 277, 276, 275, 275, 274, 273, 273, 272, 272, 271, 270,...
 269, 268, 267, 266, 265, 265, 264, 263, 262, 262, 261, 260, 259, 259, 258, 258, 257, 256, 255, 254,...
 254, 254, 254, 254, 253, 253, 253, 253, 253, 252, 251, 251, 251, 251, 252, 253, 254, 255, 256, 257,...
 258, 259, 259, 260, 261, 262, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274, 275,...
 276, 277, 278, 279, 280, 281, 282, 283, 284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294, 295,...
 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 311, 312, 313, 314, 315,...
 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 327, 327, 326, 325, 324, 324, 324, 324,...
 324, 324, 325, 326, 326, 327, 328, 329, 330, 331, 332, 333, 333, 334, 335, 336, 336, 337, 338, 339,...
 340, 341, 341, 342, 343, 344, 345, 346, 346, 347, 348, 349, 350, 351, 352, 353, 354, 355, 356, 357,...
 358, 359, 360, 361, 362, 363, 363, 364, 365, 366, 367, 368, 369, 370, 371, 372, 373, 374, 375, 376,...
 377, 378, 378, 377, 376, 375, 374, 373, 372, 371, 370, 369, 368, 367, 366, 365, 364, 363, 362, 361,...
 360, 359, 358, 358, 357, 356, 355, 354, 353, 352, 351, 350, 349, 348, 347, 346, 346, 346, 345, 344,...
 343, 343, 343, 342, 341, 341, 340, 339, 338, 337, 336, 335, 335, 335, 334, 333, 333, 333, 333, 333,...
 333, 332, 332, 332, 332, 332, 333, 333, 334, 335, 336, 337, 338, 339, 340, 340, 339, 338, 337, 336,...
 335, 334, 333, 332, 331, 330, 329, 328, 327, 326, 325, 324, 323, 322, 321, 320, 319, 318, 317, 316,...
 315, 314, 314, 313, 312, 311, 310, 309, 308, 307, 306, 305, 304, 303, 302, 301, 300, 299, 298, 297,...
 296, 295, 294, 294, 293, 292, 291, 290, 289, 288, 287, 286, 285, 284, 283, 282, 281, 281, 280, 279,...
 278, 277, 276, 275, 274, 273, 272, 271, 270, 269, 268, 267, 267, 266, 265, 264, 263, 262, 262, 262,...
 261, 261, 260, 259, 259, 259, 259, 260, 261, 261, 261, 261, 261, 261, 262, 262, 262, 262, 263, 264,...
 264, 265, 265, 265, 266, 267, 267, 267, 268, 269, 269, 270, 270, 270, 270, 271, 272, 272, 272, 272,...
 272, 273, 273, 273, 274, 275, 275, 275, 275, 275, 276, 277, 277, 277, 277, 278, 278, 278, 278, 279,...
 280, 280, 280, 281, 281, 281, 282, 283, 284, 284, 285, 286, 287, 288, 289, 290, 291, 291, 292, 293,...
 294, 295, 295, 296, 297, 298, 299, 300, 300, 299, 299, 298, 297, 296, 295, 294, 293, 292, 291, 290,...
 289, 288, 287, 286, 285, 284, 283, 282, 281, 280, 279, 278, 277, 276, 275, 274, 273, 272, 271, 270,...
 269, 268, 267, 266, 265, 264, 263, 262, 261, 260, 259, 258, 257, 256, 255, 254, 253, 253, 252, 251,...
 250, 249, 248, 247, 247, 246, 245, 244, 243, 242, 241, 240, 239, 238, 237, 236, 235, 234, 233, 232,...
 232, 231, 230, 229, 229, 229, 229, 229, 229, 229, 229, 229, 229, 229, 230, 231, 230, 229, 228, 228,...
 228, 227, 226, 225, 225, 224, 223, 223, 222, 222, 221, 220, 219, 218, 217, 216, 215, 214, 214, 213,...
 212, 211, 210, 209, 208, 207, 206, 205, 204, 203, 202, 201, 201, 200, 199, 198, 198, 197, 196, 195,...
 194, 193, 193, 192, 191, 191, 190, 189, 188, 187, 187, 186, 185, 184, 184, 183, 182, 181, 180, 179,...
 179, 178, 177, 176, 176, 175, 174, 173, 172, 171, 171, 170, 169, 168, 167, 166, 165, 164, 163, 162,...
 161, 160, 159, 158, 157, 157, 156, 156, 155, 155, 155, 155, 155, 155, 155, 155, 155, 155, 154, 153,...
 153, 153, 154, 155, 155, 155, 155, 156, 156, 157, 158, 158, 159, 160, 160, 159, 158, 157, 157, 156,...
 155, 154, 154, 153, 152, 152, 151, 150, 150, 150, 149, 148, 147, 147, 147, 146, 146, 146, 145, 144,...
 143, 142, 142, 141, 141, 140, 139, 139, 139, 138, 138, 137, 136, 136, 136, 136, 135, 135, 135, 135,...
 134, 133, 133, 133, 133, 132, 131, 131, 131, 130, 130, 130, 129, 128, 128, 128, 127, 127, 127, 126,...
 125, 125, 124, 123, 122, 122, 121, 120, 119, 119, 118, 117, 117, 116, 115, 114, 114, 113, 112, 111,...
 111, 110, 109, 108, 108, 107, 106, 106, 106, 105, 105, 104, 103, 103, 102, 101, 100, 99, 98, 97,...
 96, 95, 94, 93, 93, 92, 91, 90, 89, 88, 87, 87, 86, 85, 84, 83, 82, 81, 80, 79,...
 78, 77, 76, 76, 75, 74, 73, 72, 71, 70, 69, 68, 68, 67, 66, 65, 64, 63, 62, 61,...
 60, 59, 58, 57, 56, 55, 55, 54, 53, 52, 51, 50, 49, 48, 47, 46, 45, 44, 43, 42,...
 41, 41, 40, 39, 38, 37, 36, 35, 34, 33, 32, 31, 30, 30, 29, 28, 27, 27, 27, 28,...
 29, 30, 31, 31, 32, 33, 34, 35, 36, 37, 37, 38, 39, 40, 40, 41, 42, 43, 44, 45,...
 46, 47, 48, 48, 49, 50, 51, 52, 53, 54, 55, 56, 56, 57, 58, 59, 60, 61, 62, 62,...
 63, 64, 65, 66, 67, 67, 68, 69, 70, 71, 72, 73, 74, 75, 75, 76, 77, 78, 79, 80,...
 81, 82, 83, 84, 85, 86, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 97, 98,...
 98, 97, 97, 96, 95, 95, 94, 94, 93, 92, 92, 92, 91, 90, 90, 90, 89, 89, 89, 88,...
 87, 87, 87, 86, 86, 85, 84, 83, 82, 81, 81, 81, 80, 79, 79, 78, 77, 76, 76, 75,...
 74, 73, 72, 71, 70, 69, 68, 67, 67, 66, 65, 64, 63, 62, 62, 61, 60, 59, 58, 57,...
 56, 56, 55, 54, 53, 52, 51, 51, 51, 51, 52, 53, 54, 55, 56, 56, 57, 58, 59, 59,...
 59, 60, 61, 62, 62, 62, 62, 62, 62, 61, 60, 60, 60, 60, 60, 59, 59, 59, 58, 57,...
 57, 57, 56, 56, 56, 55, 54, 54, 53, 52, 52, 51, 51, 51, 51, 50, 49, 49, 49, 49,...
 48, 48, 48, 48, 47, 46, 46, 45, 45, 45, 44, 43, 43, 43, 42, 41, 41, 40, 40, 39,...
 38, 37, 37, 36, 35, 34, 33, 32, 31, 30, 30, 31, 32, 33, 34, 35, 35, 35, 35, 35,...
 35, 35, 35, 35, 35, 34, 34, 34, 34, 34, 34, 34, 33, 32, 32, 32, 31, 30, 30, 30,...
 30, 30, 30, 29, 29, 29, 29, 29, 29, 28, 27, 27, 27, 27, 26, 26, 25, 24, 24, 23,...
 22, 22, 22, 21, 21, 20, 19, 19, 19, 18, 18, 17, 16, 16, 16, 16 ];

  return
end

