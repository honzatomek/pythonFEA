xy = load ( 'anscombe.txt' );

subplot(2,2,1), plot ( xy(:,1), xy(:,2), 'r.', 'MarkerSize', 15 )
axis ( [0,20,0,14] )
grid
title ( '(x1,y1)' )

subplot(2,2,2), plot ( xy(:,3), xy(:,4), 'b.', 'MarkerSize', 15 )
axis ( [0,20,0,14] )
grid
title ( '(x2,y2)' )

subplot(2,2,3), plot ( xy(:,5), xy(:,6), 'g.', 'MarkerSize', 15 )
axis ( [0,20,0,14] )
grid
title ( '(x3,y3)' )

subplot(2,2,4), plot ( xy(:,7), xy(:,8), 'k.', 'MarkerSize', 15 )
axis ( [0,20,0,14] )
grid
title ( '(x4,y4)' )
