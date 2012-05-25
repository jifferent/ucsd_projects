% 1.a)
I1 = imread('house1.pgm');
I2 = imread('house2.pgm');

% [input_points, base_points] = cpselect(I1, I2, 'Wait', true);
F = fund(base_points, input_points)

p1 = [input_points(2,1); input_points(2,2); 1];
p2 = [input_points(8,1); input_points(8,2); 1];
p3 = [input_points(13,1); input_points(13,2); 1];

p1_b = [base_points(2,1); base_points(2,2); 1];
p2_b = [base_points(8,1); base_points(8,2); 1];
p3_b = [base_points(13,1); base_points(13,2); 1];

figure, imshow(I1);
drawPoint(input_points);
l11 = drawLine(F * p1, '', 'r');
l12 = drawLine(F * p2, '', 'g');
l13 = drawLine(F * p3, '', 'y');

figure, imshow(I2);
drawPoint(base_points);
l21 = drawLine(F' * p1_b, '', 'r');
l22 = drawLine(F' * p2_b, '', 'g');
l23 = drawLine(F' * p3_b, '', 'y');

% 1.b)
for i=1:size(base_points, 1)
    p = [base_points(i,1); base_points(i,2); 1];
    
    xLimits = xlim;
    yLimits = ylim;
    xmin = xLimits(1);
    xmax = xLimits(2);
    ymin = yLimits(1);
    ymax = yLimits(2);
    l = linePts(F*p, [xmin xmax], [ymin ymax]);

    Q1 = l(1,:)';
    Q2 = l(2,:)';
    P = [p(1) p(2)]';
    d = abs(det([Q2-Q1,P-Q1]))/abs(Q2-Q1)
end

% Q1 = l21(1,:)';
% Q2 = l21(2,:)';
% P = [p1_b(1) p1_b(2)]';
% d11 = abs(det([Q2-Q1,P-Q1]))/abs(Q2-Q1)
% 
% Q1 = l22(1,:)';
% Q2 = l22(2,:)';
% P = [p3_b(1) p3_b(2)]';
% d12 = abs(det([Q2-Q1,P-Q1]))/abs(Q2-Q1)
% 
% Q1 = l23(1,:)';
% Q2 = l23(2,:)';
% P = [p3_b(1) p3_b(2)]';
% d13 = abs(det([Q2-Q1,P-Q1]))/abs(Q2-Q1)
