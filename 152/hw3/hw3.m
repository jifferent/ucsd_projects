% 1.a)
I1 = imread('house1.pgm');
I2 = imread('house2.pgm');

[input_points, base_points] = cpselect(I1, I2, 'Wait', true);
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
d = 0;
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
    d = d + abs(det([Q2-Q1,P-Q1]))/abs(Q2-Q1);
end
d = d / 15

% 2.a)
I3 = im2double(imread('house1.pgm'));
W = size(I3(:,:,1), 1);
H = size(I3(:,:,1), 2);
x = 1;
for sigma=1:4
    if sigma == 3; continue; end;
    s = sigma * 3;

    k = [];
    for i=(-s:s)
        k(i + (sigma * 3 + 1)) = ( 1 / sqrt(2 * pi * sigma) ) * exp( (-(i + s) ^ 2) / (2 * sigma ^ 2));
    end

    xx = conv2(conv2(I3, k, 'same'), [-1 0 1], 'same');
    figure;
    imshow(xx, []);

    yy = conv2(conv2(I3, k, 'same'), [-1 0 1]', 'same');
    figure;
    imshow(yy, []);
    
% % 2.b)
    egv = [];
    for y=s+1:H-s-1
        for x=s+1:W-s-1
            s_i = 0;
            s_j = 0;
            s_ij = 0;
            for i=-s:s
                for j=-s:s
                    s_i  = s_i + (xx(x-i, y-j) ^ 2);
                    s_j  = s_j + (yy(x-i, y-j) ^ 2);
                    s_ij = s_ij + xx(x-i, y-j) + yy(x-i, y-j);
                end
            end

            e1 = (s_i + s_j) / 2 + sqrt(4 * (s_ij ^ 2) + (s_i - s_j) * (s_i - s_j)) / 2;
            e2 = (s_i + s_j) / 2 - sqrt(4 * (s_ij ^ 2) + (s_i - s_j) * (s_i - s_j)) / 2;

            if e1 > (10 + sigma) && e2 > (sigma / 2)
                egv(x, y) = (e1 + e2);
            else
                egv(x, y) = 0;
            end
        end
    end

    figure;
    imshow(I3);

    for y=s+1:H-s-1
        for x=s+1:W-s-1
            if egv(x, y) ~= 0
                drawPoint([x y]);
            end
        end
    end
end
