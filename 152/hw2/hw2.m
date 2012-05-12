set(0,'RecursionLimit',1000);

% 1)
coins = peakiness(im2double(rgb2gray(imread('coins.png'))), 3);
figure;
imshow(coins);

% 2.a)
S1 = ones(3);
hole = peakiness(im2double(imread('hole.png')), 3);

figure;
imshow(hole);

erode_s1_1     = erode(hole, S1, 1);
erode_s1_4     = erode(hole, S1, 4);
erode_s1_10    = erode(hole, S1, 10);

figure;
imshow(erode_s1_1);
figure;
imshow(erode_s1_4);
figure;
imshow(erode_s1_10);

% 2.b)
S2 = ones(7);

erode_s2_1     = erode(hole, S2, 1);
erode_s2_4     = erode(hole, S2, 4);
erode_s2_10    = erode(hole, S2, 10);

figure;
imshow(erode_s2_1);
figure;
imshow(erode_s2_4);
figure;
imshow(erode_s2_10);

% 2.c)
S3 = zeros(3);

dilate_s3_1     = dilate(hole, S3, 1);
dilate_s3_4     = dilate(hole, S3, 4);
dilate_s3_10    = dilate(hole, S3, 10);

figure;
imshow(dilate_s3_1);
figure;
imshow(dilate_s3_4);
figure;
imshow(dilate_s3_10);

% 3.a)
ucsd = peakiness(im2double(rgb2gray(imread('ucsd.png'))), 3);
figure;
imagesc(connected(ucsd));

% 3.b)
eroded_coins = erode(coins, S1, 4);
O = connected(eroded_coins);

number_of_coins = 0;
t = -1;
for i=1:size(O,1)
    for j=1:size(O,2)
       if t < O(i,j)
           number_of_coins = number_of_coins + 1;
           t = O(i,j);
       end
    end
end

% Display number of coins
number_of_coins

figure;
imagesc(O);

figure;
imshow(eroded_coins);

% 4)
ball = im2double(rgb2gray(imread('ball.png')));

tl = 0.2
th = 0.5
t = 3

O1 = edge(ball,'canny',[tl; th],t);
figure;
imshow(O1);

tl = tl*tl
th = th*th
t = t*t

O2 = edge(ball,'canny',[tl; th],t);
figure;
imshow(O2);

tl = tl*tl
th = th*th
t = t*t

O3 = edge(ball,'canny',[tl; th],t);
figure;
imshow(O3);
