[ trainset, trainLabels ] = loadSubset(0, './yaleBfaces');
[ W, mu ] = eigenTrain(trainset, 20);

I1 = [];
for i=1:10
    I1 = [I1 reshape(W(:, i), 50, 50)];
end

I2 = [];
for i=11:20
    I2 = [I2 reshape(W(:, i), 50, 50)];
end

figure;
imshow([I1 ; I2], []);

%%
original = [];
final = [];
for i=1:10
    x = [];
    I = im2double(imread(sprintf('./yaleBfaces/subset0/person%02d_01.png', i)));
    original = [ori I];
    for j=1:50;
        x = [x; I(:, j)];
    end
    
    y = W(:, 1:10)' * (x - mu');
    x = W(:, 1:10) * y + mu';

    final = [final reshape(x, 50, 50)];
end

figure;
imshow(original);

figure;
imshow(final);

%% 2)
c = 10;
[ W, mu ] = fisherTrain(trainset, trainLabels, c);

I = [];
for i=1:(c - 1)
    I = [I reshape(W(i, :), 50, 50)];
end

figure;
imshow(I, []);
