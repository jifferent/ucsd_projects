% 1)
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

%
out = [];
for i=1:10
    x = [];
    I = im2double(imread(sprintf('./yaleBfaces/subset0/person%02d_01.png', i)));
    for j=1:50
        x = [x; I(:, j)];
    end
    y = W(:, 1:10)' * (x - mu');
    x = W(:, 1:10) * y + mu';
    out = [out reshape(x, 50, 50)];
end

figure;
imshow(out);

%
figure;

k_plot = 1:1:20;
% k_plot = 4:1:20;
for subset=1:4
    [ testset, test ] = loadSubset(subset, './yaleBfaces');
    num = [];
    for k=1:20
%     for k=4:20
        labels = eigenTest(trainset, trainLabels, testset, W, k);
        acc = labels' - test;
        num = [num (1 - (histc(acc, 0) / size(acc, 1)))];
    end
    plot(k_plot, num);
    hold all;
end
legend('subset 1', 'subset 2', 'subset 3', 'subset 4', 'Location', 'SouthOutside');
ylabel('Error rate');
xlabel('k');

% 2)
c = 10;
[ W, mu ] = fisherTrain(trainset, trainLabels, c);

I = [];
for i=1:(c - 1)
    I = [I reshape(W(i, :), 50, 50)];
end

figure;
imshow(I, []);

%
figure;

for subset=1:4
    [ testset, test ] = loadSubset(subset, './yaleBfaces');
    num = [];
    for k=1:9
        labels = eigenTest(trainset, trainLabels, testset, W', k);
        acc = labels' - test;
        num = [num (1 - (histc(acc, 0) / size(acc, 1)))];
    end
    plot(num);
    hold all;
end
legend('subset 1', 'subset 2', 'subset 3', 'subset 4', 'Location', 'SouthOutside');
ylabel('Error rate');
xlabel('k');
