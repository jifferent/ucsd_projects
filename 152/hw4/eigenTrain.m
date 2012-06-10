function [ W, mu ] = eigenTrain( trainset, k )

N = 70;
d = 2500;
trainset = trainset';

mu = [];
for i=1:d
    acc = 0;
    for j=1:N
        acc = acc + trainset(i, j);
    end
    mu(i) = acc / N;
end

res = [];
for i=1:d
    for j=1:N
        res(i, j) = trainset(i, j) - mu(i);
    end
end
[W S V] = svds(res, k);
end
