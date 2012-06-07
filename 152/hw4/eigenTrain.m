function [ W, mu ] = eigenTrain( trainset, k )

N = 70;
d = 2500;

% Default value for k
if nargin < 2, k = 20; end
trainset = trainset';

% compute the mean
mu = [];
for i=1:d
    temp = 0;
    for j=1:N
        temp = temp + trainset(i, j);
    end
    mu(i) = temp / N;
end

A = [];
for i=1:d
    for j=1:N
        A(i, j) = trainset(i, j) - mu(i);
    end
end

% Compute the top k eigenvectors
[W S V] = svds(A, k);

end
