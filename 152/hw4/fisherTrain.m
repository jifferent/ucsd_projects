function [ W, mu ] = fisherTrain( trainset, trainLabels, c )

N = 70;

[ Wpca, mu ] = eigenTrain(trainset, N-c);
y = Wpca' * (trainset' - repmat(mu', 1, N));

new_mu = [];
n = 0;
for i=1:(N-c)
    acc = 0;
    for j=1:N
        if (i == trainLabels(j))
            acc = acc + y(i, j);
            n = n + 1;
        end
    end
    new_mu = [new_mu (acc / n)];
end

mu = new_mu';
Sb = zeros(N-c);
Sw = zeros(N-c);

for i=1:c
    Sb = Sb + n * (mu(i) - mu) * (mu(i) - mu)';
    for j=1:N
        if (i == trainLabels(j))
            Sw = Sw + (y(:, j) - mu(i)) * (y(:, j) - mu(i))';
        end
    end
end

[Wfld, D] = eig(Sw, Sb);
W = Wfld(:, 1:9)' * Wpca';
end
