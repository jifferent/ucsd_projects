function [ W, mu ] = fisherTrain( trainset, trainLabels, c )

N = 70;

[ Wpca, mu ] = eigenTrain(trainset, N - c);
y = Wpca' * (trainset' - repmat(mu', 1, N));

new_mu = [];
for i=1:(N-c)
    temp = 0;
    for j=1:N
        temp = temp + y(i, j);
    end
    new_mu = [new_mu (temp / N)];
end

mu = new_mu';
Sb = [];
Sw = [];
for i=1:c
    Sb = y(i) * (mu(i) - mu) * (mu(i) - mu)';
    for j=1:N
        if (i == trainLabels(j))
            Sw = (y(:, j) - mu(i)) * (y(:, j) - mu(i))';
        end
    end
end

[Wfld, D] = eig(Sw, Sb);
W = Wfld(:, 1:9)' * Wpca';
end
