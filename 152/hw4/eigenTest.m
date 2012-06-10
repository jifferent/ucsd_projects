function testLabels = eigenTest(trainset, trainlabels, testset, W, k)

N = 70;
M = size(testset, 1);

testLabels = [];
for j=1:M
    testset_k = (W(:, 1:k)' * (testset(j, :)'));
%     testset_k = (W(:, 4:k)' * (testset(j, :)'));
    min = 0;
    for i=1:N
        acc = norm(testset_k - (W(:, 1:k)' * (trainset(i, :)')), 1);
%         acc = norm(testset_k - (W(:, 4:k)' * (trainset(i, :)')), 1);
        if (i == 1) || (min > acc)
            min = acc;
            index = i;
        end
    end
    testLabels(j) = trainlabels(index);
end
end
