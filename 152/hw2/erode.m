function [ erode ] = erode( image, K, n )
    m = length(K) - 1;
    erode = image;
    for z=1:n
        img = [];
        x = size(erode,1);
        y = size(erode,2);
        for j=1:(y - m)
            for i=1:(x - m)
                t = erode(i:i + m,j:j + m);
                if t == K
                    img(i,j) = erode(i, j);
                else
                    img(i,j) = 0;
                end
            end
        end
        erode = img;
    end
end
