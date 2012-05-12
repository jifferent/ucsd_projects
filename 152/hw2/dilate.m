function [ dilate ] = dilate( image, K, n )
    m = length(K) - 1;
    dilate = image;
    for z=1:n
        img = [];
        x = size(dilate,1);
        y = size(dilate,2);
        for j=1:(y - m)
            for i=1:(x - m)

                if dilate(i,j) == 0
                    t = dilate(i:i + m,j:j + m);
                    if t == K
                        img(i,j) = dilate(i, j);
                    else
                        img(i,j) = 1;
                    end
                else
                    img(i,j) = dilate(i,j);
                end

            end
        end
        dilate = img;
    end
end
