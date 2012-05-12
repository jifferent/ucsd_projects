function [ mark ] = connected( I )
    mark = repmat(-1, size(I));
    marker = 0;

    for i=1:size(I,1)
        for j=1:size(I,2)
            if I(i,j) == 1 && mark(i,j) == -1
                marker = marker + 1;
                label(i,j);
            end
        end
    end

    function label( i,j )
        mark(i,j) = marker;
        for x=-1:1
            for y=-1:1
                if I(x+i,y+j) == 1 && mark(x+i,y+j) == -1
                    label(x+i,y+j);
                end
            end
        end
    end
end
