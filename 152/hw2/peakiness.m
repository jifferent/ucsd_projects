function [binary] = peakiness(img, delta)

[N,X] = hist(img(:), 10);

maxs = [];
mins = [];
minimum = Inf;
maximum = -Inf;
min_pos = NaN;
max_pos = NaN;

keep_going = 1;
for i=1:size(N,2)
    if N(i) > maximum
        maximum = N(i);
        max_pos = i;
    end
    
    if N(i) < minimum
        minimum = N(i);
        min_pos = i;
    end
    
    if keep_going
        if (N(i) < maximum - delta)
            maxs = [maxs ; max_pos maximum];
            minimum = N(i);
            min_pos = i;
            keep_going = 0;
        end
    else
        if (N(i) > minimum + delta)
            mins = [mins ; min_pos minimum];
            maximum = N(i);
            max_pos = i;
            keep_going = 1;
        end
    end
end

gi = X(maxs(1,1))
if size(maxs,1) == 1
    gj = gi
else
    gj = X(maxs(2,1))
end

gk = X(mins(1,1))
tresh = min(gi,gj)/gk

binary = img > tresh;
end
