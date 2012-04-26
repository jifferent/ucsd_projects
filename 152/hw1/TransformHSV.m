function imgOut = TransformHSV(imgIn, TETA ,S ,V)
    imgIn = im2double(imgIn);
    [Y, X] = size(imgIn(:,:,1));
    imgOut = [];

    % Convert from rgb to hsv.
    for x = 1:X
        for y = 1:Y
            r = imgIn(y, x, 1);
            g = imgIn(y, x, 2);
            b = imgIn(y, x, 3);
            min_rgb = min([r g b]);
            max_rgb = max([r g b]);
            v = max_rgb;
            
            delta = max_rgb - min_rgb;
            if max_rgb == 0
                s = 0;
            else
                s = delta / max_rgb;
            end

            if delta == 0
                h = 0;
            else
                if r == max_rgb
                    tmp = 0;
                    if g < b
                        tmp = 6;
                    end
                    h = (g - b) / delta + tmp;
                elseif g == max_rgb
                    h = 2 + ( b - r ) / delta;
                else
                    h =  4 + ( r - g ) / delta;
                end
                h = h / 6;
            end

            imgOut(y, x, 1) = h;
            imgOut(y, x, 2) = s;
            imgOut(y, x, 3) = v;
        end
    end
    
    % Modify image.
    imgOut(:,:,1) = ((imgOut(:,:,1) * 360) + TETA) / 360;
    imgOut(:,:,2) = imgOut(:,:,2) * S;
    imgOut(:,:,3) = imgOut(:,:,3) * V;
    
    % Convert from hsv to rgb.
    for x = 1:X
        for y = 1:Y
            h = imgOut(y, x, 1);
            s = imgOut(y, x, 2);
            v = imgOut(y, x, 3);
            i = floor(h * 6);
            f = h * 6 - i;
            p = v * (1 - s);
            q = v * (1 - f * s);
            t = v * (1 - (1 - f) * s);
            switch mod(i, 6)
                case 0
                    r = v;
                    g = t;
                    b = p;
                case 1
                    r = q;
                    g = v;
                    b = p;
                case 2
                    r = p;
                    g = v;
                    b = t;
                case 3
                    r = p;
                    g = q;
                    b = v;
                case 4
                    r = t;
                    g = p;
                    b = v;
                case 5
                    r = v;
                    g = p;
                    b = q;
            end
            imgOut(y, x, :) = [r g b];
        end
    end
    im2uint8(imgOut);
end

