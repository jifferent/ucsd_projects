function drawPoint(pt, ptlabel, ptcolor)
% Draw a points on the current figure
% 
% INPUTS
%   pt         - Image coordinates (one per row)
%   ptlabel    - Text label for the point (optional)
%   ptcolor    - Color for the point and label (optional)
%
% OUTPUTS
%   None.
%
% DATESTAMP
%   17-May-07 6:15pm
%
% See also PLOT, TEXT

% Set the default color and label properties
if (nargin < 3 || isempty(ptcolor))
    ptcolor = 'b';                      % Default color is blue
end
if (nargin < 2 || isempty(ptlabel))
    ptlabel = '';                       % No default label
end
if isfloat(ptlabel), ptlabel = sprintf('%d', ptlabel); end

% Display the points on the current figure as white circles with a + sign
% and display the text beside the plotted points
hold on;
plot(pt(:,1), pt(:,2), '+', 'Color', ptcolor);
plot(pt(:,1), pt(:,2), 'o', 'Color', ptcolor);
text(pt(:,1)+2, pt(:,2), ptlabel, 'Color', ptcolor, 'FontWeight', 'bold', 'FontSize', 12);
hold off;
