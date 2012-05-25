function pts = drawLine(homLine, ptlabel, ptcolor)
% Draw a homogeneous line on the current figure
% 
% INPUTS
%   homLine    - Homogeneous coordinates for the line (aka the "coimage")
%   ptlabel    - Text label for the line (optional)
%   ptcolor    - Color for the line and label (optional)
%
% OUTPUTS
%   pts        - The line's endpoints used to plot on the figure
%
% DATESTAMP
%   17-May-07 11:16pm
%
% See also LINEPTS, LINE, TEXT


% Clip our line to be within the bounding box:
% 1. Find the bounding box
xLimits = xlim;
yLimits = ylim;
xmin = xLimits(1);
xmax = xLimits(2);
ymin = yLimits(1);
ymax = yLimits(2);

pts = linePts(homLine, [xmin xmax], [ymin ymax]);

% Find the midpoint so that we can place the label
avg = sum(pts,1) / 2;

% Set the default color and label properties
if (nargin < 3 || isempty(ptcolor))
    ptcolor = 'b';                      % Default color is blue
end
if (nargin < 2 || isempty(ptlabel))
    ptlabel = '';                       % No default label
end
if isfloat(ptlabel), ptlabel = sprintf('%d', ptlabel); end

% Plot the line and the label on the current figure
line(pts(:,1), pts(:,2), 'Color', ptcolor);
text(avg(1), avg(2), ptlabel, 'Color', ptcolor, 'FontWeight', 'bold', 'FontSize', 12);
