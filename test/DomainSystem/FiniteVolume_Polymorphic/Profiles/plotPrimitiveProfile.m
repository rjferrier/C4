%% settings

% plot settings
markerSize = 150;
lineWidth = 2;
fontSize = 16;
colormap('default');
cStep = 2;
cRange = [NaN NaN];

nResX = 101;
nResY = nResX;

% diagnostics
exitFlagLookup = [-7 -4 -2 0 1 3];
queryExitFlag = char(...
    '(ill-posed or badly conditioned - no further progress)', ...
    '(ill-conditioning prevents further optimisation)', ...
    '(problem is infeasible)', ...
    '(maximum number of iterations exceeded)', ...
    '(LSQLIN converged to a solution X)', ...
    '(change in the residual smaller that the specified tolerance)');


%% calcs

% create abscissae
x = linspace(min(vertexX), max(vertexX), nResX);
y = linspace(min(vertexY), max(vertexY), nResY);

% mesh with coefs/powers then reconstruct the piecewise
% interpolation function
[~, ~, C] = ndgrid(x, y, c(:, 1));
[~, ~, PX] = ndgrid(x, y, px);
[X, Y, PY] = ndgrid(x, y, py);
for iSimplex = 1:nSimplex
    tempVertexList = simplexList(iSimplex, :);
    [X, Y, CI] = ndgrid(x, y, c(:, iSimplex));
    C(inpolygon(X, Y, vertexX(tempVertexList), ...
        vertexY(tempVertexList))) = CI(inpolygon(X, Y, ...
        vertexX(tempVertexList), vertexY(tempVertexList)));
end
clear CI
PHI = sum( C .* X.^PX .* Y.^PY, 3 );
[X, Y] = ndgrid(x, y);
mask = inpolygon(X, Y, vertexX, vertexY);

% check boundedness
fprintf('\nBoundedness check:\n');
phiBounds = [min( PHI(mask) ), max( PHI(mask) )];
fprintf('  maximum value within polygon = %g\n', ...
    phiBounds(1));
fprintf('  minimum value within polygon = %g\n', ...
    phiBounds(2));

% check conservation
fprintf('\nIntegrated average check:\n');
fprintf('  input I.A. = %g\n', cellIntAve);
cellIntAveOut = mean( PHI(mask) );
fprintf('  output I.A. at plot resolution  = %g\n', ...
    cellIntAveOut );


%% plotting

% compute colour settings
mask = inpolygon(X, Y, vertexX, vertexY);
if isnan(cRange(1))
    cRange(1) = min(PHI(mask));
end
if isnan(cRange(2))
    cRange(2) = max(PHI(mask));
end
cAbscissa = cRange(1) + ( cRange(2)-cRange(1) )*...
    ( (0:size(colormap, 1)-1)'/(size(colormap, 1)-1) );
cAbscissa = cAbscissa(1:cStep:end);

figure(1);
clf
hold on

% plot contours coloured by scalar value
PHI(~mask) = NaN;
contour( X, Y, PHI, cAbscissa, 'LineWidth', lineWidth);
caxis( cRange );
    colormap;
colorbar

% add cell boundaries and simplex boundaries
plot([vertexX vertexX(1)], [vertexY vertexY(1)], 'k', ...
    'LineWidth', lineWidth);
plot([vertexX(simplexList) vertexX(simplexList(:, 1))']', ...
    [vertexY(simplexList) vertexY(simplexList(:, 1))']', ...
    'k:', 'LineWidth', lineWidth);
scatter(gaussX(:), gaussY(:), markerSize, 'k+');

% assign collocation values
bVertVal = 0.*iVertVal;
bVertVal(vertexType == 0) = fluidVertexValue;
bVertVal(vertexType ~= 0) = boundaryPointValue;
bNodeVal = 0.*iNodeVal;
bNodeVal(nodeType == 1) = boundaryPointValue;
bNodeVal(nodeType ~= 1) = cutEdgeNodeValue;

% the following commented-out block causes Matlab on the 
% school servers to crash

% % plot points values as circles filled with their respective
% % input values.
% drawnow
% scatter(vertexX, vertexY, markerSize, ...
%     interp1(cAbscissa, cMap, bVertVal, 'linear'), 'filled' );
% drawnow
% scatter(vertexX, vertexY, markerSize, 'k' );
% drawnow
% scatter(nodeX, nodeY, markerSize, ...
%     interp1(cAbscissa, cMap, bNodeVal, 'linear'), 'filled');
% drawnow
% scatter(vertexX, vertexY, markerSize, 'k' );
% scatter(nodeX, nodeY, markerSize, 'k');
% 
% % plot centroid values
% if firstCellPointValue >= cRange(1) && ...
%         firstCellPointValue <= cRange(2)
%     scatter(centroidX(1), centroidY(1), markerSize, ...
%         interp1(cAbscissa, cMap, firstCellPointValue, ...
%         'linear'), 'filled' );
% end
% scatter(centroidX(2:end), centroidY(2:end), markerSize, ...
%     interp1(cAbscissa, cMap, cellPointValue, ...
%     'linear'), 'filled' );
% scatter( centroidX, centroidY, markerSize, 'k' );
% 

% plot simplicies and gaussian points
plot([vertexX(simplexList) vertexX(simplexList(:, 1))']', ...
    [vertexY(simplexList) vertexY(simplexList(:, 1))']', ...
    'k:', 'LineWidth', lineWidth);
scatter(gaussX(:), gaussY(:), markerSize, 'k+');

h = title(['cell int ave = ' num2str(cellIntAveOut) ...
    '; min = ' num2str(phiBounds(1)) ...
    '; max = ' num2str(phiBounds(2)) ], ...
    'FontSize', fontSize);
set( gca, 'FontSize', fontSize);
