%% -- Settings --

% polynomial degree
nPoly = 3;

% gaussian points and weights based on:
% Hammer P.C. and Stroud A.H., Numerical integration over simplexes.
% Mathematical Tables and Aids to Computation, 1956, 10:137-139.
c1 = 0.6;
c2 = 0.2;
gaussCoordsSimplex = [ 1/3*ones(1, 3);
    c1*diag(ones(1, 3)) + c2*~diag(ones(1, 3)) ];
gaussWeightsSimplex = [-9/16; 25/48*ones(3, 1)];


% cubic gaussian points for a line
gaussianCoordsLine = [
    (1 + 1/sqrt(3))/2 (1 - 1/sqrt(3))/2;
    (1 - 1/sqrt(3))/2 (1 + 1/sqrt(3))/2 ];


% collocated and integrated values
fluidVertexValue = 1;
cutEdgeNodeValue = 0.8;
boundaryPointValue = 0.5;
fluidVertexGradient = [0.1 0.2];
cellPointValues = [1.1 0.9 0.7];
cellIntAve = 0.75;

% plot settings
markerSize = 150;
lineWidth = 2;
fontSize = 16;
colormap('default');
cStep = 2;
cRange = [NaN NaN];

% diagnostics
exitFlagLookup = [-7 -4 -2 0 1 3];
queryExitFlag = char(...
    '(ill-posed or badly conditioned - no further progress)', ...
    '(ill-conditioning prevents further optimisation)', ...
    '(problem is infeasible)', ...
    '(maximum number of iterations exceeded)', ...
    '(LSQLIN converged to a solution X)', ...
    '(change in the residual smaller that the specified tolerance)');

% default arg(s)
if ~exist('iShape', 'var');
    iShape = 3;
end
if ~exist('nRes', 'var');
    nRes = 101;
end
nResX = nRes;
nResY = nRes;

% tolerance for numerical comparison
tol = 1e-12;



%% -- Shape library --

% list vertex x- and y- coordinates, vertex types, and edge types.
% Types can be uncut (0), cut (1) or floating (2) as discussed above.
% Each edge (i) joins vertices (i) and (i+1), with edge (n) joining
% vertices (n) and (1).
switch iShape
    case 1
        % triangle
        polygon = [
            0       0       0   1   ;
            0.5     0       1   2   ;
            0       0.5     1   1   ];
    case 2
        % quadrilateral
        polygon = [
            0       0       0   1   ;
            0.75    0       1   2   ;
            0.25    1       1   1   ;
            0       1       0   0   ];
    case 3
        % pentagon (clipped corner)
        polygon = [
            0       0       0   0   ;
            1       0       0   1   ;
            1       0.25    1   2   ;
            0.75    1       1   1   ;
            0       1       0   0   ];
        
    case 4
        % pentagon (solid boundary with obtuse angle)
        polygon = [
            0       0       0   1   ;
            0.5    0        1   2   ;
            0.5    0.5      2   2   ;
            0.75    1       1   1   ;
            0       1       0   0   ];
        
    case 5
        % cell penetrated by a right angle
        polygon = [
            0       0       0   1   ;
            0.5     0       1   2   ;
            0.5     0.50    2   2   ;
            1       0.50    1   1   ;
            1       1       0   0   ;
            0       1       0   0   ];
        
    case 6
        % cell penetrated by a triangle on one side
        polygon = [
            0       0       0   0   ;
            1       0       0   1   ;
            1       0.25    1   2   ;
            0.5     0.5     2   2   ;
            1       0.75    1   1   ;
            1       1       0   0   ;
            0       1       0   0   ];
        
    case 7
        % triangle
        polygon = [
            1       1       0   1   ;
            7       1       1   2   ;
            1       4       1   1   ];
end


%% -- Geometric calcs --

% parse the inputs
vertexX =     polygon(:, 1)';
vertexY =     polygon(:, 2)';
vertexType =  polygon(:, 3)';
edgeType =    polygon(:, 4)';

% list the edges
nV = length(vertexType);
edgeList = [(1:nV)' [(2:nV)'; 1]];

% divide polygon into simplices
simplexList = delaunay(vertexX, vertexY);

% the delaunay function assumed a convex hull, so filter out
% unwanted simplices by creating centroids and querying with
% inpolygon
mask = false( size(simplexList, 1), 1 );
for i = 1:size(simplexList, 1)
    tempPoint = [vertexX(simplexList(i, :)); ...
        vertexY(simplexList(i, :))]*1/3*ones(3, 1);
    mask(i) = inpolygon(tempPoint(1), tempPoint(2), vertexX, vertexY);
end
simplexList = simplexList(mask, :);
nSimplex = size(simplexList, 1);

% we have generated some new edges.  Identify them and also
% identify the connectivity with simplicies.
edgeTypeLookup = [0 1 1; 1 2 2; 1 2 2];
simplexToEdgeList = zeros(nSimplex, 3);
for i = 1:size(simplexList, 1)
    for j = 1:3
        v1 = simplexList(i, j);
        v2 = simplexList(i, mod(j, 3) + 1);
        flag = false;
        for k = 1:length(edgeList)
            if all(edgeList(k, :) == [v1 v2]) || ...
                    all(edgeList(k, :) == [v2 v1])
                flag = true;
                simplexToEdgeList(i, j) = k;
            end
        end
        if ~flag
            edgeList = [edgeList; v1 v2];  %#ok<*AGROW>
            edgeType = [edgeType ...
                edgeTypeLookup(vertexType(v1)+1, vertexType(v2)+1)];
            simplexToEdgeList(i, j) = k + 1;
        end
    end
end
nE = length(edgeType);

% populate each edge with 0, 1, or 2 points
[nodeX, nodeY, nodeType] = deal( zeros(1, sum(edgeType)) );
nodeToEdgeList = 1:sum(edgeType);  % connectivity
edgeToNodeList = cell(1, nE);
iNode = 1;
for iEdge = 1:length(edgeType)
    n_new = edgeType(iEdge);
    
    %     nodeX_new = linspace(vertexX(edgeList(iEdge,1)), ...
    %         vertexX(edgeList(iEdge,2)), 2 + n_new);
    %     nodeY_new = linspace(vertexY(edgeList(iEdge,1)), ...
    %         vertexY(edgeList(iEdge,2)), 2 + n_new);  
%     nodeX_new = nodeX_new(2 : end-1);
%     nodeY_new = nodeY_new(2 : end-1);
    
    if n_new == 2
        nodeX_new = gaussianCoordsLine* [vertexX(edgeList(iEdge,1));
            vertexX(edgeList(iEdge,2))];
        nodeY_new = gaussianCoordsLine* [vertexY(edgeList(iEdge,1));
            vertexY(edgeList(iEdge,2))];
    else
        nodeX_new = linspace(vertexX(edgeList(iEdge,1)), ...
            vertexX(edgeList(iEdge,2)), 2 + n_new);
        nodeY_new = linspace(vertexY(edgeList(iEdge,1)), ...
            vertexY(edgeList(iEdge,2)), 2 + n_new);
        nodeX_new = nodeX_new(2 : end-1);
        nodeY_new = nodeY_new(2 : end-1);
    end
    
    nodeX(iNode : iNode+n_new-1) = nodeX_new;
    nodeY(iNode : iNode+n_new-1) = nodeY_new;
    nodeType(iNode : iNode+n_new-1) = edgeType(iEdge)/2;
    nodeToEdgeList(iNode : iNode+n_new-1) = iEdge;
    edgeToNodeList{iEdge} = iNode : iNode+n_new-1;
    iNode = iNode + n_new;
end

% generate gaussian points
nGaussSimplex = size(gaussCoordsSimplex, 1);
[gaussX, gaussY] = deal( zeros(nSimplex, nGaussSimplex) );
for j = 1:nGaussSimplex
    for i = 1:nSimplex
        gaussPoint = [vertexX(simplexList(i, :)); ...
            vertexY(simplexList(i, :))]*gaussCoordsSimplex(j, :)';
        gaussX(i, j) = gaussPoint(1);
        gaussY(i, j) = gaussPoint(2);
    end
end

% compute simplex areas
areaSimplex = zeros(nSimplex, 1);
for i = 1:nSimplex
    areaSimplex(i) = 1/2*det( [vertexX(simplexList(i, :))', ...
        vertexY(simplexList(i, :))', ones(3, 1)] );
end

% adjust gaussian weights by multiplying each raw weight by the
% simplex-to-polygon area fraction
[W, S] = meshgrid(gaussWeightsSimplex, areaSimplex);
gaussWeights = W.*S/sum(areaSimplex);
clear W S

% compute centroids
centroidX = sum( vertexX(simplexList), 2 )/3;
centroidY = sum( vertexY(simplexList), 2 )/3;


%% -- Solution of underdetermined simplex profiles --

nCoefs = (nPoly + 1)*(nPoly + 2)/2;

% set up the particular solution and basis vectors.  There must be
% nSimplex copies.
u = zeros(nCoefs, nSimplex);
v = zeros(nCoefs, nSimplex);


% get powers of x and y in the order
% [1, x, x^2, x^3, y, x*y, x^2*y, y^2, ... ]
px = [0, 1, 2, 3, 0, 1, 2, 0, 1, 0];
py = [0, 0, 0, 0, 1, 1, 1, 2, 2, 3];

% loop over sub-matrices
for iSimplex = 1:nSimplex
    
    % get collocation coordinates
    tempVertexList = simplexList(iSimplex, :);
    tempNodeList = [];
    for j = 1:3
        tempNodeList = [tempNodeList ...
            edgeToNodeList{simplexToEdgeList(iSimplex, j)}];
    end
    x = [vertexX(tempVertexList) nodeX(tempNodeList)]';
    y = [vertexY(tempVertexList) nodeY(tempNodeList)]';
    
    % add gradient coordinates (duplicated by number of dimensions)
    tempVertexGradList = ...
        tempVertexList(vertexType(tempVertexList) == 0);
    x = [x; vertexX(tempVertexGradList)'; vertexX(tempVertexGradList)'];
    y = [y; vertexY(tempVertexGradList)'; vertexY(tempVertexGradList)'];
    
    % identify each section
    iVertVal = (1:length(tempVertexList));
    iNodeVal = length(tempVertexList) + (1:length(tempNodeList));
    iVertGradX = length(tempVertexList) + length(tempNodeList) + ...
        (1:length(tempVertexGradList));
    iVertGradY = length(tempVertexList) + length(tempNodeList) + ...
        length(tempVertexGradList) + (1:length(tempVertexGradList));
    
    % build the initial (zeroth-derivative) Vandermonde matrix
    [X, PX] = ndgrid(x, px);
    [Y, PY] = ndgrid(y, py);
    AP = X.^PX .* Y.^PY;
    
    % correct the x-gradient terms.  The nonlinear functions are
    % needed so that the zeroth derivatives evaluate to zero after
    % differentiation.
    PXD = PX(iVertGradX, :);
    XD = X(iVertGradX, :);
    PYD = PY(iVertGradX, :);
    YD = Y(iVertGradX, :);
    AP(iVertGradX, :) = PXD.*XD.^max(0, PXD - 1) .* YD.^PYD;
    
    % correct the y-gradient terms
    PXD = PX(iVertGradY, :);
    XD = X(iVertGradY, :);
    PYD = PY(iVertGradY, :);
    YD = Y(iVertGradY, :);
    AP(iVertGradY, :) = XD.^PXD .* PYD.*YD.^max(0, PYD - 1);
    

    fprintf('\nexactly constrained matrix #%g:\n', iSimplex);
    fprintf('         %.4f_FLOAT, %.4f_FLOAT, %.4f_FLOAT, %.4f_FLOAT, &\n', AP);
    
    
    
    % assign collocation values
    bVertVal = 0.*iVertVal;
    bVertVal(vertexType(tempVertexList) == 0) = fluidVertexValue;
    bVertVal(vertexType(tempVertexList) ~= 0) = boundaryPointValue;
    bNodeVal = 0.*iNodeVal;
    bNodeVal(nodeType(tempNodeList) == 1) = boundaryPointValue;
    bNodeVal(nodeType(tempNodeList) ~= 1) = cutEdgeNodeValue;
    bVertGradX = 0.*iVertGradX + fluidVertexGradient(1);
    bVertGradY = 0.*iVertGradY + fluidVertexGradient(2);
    
    % form the RHS
    bP = [bVertVal bNodeVal bVertGradX bVertGradY]';
    
    % perform QR factorisation
    usePivoting = true;
    if usePivoting
        [Q,R,E] = qr(AP');
    else
        [Q,R] = qr(AP');
        E = eye(size(AP, 1));
    end
    
    % null vector (last column of Q)
    v(:, iSimplex) = Q(:, end);
    
    % check consistency with svd-based function
    if ~all( abs(v(:, iSimplex) - null(E*AP)) < tol ) && ...
            ~all( abs(v(:, iSimplex) + null(E*AP)) < tol )
        fprintf('discrepancy in null space computation.\n');
    end  
    
    % compute a particular solution (this may not be unique)
    % n.b. permutation
    u(:, iSimplex) = Q(:, 1:end-1)*(R(1:end-1 ,:)'\(E'*bP));
end


%% -- Solution of overdetermined simplicial complex profile --

% initialise the overall matrices and right hand sides
AL = zeros(nSimplex);
bL = zeros(nSimplex, 1);
AG = zeros(1, nSimplex);
bG = cellIntAve;

% loop over sub-matrices
for iSimplex = 1:nSimplex
    
    % compute a Vandermonde row corresponding to this simplex's centroid
    x = centroidX(iSimplex);
    y = centroidY(iSimplex);
    aL = x.^px .* y.^py;
    
    % do matrix multiplications with u and v and put the results in the
    % appropriate linear system elements
    AL(iSimplex, iSimplex) = aL*v(:, iSimplex);
    bL(iSimplex, 1) = cellPointValues(iSimplex) - aL*u(:, iSimplex);
    
    
    % for the cell int. ave. constraint, Gaussian quadrature must be
    % used.  The quadrature evaluates to sum( w_k * phi(x_k, y_k) )
    %   = [1 sum(w_k*x_k), sum(w_k*y_k), sum(x_k^2), ... ].*c_k
    % where k is an index to each Gaussian point, w the corresponding
    % weight, x and y the point coordinates, and c the coefficient
    % vector.  The equivalent Vandermonde row is in brackets.

    [XG, PXG] = ndgrid( gaussX(iSimplex, :), px );
    [YG, PYG] = ndgrid( gaussY(iSimplex, :), py );
    WG = ndgrid( gaussWeights(iSimplex, :), px );
    aG = sum( WG.* XG.^PXG .* YG.^PYG, 1 );
    
    % do matrix multiplications with u and v and put the results
    % in the appropriate linear system elements
    AG(1, iSimplex) = aG*v(:, iSimplex);
    bG = bG - aG*u(:, iSimplex);
    
end

fprintf('\nreduced least squares matrix:\n');
fprintf('         %.14f_FLOAT, &\n', AL);

fprintf('\nreduced least squares RHS:\n');
fprintf('         %.14f_FLOAT, &\n', bL);

fprintf('\nreduced cell int ave row:\n');
fprintf('         %.14f_FLOAT, &\n', AG);

fprintf('\nreduced cell int ave RHS: %.14f_FLOAT\n', bG);


% perform constrained least squares to fix the slack variables

% QRD of transposed, exactly constrained system
[Q,R,P] = qr(AG');

% particular solution
n = 4;  % total number of rows
p = 1;  % number of exactly constrained rows

Q1 = Q(:, 1:p);
R1 = R(1:p,:);
x1 = R1'\(P'*bG);
x1 = Q1*x1;

% nullspace
Q2 = Q(:, p+1:end);  

% QRD of reduced, unconstrained lsq system
% min|| (AL*Q2)*y - (b - AL*x1) ||_2
[QL,RL,PL] = qr(AL*Q2);
y = QL'*(bL - AL*x1);
RL1 = RL(1:n-p,:);
y2 = y(1:n-p);
y2 = RL1\y2;

s = x1 + (Q2*PL)*y2;
% [s, resNorm, residual, exitFlag] = lsqlin( AL, bL, [], [], AG, bG );


fprintf('\nreduced cell int ave QR decomposition:\n');
fprintf('          %.14f_FLOAT, &\n', qr(AG') );
fprintf('\n');

fprintf('\nreduced cell int ave particular solution:\n');
fprintf('          %.14f_FLOAT, &\n', x1);
fprintf('\n');

fprintf('\nfinal matrix:\n');
fprintf('          %.14f_FLOAT, &\n', AL*Q2 );
fprintf('\n');

fprintf('\nfinal RHS:\n');
fprintf('          %.14f_FLOAT, &\n', bL - AL*x1 );
fprintf('\n');

fprintf('\nfinal QR decomposition:\n');
fprintf('          %.14f_FLOAT, &\n', qr(AL*Q2*PL) );
fprintf('\n');

fprintf('\nfinal particular solution:\n');
fprintf('          %.14f_FLOAT, &\n', y2);
fprintf('\n');

fprintf('\nsolved free variables:\n');
fprintf('          %.14f_FLOAT, &\n', s);
fprintf('\n');


% use the slack variables to finally fix the profile coefficients
S = meshgrid(s, 1:nCoefs);
c = u + S.*v;

for i = 1:3
    fprintf('\nsimplex profile coefficients [%g]:\n', i);
    fprintf('          %.14f_FLOAT, &\n', c(:, i));
    fprintf('\n');
end


%% -- Results --


% create abscissae
x = linspace(min(vertexX), max(vertexX), nResX);
y = linspace(min(vertexY), max(vertexY), nResY);

% mesh with coefs/powers then reconstruct the piecewise
% interpolation function
[X, Y, C] = ndgrid(x, y, c(:, 1));
[X, Y, PX] = ndgrid(x, y, px);
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

% % report lsqlin outputs
% fprintf('\nResiduals:\n');
% fprintf('  %g\n', residual);
% fprintf('\nDiagnostics:\n');
% fprintf('  rms(residual) = %g\n',...
%     sqrt(sum(residual.^2)/length(residual)));
% fprintf('  norm(C*X-d)^2 = %g\n', resNorm);
% iExitFlag = interp1(exitFlagLookup, 1:6, exitFlag, 'nearest');
% fprintf('  exit flag = %g %s\n', exitFlag, queryExitFlag(iExitFlag, :));

%% plot

plotPrimitiveProfile