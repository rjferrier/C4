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
cellPointValue = 0.9;
cellIntAve = 0.7;

% tolerance for numerical comparison
tol = 1e-12;


%% -- Cell spec --

% list vertex x- and y- coordinates, vertex types, and edge types.
% Types can be uncut (0), cut (1) or floating (2) as discussed above.
% Each edge (i) joins vertices (i) and (i+1), with edge (n) joining
% vertices (n) and (1).
polygon = [
    1       1       0   1   ;
    7       1       1   2   ;
    1       4       1   1   ];



%% -- Geometric calcs --

% parse the inputs
vertexX =     polygon(:, 1)';
vertexY =     polygon(:, 2)';
vertexType =  polygon(:, 3)';
edgeType =    polygon(:, 4)';

% list the edges and verticies
nV = 3;
nE = 3;
edgeList = [(1:nV)' [(2:nV)'; 1]];
vertexList = (1:nV)';

% populate each edge with 0, 1, or 2 points
[nodeX, nodeY, nodeType] = deal( zeros(1, sum(edgeType)) );
nodeToEdgeList = 1:sum(edgeType);  % connectivity
edgeToNodeList = cell(1, nE);
iNode = 1;
for iEdge = 1:length(edgeType)
    n_new = edgeType(iEdge);
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
nN = length(nodeToEdgeList);

% generate gaussian and centroid point coords 
gaussCoords = [vertexX; vertexY]*gaussCoordsSimplex';
gaussX = gaussCoords(1, :);
gaussY = gaussCoords(2, :);
centroidX = gaussCoords(1, 1);
centroidY = gaussCoords(2, 1);


%% -- Solution of underdetermined simplex profile --

nCoefs = (nPoly + 1)*(nPoly + 2)/2;

% set up the particular solution and basis vectors
u = zeros(nCoefs, 1);
v = zeros(nCoefs, 1);

% get powers of x and y in the order
% [1, x, x^2, x^3, y, x*y, x^2*y, y^2, ... ]
px = [0, 1, 2, 3, 0, 1, 2, 0, 1, 0];
py = [0, 0, 0, 0, 1, 1, 1, 2, 2, 3];

% get collocation coordinates
x = [vertexX nodeX]';
y = [vertexY nodeY]';

% add gradient coordinates (duplicated by number of dimensions)
vertexGradList = ...
    vertexList(vertexType(vertexList) == 0);
x = [x; vertexX(vertexGradList)'; vertexX(vertexGradList)'];
y = [y; vertexY(vertexGradList)'; vertexY(vertexGradList)'];
nG = length(vertexGradList);

% identify each section
iVertVal = 1:nV;
iNodeVal = nV + (1:nN);
iVertGradX = nV + nN + (1:nG);
iVertGradY = nV + nN + nG + (1:nG);

% build the initial (zeroth-derivative) Vandermonde matrix
[X, PX] = ndgrid(x, px);
[Y, PY] = ndgrid(y, py);
A = X.^PX .* Y.^PY;

% correct the x-gradient terms
PXD = PX(iVertGradX, :);
XD = X(iVertGradX, :);
PYD = PY(iVertGradX, :);
YD = Y(iVertGradX, :);
A(iVertGradX, :) = PXD.*XD.^max(0, PXD - 1) .* YD.^PYD;

% correct the y-gradient terms
PXD = PX(iVertGradY, :);
XD = X(iVertGradY, :);
PYD = PY(iVertGradY, :);
YD = Y(iVertGradY, :);
A(iVertGradY, :) = XD.^PXD .* PYD.*YD.^max(0, PYD - 1);

fprintf('\noriginal matrix:\n');
fprintf('         %.4f_FLOAT, %.4f_FLOAT, %.4f_FLOAT, %.4f_FLOAT, &\n', A);


% assign collocation values
bVertVal = 0.*iVertVal;
bVertVal(vertexType == 0) = fluidVertexValue;
bVertVal(vertexType ~= 0) = boundaryPointValue;
bNodeVal = 0.*iNodeVal;
bNodeVal(nodeType == 1) = boundaryPointValue;
bNodeVal(nodeType ~= 1) = cutEdgeNodeValue;
bVertGradX = 0.*iVertGradX + fluidVertexGradient(1);
bVertGradY = 0.*iVertGradY + fluidVertexGradient(2);

% form the RHS
b = [bVertVal bNodeVal bVertGradX bVertGradY]';

fprintf('\nRHS:\n');
fprintf('         %.14f_FLOAT, &\n', b);

% perform QR factorisation
usePivoting = true;
if usePivoting
    [Q,R,E] = qr(A');
else
    [Q,R] = qr(A');
    E = eye(size(A, 1));
end

fprintf('\n');

%% null vector (last column of Q)

qn = Q(:, end);

% check consistency with svd-based function
if ~all( abs(qn - null(E*A)) < tol ) && ...
        ~all( abs(qn + null(E*A)) < tol )
    fprintf('discrepancy in null space computation.\n');
end

fprintf('\nnull space:\n');
fprintf('          %.14f_FLOAT, &\n', qn);


%% particular solution

% n.b. permutation 
cp = R(1:end-1 ,:)'\(E'*b);
cp = Q(:, 1:end-1)*cp;

fprintf('\nparticular solution:\n');
fprintf('          %.14f_FLOAT, &\n', cp);
fprintf('\n');


%% compound profile elements

fprintf('\n');
aL = centroidX.^px .* centroidY.^py;
fprintf('deferred PM row:  %.14f_FLOAT\n', aL*qn);

bL = cellPointValue - aL*cp;
fprintf('deferred PM RHS: %.14f_FLOAT\n', bL);

[X, PX] = ndgrid( gaussX, px );
[Y, PY] = ndgrid( gaussY, py );
W = ndgrid( gaussWeightsSimplex, px );
aG = sum( W.* X.^PX .* Y.^PY, 1 );
fprintf('deferred CIA row:  %.14f_FLOAT\n', aG*qn);

bG = cellIntAve - aG*cp;
fprintf('deferred CIA RHS: %.14f_FLOAT\n', bG);


%% simple final solution (ignores the middle point)

s = bG/(aG*qn);
c = cp + qn*s;

fprintf('\nsolved coefs:\n');
fprintf('          %.14f_FLOAT, &\n', c);
fprintf('\n');


%% plotting

nSimplex = 1;
simplexList = 1:1;
plotPrimitiveProfile
