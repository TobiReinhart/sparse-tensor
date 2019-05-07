#function to compute the symbol of a rank deficient matrix 

with(LinearAlgebra);
with(combinat);
with(Threads);
with(ListTools);

#evaluate all constants randomly
evalRand := proc(M::Matrix)
    vars := convert(indets(M) minus {k__0,k__1,k__2,k__3},list);
    fRand := rand(-1000..1000);
    evalL := zip((a,b) -> a = b, vars, [seq(fRand(), i = 1..nops(vars))]);
    (simplify(subs(evalL,M)), evalL);
    end proc;

evalRandQuad := proc(Lin::Matrix, Quad::list)
    varsLin := indets(Lin) minus {k__0,k__1,k__2,k__3};
    varsQuad := map(x -> indets(x) minus {k__0,k__1,k__2,k__3}, Quad);
    vars := convert( `union`(varsLin,op(varsQuad)) ,list);
    fRand := rand(-1000..1000);
    evalL := zip((a,b) -> a = b, vars, [seq(fRand(), i = 1..nops(vars))]);
    QuadL := map(x -> simplify(subs(evalL,Quad)), evalL);
    LinM := simplify(subs(evalL, Lin)); 
    (LinM, QuadL);
    end proc;

#construct a random SubMatrix 
randSubMatrixQuad := proc(Lin::Matrix, Quad::list)
    n := RowDimension(Lin);
    rowList := [seq(1..n)];
    QuadL := map(x -> SubMatrix(x,randcomb(rowList,n-4),randcomb(rowList,n-4)), Quad);
    LinM := SubMatrix(Lin,randcomb(rowList,n-4),randcomb(rowList,n-4));
    if  Rank(LinM) < n-4 then randSubMatrixQuad(Lin,Quad) else 
        zeroList := Map(Rank(),QuadL);
        Q3 := zip((x,y) -> [x,y], QuadL, zeroList);
        Q4 := remove(x -> x[2] < n-4, Q3); 
        if nops(Q4) = 0 then randSubMatrixQuad(M); else (LinM, Q4) ; end if;  
    end if;
    end proc;

randSubMatrix := proc(M::list)
    n := RowDimension(M);
    rowList := [seq(1..n)];
    M2 := SubMatrix(M,randcomb(rowList,n-4),randcomb(rowList,n-4));
    if Rank(M2) = n-4 then M2; else randSubMatrix(M); end if;
    end proc;

#compute the principal polynomial for one matrix (linear order) and one rand combination 
linPoly2 := proc(M::Matrix)
    n := RowDimension(M);
    (MRand, evalL) := evalRand(M);
    SubM := randSubMatrix(MRand);
    Pol := factor(Determinant(SubM, method=multivar));
    if degree(Pol) = 2*n-8 then (Pol, evalL); else gcd(Pol,linPoly(M)); end if;
    end proc;

#parallel implementation for fixed number of random submatrices
linPolyParallel := proc(M::Matrix,i::integer)
    n := RowDimension(M);
    (MRand, evalL) := evalRand(M);
    SubMList := map(x -> randSubMatrix(MRand), [seq(1..i)]);
    PolyList := Map(x -> factor(Determinant(x, method=multivar)), SubMList);
    Poly := foldr(gcd,0,op(PolyList));
    (Poly,evalL);
    end proc;

#now the parallel implementation for a fixed number of evaluation points
linPoly := proc(M::Matrix, i::integer)
    Map(x -> linPoly2(M), [seq(1..i)]);
    end proc;


#construct the n^2 quadratic matrices from the linear matrix and the n (factors in front of HA) quadratic matrices 
mkQuadMatrices := proc(Lin::Matrix, Quad::list)
    n := RowDimension(Lin); 
    LinCols := [Column(Lin,[seq(1..n)])];
    QuadCols := map(x -> [Column(x,[seq(1..n)])],Quad);
    MatList := map(x -> zip((y,z) -> Matrix(subs(y = z, LinCols)), LinCols, x), QuadCols); 
    end proc;

eval1MinorL := proc(Lin::Matrix, Quad::list)
    matList := mkQuadMatrices(Lin,Quad);
    (matListRand, evalL) := evalRandQuad(matList);
    (subMatLin, subMatList) := randSubMatrixQuad(matListRand);
    PolyLin := factor(Determinant(Lin, method=multivar));
    PolyQuad := Map(x -> factor(add(Determinant(i, method=multivar)), i in x), subMatList);
    (PolyLin,PolyQuad);
    end proc; 

#how can we compute the gcd perturbatively ?












