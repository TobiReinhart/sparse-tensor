#function to compute the symbol of a rank deficient matrix 
CausalAnalysis := module()

export evalRand, randSubMatrix, linPoly, randSubMatrixQuad, evalRandQuad, prodTrace, quadPoly, solveMatrixEqns, quadPolyExact, linPolyExact, quadPolyN;

option package;

#evaluate all constants randomly
evalRand := proc(M::Matrix)
    uses LinearAlgebra;
    vars := convert(indets(M) minus {k[0],k[1],k[2],k[3]},list);
    fRand := rand(-1000..1000);
    evalL := zip((a,b) -> a = b, vars, [seq(fRand(), i = 1..nops(vars))]);
    (simplify(subs(evalL,M)), evalL);
    end proc;

evalRandFull := proc(M::Matrix)
    uses LinearAlgebra;
    vars := convert(indets(M),list);
    fRand := rand(-1000..1000);
    evalL := zip((a,b) -> a = b, vars, [seq(fRand(), i = 1..nops(vars))]);
    simplify(subs(evalL,M));
    end proc;

#construct a random SubMatrix 
randSubMatrix := proc(M::Matrix)
    uses LinearAlgebra, combinat;
    n := RowDimension(M);
    rowList := [seq(1..n)];
    M2 := SubMatrix(M,randcomb(rowList,n-4),randcomb(rowList,n-4));
    if Rank(M2) = n-4 then M2; else randSubMatrix(M); end if;
    end proc;

randSubMatrixExact := proc(M::Matrix)
    uses LinearAlgebra, combinat;
    n := RowDimension(M);
    rowList := [seq(1..n)];
    M2 := SubMatrix(M,randcomb(rowList,n-4),randcomb(rowList,n-4));
    M2Num := evalRandFull(M2);
    if Rank(M2Num) = n-4 then M2; else randSubMatrixExact(M); end if;
    end proc;

#compute the principal polynomial for one matrix (linear order) and one rand combination 
linPoly := proc(M::Matrix)
    uses LinearAlgebra;
    (MRand, evalL) := evalRand(M);
    SubM := randSubMatrix(MRand);
    Pol := factor(Determinant(SubM, method=multivar));
    end proc;

#try to compute the polynomial without making approximations, inserting random values -> not possible with 26GB
linPolyExact := proc(M::Matrix)
    uses LinearAlgebra;
    SubM := randSubMatrixExact(M);
    Pol := factor(Determinant(SubM, method=multivar));
    end proc;

#construct the subMatrices for the linear Matrix and the list of quadratic matrices
#linear subMatrix must have full rank
randSubMatrixQuad := proc(Lin::Matrix, Quad::list)
    uses LinearAlgebra, combinat;
    n := RowDimension(Lin);
    rowList := [seq(1..n)];
    rowComb := randcomb(rowList,n-4);
    colComb := randcomb(rowList,n-4);
    QuadL := map(x -> SubMatrix(x,rowComb,colComb), Quad);
    LinM := SubMatrix(Lin,rowComb,colComb);
    if  Rank(LinM) < n-4 then 
        randSubMatrixQuad(Lin,Quad); 
        else (LinM, QuadL); 
    end if;
    end proc;

randSubMatrixQuadExact := proc(Lin::Matrix, Quad::list)
    uses LinearAlgebra, combinat;
    n := RowDimension(Lin);
    rowList := [seq(1..n)];
    rowComb := randcomb(rowList,n-4);
    colComb := randcomb(rowList,n-4);
    QuadL := map(x -> SubMatrix(x,rowComb,colComb), Quad);
    LinM := SubMatrix(Lin,rowComb,colComb);
    LinMNum := evalRandFull(LinM);
    if  Rank(LinMNum) < n-4 then 
        randSubMatrixQuadExact(Lin,Quad); 
        else (LinM, QuadL); 
    end if;
    end proc;

#evaluate randomly in the linear constants as this is probably 
evalRandQuad := proc(Lin::Matrix, Quad::list)
    uses LinearAlgebra;
    varsLin := convert(indets(Lin) minus {k[0],k[1],k[2],k[3]},list);
    fRand := rand(-1000..1000);
    evalL := zip((a,b) -> a = b, varsLin, [seq(fRand(), i = 1..nops(varsLin))]);
    QuadL := map(x -> simplify(subs(evalL,x)), Quad);
    LinM := simplify(subs(evalL, Lin)); 
    (LinM, QuadL);
    end proc;

#compute the trace of a mutrix product 
prodTrace := proc(M::Matrix, Q::Matrix)
    uses LinearAlgebra;
    size := min(RowDimension(M),ColumnDimension(Q));
    rowsM := [Row(M,[seq(1..size)])];
    colsQ := [Column(Q,[seq(1..size)])];
    l := zip((x,y) -> Multiply(x,y), rowsM, colsQ);
    factor(add(l));
    end proc;

#better use area metric dofs 
quadPoly := proc(M::Matrix, Q::list)
    uses LinearAlgebra;
    (randM, randQ) := evalRandQuad(M,Q);
    (randSubM, randSubQ) := randSubMatrixQuad(randM, randQ);
    subMInv := MatrixInverse(randSubM, method = polynom);
    polyL := zip((x,i) -> H[i] * prodTrace(subMInv,x), randSubQ, [seq(1..21)]);
    poly := add(polyL);
    fac1 := Determinant(randSubM, method = multivar);
    simplify(fac1*poly);
    end proc;

quadPolyExact := proc(M::Matrix, Q::list)
    uses LinearAlgebra;
    (randSubM, randSubQ) := randSubMatrixQuad(M, Q);
    subMInv := MatrixInverse(randSubM, method = polynom);
    polyL := zip((x,i) -> H[i] * prodTrace(subMInv,x), randSubQ, [seq(1..21)]);
    poly := add(polyL);
    fac1 := Determinant(randSubM, method = multivar);
    simplify(fac1*poly);
    end proc;

quadPoly2 := proc(M::Matrix, Q::list)
    uses LinearAlgebra;
    (randSubM, randSubQ) := randSubMatrixQuad(randM, randQ);
    subMInv := MatrixInverse(randSubM, method = polynom);
    polyL := map(x -> prodTrace(subMInv,x), randSubQ);
    fac1 := Determinant(randSubM, method = multivar);
    map(x -> simplify(fac1*x),polyL);
    end proc;

quadPolyN := proc(M::Matrix, Q::list, n::integer)
    uses LinearAlgebra, Threads;
    l := [seq(1..n)];
    (randM, randQ) := evalRandQuad(M,Q);
    PolyL := Map(x -> quadPoly2(M,Q), l);
    foldr((x1,x2) -> zip((y,z) -> gcd(y,z), x1, x2), 0, PolyL);
    end proc;

solveMatrixEqns := proc(M::Matrix)
    uses LinearAlgebra;
    colsM := ColumnDimension(M);
    rowsM := RowDimension(M);
    zeroVec := ZeroVector(rowsM);
    sol := convert(LinearSolve(M,zeroVec), list);
    vars := [seq(x[i],i=1..colsM)];
    evalL := zip((x,y) -> x = y, vars, sol); 
    end proc;

end module;
