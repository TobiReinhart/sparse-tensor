#function to compute the symbol of a rank deficient matrix 
CausalAnalysis := module()

export evalRand, evalRandFull, randSubMatrix, randSubMatrixN, linPoly, linPolyN, linPolyNGCD, randSubMatrixQuad, randSubMatrixQuadN, evalRandQuad, prodTrace, quadPoly, solveMatrixEqns, quadPolyN ;

option package;

uses LinearAlgebra, combinat;

#evaluate all constants except the ks randomly
evalRand := proc(M::Matrix)
    uses LinearAlgebra;
    vars := convert(indets(M) minus {k[0],k[1],k[2],k[3]},list);
    fRand := rand(-1000..1000);
    evalL := zip((a,b) -> a = b, vars, [seq(fRand(), i = 1..nops(vars))]);
    simplify(subs(evalL,M));
    end proc;

#evaluate all constants randomly
evalRandFull := proc(M::Matrix)
    uses LinearAlgebra;
    vars := convert(indets(M),list);
    fRand := rand(-1000..1000);
    evalL := zip((a,b) -> a = b, vars, [seq(fRand(), i = 1..nops(vars))]);
    simplify(subs(evalL,M));
    end proc;

#solve a set of matrix eqns of the form (A.x = 0)
solveMatrixEqns := proc(M::Matrix)
    uses LinearAlgebra;
    colsM := ColumnDimension(M);
    rowsM := RowDimension(M);
    zeroVec := ZeroVector(rowsM);
    sol := convert(LinearSolve(M,zeroVec), list);
    vars := [seq(x[i],i=1..colsM)];
    evalL := zip((x,y) -> x = y, vars, sol); 
    end proc;

#construct a random SubMatrix 
randSubMatrix := proc(M::Matrix)
    uses LinearAlgebra, combinat;
    n := RowDimension(M);
    rowList := [seq(1..n)];
    M2 := SubMatrix(M,randcomb(rowList,n-4),randcomb(rowList,n-4));
    M2Rand := evalRandFull(M2); 
    if Rank(M2Rand) = n-4 then M2; else randSubMatrix(M); end if;
    end proc;

#construct n different submatrices
randSubMatrixN := proc(M::Matrix, n::integer)
    uses LinearAlgebra, combinat;
    rowDim := RowDimension(M);
    rowList := [seq(1..rowDim)];
    l := [seq(1..n)];
    SubComb := [randcomb(rowList, rowDim-4), randcomb(rowList, rowDim-4)];
    MList := [];
    while nops(MList) < n do 
        newSubM := SubMatrix(M,SubComb[1], SubComb[2]);
        newSubMRand := evalRandFull(newSubM);
        if Rank(newSubMRand) = rowDim-4 then
            MList:=ListTools:-MakeUnique([op(MList), newSubM]);
        end if;
        SubComb := [randcomb(rowList,rowDim-4), randcomb(rowList,rowDim-4)];
    end do; 
    MList;
    end proc;

#compute the principal polynomial for one matrix (linear order) and one rand combination 
linPoly := proc(M::Matrix)
    uses LinearAlgebra;
    MRand := evalRand(M);
    SubM := randSubMatrix(MRand);
    Pol := simplify(Determinant(SubM, method=multivar));
    end proc;

linPolyN := proc(M::Matrix, n::integer)
    uses LinearAlgebra, Threads;
    l := [seq(1..n)];
    MRand := evalRand(M);
    SubML := randSubMatrixN(MRand, n);
    PolyL := Map(x -> simplify(Determinant(x, method = multivar)),subML);
    end proc; 

linPolyNGCD := proc(M::Matrix, n::integer)
    uses LinearAlgebra, Threads;
    l := [seq(1..n)];
    MRand := evalRand(M);
    SubML := randSubMatrixN(MRand, n);
    PolyL := Map(x -> simplify(Determinant(x, method = multivar)),subML);
    foldr(gcd,0,PolyL);
    end proc; 

#now quadratic order

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
    LinMRand := evalRandFull(LinM);
    if  Rank(LinMRand) < n-4 then 
        randSubMatrixQuad(Lin,Quad); 
        else (LinM, QuadL); 
    end if;
    end proc;

randSubMatrixQuadN := proc(M::Matrix, Q::list, n::integer)
    uses LinearAlgebra, combinat;
    rowDim := RowDimension(M);
    rowList := [seq(1..rowDim)];
    l := [seq(1..n)];
    SubComb := [randcomb(rowList, rowDim-4), randcomb(rowList, rowDim-4)];
    MList := [];
    while nops(MList) < n do 
        newSubM := SubMatrix(M,SubComb[1],SubComb[2]);
        newSubQ := map(x -> SubMatrix(x,SubComb[1], SubComb[2]),Q);
        newSubMRand := evalRandFull(newSubM);
        if Rank(newSubMRand) = rowDim-4 then
            MList:=ListTools:-MakeUnique([op(MList), [newSubM, newSubQ]]);
        end if;
        SubComb := [randcomb(rowList,rowDim-4), randcomb(rowList,rowDim-4)];
    end do; 
    MList;
    end proc;

#evaluate randomly in the linear order constants
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
    simplify(add(l));
    end proc;

#polynomial up to quadratic order
quadPoly := proc(M::Matrix, Q::list)
    uses LinearAlgebra;
    (randM, randQ) := evalRandQuad(M,Q);
    (randSubM, randSubQ) := randSubMatrixQuad(randM, randQ);
    quadPolySubF(randSubM, randSubQ);
    end proc;

quadPolySubF := proc(M::Matrix, Q::list)
    uses LinearAlgebra;
    subMInv := MatrixInverse(M, method = polynom);
    polyL := map(x -> prodTrace(M,x), Q);
    fac1 := Determinant(M, method = multivar);
    Poly := map(x -> factor(fac1*x),polyL);
    (fac1,Poly);
    end proc;

quadPolyN := proc(M::Matrix, Q::list, n::integer)
    uses LinearAlgebra, Threads;
    l := [seq(1..n)];
    (randM, randQ) := evalRandQuad(M,Q);
    SubML := randSubMatrixQuadN(randM, randQ, n);
    PolyL := Map(x -> quadPolySubF(op(x)), subML);
    end proc; 


end module;

