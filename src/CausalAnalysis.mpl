#function to compute the symbol of a rank deficient matrix 
CausalAnalysis := module()

export evalRand, evalRandFull, randSubMatrix, randSubMatrixN, linPoly, linPolyN, linPolyNGCD, randSubMatrixQuad, randSubMatrixQuadN,
       evalRandQuad, prodTrace, quadPoly, solveMatrixEqns, quadPolyN, calc, quadPolyNExact, calcExact, quadPolyN2, calc2, preFLin, preFQuad, randSubMatrixLabel, randSubMatrixQuadLabel, calc3, calc4, QUadPolyH ;

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

randSubMatrixLabel := proc(M::Matrix)
    uses LinearAlgebra, combinat;
    n := RowDimension(M);
    rowList := [seq(1..n)];
    rows := randcomb(rowList,n-4);
    cols := randcomb(rowList,n-4);
    rows2 := remove(x -> member(x,rows), rowList);
    cols2 := remove(x -> member(x,cols), rowList);
    M2 := SubMatrix(M,rows,cols);
    M2Rand := evalRandFull(M2); 
    if Rank(M2Rand) = n-4 then (M2,rows2,cols2); else randSubMatrixLabel(M); end if;
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
    uses LinearAlgebra;
    l := [seq(1..n)];
    MRand := evalRand(M);
    SubML := randSubMatrixN(MRand, n);
    PolyL := map(x -> simplify(Determinant(x, method = fracfree)),subML);
    end proc; 

linPolyNGCD := proc(M::Matrix, n::integer)
    uses LinearAlgebra;
    l := [seq(1..n)];
    MRand := evalRand(M);
    SubML := randSubMatrixN(MRand, n);
    PolyL := map(x -> simplify(Determinant(x, method = fracfree)),subML);
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
        else [LinM, QuadL]; 
    end if;
    end proc;

randSubMatrixQuadLabel := proc(Lin::Matrix, Quad::list)
    uses LinearAlgebra, combinat;
    n := RowDimension(Lin);
    rowList := [seq(1..n)];
    rowComb := randcomb(rowList,n-4);
    colComb := randcomb(rowList,n-4);
    rows2 := remove(x -> member(x,rowComb), rowList);
    cols2 := remove(x -> member(x,colComb), rowList);
    QuadL := map(x -> SubMatrix(x,rowComb,colComb), Quad);
    LinM := SubMatrix(Lin,rowComb,colComb);
    LinMRand := evalRandFull(LinM);
    if  Rank(LinMRand) < n-4 then 
        randSubMatrixQuadLabel(Lin,Quad); 
        else (LinM, QuadL, rows2, cols2); 
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
    QuadL := map(x -> subs(evalL,x), Quad);
    LinM := subs(evalL, Lin); 
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
    randSubL := randSubMatrixQuad(randM, randQ);
    randSubM := randSubL[1];
    randSubQ := randSubL[2];
    quadPolySubF(randSubM, randSubQ);
    end proc;

#for already randomly eavluated matrix
quadPolyH := proc(M::Matrix, Q::list)
    uses LinearAlgebra; 
    PolyL := quadPolySubF(M, Q);
    Lin := PolyL[1];
    QuadL := PolyL[2];
    QuadSum := simplify(zip((x,y) -> x * H[y], QuadL, [seq(0..20)]));
    (Lin,QuadSum);
    end proc;

quadPolySubF := proc(M::Matrix, Q::list)
    uses LinearAlgebra;
    print("calculating determinant");
    fac1 := Determinant(M, method = fracfree);
    print("determinant calculated");
    print("inverting matrix");
    subMInv := LinearAlgebra:-MatrixInverse(M, method = polynom);
    print("matrix inverted");
    print("calculating prod trace");
    subMInv2 := fac1 * subMInv;
    polyL := map(x -> prodTrace(subMInv2,x), Q);
    print("another one finsihed!");
    [simplify(fac1),simplify(polyL)];
    end proc;

quadPolyN := proc(M::Matrix, Q::list, n::integer)
    uses LinearAlgebra;
    l := [seq(1..n)];
    (randM, randQ) := evalRandQuad(M,Q);
    SubML := randSubMatrixQuadN(randM, randQ, n);
    print("list is constructed");
    PolyL := map(x -> quadPolySubF(x[1], x[2]), SubML);
    end proc; 

quadPolyNExact := proc(M::Matrix, Q::list, n::integer)
    uses LinearAlgebra;
    l := [seq(1..n)];
    SubML := randSubMatrixQuadN(M, Q, n);
    print("list is constructed");
    PolyL := map(x -> quadPolySubF(x[1], x[2]), SubML);
    end proc; 

calc := proc(n::integer)
    read "RomAll.txt":
    sol := solveMatrixEqns(QuadKin):
    LinSymSol := subs(sol,LinSym):
    QuadSymSol := subs(sol, QuadSymList):
    quadPolyN(LinSymSol, QuadSymSol, n);
    end proc;    

calcExact := proc(n::integer)
    read "RomAll.txt":
    sol := solveMatrixEqns(QuadKin):
    LinSymSol := subs(sol,LinSym):
    QuadSymSol := subs(sol, QuadSymList):
    quadPolyNExact(LinSymSol, QuadSymSol, n);
    end proc;    

quadPolyN2 := proc(M::Matrix, Q::list, n :: integer, m:: integer)
    uses LinearAlgebra;
    l := [seq(1..n)];
    (randM, randQ) := evalRandQuad(M,Q);
    SubML := randSubMatrixQuadN(randM, randQ, n);
    SubLinL := randSubMatrixN(randM, m);
    print("lists are constructed");
    print("computing linear Polynomials");
    PolyLin := map(x -> simplify(Determinant(x, method = fracfree)),SubLinL); 
    print("Linear Polynomials calculated");
    PolyQuad := map(x -> quadPolySubF(x[1], x[2]), SubML);
    [PolyLin, PolyQuad];
    end proc; 

calc2 := proc(n::integer, m::integer)
    read "RomAll.txt":
    sol := solveMatrixEqns(QuadKin):
    LinSymSol := subs(sol,LinSym):
    QuadSymSol := subs(sol, QuadSymList):
    quadPolyN2(LinSymSol, QuadSymSol, n, m);
    end proc; 

#compute the prefactor of the polyinomial w.r.t. the given subMatrix 
#linear order 

preFLin := proc(rows::list, cols::list)
    uses LinearAlgebra;
    read "RankDef.txt";
    X := Determinant(SubMatrix(RankDefLin, rows, [1,2,3,4]));
    Y := Determinant(SubMatrix(RankDefLin, cols, [1,2,3,4]));
    simplify(X*Y);
    end proc;

#quadratic order
preFQuad := proc(rows::list, cols::list)
    uses LinearAlgebra;
    read "RankDef.txt";
    X := SubMatrix(RankDefLin, rows, [1,2,3,4]);
    Y := SubMatrix(RankDefLin, cols, [1,2,3,4]);
    X2 := SubMatrix(RankDefQuad, rows, [1,2,3,4]);
    Y2 := SubMatrix(RankDefQuad, cols, [1,2,3,4]);
    Lin := simplify(Determinant(X)*Determinant(Y));
    X2Inv := LinearAlgebra:-MatrixInverse(X2, method=polynom);
    Y2Inv := LinearAlgebra:-MatrixInverse(Y2, method=polynom);
    Trace1 := prodTrace(X2Inv,X);
    Trace2 := prodTrace(Y2Inv,Y);
    (Lin,simplify(Lin*(Trace1+Trace2)));
    end proc;

calc3 := proc()
    read "RomAll.txt":
    sol := solveMatrixEqns(LinKin):
    LinSymSol := subs(sol,LinSym):
    MRand := evalRand(LinSymSol);
    (subM, rows, cols) := randSubMatrixLabel(MRand);
    Poly := simplify(Determinant(subM, method=fracfree));
    PreFac := preFLin(rows, cols);
    (Poly, PreFac);
    end proc; 

calc4 := proc()
    read "RomAll.txt":
    sol := solveMatrixEqns(QuadKin):
    LinSymSol := subs(sol,LinSym):
    QuadSymSol := subs(sol, QuadSymList);
    (randM, randQ) := evalRandQuad(LinSymSol,QuadSymSol);
    (M,Q,rows,cols) := randSubMatrixQuadLabel(randM, randQ);
    (PolyLin, PolyQuad) := quadPolyH(M,Q);
    (PreFacLin, PreFacQuad) := preFQuad(rows, cols);
    (PolyLin, PolyQuad, PreFacLin, PreFacQuad);
    end proc;  

end module;

