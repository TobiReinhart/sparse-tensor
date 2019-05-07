#function to compute the symbol of a rank deficient matrix 

with(LinearAlgebra);
with(combinat);
with(Threads);

#evaluate all constants randomly
evalRand := proc(M::Matrix)
    vars := convert(indets(M) minus {k__0,k__1,k__2,k__3},list);
    fRand := rand(-1000..1000);
    evalL := zip((a,b) -> a = b, vars, [seq(fRand(), i = 1..nops(vars))]);
    (simplify(subs(evalL,M)), evalL);
    end proc;

#construct a random SubMatrix 
randSubMatrix := proc(M::Matrix)
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
    Poly := foldr(gcd,0,PolyList);
    (Poly,evalL);
    end proc;

#now the parallel implementation for a fixed number of evaluation points
linPoly := proc(M::Matrix, i::integer)
    Map(x -> linPoly2(M), [seq(1..i)]);
    end proc;











