// Copyright (C) 2011 - SCILAB ENTERPRISES - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=loi_binomiale(n,p,varargin)
    [lhs,rhs]=argn();
    if rhs<>2&rhs<>3 then error("La fonction ""loi_binomiale"" doit avoir deux ou trois arguments."); end
    if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<1 then
        error("Le premier argument de la fonction ""loi_binomiale"" doit être un entier positif.");
    end
    if type(p)<>1|size(p,"*")<>1|p<0|p>1 then
        error("Le deuxième argument de la fonction ""loi_binomiale"" doit être un nombre entre 0 et 1.");
    end
    un=ones(1,n+1);
    r=cdfbin("PQ",0:n,n*un,p*un,(1-p)*un);
    r(2:n+1)=r(2:n+1)-r(1:n);
    if rhs==3 then
        k=varargin(1);
        if type(k)<>1|size(k,"*")<>1|int(k)-k<>0|k<0|k>n then
            error("Le troisième argument de la fonction ""loi_binomiale"" doit être un entier entre 0 et le premier argument.");
        end
        r=r(k+1);
    end
endfunction
