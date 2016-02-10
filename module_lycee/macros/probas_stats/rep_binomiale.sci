// Copyright (C) 2011 - SCILAB ENTERPRISES - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function r=rep_binomiale(n,p,varargin)
    [lhs,rhs]=argn();
    if rhs<>2&rhs<>3 then error("La fonction ""rep_binomiale"" doit avoir deux ou trois arguments."); end
    if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<1 then
        error("Le premier argument de la fonction ""rep_binomiale"" doit être un entier positif.");
    end
    if type(p)<>1|size(p,"*")<>1|p<0|p>1 then
        error("Le deuxième argument de la fonction ""rep_binomiale"" doit être un nombre entre 0 et 1.");
    end
    if rhs==2 then
        un=ones(1,n+1);
        r=cdfbin("PQ",0:n,n*un,p*un,(1-p)*un);
    else
        t=varargin(1);
        if type(t)<>1|size(t,"*")<>1|t<0 then
            error("Le troisième argument de la fonction ""rep_binomiale"" doit être un nombre positif.");
        end
        if t>n then
            r=1;
        else
            t=floor(t);
            r=cdfbin("PQ",t,n,p,1-p);
        end
    end
endfunction
