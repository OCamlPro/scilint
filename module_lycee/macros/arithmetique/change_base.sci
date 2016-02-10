// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function n=change_base(m,b1,b2,varargin)
  [lhs,rhs]=argn();
  if rhs<>3 then error("La fonction ""change_base"" doit avoir trois arguments."); end
  // le premier argument est une chaîne de caractères
  if type(m)==10 & size(m,"*")==1 then
    if type(b1)<>1 | size(b1,"*")<>1 | int(b1)-b1<>0 | b1<2 | (b1>10&b1<16) | b1>16 then
      error("Le deuxième argument de la fonction ""change_base"" doit être un entier compris entre 2 et 10 ou égal à 16.");
    end
    if type(b2)<>1 | size(b2,"*")<>1 | int(b2)-b2<>0 | b2<2 | (b2>10&b2<16) | b2>16 then
      error("Le troisième argument de la fonction ""change_base"" doit être un entier compris entre 2 et 10 ou égal à 16.");
    end
    nm=convstr(m,"l");
    nm=(ascii(nm)-48)';
    if min(nm)<0 | max(nm)>=b1 then
      error("Le premier argument de la fonction ""change_base"" ne représente pas un entier positif en base "+string(b1));
    end
    nn=change_base2(nm,b1,b2);
    n=ascii(nn+48);
    n=convstr(n,"u");
  elseif type(m)==1 & find(size(m)==1)<>[] & min(m)>=0 & int(m)-m==0 then
  // le premier argument est un vecteur d'entiers positifs
      if type(b1)<>1 | size(b1,"*")<>1 | int(b1)-b1<>0 | b1<2 then
        error("Le deuxième argument de la fonction ""change_base"" doit être un entier plus grand que 1.");
      end
      if type(b2)<>1 | size(b2,"*")<>1 | int(b2)-b2<>0 | b2<2 then
        error("Le troisième argument de la fonction ""change_base"" doit être un entier plus grand que 1.");
      end
      if min(m)<0 | max(m)>=b1 then
        error("Le premier argument de la fonction ""change_base"" ne représente pas un entier positif en base "+string(b1));
      end
      m=matrix(m,-1,1);
      n=change_base2(m,b1,b2); n=n';
  else
    error("Le premier argument de la fonction ""change_base"" doit être une chaîne de caractères,..
ou un vecteur d''entiers positifs.");
end
endfunction

function n=change_base2(m,b1,b2)
  // m est le vecteur d'entiers en base b1
  // n est le vecteur d'entiers en base b2
  lm=size(m,"*");
  p=cumprod([1,b1*ones(1,lm-1)]);
  n1=p(lm:-1:1)*m;
  // conversion de m en base b2
  k=int(log(n1)/log(b2));
  for i=k:-1:1
    n(k-i+1)=int(n1/b2^i);
    n1=n1-n(k-i+1)*b2^i;
  end
  n(k+1)=n1;
endfunction

