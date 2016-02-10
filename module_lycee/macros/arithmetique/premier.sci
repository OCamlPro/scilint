// Copyright (C) 2002-2008 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function [r,i]=premier(n,varargin)
  [lhs,rhs]=argn();
  if rhs<>1 then error("La fonction ""premier"" doit avoir un argument."); end
  if type(n)<>1|size(n,"*")<>1|int(n)-n<>0|n<0 then
    error("L''argument de la fonction ""premier"" doit être un entier positif ou nul.");
  end
  c=3.1;

 	p=1:n;
	i=2;
	while i<=sqrt(n)
		if p(i)<>0
			p(2*p(i):p(i):n)=0;
  		end
  		i=i+1;
	end
 	p(p<=1)=[];
  i=0;
  if p($)==n then r=%T; i=size(p,"*"); else r=%F; end
endfunction

