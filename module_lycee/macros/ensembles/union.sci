// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

// Fonction qui garde la fonction "union" standard de Scilab
// "union_orig" ci-dessous

function [x,ka,kb]=union(ens1,ens2,varargin)
  [lhs,rhs]=argn();
  if rhs<2|rhs>3 then
    error("La fonction ""union"" doit avoir deux ou trois arguments.");
  end
  if rhs==2&typeof(ens1)<>"ens"&typeof(ens1)<>"ens" then
    [x,ka,kb]=union_orig(ens1,ens2);
  elseif rhs==3&typeof(ens1)<>"ens"&typeof(ens1)<>"ens" then
    orient=varargin(1);
    [x,ka,kb]=union_orig(ens1,ens2,orient);
  else
    if rhs<>2 then error("La fonction ""union"" doit avoir deux arguments."); end
    if typeof(ens1)<>"ens"|typeof(ens2)<>"ens" then
      error("Les arguments de la fonction ""union"" doivent être des ensembles.");
    end
    v1=ens1.elem; v2=ens2.elem;
    x=ensemble_vecteur([v1;v2]);
  end
endfunction

function [x,ka,kb]=union_orig(a,b,orient)
// returns the union of  unique components of  vector a and b
// author Serge Steer INRIA
  if argn(2)<3 then
    if argn(1)==1 then
      x=unique([a(:);b(:)])
      x=x';
    else
      kab=[1:size(a,'*'), -(1:size(b,'*'))]
      [x,k]=unique([a(:);b(:)])
      x=x'
      kab=kab(k)
      ka=kab(kab>0)
      kb=-kab(kab<0)
    end
  else
    if  orient==1|orient=="r" then
      if argn(1)==1 then
	x=unique([a;b],'r')
      else
	kab=[1:size(a,'r'), -(1:size(b,'r'))]
	[x,k]=unique([a;b],'r')
	kab=kab(k)
	ka=kab(kab>0)
	kb=-kab(kab<0)
      end
    elseif orient==2|orient=="c" then
      if argn(1)==1 then
	x=unique([a b],'c')
      else
	kab=[1:size(a,'c'), -(1:size(b,'c'))]
	[x,k]=unique([a b],'c')
	kab=kab(k)
	ka=kab(kab>0)
	kb=-kab(kab<0)
      end

    else
      error(msprintf(gettext("%s: Wrong value for input argument #%d: %d,''%s'',%d or ''%s'' expected\n"),'unique',3,1,"r",2,"c"));
    end
  end
endfunction

