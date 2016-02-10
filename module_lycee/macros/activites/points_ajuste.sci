// Copyright (C) 2007-2008 - INRIA, DIGITEO - Claude Gomez, Vincent Couvert
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function points_ajuste(np)
global x y n pente_reg X xbar ybar;
global f;
n=np;
x=gsort(rand(1,n)); y=gsort(rand(1,n));
X=linspace(0,1,100);
xbar=sum(x(1:n))/(n);
ybar=sum(y(1:n))/(n);
cov=sum(x.*y)/(n)-xbar*ybar;
varx=sum(x.*x)/(n)-xbar*xbar;
pente_reg=cov/varx;
scf(f);
a=gca();
a.auto_scale="off";
if ~isempty(a.children) then
  delete(a.children);
end
drawlater()
plot(x,y,"xk")
e=gce();e=e.children; e.mark_size=10;
a.axes_visible=["off","off","on"];
drawnow();
endfunction
