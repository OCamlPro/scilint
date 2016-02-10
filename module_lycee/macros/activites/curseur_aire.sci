// Copyright (C) 2007-2008 - INRIA, DIGITEO - Claude Gomez, Vincent Couvert
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

///////////////
// graphique //
///////////////

function curseur_aire()
global lock slider valueDisplay minDisplay maxDisplay line aedit bedit fcedit;
global ff fig;
global sliderMin sliderMax sliderSteps;

try
  close(fig)
end
try
  delete(ff);
end

///////////////////////////
// création de la figure //
///////////////////////////

figWidth=620; // largeur de la figure
figHeight=800; // hauteur de la figure
fig=figure("Position",[10,10,figWidth,figHeight]);
set(fig,"figure_name","Calcul d''aire");
fig.auto_resize = "off";

ff=scf(fig);
ff.background = -2;
axes=gca();
axes.box = "on";
axes.visible = "on";
axes.axes_bounds = [-0.05 -0.05 1.1 0.8];
axes.margins = [0.1 0.1 0.1 0.1];

// cadre

frX=50
frWidth=300;
uicontrol(fig,"Style","frame",..
              "Position",[frX,90,frWidth,100],..
              "BackgroundColor",[0.9,0.9,0.9]);

// fonction

fctWidth=100;	
uicontrol(fig,"Style","text",..
              "Position" ,[frX+20,160,fctWidth,20],..
              "BackgroundColor",[0.9,0.9,0.9],..
              "Horizontalalignment","center",..
              "String","Fonction :","FontSize",14,"FontWeight","bold");
fcWidth=150;
fcedit=uicontrol(fig,"Style","edit",..
                 "Position",[frX+110,160,fcWidth,20],..
                 "String","x^2","FontSize",14,"FontWeight","bold");

// borne a

atWidth=50;	
uicontrol(fig,"Style","text",..
              "Position" ,[(figWidth-atWidth)/2-220,130,atWidth,20],..
              "BackgroundColor",[0.9,0.9,0.9],..
              "Horizontalalignment","center",..
              "String","a :","FontSize",14,"FontWeight","bold");
aWidth=40;
aedit=uicontrol(fig,"Style","edit",..
                 "Position",[(figWidth-aWidth)/2-170,130,aWidth,20],..
                 "String","0","FontSize",14,"FontWeight","bold");

// borne b

btWidth=50;	
uicontrol(fig,"Style","text",..
              "Position" ,[(figWidth-btWidth)/2-120,130,btWidth,20],..
              "BackgroundColor",[0.9,0.9,0.9],..
              "Horizontalalignment","center",..
              "String","b :","FontSize",14,"FontWeight","bold");
bWidth=40;
bedit=uicontrol(fig,"Style","edit",..
                 "Position",[(figWidth-bWidth)/2-70,130,bWidth,20],..
                 "String","1","FontSize",14,"FontWeight","bold");

// bouton ok

npokWidth=40;
uicontrol(fig,"Style","pushbutton",..
              "Position",[(figWidth-npokWidth)/2-120,100,npokWidth,20],..
              "String","Ok","FontSize",14,"FontWeight","bold",..
              "Callback","newdata_aire");

/////////////////////////
// création du curseur //
/////////////////////////

sliderMin=1; //borne min du curseur
sliderMax=100; //borne max du curseur
sliderX=50; // abscisse du point en bas à gauche du curseur
sliderY=50; // ordonnée du point en bas à gauche du curseur
sliderWidth=500; // largeur du curseur
sliderHeight=20; // hauteur du curseur
sliderSteps=sliderMax-sliderMin; // nombre de pas du curseur

// création du curseur
slider=uicontrol(fig,"Style","slider",..
                     "Position",[sliderX,sliderY,sliderWidth,sliderHeight],..
                     "Min",0,"Max",sliderSteps,..
                     "callback","update_aire");

// affichage de la borne min
lowerBoundPos =[sliderX-30,sliderY,20,sliderHeight];
uicontrol(fig,"Style","text",..
              "Horizontalalignment","right",..
              "Position",lowerBoundPos,..
	      "BackgroundColor", [1 1 1],..
              "String",string(sliderMin),"FontSize",14,"FontWeight","bold");

// affichage de la borne max
upperBoundPos=[sliderX+sliderWidth+20,sliderY,30,sliderHeight];
uicontrol(fig,"style","text",..
              "Horizontalalignment","left",..
              "Position",upperBoundPos,..
	      "BackgroundColor", [1 1 1],..
              "String",string(sliderMax),"FontSize",14,"FontWeight","bold");

// affichage de la valeur courante
valueDisplayPos=[sliderX+sliderWidth/2-25,sliderY-30,150,20] ;
valueDisplay=uicontrol(fig,"style","text","Horizontalalignment","left",..
    "BackgroundColor", [1 1 1],..
    "Position",valueDisplayPos,"FontSize",14,"FontWeight","bold");

// affichage des valeurs
titlePos=[frX+frWidth+20,160,200,20];
maxDisplay=uicontrol(fig,"style","text","Horizontalalignment","left",..
    "BackgroundColor", [1 1 1],..
    "Position",titlePos,"FontSize",14,"FontWeight","bold");

titlePos=[frX+frWidth+20,120,200,20];
minDisplay=uicontrol(fig,"style","text","Horizontalalignment","left",..
    "BackgroundColor", [1 1 1],..
    "Position",titlePos,"FontSize",14,"FontWeight","bold");

// bouton Quitter
//quitWidth=50;
//uicontrol(fig,"Style","pushbutton",..
//              "Position",[figWidth-quitWidth-10,20,quitWidth,20],..
//              "String","Quitter",..
//              "Callback","quitter_aire");

// initialisation
lock=%t;

endfunction

