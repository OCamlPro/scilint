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

function curseur_ajuste()
global lock slider valueDisplay errorDisplay line npedit;
global f fig;
global sliderMin sliderMax sliderSteps;

try
  close(fig)
end
try
  delete(f);
end

///////////////////////////
// création de la figure //
///////////////////////////

figWidth=600; // largeur de la figure
figHeight=800; // hauteur de la figure
fig=figure("Position",[10,10,figWidth,figHeight]);
set(fig,"figure_name","Ajustement affine");
fig.auto_resize = "off";

f=scf(fig);
f.background = -2; // Blanc
axes=gca();
axes.box = "on"; // Sinon le background n'est pas pris en compte (propriété filled à venir et dispo dans Scilab 5.0)
axes.visible = "on";
axes.isoview="on";
axes.x_location="middle";
axes.y_location="middle";
axes.axes_bounds = [-0.01 -0.05 1 0.9];
axes.margins = [0.1 0.1 0.1 0.1];

// nombre de points

frX=50;
frWidth=200;
uicontrol(fig,"Style","frame",..
              "Position",[frX,50,frWidth,100],..
              "BackgroundColor",[0.9,0.9,0.9]);
nptWidth=140;	
uicontrol(fig,"Style","text","FontSize",14,"FontWeight","bold",..
              "Position" ,[frX+(frWidth-nptWidth)/2,120,nptWidth,20],..
              "BackgroundColor",[0.9,0.9,0.9],..
              "Horizontalalignment","center",..
              "String","Nombre de points");
npWidth=35;
npedit=uicontrol(fig,"Style","edit",..
                 "Position",[frX+(frWidth-npWidth)/2,90,npWidth,20],..
                 "String","10","FontSize",14,"FontWeight","bold")
//                 "Callback","newnp_ajuste");
npokWidth=40;
uicontrol(fig,"Style","pushbutton",..
              "Position",[frX+(frWidth-npokWidth)/2,60,npokWidth,20],..
              "String","Ok","FontSize",14,"FontWeight","bold",..
              "Callback","newnp_ajuste");

/////////////////////////
// création du curseur //
/////////////////////////

sliderMin=0; //borne min du curseur
sliderMax=5; //borne max du curseur
sliderX=50; // abscisse du point en bas à gauche du curseur
sliderY=10; // ordonnée du point en bas à gauche du curseur
sliderWidth=500; // largeur du curseur
sliderHeight=20; // hauteur du curseur
sliderSteps=500; // nombre de pas du curseur

// création du curseur
slider=uicontrol(fig,"Style","slider",..
                     "Position",[sliderX,sliderY,sliderWidth,sliderHeight],..
                     "Min",0,"Max",sliderSteps,..
                     "callback","update_ajuste");

// affichage de la borne min
lowerBoundPos =[sliderX-30,sliderY,20,sliderHeight];
uicontrol(fig,"Style","text",..
              "Horizontalalignment","right",..
              "Position",lowerBoundPos,..
	      "BackgroundColor", [1 1 1],..
              "String",string(sliderMin),"FontSize",14,"FontWeight","bold");

// affichage de la borne max
upperBoundPos=[sliderX+sliderWidth+15,sliderY,20,sliderHeight];
uicontrol(fig,"style","text",..
              "Horizontalalignment","left",..
              "Position",upperBoundPos,..
	      "BackgroundColor", [1 1 1],..
              "String",string(sliderMax),"FontSize",14,"FontWeight","bold");

// affichage de la valeur courante
valueDisplayPos=[sliderX+sliderWidth/2-25,65,150,20] ;
valueDisplay=uicontrol(fig,"style","text","Horizontalalignment","left",..
                           "FontSize",14,"FontWeight","bold",..
			   "BackgroundColor", [1 1 1],..
                           "Position",valueDisplayPos);

// affichage de l'erreur
titlePos=[sliderX+sliderWidth/2-25,115,175,20];
errorDisplay=uicontrol(fig,"style","text","Horizontalalignment","left",..
                           "FontSize",14,"FontWeight","bold",..
			   "BackgroundColor", [1 1 1],..
                           "Position",titlePos);

// bouton Quitter
//quitWidth=50;
//uicontrol(fig,"Style","pushbutton",..
//              "Position",[figWidth-quitWidth-50,90,quitWidth,20],..
//              "String","Quitter",..
//              "Callback","quitter_ajuste");

// initialisation
lock=%t;

endfunction

