// Copyright (C) 2002-2009 - DIGITEO - Claude Gomez
//
// This file must be used under the terms of the CeCILL.
// This source file is licensed as described in the file COPYING, which
// you should have received as part of this distribution.  The terms
// are also available at
// http://www.cecill.info/licences/Licence_CeCILL_V2-en.txt

function j=jeu_tarot(varargin)
  [lhs,rhs]=argn();
  if rhs<>0 then error("La fonction ""jeu_tarot"" ne doit pas avoir d''argument."); end
  j=ensemble(..
  "pique_2","pique_3","pique_4","pique_5","pique_6",..
  "pique_7","pique_8","pique_9","pique_10","pique_valet","pique_cavalier","pique_dame","pique_roi","pique_as",..
  "coeur_2","coeur_3","coeur_4","coeur_5","coeur_6",..
  "coeur_7","coeur_8","coeur_9","coeur_10","coeur_valet","coeur_cavalier","coeur_dame","coeur_roi","coeur_as",..
  "carreau_2","carreau_3","carreau_4","carreau_5","carreau_6",..
  "carreau_7","carreau_8","carreau_9","carreau_10","carreau_valet","carreau_cavalier","carreau_dame","carreau_roi","carreau_as",..
  "trefle_2","trefle_3","trefle_4","trefle_5","trefle_6",..
  "trefle_7","trefle_8","trefle_9","trefle_10","trefle_valet","trefle_cavalier","trefle_dame","trefle_roi","trefle_as",..
  "atout_1","atout_2","atout_3","atout_4","atout_5","atout_6","atout_7","atout_8","atout_9","atout_10","atout_11",..
  "atout_12","atout_13","atout_14","atout_15","atout_16","atout_17","atout_18","atout_19","atout_20","atout_21",..
  "excuse");
endfunction

