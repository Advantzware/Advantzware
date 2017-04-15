/* menutrig.i */

&IF "{&ITEM{1}}" NE "" &THEN
{methods/menus/acclrtrs.i "{&ITEM{1}}"}
ON CHOOSE OF MENU-ITEM m_{&ITEM{1}} OR
   CHOOSE OF MENU-ITEM p_{&ITEM{1}}
DO:
  RUN Select_{&ITEM{1}}.
END.

PROCEDURE Select_{&ITEM{1}}:
  &IF '{&ITEM{1}}' EQ 'List' AND DEFINED(setUserPrint) NE 0 &THEN
  RUN setUserPrint.
  &ENDIF
  
  &IF '{&ITEM{1}}' EQ 'Exit' AND DEFINED(setUserExit) NE 0 &THEN
  RUN setUserExit.
  &ENDIF
  
  {&PROC{1}}
END PROCEDURE.
&ENDIF
