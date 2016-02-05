/* winReSizePgChg.i */

&IF DEFINED(winReSize) NE 0 &THEN
  &IF '{1}' EQ '' &THEN
    DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE objectWidget AS WIDGET-HANDLE NO-UNDO.
  &ENDIF
  
  /* scop-def h_ObjectXX in window container */
  {methods/moveObject.i 01}
  {methods/moveObject.i 02}
  {methods/moveObject.i 03}
  {methods/moveObject.i 04}
  {methods/moveObject.i 05}
  {methods/moveObject.i 06}
  {methods/moveObject.i 07}
  {methods/moveObject.i 08}
  {methods/moveObject.i 09}
  {methods/moveObject.i 10}
  {methods/moveObject.i 11}
  {methods/moveObject.i 12}
  {methods/moveObject.i 13}
  {methods/moveObject.i 14}
  {methods/moveObject.i 15}
  {methods/moveObject.i 16}
  {methods/moveObject.i 17}
  {methods/moveObject.i 18}
  {methods/moveObject.i 19}
  {methods/moveObject.i 20}
&ENDIF
