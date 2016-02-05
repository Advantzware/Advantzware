/* winReSizeLocInit.i */

&IF DEFINED(winReSize) NE 0 &THEN
  DEFINE VARIABLE pHandle AS HANDLE NO-UNDO.
  
  {methods/winReSizeSetValue.i}
&ENDIF
