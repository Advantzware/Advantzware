/* winReSizeSetValue.i */

&IF DEFINED(winReSize) NE 0 &THEN
  IF winReSize EQ NO THEN DO:
    {methods/run_link.i "CONTAINER-SOURCE" "winSize" "(OUTPUT rowDiff,OUTPUT colDiff)"}
    IF rowDiff NE 0 AND colDiff NE 0 THEN
    RUN winReSize (rowDiff,colDiff).
    SESSION:SET-WAIT-STATE('').
  END.
&ENDIF
