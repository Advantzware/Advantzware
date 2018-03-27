/* sizeBrowse.i */

&IF DEFINED(h_Browse{1}) NE 0 &THEN
  IF VALID-HANDLE({&h_Browse{1}}) AND winReSize EQ NO THEN
  RUN winReSize IN {&h_Browse{1}} (rowDiff,colDiff) NO-ERROR.
&ENDIF
