/* winReSizePgChg.i */



&IF DEFINED(winReSize) NE 0 &THEN
  &IF '{1}' EQ '' &THEN
    DEFINE VARIABLE currentWidget      AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE objectWidget       AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE iPageColDiff       AS INTEGER       NO-UNDO.
    DEFINE VARIABLE iPageRowDiff       AS INTEGER       NO-UNDO.
    DEFINE VARIABLE iWinKitCurrentPage AS INTEGER       NO-UNDO.
    DEFINE VARIABLE hWinKitFrame       AS HANDLE        NO-UNDO.
    DEFINE VARIABLE oWinKitControl     AS System.Windows.Forms.Control NO-UNDO .
  &ENDIF

  RUN GET-ATTRIBUTE('CURRENT-PAGE').
  iWinKitCurrentPage = INTEGER(RETURN-VALUE).
  ASSIGN
    hWinKitFrame   = oFormControl:GetTabPageFrame (iWinKitCurrentPage)
    oWinKitControl = oFormControl:GetTabPageControl (iWinKitCurrentPage) 
    .

  IF oWinKitControl:Width <> 0 AND oWinKitControl:Height <> 0 THEN DO:
      ASSIGN
          iPageColDiff = (oWinKitControl:Width - hWinKitFrame:WIDTH-PIXELS) // SESSION:PIXELS-PER-COLUMN
          iPageRowDiff = (oWinKitControl:Height - hWinKitFrame:HEIGHT-PIXELS) // SESSION:PIXELS-PER-ROW
          hWinKitFrame:VIRTUAL-HEIGHT-PIXELS = MAXIMUM (hWinKitFrame:HEIGHT-PIXELS, hWinKitFrame:HEIGHT-PIXELS + iPagerowDiff)
          hWinKitFrame:VIRTUAL-WIDTH-PIXELS  = MAXIMUM (hWinKitFrame:WIDTH-PIXELS, hWinKitFrame:WIDTH-PIXELS + iPagecolDiff)
          .
    
      IF iPageColDiff NE 0 AND iPageRowDiff NE 0 THEN DO:
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
    
        ASSIGN
          hWinKitFrame:HEIGHT-PIXELS  = oWinKitControl:Height
          hWinKitFrame:WIDTH-PIXELS   = oWinKitControl:Width
          hWinKitFrame:VIRTUAL-HEIGHT = hWinKitFrame:HEIGHT
          hWinKitFrame:VIRTUAL-WIDTH  = hWinKitFrame:WIDTH
          .
      END. // NE 0 and NE 0
  END.
&ENDIF
