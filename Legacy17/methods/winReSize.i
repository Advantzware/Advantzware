/* winReSize.i - browse */

&IF DEFINED(winReSize) NE 0 &THEN
PROCEDURE winReSize:
  DEFINE INPUT PARAMETER ipRowDiff AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipColDiff AS DECIMAL NO-UNDO.

  DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE oGrid         AS Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseControl NO-UNDO.

/*  MESSAGE                                                                          */
/*      1 PROGRAM-NAME (1) SKIP                                                      */
/*      2 PROGRAM-NAME (2) SKIP                                                      */
/*      3 PROGRAM-NAME (3) SKIP                                                      */
/*      4 PROGRAM-NAME (4) SKIP                                                      */
/*      5 PROGRAM-NAME (5) SKIP                                                      */
/*      6 PROGRAM-NAME (6) SKIP                                                      */
/*      7 PROGRAM-NAME (7) SKIP (1)                                                  */
/*      "FRAME {&FRAME-NAME}:HEIGHT:" FRAME {&FRAME-NAME}:HEIGHT SKIP                */
/*      "FRAME {&FRAME-NAME}:WIDTH:" FRAME {&FRAME-NAME}:WIDTH SKIP (1)              */
/*      "FRAME {&FRAME-NAME}:WINDOW:HEIGHT:" FRAME {&FRAME-NAME}:WINDOW:HEIGHT SKIP  */
/*      "FRAME {&FRAME-NAME}:WINDOW:WIDTH:" FRAME {&FRAME-NAME}:WINDOW:WIDTH SKIP (1)*/
/*      "FRAME {&FRAME-NAME}:WINDOW:NAME:" FRAME {&FRAME-NAME}:WINDOW:Name SKIP (1)  */
/*      "FRAME {&FRAME-NAME}:WINDOW:HWND:" FRAME {&FRAME-NAME}:WINDOW:HWND SKIP (1)  */
/*  VIEW-AS ALERT-BOX TITLE "Browser winReSize".                                     */
  
  // No matter what resizing we need to ensure, the virtual size of the frame
  // is large enough here to contain all widgets
  ASSIGN 
      FRAME {&FRAME-NAME}:FRAME:VIRTUAL-HEIGHT = FRAME {&FRAME-NAME}:WINDOW:HEIGHT
      FRAME {&FRAME-NAME}:FRAME:VIRTUAL-WIDTH  = FRAME {&FRAME-NAME}:WINDOW:WIDTH
      FRAME {&FRAME-NAME}:FRAME:HEIGHT         = FRAME {&FRAME-NAME}:WINDOW:HEIGHT
      FRAME {&FRAME-NAME}:FRAME:WIDTH          = FRAME {&FRAME-NAME}:WINDOW:WIDTH
      winReSize = YES
      /* shouldn't be needed, but here because of .net object sizing setting */
      FRAME {&FRAME-NAME}:SCROLLABLE     = YES
    &IF '{&sizeOption}' EQ 'HEIGHT' &THEN
      FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = MAXIMUM (FRAME {&FRAME-NAME}:HEIGHT, FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff)
    &ELSEIF '{&sizeOption}' EQ 'WIDTH' &THEN
      FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = MAXIMUM (FRAME {&FRAME-NAME}:WIDTH, FRAME {&FRAME-NAME}:WIDTH + ipColDiff)
    &ELSE
      FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = MAXIMUM (FRAME {&FRAME-NAME}:HEIGHT, FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff)
      FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = MAXIMUM (FRAME {&FRAME-NAME}:WIDTH, FRAME {&FRAME-NAME}:WIDTH + ipColDiff)
    &ENDIF
      currentWidget = FRAME {&FRAME-NAME}:HANDLE
      currentWidget = currentWidget:FIRST-CHILD
      currentWidget = currentWidget:FIRST-CHILD
      .

  DO WHILE currentWidget NE ?:
    IF currentWidget:TYPE EQ 'BROWSE' THEN DO:
      ASSIGN
      &IF '{&sizeOption}' EQ 'HEIGHT' &THEN
        currentWidget:HEIGHT = currentWidget:HEIGHT + ipRowDiff
      &ELSEIF '{&sizeOption}' EQ 'WIDTH' &THEN
        currentWidget:WIDTH  = currentWidget:WIDTH + ipColDiff
      &ELSE
        currentWidget:HEIGHT = currentWidget:HEIGHT + ipRowDiff
        currentWidget:WIDTH  = currentWidget:WIDTH + ipColDiff
      &ENDIF
        .
      &IF DEFINED(repositionBrowse) NE 0 &THEN
      APPLY 'HOME':U TO currentWidget.
      &ENDIF
      oGrid = Consultingwerk.WindowIntegrationKit.Controls.WinKitControls:FromBrowseHandle (currentWidget) .
      IF VALID-OBJECT (oGrid) THEN DO:
        //oGrid:Anchor = Progress.Util.DockStyle
        oGrid:RepositionControl () .
      END.
    END. /* if browse type */
    &IF DEFINED(browseOnly) EQ 0 &THEN
      ELSE currentWidget:ROW = currentWidget:ROW + ipRowDiff.
    &ENDIF
    currentWidget = currentWidget:NEXT-SIBLING.
  END.

  // now set the frame to it's target size, both virtual and actual frame size
  ASSIGN
    &IF '{&sizeOption}' EQ 'HEIGHT' &THEN
      FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff
    &ELSEIF '{&sizeOption}' EQ 'WIDTH' &THEN
      FRAME {&FRAME-NAME}:WIDTH  = FRAME {&FRAME-NAME}:WIDTH  + ipColDiff
    &ELSE
      FRAME {&FRAME-NAME}:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT + ipRowDiff
      FRAME {&FRAME-NAME}:WIDTH  = FRAME {&FRAME-NAME}:WIDTH  + ipColDiff
    &ENDIF
      FRAME {&FRAME-NAME}:VIRTUAL-HEIGHT = FRAME {&FRAME-NAME}:HEIGHT
      FRAME {&FRAME-NAME}:VIRTUAL-WIDTH  = FRAME {&FRAME-NAME}:WIDTH
      currentWidget = FRAME {&FRAME-NAME}:HANDLE
      currentWidget = currentWidget:FIRST-CHILD
      currentWidget = currentWidget:FIRST-CHILD
      .
  // correct if browser too large for the frame
  DO WHILE currentWidget NE ?:
    IF currentWidget:TYPE EQ 'BROWSE' THEN DO:
      IF currentWidget:HEIGHT GT FRAME {&FRAME-NAME}:HEIGHT THEN
      currentWidget:HEIGHT = FRAME {&FRAME-NAME}:HEIGHT.
      IF currentWidget:WIDTH GT FRAME {&FRAME-NAME}:WIDTH THEN
      currentWidget:WIDTH = FRAME {&FRAME-NAME}:WIDTH.
    END. /* if browse type */
    currentWidget = currentWidget:NEXT-SIBLING.
  END.
    
END PROCEDURE.
&ENDIF
