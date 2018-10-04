/* winReSize.i - browse */

&IF DEFINED(winReSize) NE 0 &THEN
PROCEDURE winReSize:
  DEFINE INPUT PARAMETER ipRowDiff AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER ipColDiff AS DECIMAL NO-UNDO.

  DEFINE VARIABLE iWindowHeight AS INTEGER       NO-UNDO.
  DEFINE VARIABLE iWindowWidth  AS INTEGER       NO-UNDO.
  DEFINE VARIABLE currentWidget AS WIDGET-HANDLE NO-UNDO.
  DEFINE VARIABLE oGrid         AS Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseControl NO-UNDO.
  
  // No matter what resizing we need to ensure, the virtual size of the frame
  // is large enough here to contain all widgets
  ASSIGN 
      iWindowHeight = IF iWindowHeight EQ 0 THEN {&WINDOW-NAME}:HEIGHT
                      ELSE iWindowHeight 
      iWindowWidth  = IF iWindowWidth  EQ 0 THEN {&WINDOW-NAME}:WIDTH
                      ELSE iWindowWidth
      FRAME {&FRAME-NAME}:FRAME:VIRTUAL-HEIGHT = iWindowHeight
      FRAME {&FRAME-NAME}:FRAME:VIRTUAL-WIDTH  = iWindowWidth
      FRAME {&FRAME-NAME}:FRAME:HEIGHT         = iWindowHeight
      FRAME {&FRAME-NAME}:FRAME:WIDTH          = iWindowWidth
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
