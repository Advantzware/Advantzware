/* jobMoving.i - used in procedure jobMoving in board.w */

/*------------------------------------------------------------------------------
  Purpose:     show pixel's date and time as job widget is being moved
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  DEFINE VARIABLE pDate AS DATE NO-UNDO.
  DEFINE VARIABLE pTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE adjustLeft AS INTEGER NO-UNDO.
  DEFINE VARIABLE adjustDown AS INTEGER NO-UNDO.

  IF NOT jobMoving THEN RETURN.
  RUN mousePosition.
  i = cursorX - resourceGrid:WIDTH-PIXELS IN FRAME {&FRAME-NAME}.
  IF i LT 1 THEN i = 1.
  ELSE
  IF i GT maxPixel THEN i = maxPixel.
  ASSIGN
    pDate = pixelDate(dateTimePixel[i])
    pTime = pixelTime(dateTimePixel[i])
    jobMovingDisplay:SCREEN-VALUE = STRING(pDate,'99.99.99') + ' ' +
                                    STRING(pTime,'HH:MM:SSam')
    /* Move the fill-in to follow the cursor */
    adjustLeft = TRUNCATE(jobMovingDisplay:WIDTH-PIXELS / 2,0)
    adjustDown = 20
    jobMovingDisplay:X = IF cursorX LT FRAME {&FRAME-NAME}:X + adjustLeft THEN FRAME {&FRAME-NAME}:X 
            ELSE IF cursorX GT FRAME {&FRAME-NAME}:WIDTH-PIXELS - jobMovingDisplay:WIDTH-PIXELS + adjustLeft
                 THEN FRAME {&FRAME-NAME}:WIDTH-PIXELS - jobMovingDisplay:WIDTH-PIXELS
                 ELSE cursorX - adjustLeft
    jobMovingDisplay:Y = IF cursorY LT FRAME {&FRAME-NAME}:Y - adjustDown THEN FRAME {&FRAME-NAME}:Y 
            ELSE IF cursorY GT FRAME {&FRAME-NAME}:HEIGHT-PIXELS - jobMovingDisplay:HEIGHT-PIXELS - adjustDown
                 THEN FRAME {&FRAME-NAME}:HEIGHT-PIXELS - jobMovingDisplay:HEIGHT-PIXELS
                 ELSE cursorY + adjustDown.
  PROCESS EVENTS.
.
