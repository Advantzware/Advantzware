/* createDowntime.i */

/*------------------------------------------------------------------------------
  Purpose:     create downtime rectangle, no triggers
  Parameters:  object number assigned, X, Y, width and height
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipX AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipY AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipWidth AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipHeight AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  {{&includes}/ttblWidgetFind.i "downtimeWidget" ipIdx}
  {{&includes}/ttblWidgetAssign.i "downtimeWidget" pWidget}
  ELSE
  DO:
    IF downtimeTop THEN
    CREATE TEXT pWidget IN WIDGET-POOL 'downtimePool'
      ASSIGN
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        PRIVATE-DATA = 'Downtime'.
    ELSE
    CREATE RECTANGLE pWidget IN WIDGET-POOL 'downtimePool'
      ASSIGN
        FRAME = FRAME {&FRAME-NAME}:HANDLE
        EDGE-PIXELS = 1
        PRIVATE-DATA = 'Downtime'.
    {{&includes}/ttblWidgetCreate.i "downtimeWidget" ipIdx pWidget}
  END.
  ASSIGN
    pWidget:HIDDEN = YES
    pWidget:X = ipX
    pWidget:Y = ipY - downtimeSize / 2
    pWidget:WIDTH-PIXELS = IF ipWidth NE 0 THEN ipWidth ELSE 1
    pWidget:HEIGHT-PIXELS = ipHeight + downtimeSize
    pWidget:BGCOLOR = jobBGColor[1]
    pWidget:FGCOLOR = IF jobBGColor[1] NE 0 THEN 0 ELSE 7
    ldummy = pWidget:MOVE-TO-TOP().
  IF NOT downtimeTop THEN /* if using rectangle type */
  pWidget:FILLED = IF jobBGColor[1] EQ ? THEN FALSE ELSE TRUE.
