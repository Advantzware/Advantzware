/* createLightBulb.i - used in procedure createLightBulb in board.w */

/*------------------------------------------------------------------------------
  Purpose:     create lightBulbWidget rectangle
  Parameters:  resource name and object number assigned
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.

  DEFINE VARIABLE pWidget AS WIDGET-HANDLE NO-UNDO.

  IF VALID-HANDLE(lightBulbWidget[ipIdx]) THEN
  pWidget = lightBulbWidget[ipIdx].
  ELSE
  CREATE RECTANGLE pWidget IN WIDGET-POOL 'resourcePool'
    ASSIGN
      FRAME = FRAME {&FRAME-NAME}:HANDLE
      EDGE-PIXELS = 1.
  ASSIGN
    pWidget:X = resourceXCoord
    pWidget:Y = resourceYCoord
    pWidget:FILLED = TRUE
    pWidget:BGCOLOR = lightBulbColor
    pWidget:WIDTH-PIXELS = resourceGrid:WIDTH-PIXELS - 1
    pWidget:HEIGHT-PIXELS = hPixels
    pWidget:PRIVATE-DATA = ipResource
    pWidget:HIDDEN = YES
    lightBulbWidget[ipIdx] = pWidget:HANDLE.
