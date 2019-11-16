/* showLightBulb.i - used in procedure showLightBulb in boardProc.i */

/*------------------------------------------------------------------------------
  Purpose:     show/hide lightBulbWidget color
  Parameters:  job value
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  DO i = 1 TO resourceIdx:
    lightBulbWidget[i]:HIDDEN = NOT CAN-FIND(FIRST ttblJob
                       WHERE ttblJob.job EQ ipJob
                         AND ttblJob.resource EQ lightBulbWidget[i]:PRIVATE-DATA).
  END.
