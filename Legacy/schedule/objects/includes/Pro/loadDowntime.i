/* loadDowntime.i */

/*------------------------------------------------------------------------------
  Purpose:     reload downtime values and rebuild entire board
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN setScreenStatus.
  RUN getDowntime.
  RUN buildBoardDowntime.
  APPLY 'VALUE-CHANGED' TO intervals IN FRAME {&FRAME-NAME}.
  APPLY 'ENTRY' TO intervals.
