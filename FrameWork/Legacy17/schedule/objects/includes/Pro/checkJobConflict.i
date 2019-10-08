/* checkJobConflict.i - used in function checkJobConflict in pro/boardProc.i
                        & objects/resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:  check to see if an existing job is in conflict with new move
    Notes:  
------------------------------------------------------------------------------*/
  {{&includes}/{&Board}/conflictScop.i}
  RETURN {{&includes}/{&Board}/conflictFunc.i buffJob ipResource ip}.
  {{&includes}/{&Board}/conflictUndef.i}
