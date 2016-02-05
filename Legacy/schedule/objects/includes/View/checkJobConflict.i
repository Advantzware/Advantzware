/* checkJobConflict.i - used in function checkJobConflict in Pro/boardProc.i
                        & objects/resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:  check to see if an existing job is in conflict with new move
    Notes:  
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE addtnlWhere AND ROWID(~{1}) NE ipRowID
  RETURN {{&includes}/{&Board}/conflictFunc.i buffJob ipResource ip}.

