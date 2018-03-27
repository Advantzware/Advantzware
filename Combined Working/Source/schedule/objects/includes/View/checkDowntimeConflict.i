/* checkDowntimeConflict.i - used in fuction checkDowntimeConflict in
                             Pro/boardProc.i & objects/resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:  check to see if an existing downtime is in conflict with new move
    Notes:  
------------------------------------------------------------------------------*/
  &SCOPED-DEFINE addtnlWhere 
  RETURN {{&includes}/{&Board}/conflictFunc.i boardDowntime ipResource ip}.
