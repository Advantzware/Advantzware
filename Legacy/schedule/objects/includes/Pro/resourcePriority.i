/* resourcePriority.i - used in function calcPriority in board.i & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ttblResource NO-LOCK WHERE ttblResource.resource EQ ipResource NO-ERROR.
  RETURN IF AVAILABLE ttblResource THEN ttblResource.priority ELSE 0.
