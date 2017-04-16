/* calcPriority.i - used in function calcPriority in board.i & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:  set job priorty[1,2,3] value based on configuration settings
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnValue AS INTEGER NO-UNDO.

  IF ipPriority EQ priority1 THEN rtnValue = resourcePriority(ipResource).
  ELSE IF ipPriority EQ priority2 THEN rtnValue = ipJobSequence.
  ELSE IF ipPriority EQ priority3 THEN rtnValue = ipResourceSequence.
  ELSE rtnValue = 0.

  RETURN rtnValue.
