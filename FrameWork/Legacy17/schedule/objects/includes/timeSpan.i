/* timeSpan.i - used in function timeSpan in board.w & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:  calculate time span between 2 dates & times in seconds
    Notes:  
------------------------------------------------------------------------------*/
  RETURN IF ipStartDate EQ ipEndDate THEN ipEndTime - ipStartTime
         ELSE (86400 - ipStartTime) + (ipEndDate - ipStartDate - 1) * 86400 + ipEndTime.
