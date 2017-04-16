/* pixelTime.i - used in function pixelDate in board.w & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:  extract time from decimal format of date & time YYYYMMDD.TTTTT
    Notes:  
------------------------------------------------------------------------------*/
  RETURN INTEGER(SUBSTRING(STRING(ipDateTime,'99999999.99999'),10)).
