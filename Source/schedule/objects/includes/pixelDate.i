/* pixelDate.i - used in function pixelDate in board.w & resourceDetail.w */

/*------------------------------------------------------------------------------
  Purpose:  extract date from decimal format of date & time YYYYMMDD.TTTTT
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE yr AS INTEGER NO-UNDO.
  DEFINE VARIABLE mo AS INTEGER NO-UNDO.
  DEFINE VARIABLE dy AS INTEGER NO-UNDO.
  
  ASSIGN
    yr = INTEGER(SUBSTRING(STRING(ipDateTime),1,4))
    mo = INTEGER(SUBSTRING(STRING(ipDateTime),5,2))
    dy = INTEGER(SUBSTRING(STRING(ipDateTime),7,2)).
  RETURN DATE(mo,dy,yr).
