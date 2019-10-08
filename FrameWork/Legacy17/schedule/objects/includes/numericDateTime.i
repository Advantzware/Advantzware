/* numericDateTime.i - used in function numericDateTime in board.w &
                       viewers/resourceDetail.w & viewers/prompts/move.w */

/*------------------------------------------------------------------------------
  Purpose:  put the date and time in decimal format YYYYMMDD.TTTTT
    Notes:  
------------------------------------------------------------------------------*/
  IF ipTime GT 86400 THEN
  ipTime = ipTime - TRUNCATE(ipTime / 86400,0) * 86400.
  IF ipDate EQ ? AND ipTime EQ ? THEN
  RETURN 0.99999.
  ELSE
  IF ipDate EQ ? THEN
  RETURN DECIMAL('0.' + STRING(ipTime,'99999')).
  ELSE
  RETURN DECIMAL(STRING(YEAR(ipDate),'9999') +
                 STRING(MONTH(ipDate),'99') +
                 STRING(DAY(ipDate),'99') + '.' +
                 STRING(ipTime,'99999')).
