/* jobFGColor.i */

/*------------------------------------------------------------------------------
  Purpose:  return job fg color, calculated in jobbgcolor function
    Notes:  need: DEFINE VARIABLE fgJobColor AS INTEGER NO-UNDO.
            need: SCOPED-DEFINE colorJobTable ttblJob|pendingJob
------------------------------------------------------------------------------*/
  RETURN fgJobColor.
