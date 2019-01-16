/* pMove.i - rstark - 12.20.2018 - used in queryBldr.w */

/* first column, can't move up */
IF {1}.sortOrder EQ 1 AND ipiMove EQ -1 THEN RETURN.
/* check if at bottom, can't move down */
FOR EACH b{1} NO-LOCK
    WHERE b{1}.subjectID EQ ttSubject.subjectID
       BY b{1}.sortOrder DESCENDING
    :
    LEAVE.
END. /* for each */
IF AVAILABLE b{1} THEN DO:
    /* check if at bottom, can't move down */
    IF b{1}.sortOrder EQ {1}.sortOrder AND
       ipiMove EQ 1 THEN
    RETURN.
END. /* if avail */
ELSE RETURN.
ASSIGN
    iCurrent = {1}.sortOrder
    iMoveTo  = {1}.sortOrder + ipiMove
    .
FIND FIRST b{1}
     WHERE b{1}.subjectID   EQ ttSubject.subjectID
       AND b{1}.sortOrder EQ iMoveTo
     NO-ERROR.
IF AVAILABLE b{1} THEN
ASSIGN
    {1}.sortOrder  = 0
    b{1}.sortOrder = iCurrent
    {1}.sortOrder  = iMoveTo
    .
rRowID = ROWID({1}).
{&OPEN-QUERY-{2}}
REPOSITION {2} TO ROWID rRowID.
fSetSaveButton (YES).
fShowQuery().
