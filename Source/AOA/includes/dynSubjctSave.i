/* dynSubjctSave.i - rstark - 2.20.2019 */

/* used in procedure pSaveSubject in dynSubjct.w */

/* check if record has been removed */
FOR EACH dynSubject{1} EXCLUSIVE-LOCK
    WHERE dynSubject{1}.subjectID EQ dynSubject.subjectID
    :
    IF CAN-FIND(FIRST bttSubject{1}
        WHERE bttSubject{1}.tableRowID EQ ROWID(dynSubject{1})) THEN
    NEXT.
    /* no temp-table record exists, delete record */
    DELETE dynSubject{1}.
END. /* each dynSubject{1} */
/* update existing, add new records */
FOR EACH bttSubject{1}
    WHERE bttSubject{1}.subjectID EQ dynSubject.subjectID
    :
    /* temp-table record matches existing record */
    IF bttSubject{1}.tableRowID NE ? THEN
    FIND FIRST dynSubject{1} EXCLUSIVE-LOCK
         WHERE ROWID(dynSubject{1}) EQ bttSubject{1}.tableRowID
         NO-ERROR.
    /* no record exists, new add, create record */
    IF bttSubject{1}.tableRowID EQ ? OR
       NOT AVAILABLE dynSubject{1} THEN
    CREATE dynSubject{1}.
    /* update record with temp-table values */
    BUFFER-COPY bttSubject{1} EXCEPT rec_key TO dynSubject{1}.
END. /* each bttSubject{1} */
RELEASE dynSubject{1}.
