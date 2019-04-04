/* dynSubjctCopy.i - rstark - 2.20.2019 */

/* used in dynSubjct.w procedure pCRUD copy */

FOR EACH dyn{1} NO-LOCK
    WHERE dyn{1}.subjectID EQ iSubjectID
    :
    CREATE bDyn{1}.
    BUFFER-COPY dyn{1} EXCEPT subjectID rec_key TO bDyn{1}
        ASSIGN bDyn{1}.subjectID = dynSubject.subjectID.
    CREATE tt{1}.
    BUFFER-COPY bDyn{1} TO tt{1}
        ASSIGN tt{1}.tableRowID = ROWID(bDyn{1}).
END. /* for each */
