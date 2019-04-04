/* dynSubjctDelete.i - rstark - 2.20.2019 */

/* used in dynSubjct.w procedure pCRUD delete */

FOR EACH {1} EXCLUSIVE-LOCK
    WHERE {1}.subjectID EQ dynSubject.subjectID
    :
    DELETE {1}.
END. /* each {1} */
