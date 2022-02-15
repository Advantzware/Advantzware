/* dynFormDelete.i - rstark - 7.12.2021 */

/* used in dynSubjct.w procedure pCRUD delete */

FOR EACH {1} EXCLUSIVE-LOCK
    WHERE {1}.formID   EQ dynForm.FormID
      AND {1}.clientID EQ dynForm.clientID
    :
    DELETE {1}.
END. /* each {1} */
