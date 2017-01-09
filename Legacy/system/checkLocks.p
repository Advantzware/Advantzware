
/*------------------------------------------------------------------------
    File        : checkLocks.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jul 18 14:27:48 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE tt-allLocks REFERENCE-ONLY 
FIELD allFileName AS CHAR FORMAT "x(20)" COLUMN-LABEL "Table"
FIELD allTableNumber AS INT COLUMN-LABEL ""
FIELD allUserNum AS INT COLUMN-LABEL ""
FIELD allUser AS  CHAR FORMAT "x(12)"  COLUMN-LABEL "User"
FIELD allType AS CHAR COLUMN-LABEL "Lock Type"
FIELD allFlags AS CHAR COLUMN-LABEL "Flags"
FIELD allCnt AS INT COLUMN-LABEL "Count".

DEFINE INPUT PARAMETER TABLE FOR tt-allLocks  .
DEFINE OUTPUT PARAMETER oplIsLocks AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIslocks AS LOGICAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

EMPTY TEMP-TABLE tt-allLocks.
lIslocks = FALSE.
FIND FIRST asi._myconnection.

FOR EACH asi._lock WHERE  .
  IF asi._lock._lock-usr = _MyConn-UserId THEN DO:
    lIslocks = TRUE.
           FIND asi._file WHERE asi._file._file-number = asi._lock._lock-table NO-LOCK NO-ERROR.
    
      FIND FIRST tt-allLocks
        WHERE allFileName = asi._file._file-name
         AND allTableNumber = asi._file._file-number
         AND allUserNum = asi._lock._lock-usr
         AND allType = asi._lock._lock-type
         AND allFlags = asi._lock._lock-flags
         NO-ERROR
         .
    IF NOT AVAIL tt-allLocks THEN DO: 
    CREATE tt-allLocks.
    ASSIGN
     allFileName = asi._file._file-name
     allTableNumber = asi._file._file-number
     allUserNum = asi._lock._lock-usr
     allType = asi._lock._lock-type
     allFlags = asi._lock._lock-flags.
    END.
     
    allCnt = allCnt + 1.
  END.
     IF asi._Lock._Lock-Usr = ? THEN LEAVE.
END.

FIND FIRST ASI._myconnection.

FOR EACH ASI._lock WHERE  .
IF ASI._Lock._Lock-Usr = ? THEN LEAVE.
IF ASI._lock._lock-usr = ASI._myconnection._MyConn-UserId THEN DO:
  lIslocks = TRUE.
      FIND ASI._file WHERE ASI._file._file-number = ASI._lock._lock-table NO-LOCK NO-ERROR.
    
      FIND FIRST tt-allLocks
        WHERE allFileName = ASI._file._file-name
         AND allTableNumber = ASI._file._file-number
         AND allUserNum = ASI._lock._lock-usr
         AND allType = ASI._lock._lock-type
         AND allFlags = ASI._lock._lock-flags
         NO-ERROR
         .
    IF NOT AVAIL tt-allLocks THEN DO: 
    CREATE tt-allLocks.
    ASSIGN
     allFileName = ASI._file._file-name
     allTableNumber = ASI._file._file-number
     allUserNum = ASI._lock._lock-usr
     allType = ASI._lock._lock-type
     allFlags = ASI._lock._lock-flags.
    END.
     
    allCnt = allCnt + 1.
  END.  
END.

oplIsLocks = lIslocks.