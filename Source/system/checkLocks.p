
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

FIND FIRST nosweat._myconnection.

FOR EACH nosweat._lock WHERE  .
IF nosweat._Lock._Lock-Usr = ? THEN LEAVE.
IF nosweat._lock._lock-usr = nosweat._myconnection._MyConn-UserId THEN DO:
  lIslocks = TRUE.
      FIND nosweat._file WHERE nosweat._file._file-number = nosweat._lock._lock-table NO-LOCK NO-ERROR.
    
      FIND FIRST tt-allLocks
        WHERE allFileName = nosweat._file._file-name
         AND allTableNumber = nosweat._file._file-number
         AND allUserNum = nosweat._lock._lock-usr
         AND allType = nosweat._lock._lock-type
         AND allFlags = nosweat._lock._lock-flags
         NO-ERROR
         .
    IF NOT AVAIL tt-allLocks THEN DO: 
    CREATE tt-allLocks.
    ASSIGN
     allFileName = nosweat._file._file-name
     allTableNumber = nosweat._file._file-number
     allUserNum = nosweat._lock._lock-usr
     allType = nosweat._lock._lock-type
     allFlags = nosweat._lock._lock-flags.
    END.
     
    allCnt = allCnt + 1.
  END.  
END.

oplIsLocks = lIslocks.