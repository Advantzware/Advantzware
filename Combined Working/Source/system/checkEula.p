
/*------------------------------------------------------------------------
    File        : checkEula.p
    Purpose     : 

    Syntax      :

    Description : Check for acceptance of current Eula	

    Author(s)   : Wade Kaldawi
    Created     : Mon Jun 20 10:09:47 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcEulaFile AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplEulaAccepted AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcEulaVersion  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVersion AS CHARACTER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE cInputLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEntireText AS CHARACTER NO-UNDO.
DEFINE VARIABLE lVersionRead AS LOGICAL NO-UNDO.

cEntireText = "".
INPUT FROM VALUE(ipcEulaFile).
REPEAT:
    
    cInputLine = "".
    IMPORT UNFORMATTED cInputLine.
    
    IF NOT lVersionRead THEN DO:
        IF NOT cInputLine BEGINS "[" THEN 
          ASSIGN cVersion = cInputLine lVersionRead = YES.
                 
        NEXT.
    END.
    
    IF cInputLine BEGINS "[" THEN
      NEXT.
      
    cEntireText = cEntireText + cInputLine + CHR(13).
    
END.
INPUT CLOSE.

FIND FIRST userEula NO-LOCK WHERE userEula.eula_code EQ cVersion
  AND userEula.user_id = USERID(ldbname(1)) NO-ERROR.
IF AVAILABLE userEula AND userEula.accepted EQ TRUE THEN 
  oplEulaAccepted = TRUE.
opcEulaVersion = cVersion.