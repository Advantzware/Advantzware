DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdtDate AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER iplMessage AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER oplContinue AS LOGICAL NO-UNDO.

DEFINE VARIABLE iFutureDays AS INTEGER NO-UNDO.
DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lValidate AS LOGICAL NO-UNDO.
DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (ipcCompany, "OeDateWarn", "I", NO, NO, "", "", 
                      OUTPUT cReturn, OUTPUT lFound).
IF lFound THEN
    iFutureDays = INT(cReturn).
ELSE
    iFutureDays = 0.
RUN sys/ref/nk1look.p (ipcCompany, "OeDateWarn", "L", NO, NO, "", "", 
                      OUTPUT cReturn, OUTPUT lFound).    
lValidate = LOGICAL(cReturn).

IF iFutureDays EQ 0 THEN    
  iFutureDays = 90.


  ASSIGN lContinue = TRUE
         lValid    = TRUE.
IF lValidate THEN DO:
    lValid = YES.
    IF ipdtDate GT TODAY + iFutureDays THEN DO:
        lValid = NO.
        IF iplMessage THEN 
          MESSAGE 'Date entered ' + STRING(ipdtDate) +  ' is is more than ' + STRING(iFutureDays) +  " days into the future. " + 'Continue with this date?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE lContinue.
    END.
END.


         
         
ASSIGN oplValid    = lValid
       oplContinue = lContinue.


