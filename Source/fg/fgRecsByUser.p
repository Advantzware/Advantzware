
/*------------------------------------------------------------------------
    File        : fgRecsByUser.p
    Purpose     : 

    Syntax      :

    Description : Return w-fg-rctd for fg-rctd records per user

    Author(s)   : WFK
    Created     : Thu Mar 14 16:12:51 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
/* For fgpostBatch.p */
{fg/invrecpt.i NEW}
{fg/fgPostBatch.i}

DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcRitaCode AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserCode AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR w-fg-rctd.

DEFINE BUFFER bf-fg-rctd FOR fg-rctd.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cReturnValue     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFGTagValidation AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFGTagValidation AS CHARACTER NO-UNDO.
    
/* ***************************  Main Block  *************************** */
FOR EACH fg-rctd NO-LOCK 
     WHERE fg-rctd.company        EQ ipcCompany
       AND fg-rctd.rita-code      EQ ipcRitaCode
       AND (fg-rctd.created-by    EQ ipcUserCode
            OR fg-rctd.updated-by EQ ipcUserCode)
       AND fg-rctd.setHeaderRNo   EQ 0
     :
    FIND FIRST itemfg NO-LOCK 
         WHERE itemfg.company EQ ipcCompany  
          AND itemfg.i-no     EQ fg-rctd.i-no 
         NO-ERROR.
            
    RUN sys/ref/nk1look.p(
        INPUT ipcCompany,
        INPUT "FGTagValidation",
        INPUT "L",
        INPUT YES,
        INPUT YES,
        INPUT IF AVAILABLE itemfg THEN itemfg.cust-no ELSE "",
        INPUT "",
        OUTPUT cReturnValue,
        OUTPUT lRecFound
        ).
    lFGTagValidation = LOGICAL(cReturnValue).
    
    RUN sys/ref/nk1look.p(
        INPUT ipcCompany,
        INPUT "FGTagValidation",
        INPUT "C",
        INPUT YES,
        INPUT YES,
        INPUT IF AVAILABLE itemfg THEN itemfg.cust-no ELSE "",
        INPUT "",
        OUTPUT cReturnValue,
        OUTPUT lRecFound
        ).
    cFGTagValidation = cReturnValue.
   
    IF lFGTagValidation AND fg-rctd.tag EQ "" THEN 
        NEXT.
        
    IF cFGTagValidation EQ "ItemMatch" AND NOT fg-rctd.tag BEGINS fg-rctd.i-no THEN 
        NEXT.
         
    CREATE w-fg-rctd.
    BUFFER-COPY fg-rctd TO w-fg-rctd
        ASSIGN 
            w-fg-rctd.row-id  = ROWID(fg-rctd)
            w-fg-rctd.has-rec = YES
            .  
    FOR EACH bf-fg-rctd NO-LOCK 
        WHERE bf-fg-rctd.company      EQ ipcCompany
          AND bf-fg-rctd.rita-code    EQ ipcRitaCode
          AND(bf-fg-rctd.setHeaderRNo GT 0 AND 
              bf-fg-rctd.setHeaderRNo EQ fg-rctd.r-no):
                  
        CREATE w-fg-rctd.
        BUFFER-COPY bf-fg-rctd TO w-fg-rctd
        ASSIGN 
            w-fg-rctd.row-id  = ROWID(bf-fg-rctd)
            w-fg-rctd.has-rec = YES
            .           
    END.                           
END.  