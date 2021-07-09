/* pBuildCustList.i */

PROCEDURE pBuildCustList:
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcID        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcStartCust AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcEndCust   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplList      AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE lActive AS LOGICAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttCustList.
    ASSIGN
        opcStartCust = CHR(32)
        opcEndCust   = CHR(254)
        .
    RUN sys/ref/CustList.p (ipcCompany, ipcID, YES, OUTPUT lActive).
    IF lActive THEN DO:
        FIND FIRST ttCustList
             WHERE ttCustList.log-fld EQ TRUE
             USE-INDEX cust-No  
             NO-ERROR.
        IF AVAILABLE ttCustList THEN
        opcStartCust = ttCustList.cust-no.
        FIND LAST ttCustList
             WHERE ttCustList.log-fld EQ TRUE
             USE-INDEX cust-No
             NO-ERROR.
        IF AVAILABLE ttCustList THEN 
        opcEndCust = ttCustList.cust-no.
    END. /* if lactive */
    oplList = lActive.
END PROCEDURE.
