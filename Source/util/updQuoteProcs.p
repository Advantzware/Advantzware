/*------------------------------------------------------------------------
    File        : updQuoteProcs.p
    Purpose     :

    Syntax      :

    Description : quote Procedures

    Author(s)   : Sewa Singh
    Created     : Wed Jan 1 19:29:35 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/
PROCEDURE UpdateExpireDate:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to update Expire Date
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprRowID AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCostPart AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCheckPartNo AS LOGICAL NO-UNDO .
    DEFINE BUFFER bf-quoteitm FOR quoteitm .  

    FIND FIRST quotehd EXCLUSIVE-LOCK
              WHERE rowid(quotehd) EQ iprRowID NO-ERROR .
    IF AVAILABLE quotehd THEN do:                   
        lCheckPartNo = NO .
        FOR EACH bf-quoteitm NO-LOCK
            WHERE bf-quoteitm.company EQ quotehd.company
              AND bf-quoteitm.q-no EQ  quotehd.q-no
              AND bf-quoteitm.part-no NE ipcCostPart
              AND bf-quoteitm.cust-no EQ quotehd.cust-no :
            lCheckPartNo = YES .
        END.
        IF NOT lCheckPartNo THEN
            ASSIGN quotehd.expireDate = TODAY .
    END.
    FIND CURRENT quotehd NO-LOCK NO-ERROR .

END PROCEDURE.

PROCEDURE UpdateExpireCustFGItem:
    /*------------------------------------------------------------------------------
         Purpose: Public wrapper procedure to update Expire Date for inactive customer and FGItem
         Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprRowID AS ROWID NO-UNDO.
    FIND FIRST quoteitm NO-LOCK
        WHERE ROWID(quoteitm) EQ iprRowID NO-ERROR .
    IF AVAILABLE quoteitm THEN 
    DO:
        FIND FIRST quotehd EXCLUSIVE-LOCK
            WHERE quotehd.company EQ quoteitm.company
            AND quotehd.q-no EQ quoteitm.q-no NO-ERROR .
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ quotehd.company
            AND cust.cust-no EQ quotehd.cust-no
            AND cust.cust-no NE "" NO-ERROR .

        IF AVAILABLE cust AND cust.ACTIVE EQ "I" AND (quotehd.expireDate GT TODAY OR quotehd.expireDate EQ ?) THEN
            ASSIGN quotehd.expireDate = TODAY .

        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ quoteitm.company
            AND itemfg.i-no EQ quoteitm.i-no
            AND itemfg.i-no NE "" NO-ERROR .

        IF AVAILABLE itemfg AND itemfg.stat EQ "I" AND (quotehd.expireDate GT TODAY OR quotehd.expireDate EQ ?) THEN
            ASSIGN quotehd.expireDate = TODAY .

        FIND CURRENT quotehd NO-LOCK NO-ERROR .
    END.
END PROCEDURE.

PROCEDURE UpdateExpireDate_allQuote:
    /*------------------------------------------------------------------------------
         Purpose: Public wrapper procedure to update Expire Date
         Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprRowID AS ROWID NO-UNDO.
    DEFINE BUFFER bf-quotehd  FOR quotehd .
    DEFINE BUFFER bf-quoteitm FOR quoteitm .

    FIND FIRST quoteitm NO-LOCK
        WHERE ROWID(quoteitm) EQ iprRowID NO-ERROR .

    IF AVAILABLE quoteitm THEN 
    DO:
        FIND FIRST quotehd NO-LOCK
            WHERE quotehd.company EQ quoteitm.company
            AND quotehd.q-no EQ quoteitm.q-no NO-ERROR .
        FOR EACH bf-quotehd NO-LOCK
            WHERE bf-quotehd.company EQ quoteitm.company
            AND bf-quotehd.loc EQ quoteitm.loc
            AND bf-quotehd.cust-no EQ quotehd.cust-no,
            FIRST bf-quoteitm OF bf-quotehd
            WHERE bf-quoteitm.part-no EQ quoteitm.part-no
            NO-LOCK BREAK BY bf-quotehd.cust-no
            BY bf-quoteitm.part-no
            BY bf-quotehd.quo-date:
            IF NOT LAST(bf-quotehd.quo-date) AND (bf-quotehd.expireDate GT TODAY OR bf-quotehd.expireDate EQ ?) THEN 
            DO:                    
                RUN UpdateExpireDate(rowid(bf-quotehd),bf-quoteitm.part-no) .
            END.
            ELSE IF LAST(bf-quotehd.quo-date) THEN 
                DO:                 
                    RUN UpdateExpireCustFGItem (ROWID(bf-quoteitm)) .
                END.
        END.
    END.

END PROCEDURE.
