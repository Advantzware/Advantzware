/*------------------------------------------------------------------------
    File        : updQuoteProcs.p
    Purpose     :

    Syntax      :

    Description : quote Procedures

    Author(s)   : Sewa Singh
    Created     : Wed Jan 1 19:29:35 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/
  
PROCEDURE pExpPriceMatrix:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to update Expire Date
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuote AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtNewExpire AS DATE NO-UNDO.
    DEFINE VARIABLE lQuotePriceMatrix AS LOGICAL NO-UNDO.
    DEFINE BUFFER bf-oe-prmtx FOR oe-prmtx.
    
    RUN pGetNk1Settings(INPUT ipcCompany, OUTPUT lQuotePriceMatrix).
    IF lQuotePriceMatrix THEN 
    DO:        
        FOR EACH bf-oe-prmtx EXCLUSIVE-LOCK
             WHERE bf-oe-prmtx.company EQ ipcCompany
             AND bf-oe-prmtx.quoteId  EQ ipiQuote :
        
           bf-oe-prmtx.exp-date = ipdtNewExpire. 
        END.                                         
    END.
    RELEASE bf-oe-prmtx.
END PROCEDURE.

PROCEDURE pGetNk1Settings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.     
    DEFINE OUTPUT PARAMETER iplFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    
    RUN sys/ref/nk1look.p  (INPUT ipcCompany,
        INPUT "QuotePriceMatrix", 
        INPUT "L", 
        INPUT NO, 
        INPUT NO, 
        INPUT "",
        INPUT "", 
        OUTPUT cReturn, 
        OUTPUT iplFound ).
                     
END PROCEDURE.

PROCEDURE UpdateExpireDate:
    /*------------------------------------------------------------------------------
     Purpose: Public wrapper procedure to update Expire Date
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprRowID AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustPart AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtNewExpire AS DATE NO-UNDO.
    DEFINE VARIABLE lCheckPartNo AS LOGICAL NO-UNDO .
    DEFINE BUFFER bf-quoteitm FOR quoteitm .  

    FIND FIRST quotehd EXCLUSIVE-LOCK
              WHERE ROWID(quotehd) EQ iprRowID NO-ERROR .
    IF AVAILABLE quotehd THEN DO:                   
        lCheckPartNo = NO .
        FOR EACH bf-quoteitm NO-LOCK
            WHERE bf-quoteitm.company EQ quotehd.company
              AND bf-quoteitm.q-no EQ  quotehd.q-no
              AND bf-quoteitm.part-no NE ipcCustPart
              AND bf-quoteitm.cust-no EQ quotehd.cust-no :
            lCheckPartNo = YES .
        END.
        IF NOT lCheckPartNo THEN DO:        
            ASSIGN quotehd.expireDate = ipdtNewExpire .
            RUN pExpPriceMatrix(quotehd.company,quotehd.q-no,ipdtNewExpire).
        END.    
    END.
    FIND CURRENT quotehd NO-LOCK NO-ERROR .

END PROCEDURE.

PROCEDURE UpdateExpireCustFGItem:
    /*------------------------------------------------------------------------------
         Purpose: Public wrapper procedure to update Expire Date for inactive customer and FGItem
         Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprRowID AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdtNewExpire AS DATE NO-UNDO.
    
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
        DO:         
            ASSIGN quotehd.expireDate = ipdtNewExpire .
            RUN pExpPriceMatrix(quotehd.company,quotehd.q-no,ipdtNewExpire).
        END.
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ quoteitm.company
            AND itemfg.i-no EQ quoteitm.i-no
            AND itemfg.i-no NE "" NO-ERROR .

        IF AVAILABLE itemfg AND itemfg.stat EQ "I" AND (quotehd.expireDate GT TODAY OR quotehd.expireDate EQ ?) THEN
        DO:        
            ASSIGN quotehd.expireDate = ipdtNewExpire .
            RUN pExpPriceMatrix(quotehd.company,quotehd.q-no,ipdtNewExpire).
        END.
        FIND CURRENT quotehd NO-LOCK NO-ERROR .
    END.
END PROCEDURE.

PROCEDURE UpdateExpireDate_allQuote:
    /*------------------------------------------------------------------------------
         Purpose: Public wrapper procedure to update Expire Date
         Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iprRowID AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdtNewExpire AS DATE NO-UNDO.
    
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
                RUN UpdateExpireDate(ROWID(bf-quotehd),bf-quoteitm.part-no, ipdtNewExpire) .
            END.
            ELSE IF LAST(bf-quotehd.quo-date) THEN 
                DO:                 
                    RUN UpdateExpireCustFGItem (ROWID(bf-quoteitm), ipdtNewExpire) .
                END.
        END.
    END.

END PROCEDURE.
