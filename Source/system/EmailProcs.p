/* EmailProcs.p - rstark - 8.30.2021 */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBuildToList:
    DEFINE INPUT  PARAMETER ipcType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcRecKey    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEMail     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCode      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRecipient AS CHARACTER NO-UNDO.

    DEFINE BUFFER bCust FOR cust.

    CASE ipcCode:
        WHEN "po-ordl_." THEN
        ipcCode = "r-poprt.".
    END CASE.
    FOR EACH phone NO-LOCK 
        WHERE phone.table_rec_key EQ ipcRecKey
           OR ipcRecKey EQ "ALL"
        :    
        IF CAN-FIND(FIRST emaildtl
                    WHERE emaildtl.emailcod      EQ ipcCode
                      AND emaildtl.table_rec_key EQ phone.rec_key) 
                       OR phone.titlcode         EQ ipcCode THEN DO:    
            IF phone.e_mail NE "" AND NOT CAN-DO(opcRecipient,phone.e_mail) THEN
            opcRecipient = opcRecipient
                         + (IF opcRecipient NE "" THEN "," ELSE "")
                         + phone.e_mail
                         .
        END.        
    END. /* each phone */
    IF opcRecipient EQ "" OR opcRecipient EQ ? THEN DO:
        IF ipcType BEGINS "CUSTOMER" THEN DO:
            FIND FIRST bCust NO-LOCK 
                WHERE bCust.rec_key EQ ipcRecKey
                  AND bCust.active  EQ "X" 
                NO-ERROR.
            IF AVAILABLE bCust THEN DO:
                FOR EACH phone NO-LOCK
                    WHERE phone.table_rec_key EQ bCust.rec_key,
                     EACH reftable NO-LOCK
                    WHERE reftable.rec_key    EQ phone.rec_key
                      AND reftable.code       EQ ipcCode
                    :            
                    opcRecipient = opcRecipient
                                 + (IF opcRecipient NE "" THEN "," ELSE "") 
                                 + phone.e_mail
                                 .
                END.
                IF opcRecipient EQ "" OR opcRecipient EQ ? THEN 
                opcRecipient = bCust.email.
            END.
            ELSE opcRecipient = ipcEMail.
        END.
    END.
END PROCEDURE.

PROCEDURE pCustomer:
    DEFINE INPUT  PARAMETER ipcType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIdxKey    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCode      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRecipient AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cRecipient AS CHARACTER NO-UNDO.

    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ ipcCompany
          AND cust.cust-no EQ ipcIdxKey
        NO-ERROR.
    IF AVAILABLE cust THEN
    RUN pBuildToList (
        ipcType,
        cust.rec_key,
        cust.email,
        ipcCode,
        OUTPUT opcRecipient
        ).
    IF NUM-ENTRIES (ipcType,"|") GE 2 THEN DO: /* shipto */
        FIND FIRST shipto NO-LOCK
            WHERE shipto.company EQ ipcCompany
              AND shipto.cust-no EQ ipcIdxKey
              AND shipto.ship-id EQ ENTRY(2,ipcType,"|")
            NO-ERROR.
        IF AVAILABLE shipto THEN
        RUN pBuildToList (
            ipcType,
            shipto.rec_key,
            "",
            ipcCode,
            OUTPUT cRecipient
            ).
    END.
    IF cRecipient NE "" THEN
    opcRecipient = cRecipient + "," + opcRecipient. 
END PROCEDURE.

PROCEDURE pCustomerExt:
    DEFINE INPUT  PARAMETER ipcType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIdxKey    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipc2ndGroup  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipc2ndKey    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCode      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRecipient AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cRecipient AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount     AS INTEGER   NO-UNDO.

    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ ipcCompany
          AND cust.cust-no EQ ipcIdxKey
        NO-ERROR.
    IF AVAILABLE cust THEN
    RUN pBuildToList (
        ipcType,
        cust.rec_key,
        cust.email,
        ipcCode,
        OUTPUT opcRecipient
        ).
    IF ipc2ndKey NE "" THEN DO:
        ASSIGN
            cRecipient = ""
            iCount      = 0
            .  
        CASE ipc2ndGroup:
            WHEN "SHIPTO" THEN
            DO WHILE iCount LE NUM-ENTRIES(ipc2ndKey):
                iCount = iCount + 1.
                FIND FIRST shipto NO-LOCK
                    WHERE shipto.company EQ ipcCompany
                      AND shipto.rec_key EQ ENTRY(iCount,ipc2ndKey)
                    NO-ERROR.
                IF AVAILABLE shipto THEN
                RUN buildToList (
                    ipcType,
                    shipto.rec_key,
                    "",
                    ipcCode,
                    OUTPUT cRecipient
                    ).
                IF cRecipient NE "" THEN
                opcRecipient = cRecipient + "," + opcRecipient.
            END.
        END CASE.        
    END.
END PROCEDURE.

PROCEDURE pLoc:
    DEFINE INPUT  PARAMETER ipcType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIdxKey    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRecipient AS CHARACTER NO-UNDO.

    FOR EACH loc NO-LOCK
        WHERE loc.company EQ ipcCompany
          AND loc.loc     EQ ipcIdxKey, 
        EACH location NO-LOCK
        WHERE location.locationCode EQ loc.loc 
          AND location.rec_key      EQ loc.addrRecKey
        :
        opcRecipient = location.email.
    END.   
END PROCEDURE.

PROCEDURE pSalesRep:
    DEFINE INPUT  PARAMETER ipcType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIdxKey    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCode      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRecipient AS CHARACTER NO-UNDO.

    FIND FIRST sman NO-LOCK
        WHERE sman.company EQ ipcCompany
          AND sman.sman    EQ ipcIdxKey
        NO-ERROR.
    IF AVAILABLE sman THEN
    RUN pBuildToList (
        ipcType,
        sman.rec_key,
        "",
        ipcCode,
        OUTPUT opcRecipient
        ).
END PROCEDURE.

PROCEDURE pShipTo:
    DEFINE INPUT  PARAMETER ipcType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIdxKey    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCode      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRecipient AS CHARACTER NO-UNDO.

    FIND FIRST shipto NO-LOCK
        WHERE shipto.company EQ ipcCompany
          AND shipto.rec_key EQ ipcIdxKey
        NO-ERROR.
    IF AVAILABLE shipto THEN
    RUN pBuildToList (
        ipcType,
        shipto.rec_key,
        "",
        ipcCode,
        OUTPUT opcRecipient
        ).
END PROCEDURE.

PROCEDURE pSoldTo:
    DEFINE INPUT  PARAMETER ipcType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIdxKey    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCode      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRecipient AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cRecipient AS CHARACTER NO-UNDO.

    FIND FIRST soldto NO-LOCK
        WHERE soldto.company EQ ipcCompany
          AND soldto.cust-no EQ ENTRY(1,ipcIdxKey,"|")
          AND soldto.sold-id EQ (ENTRY(2,ipcIdxKey,"|"))
        NO-ERROR.
    IF AVAILABLE soldto THEN DO:
        RUN pBuildToList (
            ipcType,
            soldto.rec_key,
            "",
            ipcCode,
            OUTPUT opcRecipient
            ).
        FIND cust OF soldto NO-LOCK NO-ERROR.
        IF AVAILABLE cust THEN DO:
            cRecipient = "".
            RUN pBuildToList (
                ipcType,
                cust.rec_key,
                "",
                ipcCode,
                OUTPUT cRecipient
                ).
            IF cRecipient NE "" THEN
            opcRecipient = cRecipient + "," + opcRecipient.
        END.
        IF NUM-ENTRIES (ipcIdxKey,"|") GE 3 THEN DO:  /* shipto */
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ ipcCompany
                  AND shipto.cust-no EQ ENTRY(1,ipcIdxKey,"|")
                  AND shipto.ship-id EQ (ENTRY(3,ipcIdxKey,"|"))
                NO-ERROR.         
            IF AVAILABLE shipto THEN DO:
                cRecipient = "".  
                RUN pBuildToList (
                    ipcType,
                    shipto.rec_key,
                    "",
                    ipcCode,
                    OUTPUT cRecipient
                    ).
                IF cRecipient NE "" THEN
                opcRecipient = cRecipient + "," + opcRecipient.                         
            END.
        END.
    END.
    ELSE opcRecipient = "".
    IF opcRecipient EQ "" THEN DO:
        ipcType = "CUSTOMER".
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ ipcCompany
              AND cust.cust-no EQ ENTRY(1,ipcIdxKey,"|")
            NO-ERROR.
        IF AVAILABLE cust THEN
        RUN pBuildToList (
            ipcType,
            cust.rec_key,
            cust.email,
            ipcCode,
            OUTPUT opcRecipient
            ).         
    END.  
END PROCEDURE.

PROCEDURE pVendor:
    DEFINE INPUT  PARAMETER ipcType      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcIdxKey    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCode      AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRecipient AS CHARACTER NO-UNDO.

    FIND FIRST vend NO-LOCK
        WHERE vend.company EQ ipcCompany
          AND vend.vend-no EQ ipcIdxKey
        NO-ERROR.
    IF AVAILABLE vend THEN
    RUN pBuildToList (
        ipcType,
        vend.rec_key,
        "",
        ipcCode,
        OUTPUT opcRecipient
        ).
END PROCEDURE.
