/* aoaAR.p */

/* ** temp-table definitions **************************************** */

DEFINE TEMP-TABLE ttCust NO-UNDO RCODE-INFORMATION
    FIELD company  LIKE cust.company  
    FIELD custNo   LIKE cust.cust-no
    FIELD custName LIKE cust.name
        INDEX ttCust company custNo
        .

/* ** function declarations ***************************************** */

FUNCTION fCustomers RETURNS HANDLE (ipcCompanyNo   AS CHARACTER,
                                    ipcStartCustNo AS CHARACTER,
                                    ipcEndCustNo   AS CHARACTER):
    EMPTY TEMP-TABLE ttCust .
    
    FOR EACH cust NO-LOCK 
        WHERE cust.company EQ ipcCompanyNo
          AND cust.cust-no GT ipcStartCustNo
          AND cust.cust-no LE ipcEndCustNo
        :
        CREATE ttCust .
        ASSIGN 
            ttCust.company  = cust.company
            ttCust.custNo   = cust.cust-no
            ttCust.custName = cust.name
            .
    END .

    RETURN TEMP-TABLE ttCust:HANDLE .
END FUNCTION .

/* ** procedure declarations **************************************** */
