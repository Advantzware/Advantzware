/* aoaAR.p */

/* ** temp-table definitions **************************************** */
DEFINE TEMP-TABLE ttCust NO-UNDO RCODE-INFORMATION
    FIELD company  LIKE cust.company  
    FIELD custNo   LIKE cust.cust-no
    FIELD custName LIKE cust.name
        INDEX ttCompany  company 
        INDEX ttCustNo   custNo
        INDEX ttCustName custName
        .

/* ** function declarations ***************************************** */
FUNCTION fCustomers RETURNS HANDLE ():
    DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO .
    
    EMPTY TEMP-TABLE ttCust .
    
    FOR EACH cust NO-LOCK 
        WHERE cust.company EQ DYNAMIC-FUNCTION('fGetCompanySCC') :
        CREATE ttCust .
        ASSIGN 
            ttCust.company  = cust.company
            ttCust.custNo   = cust.cust-no
            ttCust.custName = cust.name
            .
        BUFFER-COPY cust TO ttCust .
    END .

    RETURN TEMP-TABLE ttCust:HANDLE .
END FUNCTION .

/* ** procedure declarations **************************************** */
