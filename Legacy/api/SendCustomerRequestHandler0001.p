  /*------------------------------------------------------------------------
    File        : api/SendCustomerRequestHandler0001.p
    Purpose     : Returns the request data for customer addition

    Syntax      :

    Description : Returns the request data for customer addition

    Author(s)   : Vishnu Vellanki
    Created     : Tue Jun 07 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
    {api/ttArgs.i}
    
    DEFINE INPUT        PARAMETER TABLE               FOR ttArgs.
    DEFINE INPUT-OUTPUT PARAMETER ioplcRequestData    AS LONGCHAR  NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage          AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cCalcAddress AS CHARACTER NO-UNDO.
        
    FIND FIRST ttArgs
         WHERE ttArgs.argType  = "ROWID"
           AND ttArgs.argKey   = "cust" NO-ERROR.
    IF NOT AVAILABLE ttArgs THEN DO:
        ASSIGN
            opcMessage = "No valid cust record passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
    
    FIND FIRST cust NO-LOCK
         WHERE ROWID(cust) = TO-ROWID(ttArgs.argValue) NO-ERROR.
    IF NOT AVAILABLE cust THEN DO:
        ASSIGN
            opcMessage = "Invalid cust ROWID passed to handler"
            oplSuccess = FALSE
            .
        RETURN.
    END.
    
    ASSIGN
        ioplcRequestData = REPLACE(ioplcRequestData,"cust.company",cust.company)
        ioplcRequestData = REPLACE(ioplcRequestData,"cust.cust-no",cust.cust-no)
        ioplcRequestData = REPLACE(ioplcRequestData,"cust.name",cust.name)
        ioplcRequestData = REPLACE(ioplcRequestData,"cust.type",cust.type)
        cCalcAddress     = cust.addr[1] + ", " + cust.addr[2] + ", " + cust.city + ", " + cust.state + ", " + cust.zip
        ioplcRequestData = REPLACE(ioplcRequestData,"calcAddress", cCalcAddress)
        opcMessage       = ""
        oplSuccess       = TRUE
        .                       
