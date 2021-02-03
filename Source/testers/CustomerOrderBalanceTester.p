
/*------------------------------------------------------------------------
    File        : testers/CustomerOrderBalanceTester.p
    Purpose     : 

    Syntax      :

    Description : Tester for Customer_CalculateOrderBalance procedure

    Author(s)   : Rahul Rawat
    Created     : Wed Feb 03 00:40:39 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/
    {sys/inc/var.i "new shared"}
/* ***************************  Definitions  ************************** */

    DEFINE VARIABLE cBeginingCust      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndingCust        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dOrdBalanceLegacy  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOrderBalanceProcs AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hdCustomerProcs    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cCustomerNo        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lError             AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
/* ***************************  Main Block  *************************** */
    
    RUN system/Customerprocs.p PERSISTENT SET hdCustomerProcs.
    
    FORM
        SKIP 
        cBeginingCust COLON 20 LABEL "Begining Customer"
        cEndingCust   COLON 55 LABEL "Ending Customer"
    WITH FRAME a SIDE-LABELS 
    WIDTH 80 CENTERED 
    TITLE "Order Balance Tester".
    
    FORM 
        cCustomerNo        LABEL "Customer#"
        dOrdBalanceLegacy  LABEL "Orde balance (updCust1.p)" FORMAT "->>,>>>,>>9.99"
        dOrderBalanceProcs LABEL "Order balance (New Procs)" FORMAT "->>,>>>,>>9.99"
    WITH FRAME b WIDTH 80.
    
    UPDATE 
        cBeginingCust 
        cEndingCust 
    WITH FRAME a.
        
    FOR EACH cust NO-LOCK
        WHERE company EQ "001"
          AND cust-no GE cBeginingCust
          AND cust-no LE cEndingCust:
        RUN ar/updcust1.p (
            INPUT  YES,
            BUFFER cust,
            OUTPUT dOrdBalanceLegacy
            ).
        RUN Customer_CalculateOrderBalance IN hdCustomerProcs(
            INPUT  ROWID(cust),
            INPUT  YES,
            OUTPUT dOrderBalanceProcs,
            OUTPUT lError,
            OUTPUT cMessage
            ).
        DISPLAY 
            cust-no @ cCustomerNo 
            dOrdBalanceLegacy 
            dOrderBalanceProcs 
        WITH DOWN FRAME b WIDTH 80.
        DOWN WITH FRAME b.
    END.
    IF VALID-HANDLE(hdCustomerProcs) THEN
        DELETE PROCEDURE hdCustomerProcs.