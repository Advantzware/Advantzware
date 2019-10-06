
/*------------------------------------------------------------------------
    File        : UpdVendPayType.p
    Purpose     : 

    Syntax      :

    Description : Update  new vendor payment type fields from old fields

    Author(s)   : 
    Created     : Sat Sep 10 14:36:35 EDT 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN pUpdateVendorPaymentType.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pCheckPayMaster:
    /*------------------------------------------------------------------------------
     Purpose: Creates a payment-type record if it does not exist
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDescription AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPaper AS LOGICAL NO-UNDO.

    IF NOT CAN-FIND(FIRST payment-type WHERE payment-type.company EQ ipcCompany AND payment-type.type EQ ipcType) THEN 
    DO:
        CREATE payment-type.
        ASSIGN
            payment-type.company    = company.company
            payment-type.type       = ipcType
            payment-type.dscr       = ipcDescription
            payment-type.paperCheck = iplPaper
            .
    END. 

END PROCEDURE.

PROCEDURE pUpdateVendorPaymentType:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH company NO-LOCK:
        /*Set Payment Type Defaults*/
        RUN pCheckPayMaster (company.company,"Check","Paper Check",YES).
        RUN pCheckPayMaster (company.company,"Bill Pay","Online Bill Pay",NO).
        RUN pCheckPayMaster (company.company,"Credit Card","Credit Card",NO).
        RUN pCheckPayMaster (company.company,"ACH","ACH Electronic Transfer",NO).
        
        /*Convert spare-int values to payment-type*/
        FOR EACH vend 
            WHERE vend.company EQ company.company:
            IF vend.spare-int-1 = 1 THEN vend.payment-type = "ACH".
            ELSE IF vend.spare-int-2 = 1 THEN vend.payment-type = "Bill Pay".
                ELSE vend.payment-type = "Check".
               
        END.
    END.
END PROCEDURE.




