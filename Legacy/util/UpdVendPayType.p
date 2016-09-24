
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

RUN UpdateVendorPaymentType.

/* **********************  Internal Procedures  *********************** */

PROCEDURE UpdateVendorPaymentType:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH company NO-LOCK:
        CREATE payment-type.
        ASSIGN
            payment-type.company    = company.company
            payment-type.type       = "Check"
            payment-type.dscr       = "Paper Check"
            payment-type.paperCheck = YES
            .
        CREATE payment-type.
        ASSIGN
            payment-type.company    = company.company
            payment-type.type       = "Bill Pay"
            payment-type.dscr       = "Online Bill Pay"
            payment-type.paperCheck = NO
            .
        CREATE payment-type.
        ASSIGN
            payment-type.company    = company.company
            payment-type.type       = "Credit Card"
            payment-type.dscr       = "Credit Card"
            payment-type.paperCheck = NO
            .
        CREATE payment-type.
        ASSIGN
            payment-type.company    = company.company
            payment-type.type       = "ACH"
            payment-type.dscr       = "ACH Electronic Transfer"
            payment-type.paperCheck = NO
            .
        FOR EACH vend 
            WHERE vend.company EQ company.company:
            IF vend.spare-int-1 = 1 THEN vend.payment-type = "Credit Card".
            IF vend.spare-int-2 = 1 THEN vend.payment-type = "Bill Pay".
            ELSE vend.payment-type = "Check".
               
        END.
    END.
END PROCEDURE.



