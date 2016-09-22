
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
   FOR EACH vend :
       If vend.spare-int-1 = 1 THEN vend.payment-type = "Credit Card".
       IF vend.spare-int-2 = 1 THEN vend.payment-type = "Bill Pay".
       ELSE vend.payment-type = "Check".
           
   END.

END PROCEDURE.



