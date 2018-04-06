
/*------------------------------------------------------------------------
    File        : ConvertQtyPerSet.p
    Purpose     : 

    Syntax      :

    Description : Converts .yld-qty field to .quantityPerSet - ticket 26146

    Author(s)   : BV
    Created     : Mon Mar 19 16:19:58 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE dQtyPerSet AS DECIMAL NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountProcessed AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountInitialized AS INTEGER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FOR EACH company NO-LOCK 
  ,EACH est NO-LOCK
    WHERE est.company EQ company.company,
    EACH eb OF est EXCLUSIVE-LOCK 
    :
        iCount = iCount + 1.
        CASE est.est-type:
            WHEN 5 OR WHEN 6 THEN DO:
                IF eb.quantityPerSet EQ 0 THEN DO: 
                    ASSIGN 
                        iCountProcessed = iCountProcessed + 1 
                        dQtyPerSet = eb.yld-qty
                        .
                    IF dQtyPerSet LT 0 THEN dQtyPerSet = -1 / dQtyPerSet.
                    IF dQtyPerSet EQ 0 THEN dQtyPerSet = 1.
                    eb.quantityPerSet = dQtyPerSet.   
                END.
            END.
  /*Folding carton uses %-cust - out of scope for ticket 25146*/     
/*            WHEN 1 OR WHEN 2 THEN      */
/*                dQtyPerSet = eb.cust-%.*/
/*                                       */
        END CASE.
        IF eb.quantityPerSet EQ 0 THEN 
            ASSIGN 
                eb.quantityPerSet = 1
                iCountInitialized = iCountInitialized + 1.
END.
MESSAGE "Total Estimates: " iCount SKIP 
        "Converted from .yld-qty: " iCountProcessed SKIP
        "Initialized to 1: " iCountInitialized VIEW-AS ALERT-BOX.