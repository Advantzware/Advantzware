
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
DEFINE VARIABLE iCountSets AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountFGSets AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountFGSetsProcessed AS INTEGER NO-UNDO.
DEFINE VARIABLE iCountFGSetsInitialized AS INTEGER NO-UNDO.
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
FOR EACH company NO-LOCK 
, EACH fg-set EXCLUSIVE-LOCK
    WHERE fg-set.company EQ company.company:
        iCountFGSets = iCountFGSets + 1.
        IF fg-set.qtyPerSet EQ 0 AND fg-set.part-qty NE 0 THEN 
            ASSIGN 
                iCountFGSetsProcessed = iCountFGSetsProcessed + 1 
                fg-set.qtyPerSet = fg-set.part-qty.
        IF fg-set.qtyPerSet EQ 0 THEN 
            ASSIGN 
                iCountFGSetsInitialized = iCountFGSetsInitialized + 1 
                fg-set.qtyPerSet = 1.
END.     
MESSAGE "Total Estimates: " iCount SKIP 
        "Converted from .yld-qty: " iCountProcessed SKIP
        "Initialized to 1: " iCountInitialized SKIP(2) 
        "Total Sets: " iCountFGSets SKIP 
        "Sets Converted from .part-qty to .qtyPerSet: " iCountFGSetsProcessed SKIP 
        "Sets Initialized to 1" iCountFGSetsInitialized 
        VIEW-AS ALERT-BOX.