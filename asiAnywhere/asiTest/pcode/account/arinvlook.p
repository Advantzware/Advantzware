/*------------------------------------------------------------------------
    File        : arinvlook.p
    Purpose     : Invoice

    Syntax      :

    Description : Return a Dataset of all customer a/c no

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCrDbInvoiceLookup NO-UNDO 
    FIELD custno       AS CHAR 
    FIELD invno        AS INT
    FIELD invdt        AS CHAR
    FIELD net          AS DEC
    FIELD paid         AS DEC
    FIELD due          AS DEC
        .

DEFINE DATASET dsCrDbInvoiceLookup FOR ttCrDbInvoiceLookup.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCust   AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCrDbInvoiceLookup.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmCust   = ? THEN ASSIGN prmCust   = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


    if prmAction <> "search" then do:
        MESSAGE "test1 " prmCust .
        FOR EACH ar-inv WHERE ar-inv.posted = yes 
            AND ASI.ar-inv.company = prmComp 
            AND ar-inv.cust-no = prmCust NO-LOCK:
        MESSAGE "test2 " .
            create ttCrDbInvoiceLookup.
                assign                                        
                   ttCrDbInvoiceLookup.custno  =  ar-inv.cust-no
                   ttCrDbInvoiceLookup.invno   =  ar-inv.inv-no
                   ttCrDbInvoiceLookup.invdt   =  STRING(ar-inv.inv-date)  
                   ttCrDbInvoiceLookup.net     =  ar-inv.net
                   ttCrDbInvoiceLookup.paid    =  ar-inv.paid
                   ttCrDbInvoiceLookup.due     =  ar-inv.due      .
        END.	 /* FOR EACH item */     

    END.  /*if prmAction <> "search" then do*/ 


    IF prmAction = "search" then do:

        if prmField = "inv"  then do:
            if prmCondition = "EQUAL" then do:
                  FOR EACH ar-inv WHERE ar-inv.posted = yes
                      AND ASI.ar-inv.company = prmComp 
                      AND ar-inv.cust-no = prmCust
                       AND ar-inv.inv-no EQ INT(prmText)  NO-LOCK :

                      create ttCrDbInvoiceLookup.
                      assign                                        
                          ttCrDbInvoiceLookup.custno  =  ar-inv.cust-no
                          ttCrDbInvoiceLookup.invno   =  ar-inv.inv-no
                          ttCrDbInvoiceLookup.invdt   =  STRING(ar-inv.inv-date)  
                          ttCrDbInvoiceLookup.net     =  ar-inv.net
                          ttCrDbInvoiceLookup.paid    =  ar-inv.paid
                          ttCrDbInvoiceLookup.due     =  ar-inv.due      .

                   END.	 /* FOR EACH item */     
              END. /*if prmCondition = EQUAL */
         end.  /* if prmField = est  */

    END.  /* IF prmAction = search then do: */
