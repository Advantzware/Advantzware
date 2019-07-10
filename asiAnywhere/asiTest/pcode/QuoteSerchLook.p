
/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttQuoteSerchLook NO-UNDO 
        FIELD vEst AS CHARACTER
        FIELD vCustomer AS CHARACTER
        FIELD vQuote     AS INTEGER  
        FIELD jkiyugg   AS CHAR 
        .
DEFINE DATASET dsQuoteSerchLook  FOR ttQuoteSerchLook .

DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmStat      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQuoteSerchLook.

DEFINE VAR vEst AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmText      = ? THEN ASSIGN prmText      = "".
ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .
IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmCust        = ? THEN ASSIGN prmCust        = "".
IF prmStat        = ? THEN ASSIGN prmStat = "open".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction = "search" then do:

    FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
        IF prmStat = "open" THEN DO:
        OE-LOOP:
        FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no AND oe-ordl.q-no <> 0 AND (oe-ordl.q-no = int(prmText) OR prmText = "")
                AND oe-ordl.opened = YES NO-LOCK,
            first oe-ord of oe-ordl no-lock where oe-ord.opened eq yes AND oe-ordl.stat NE "C",
           EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.q-no = oe-ordl.q-no   NO-LOCK BY quotehd.q-no DESC:
            FIND FIRST ttQuoteSerchLook WHERE ttQuoteSerchLook.vQuote = quotehd.q-no  NO-LOCK NO-ERROR.
            IF AVAIL ttQuoteSerchLook THEN NEXT OE-LOOP.
            create ttQuoteSerchLook.
            assign                                     
                ttQuoteSerchLook.vEst           = quotehd.est-no
                ttQuoteSerchLook.vCustomer      = quotehd.cust-no 
                ttQuoteSerchLook.vQuote        = quotehd.q-no
                .                       
        END.	 /* FOR EACH eb */
        
        END. /* prmStat = open*/
         IF prmStat = "closed" THEN DO:
        OE-LOOP:
        FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no AND oe-ordl.q-no <> 0 AND (oe-ordl.q-no = int(prmText) OR prmText = "")
                AND oe-ordl.opened = NO NO-LOCK,
                first oe-ord of oe-ordl no-lock where oe-ord.opened eq no OR oe-ordl.stat EQ "C",
           EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.q-no = oe-ordl.q-no   NO-LOCK BY quotehd.q-no DESC:
            FIND FIRST ttQuoteSerchLook WHERE ttQuoteSerchLook.vQuote = quotehd.q-no  NO-LOCK NO-ERROR.
            IF AVAIL ttQuoteSerchLook THEN NEXT OE-LOOP.
            create ttQuoteSerchLook.
            assign                                     
                ttQuoteSerchLook.vEst           = quotehd.est-no
                ttQuoteSerchLook.vCustomer      = quotehd.cust-no 
                ttQuoteSerchLook.vQuote        = quotehd.q-no
                .                       
        END.	 /* FOR EACH eb */
        
        END. /* prmStat = open*/
         IF prmStat = "pending" THEN DO:
        OE-LOOP:
        FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no AND oe-ordl.q-no <> 0 AND (oe-ordl.q-no = int(prmText) OR prmText = "")
                AND oe-ordl.opened = YES NO-LOCK,
            first oe-ord of oe-ordl no-lock where oe-ord.opened eq yes and oe-ord.stat eq "w",
           EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.q-no = oe-ordl.q-no   NO-LOCK BY quotehd.q-no DESC:
            FIND FIRST ttQuoteSerchLook WHERE ttQuoteSerchLook.vQuote = quotehd.q-no  NO-LOCK NO-ERROR.
            IF AVAIL ttQuoteSerchLook THEN NEXT OE-LOOP.
            create ttQuoteSerchLook.
            assign                                     
                ttQuoteSerchLook.vEst           = quotehd.est-no
                ttQuoteSerchLook.vCustomer      = quotehd.cust-no 
                ttQuoteSerchLook.vQuote        = quotehd.q-no
                .                       
        END.	 /* FOR EACH eb */
        
        END. /* prmStat = open*/
         IF prmStat = "any" THEN DO:
        OE-LOOP:
        FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no AND oe-ordl.q-no <> 0 AND (oe-ordl.q-no = int(prmText) OR prmText = "")
                 NO-LOCK,
           EACH quotehd WHERE   quotehd.company = prmComp AND quotehd.q-no = oe-ordl.q-no   NO-LOCK BY quotehd.q-no DESC:
            FIND FIRST ttQuoteSerchLook WHERE ttQuoteSerchLook.vQuote = quotehd.q-no  NO-LOCK NO-ERROR.
            IF AVAIL ttQuoteSerchLook THEN NEXT OE-LOOP.
            create ttQuoteSerchLook.
            assign                                     
                ttQuoteSerchLook.vEst           = quotehd.est-no
                ttQuoteSerchLook.vCustomer      = quotehd.cust-no 
                ttQuoteSerchLook.vQuote        = quotehd.q-no
                .                       
        END.	 /* FOR EACH eb */
        
        END. /* prmStat = open*/
    END. /*FOR EACH usercust WHERE*/
 END.  /*if prmAction <> "search" */







 /*
IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            MESSAGE "user2" prmUser.
            FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
                FOR EACH oe-ordl WHERE oe-ordl.company = prmComp
                AND oe-ordl.cust-no = usercust.cust-no AND oe-ordl.est-no <> ""
                AND oe-ordl.opened = YES NO-LOCK:
           FOR EACH eb where eb.company = prmComp  AND eb.cust-no = usercust.cust-no AND eb.est-no = vEst no-lock:
               create ttQuoteSerchLook.
               assign                                     
                   ttQuoteSerchLook.Estimate    = eb.est-no
                   ttQuoteSerchLook.Customer    = eb.cust-no 
                   ttQuoteSerchLook.Type        = eb.est-type.
               
               
               END.   /*FOR EACH eb*/
            END. /*for each usercust*/
    END.   /* IF prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
       FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
           FOR EACH eb where eb.company = prmComp  AND eb.cust-no = usercust.cust-no AND eb.est-no begins vEst no-lock:

            create ttQuoteSerchLook.
               assign                                     
                   ttQuoteSerchLook.Estimate    = eb.est-no
                   ttQuoteSerchLook.Customer    = eb.cust-no 
                   ttQuoteSerchLook.Type        = eb.est-type.
            
        END.  /*FOR EACH eb*/
       END. /*for each usercust*/
    END.   /*if prmCondition = "BEGIN" */     
    END.   /*IF prmField = "ANY" then do:*/
    if prmField = "est-no" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
           FOR EACH eb where eb.company = prmComp  AND eb.cust-no = usercust.cust-no AND eb.est-no = vEst no-lock:
               MESSAGE "test22" prmComp vEst.
                create ttQuoteSerchLook.
                assign                                     
                    ttQuoteSerchLook.Estimate    = eb.est-no
                    ttQuoteSerchLook.Customer    = eb.cust-no 
                    ttQuoteSerchLook.Type        = eb.est-type
                    .                       
           END.  /*FOR EACH eb*/
            END. /*for each usercust*/
        END.   /*if prmCondition = "EQUAL"*/
        if prmCondition = "BEGIN" then do:
                  FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
            FOR EACH eb where eb.company = prmComp  AND eb.cust-no = usercust.cust-no AND eb.est-no begins vEst no-lock:
                create ttQuoteSerchLook.
                assign                                                                  
                    
                    ttQuoteSerchLook.Estimate    = eb.est-no
                    ttQuoteSerchLook.Customer    = eb.cust-no 
                    ttQuoteSerchLook.Type        = eb.est-type
                    .                       
            END.  /*FOR EACH eb*/
            END. /*for each usercust*/
        END.  /*if prmCondition = "BEGIN"*/       
    END.   /*  if prmField = "part-no" then do:*/
 END. /* IF prmAction = search then do: */


*/
