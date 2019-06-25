
/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{EbLook.i}
DEFINE INPUT PARAMETER prmCust      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEbLook.

DEFINE VAR vEst AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

IF prmText      = ? THEN ASSIGN prmText      = "".
ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .
IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmCust        = ? THEN ASSIGN prmCust        = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    v-count = 0.
    MAIN-LOOP:
    FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
        OE-LOOP:
        FOR EACH eb WHERE eb.company = prmComp AND eb.cust-no = usercust.cust-no no-lock:
            FIND FIRST ttEbLook WHERE ttEbLook.Estimate = eb.est-no NO-LOCK NO-ERROR.
            IF AVAIL ttEbLook THEN NEXT OE-LOOP.
            create ttEbLook.
            assign                                     
                ttEbLook.Estimate    = eb.est-no
                ttEbLook.Customer    = eb.cust-no 
                ttEbLook.Type        = eb.est-type
                .                    
                      v-count = v-count + 1.
                      IF v-count = 150 THEN LEAVE MAIN-LOOP.
        END.	 /* FOR EACH eb */
    END. /*FOR EACH usercust WHERE*/
 END.  /*if prmAction <> "search" */

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
             v-count = 0.
             MAIN-LOOP:
            FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
           FOR EACH eb where eb.company = prmComp  AND eb.cust-no = usercust.cust-no AND eb.est-no = vEst no-lock:
               create ttEbLook.
               assign                                     
                   ttEbLook.Estimate    = eb.est-no
                   ttEbLook.Customer    = eb.cust-no 
                   ttEbLook.Type        = eb.est-type.
                     
                      v-count = v-count + 1.
                      IF v-count = 100 THEN LEAVE MAIN-LOOP.
               
               END.   /*FOR EACH eb*/
            END. /*for each usercust*/
    END.   /* IF prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
         v-count = 0.
        MAIN-LOOP:
       FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
           FOR EACH eb where eb.company = prmComp  AND eb.cust-no = usercust.cust-no AND eb.est-no begins vEst no-lock:

            create ttEbLook.
               assign                                     
                   ttEbLook.Estimate    = eb.est-no
                   ttEbLook.Customer    = eb.cust-no 
                   ttEbLook.Type        = eb.est-type.

                     v-count = v-count + 1.
                      IF v-count = 100 THEN LEAVE MAIN-LOOP.
            
        END.  /*FOR EACH eb*/
       END. /*for each usercust*/
    END.   /*if prmCondition = "BEGIN" */     
    END.   /*IF prmField = "ANY" then do:*/
    if prmField = "est-no" then do:
        if prmCondition = "EQUAL" then do:
             v-count = 0.
            MAIN-LOOP:
            FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
           FOR EACH eb where eb.company = prmComp  AND eb.cust-no = usercust.cust-no AND eb.est-no = vEst no-lock:
               
                create ttEbLook.
                assign                                     
                    ttEbLook.Estimate    = eb.est-no
                    ttEbLook.Customer    = eb.cust-no 
                    ttEbLook.Type        = eb.est-type
                    .             
                 v-count = v-count + 1.
                      IF v-count = 100 THEN LEAVE MAIN-LOOP.
           END.  /*FOR EACH eb*/
            END. /*for each usercust*/
        END.   /*if prmCondition = "EQUAL"*/
        if prmCondition = "BEGIN" then do:
             v-count = 0.
              MAIN-LOOP:
                  FOR EACH usercust WHERE
        usercust.user_id = prmUser AND
        usercust.company = prmComp
        AND (usercust.cust-no = prmCust OR prmCust = "") NO-LOCK:
            FOR EACH eb where eb.company = prmComp  AND eb.cust-no = usercust.cust-no AND eb.est-no begins vEst no-lock:
                create ttEbLook.
                assign                                                                  
                    
                    ttEbLook.Estimate    = eb.est-no
                    ttEbLook.Customer    = eb.cust-no 
                    ttEbLook.Type        = eb.est-type
                    .                       
                     v-count = v-count + 1.
                      IF v-count = 100 THEN LEAVE MAIN-LOOP.
            END.  /*FOR EACH eb*/
            END. /*for each usercust*/
        END.  /*if prmCondition = "BEGIN"*/       
    END.   /*  if prmField = "part-no" then do:*/
 END. /* IF prmAction = search then do: */


