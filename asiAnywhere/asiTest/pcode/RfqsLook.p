
/*------------------------------------------------------------------------
    File         : Rfqs Lookup
    Purpose     :  Rfqs lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Kuldeep
    Created     : Feb 13 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{RfqsLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer  AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRfqsLook.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.

IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmCustomer  = ? THEN ASSIGN prmCustomer  = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.company = prmComp AND
     usercomp.loc NE "" AND
     usercomp.loc_default = yes
     NO-LOCK NO-ERROR.

prmLoc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

FOR EACH usercust WHERE usercust.user_id = prmUser 
                    AND usercust.company EQ prmComp AND (usercust.cust-no EQ prmCustomer OR prmCustomer = "")
                    NO-LOCK :

    
if prmAction <> "search" then do:
    
        FOR EACH rfq WHERE rfq.cust-no EQ usercust.cust-no NO-LOCK  :
            create ttRfqsLook.
            assign                                     
                ttRfqsLook.kRqfsNo    = rfq.rfq-no
                ttRfqsLook.kCust      = rfq.cust-no.
        END. /* FOR EACH rfq */
     
END.  /*ifif prmAction <> "search" */

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH rfq where rfq.company EQ prmComp AND rfq.cust-no EQ usercust.cust-no
                no-lock:

               IF rfq.rfq-no = int(prmText)  THEN
               DO:
                  create ttRfqsLook.
                  assign                                     
                      ttRfqsLook.kRqfsNo    = rfq.rfq-no
                      ttRfqsLook.kCust    = rfq.cust-no.
               END.
           END.   /*FOR EACH rfq*/
    END.   /* IF prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
        FOR EACH rfq where 
            rfq.company EQ prmComp AND rfq.cust-no EQ usercust.cust-no
            no-lock:

            IF rfq.rfq-no = int(prmText) THEN
            DO:
               create ttRfqsLook.
               assign                                     
                   ttRfqsLook.kRqfsNo    = rfq.rfq-no
                   ttRfqsLook.kCust    = rfq.cust-no.
            END.

        END.  /*FOR EACH rfq*/
    END.   /*if prmCondition = "BEGIN" */     
    END.   /*IF prmField = "ANY" then do:*/
    if prmField = "rfq-no" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH rfq where rfq.company EQ prmComp AND rfq.cust-no EQ usercust.cust-no
                no-lock:

               IF rfq.rfq-no = int(prmText)  THEN
               DO:
                  create ttRfqsLook.
                  assign                                     
                      ttRfqsLook.kRqfsNo    = rfq.rfq-no
                      ttRfqsLook.kCust    = rfq.cust-no.
               END.
           END.   /*FOR EACH rfq*/
        END.   /*if prmCondition = "EQUAL"*/
        if prmCondition = "BEGIN" then do:
            FOR EACH rfq where rfq.company EQ prmComp AND rfq.cust-no EQ usercust.cust-no
                no-lock:

               IF rfq.rfq-no = int(prmText)  THEN
               DO:
                  create ttRfqsLook.
                  assign                                     
                      ttRfqsLook.kRqfsNo    = rfq.rfq-no
                      ttRfqsLook.kCust    = rfq.cust-no.
               END.
           END.   /*FOR EACH rfq*/
        END.  /*if prmCondition = "BEGIN"*/       
    END.   /*  if prmField = "rfq-no" then do:*/
    
END. /* IF prmAction = search then do: */

END.
