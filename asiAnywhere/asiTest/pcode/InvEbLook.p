
/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{InvEbLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvEbLook.

DEFINE VAR vEst AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmOrderNum      = ? THEN ASSIGN prmOrderNum      = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH eb WHERE eb.ord-no = INT(prmOrderNum) AND eb.company = prmComp  NO-LOCK  :
        create ttInvEbLook.
        assign                                     
            ttInvEbLook.InvEstimate    = eb.est-no
            ttInvEbLook.InvCustomer    = eb.cust-no 
            ttInvEbLook.InvType        = eb.est-type
            .                       
    END.	 /* FOR EACH eb */
    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
    IF AVAILABLE users THEN DO:
        IF users.internal-user = NO THEN DO:
            FOR EACH ttInvEbLook:
                FIND FIRST usercust WHERE usercust.user_id = prmUser 
                    AND usercust.cust-no = ttInvEbLook.InvCustomer
                    AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE usercust THEN DELETE ttInvEbLook.
            END. /*FOR EACH ttInvEbLook*/
        END. /*IF users.internal-user = NO*/
        IF users.internal-user = YES THEN DO:
            FIND FIRST usercust WHERE usercust.user_id = prmUser 
                 AND usercust.company EQ prmComp
                NO-LOCK NO-ERROR.
            IF AVAILABLE usercust THEN DO:
                FOR EACH ttInvEbLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttInvEbLook.InvCustomer
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttInvEbLook.
                END. /*FOR EACH ttInvEbLook*/
            END. /*IF AVAILABLE usercust*/
        END. /*IF users.internal-user = YES*/
    END. /*IF AVAILABLE users*/
    ELSE DO:
        FOR EACH ttInvEbLook:
            DELETE ttInvEbLook.
        END.
    END. /*IF NOT AVAILABLE users*/
END.  /*ifif prmAction <> "search" */
IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
           FOR EACH eb WHERE eb.ord-no = INT(prmOrderNum) AND eb.company = prmComp no-lock:

               IF eb.est-no = vEst or eb.cust-no = prmText or eb.est-type = int(prmText) THEN
               DO:
                  create ttInvEbLook.
                  assign                                     
                      ttInvEbLook.InvEstimate    = eb.est-no
                      ttInvEbLook.InvCustomer    = eb.cust-no 
                      ttInvEbLook.InvType        = eb.est-type.
               END.
           END.   /*FOR EACH eb*/
    END.   /* IF prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
        FOR EACH eb WHERE eb.ord-no = INT(prmOrderNum) AND eb.company = prmComp no-lock:

            IF eb.est-no begins vEst or eb.cust-no begins prmText or eb.est-type = int(prmText) THEN
            DO:
               create ttInvEbLook.
               assign                                     
                   ttInvEbLook.InvEstimate    = eb.est-no
                   ttInvEbLook.InvCustomer    = eb.cust-no 
                   ttInvEbLook.InvType        = eb.est-type.
            END.
        END.  /*FOR EACH eb*/
    END.   /*if prmCondition = "BEGIN" */     
    END.   /*IF prmField = "ANY" then do:*/
    if prmField = "est-no" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH eb WHERE eb.ord-no = INT(prmOrderNum) AND eb.company = prmComp AND eb.est-no = vEst no-lock:
                create ttInvEbLook.
                assign                                     
                    ttInvEbLook.InvEstimate    = eb.est-no
                    ttInvEbLook.InvCustomer    = eb.cust-no 
                    ttInvEbLook.InvType        = eb.est-type
                    .                       
           END.  /*FOR EACH eb*/
        END.   /*if prmCondition = "EQUAL"*/
        if prmCondition = "BEGIN" then do:
            FOR EACH eb WHERE eb.ord-no = INT(prmOrderNum) AND eb.company = prmComp AND eb.est-no begins vEst no-lock:
                create ttInvEbLook.
                assign                                                                  
                    
                    ttInvEbLook.InvEstimate    = eb.est-no
                    ttInvEbLook.InvCustomer    = eb.cust-no 
                    ttInvEbLook.InvType        = eb.est-type
                    .                       
            END.  /*FOR EACH eb*/
        END.  /*if prmCondition = "BEGIN"*/       
    END.   /*  if prmField = "part-no" then do:*/
    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
    IF AVAILABLE users THEN DO:
        IF users.internal-user = NO THEN DO:
            FOR EACH ttInvEbLook:
                FIND FIRST usercust WHERE usercust.user_id = prmUser 
                    AND usercust.cust-no = ttInvEbLook.InvCustomer
                    AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE usercust THEN DELETE ttInvEbLook.
            END. /*FOR EACH ttInvEbLook*/
        END. /*IF users.internal-user = NO*/
        IF users.internal-user = YES THEN DO:
            FIND FIRST usercust WHERE usercust.user_id = prmUser 
                 AND usercust.company EQ prmComp
                NO-LOCK NO-ERROR.
            IF AVAILABLE usercust THEN DO:
                FOR EACH ttInvEbLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttInvEbLook.InvCustomer
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttInvEbLook.
                END. /*FOR EACH ttInvEbLook*/
            END. /*IF AVAILABLE usercust*/
        END. /*IF users.internal-user = YES*/
    END. /*IF AVAILABLE users*/
    ELSE DO:
        FOR EACH ttInvEbLook:
            DELETE ttInvEbLook.
        END.
    END. /*IF NOT AVAILABLE users*/



END. /* IF prmAction = search then do: */

