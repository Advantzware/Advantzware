
/*------------------------------------------------------------------------
    File         : InvNumLook.p
    Purpose     : Invoice lookup

    Syntax      :

    Description : Return a Dataset of all ArInvoice Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Feb 07, 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{InvNumLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvoiceLook.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

DEF VAR prmComp AS CHARACTER NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH ar-invl WHERE ar-invl.company = prmComp NO-LOCK:
        create ttInvoiceLook.
            assign                                         
                ttInvoiceLook.ArInv     = ar-invl.inv-no
                ttInvoiceLook.ArInvItem = ar-invl.i-no
                ttInvoiceLook.ArInvName = ar-invl.i-name 
                ttInvoiceLook.ArInvcust-no = ar-invl.cust-no

                .
    END.	 /* FOR EACH ar-invl */
    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttInvoiceLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttInvoiceLook.ArInvcust-no
                        AND usercust.company = prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttInvoiceLook.
                END. /*FOR EACH ttInvoiceLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                     usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttInvoiceLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttInvoiceLook.ArInvcust-no
                            AND usercust.company = prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttInvoiceLook.
                    END. /*FOR EACH ttInvoiceLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttInvoiceLook:
                DELETE ttInvoiceLook.
            END.
        END. /*IF NOT AVAILABLE users*/



END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH ar-invl where ar-invl.company = prmComp no-lock:

                IF (ar-invl.inv-no = int(prmText) OR ar-invl.i-no = prmText or ar-invl.i-name = prmText) THEN
                DO:
                   create ttInvoiceLook.
                   assign
                       ttInvoiceLook.ArInv     = ar-invl.inv-no
                       ttInvoiceLook.ArInvItem = ar-invl.i-no
                       ttInvoiceLook.ArInvName = ar-invl.i-name 
                       ttInvoiceLook.ArInvcust-no = ar-invl.cust-no.
                END.
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp no-lock:

                IF (ar-invl.inv-no = int(prmText) OR ar-invl.i-no begins prmText or ar-invl.i-name begins prmText) THEN
                DO:
                   create ttInvoiceLook.
                   assign                 
                       ttInvoiceLook.ArInv     = ar-invl.inv-no
                       ttInvoiceLook.ArInvItem = ar-invl.i-no
                       ttInvoiceLook.ArInvName = ar-invl.i-name 
                       ttInvoiceLook.ArInvcust-no = ar-invl.cust-no.
                END.
             END.  /*FOR EACH ar-invl where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "inv-no"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp AND ar-invl.inv-no = int(prmText) no-lock:
                create ttInvoiceLook.
                assign                                         
                    ttInvoiceLook.ArInv     = ar-invl.inv-no
                    ttInvoiceLook.ArInvItem = ar-invl.i-no
                    ttInvoiceLook.ArInvName = ar-invl.i-name 
                    ttInvoiceLook.ArInvcust-no = ar-invl.cust-no
                    .
            end. /*FOR EACH ar-invl where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp  AND ar-invl.inv-no = INT(prmText) no-lock:
                create ttInvoiceLook.
                assign                                          
                    ttInvoiceLook.ArInv     = ar-invl.inv-no
                    ttInvoiceLook.ArInvItem = ar-invl.i-no
                    ttInvoiceLook.ArInvName = ar-invl.i-name 
                    ttInvoiceLook.ArInvcust-no = ar-invl.cust-no
                    .
            end.  /*FOR EACH ar-invl wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */
    IF prmField = "i-no" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp  AND ar-invl.i-no = prmText no-lock:
                create ttInvoiceLook.
                assign
                    ttInvoiceLook.ArInv     = ar-invl.inv-no
                    ttInvoiceLook.ArInvItem = ar-invl.i-no
                    ttInvoiceLook.ArInvName = ar-invl.i-name 
                    ttInvoiceLook.ArInvcust-no = ar-invl.cust-no
                    
                    .
            END. /*FOR EACH ar-invl where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH ar-invl WHERE ar-invl.company EQ prmComp AND ar-invl.i-no begins prmText no-lock:
                create ttInvoiceLook.
                assign                                                      
                    ttInvoiceLook.ArInv     = ar-invl.inv-no
                    ttInvoiceLook.ArInvItem = ar-invl.i-no
                    ttInvoiceLook.ArInvName = ar-invl.i-name 
                    ttInvoiceLook.ArInvcust-no = ar-invl.cust-no
                    .
            END. /*FOR EACH ar-invl where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = i-no */
    if prmField = "i-name"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp  AND ar-invl.i-name = prmText no-lock:
                create ttInvoiceLook.
                assign                                         
                    ttInvoiceLook.ArInv     = ar-invl.inv-no
                    ttInvoiceLook.ArInvItem = ar-invl.i-no
                    ttInvoiceLook.ArInvName = ar-invl.i-name 
                    ttInvoiceLook.ArInvcust-no = ar-invl.cust-no
                    .
            end. /*FOR EACH item-fg where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp AND  ar-invl.i-name begins prmText no-lock:
                create ttInvoiceLook.
                assign                                          
                    ttInvoiceLook.ArInv     = ar-invl.inv-no
                    ttInvoiceLook.ArInvItem = ar-invl.i-no
                    ttInvoiceLook.ArInvName = ar-invl.i-name 
                    ttInvoiceLook.ArInvcust-no = ar-invl.cust-no
                    .
            end.  /*FOR EACH item-fg wher*/
        end.    /*if prmCondition = BEGIN*/    
    end.  /* if prmField = name  */
    
     FIND FIRST users WHERE  users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
     IF AVAILABLE users THEN DO:
         IF users.internal-user = NO THEN DO:
             FOR EACH ttInvoiceLook:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser 
                     AND usercust.cust-no = ttInvoiceLook.ArInvcust-no
                     AND usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE usercust THEN DELETE ttInvoiceLook.
             END. /*FOR EACH ttInvoiceLook*/
             END. /*IF users.internal-user = NO*/
             IF users.internal-user = YES THEN DO:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                      usercust.company EQ prmComp
                      NO-LOCK NO-ERROR.
                 IF AVAILABLE usercust THEN DO:
                     FOR EACH ttInvoiceLook:
                         FIND FIRST usercust WHERE usercust.user_id = prmUser 
                             AND usercust.cust-no = ttInvoiceLook.ArInvcust-no
                             AND usercust.company EQ prmComp
                             NO-LOCK NO-ERROR.
                         IF NOT AVAILABLE usercust THEN DELETE ttInvoiceLook.
                     END. /*FOR EACH ttInvoiceLook*/
                 END. /*IF AVAILABLE usercust*/
                 END. /*IF users.internal-user = YES*/
             END. /*IF AVAILABLE users*/
             ELSE DO:
                 FOR EACH ttInvoiceLook:
                     DELETE ttInvoiceLook.
                 END.
             END. /*IF NOT AVAILABLE users*/
END.  /* IF prmAction = search then do: */

