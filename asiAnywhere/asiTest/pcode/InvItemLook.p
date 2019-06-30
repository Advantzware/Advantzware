
/*------------------------------------------------------------------------
    File         : InvitemLookup
    Purpose     : item lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{InvItemLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvItemLook.

DEF VAR prmComp AS CHAR NO-UNDO.
       
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
    FOR EACH ar-invl WHERE ar-invl.company = prmComp AND
        ar-invl.ord-no = INT(prmOrderNum) NO-LOCK:
        create ttInvItemLook.
            assign                                         
                ttInvItemLook.InvItem = ar-invl.i-no
                ttInvItemLook.InvName = ar-invl.i-name 
                ttInvItemLook.InvDscr = ar-invl.i-dscr
                ttInvItemLook.Invcust-no = ar-invl.cust-no

                .
    END.	 /* FOR EACH ar-invl */
    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttInvItemLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttInvItemLook.Invcust-no
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttInvItemLook.
                END. /*FOR EACH ttInvItemLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                     usercust.company = prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttInvItemLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttInvItemLook.Invcust-no
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttInvItemLook.
                    END. /*FOR EACH ttInvItemLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttInvItemLook:
                DELETE ttInvItemLook.
            END.
        END. /*IF NOT AVAILABLE users*/



END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH ar-invl where
                ar-invl.company = prmComp AND
                ar-invl.ord-no = INT(prmOrderNum)
                 no-lock:

                IF ar-invl.i-no = prmText or ar-invl.i-name = prmText or ar-invl.i-dscr = prmText THEN
                DO:
                   create ttInvItemLook.
                   assign                                                            
                       ttInvItemLook.InvItem = ar-invl.i-no
                       ttInvItemLook.InvName = ar-invl.i-name 
                       ttInvItemLook.InvDscr = ar-invl.i-dscr
                       ttInvItemLook.Invcust-no = ar-invl.cust-no.
                END.
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp AND
                ar-invl.ord-no = INT(prmOrderNum)
                no-lock:

                IF (ar-invl.i-no begins prmText or ar-invl.i-name begins prmText or ar-invl.i-dscr begins prmText) THEN
                DO:
                   create ttInvItemLook.
                   assign                 
                       ttInvItemLook.InvItem = ar-invl.i-no
                       ttInvItemLook.InvName = ar-invl.i-name 
                       ttInvItemLook.InvDscr = ar-invl.i-dscr
                       ttInvItemLook.Invcust-no = ar-invl.cust-no.
                END.
             END.  /*FOR EACH ar-invl where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/     
    IF prmField = "i-no" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp AND
                ar-invl.ord-no = INT(prmOrderNum) AND
                ar-invl.i-no = prmText no-lock:
                create ttInvItemLook.
                assign                 
                    ttInvItemLook.InvItem = ar-invl.i-no
                    ttInvItemLook.InvName = ar-invl.i-name 
                    ttInvItemLook.InvDscr = ar-invl.i-dscr
                    ttInvItemLook.Invcust-no = ar-invl.cust-no
                    
                    .
            END. /*FOR EACH ar-invl where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp AND
                ar-invl.ord-no = INT(prmOrderNum) AND
                ar-invl.i-no begins prmText no-lock:
                create ttInvItemLook.
                assign                                                      
                    ttInvItemLook.InvItem = ar-invl.i-no
                    ttInvItemLook.InvName = ar-invl.i-name 
                    ttInvItemLook.InvDscr = ar-invl.i-dscr
                    ttInvItemLook.Invcust-no = ar-invl.cust-no
                    .
            END. /*FOR EACH ar-invl where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = i-no */
    if prmField = "i-name"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp AND ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.i-name = prmText no-lock:
                create ttInvItemLook.
                assign                                         
                    ttInvItemLook.InvItem = ar-invl.i-no
                    ttInvItemLook.InvName = ar-invl.i-name 
                    ttInvItemLook.InvDscr = ar-invl.i-dscr
                    ttInvItemLook.Invcust-no = ar-invl.cust-no
                    .
            end. /*FOR EACH item-fg where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH ar-invl where ar-invl.company = prmComp AND ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.i-name begins prmText no-lock:
                create ttInvItemLook.
                assign                                          
                    ttInvItemLook.InvItem = ar-invl.i-no
                    ttInvItemLook.InvName = ar-invl.i-name 
                    ttInvItemLook.InvDscr = ar-invl.i-dscr
                    ttInvItemLook.Invcust-no = ar-invl.cust-no
                    .
            end.  /*FOR EACH item-fg wher*/
        end.    /*if prmCondition = BEGIN*/    
    end.  /* if prmField = name  */
    if prmField = "i-dscr"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp AND
                ar-invl.ord-no = INT(prmOrderNum) AND
                ar-invl.i-dscr = prmText no-lock:
                create ttInvItemLook.
                assign                                         
                    ttInvItemLook.InvItem = ar-invl.i-no
                    ttInvItemLook.InvName = ar-invl.i-name 
                    ttInvItemLook.InvDscr = ar-invl.i-dscr
                    ttInvItemLook.Invcust-no = ar-invl.cust-no
                    .
            end. /*FOR EACH ar-invl where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH ar-invl WHERE ar-invl.company = prmComp AND ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.i-name begins prmText no-lock:
                create ttInvItemLook.
                assign                                          
                    ttInvItemLook.InvItem = ar-invl.i-no
                    ttInvItemLook.InvName = ar-invl.i-name 
                    ttInvItemLook.InvDscr	= ar-invl.i-dscr
                    ttInvItemLook.Invcust-no = ar-invl.cust-no
                    .
            end.  /*FOR EACH ar-invl wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */
     FIND FIRST users WHERE  users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
     IF AVAILABLE users THEN DO:
         IF users.internal-user = NO THEN DO:
             FOR EACH ttInvItemLook:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser 
                     AND usercust.cust-no = ttInvItemLook.Invcust-no
                     AND usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE usercust THEN DELETE ttInvItemLook.
             END. /*FOR EACH ttInvItemLook*/
             END. /*IF users.internal-user = NO*/
             IF users.internal-user = YES THEN DO:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                      usercust.company EQ prmComp
                      NO-LOCK NO-ERROR.
                 IF AVAILABLE usercust THEN DO:
                     FOR EACH ttInvItemLook:
                         FIND FIRST usercust WHERE usercust.user_id = prmUser 
                             AND usercust.cust-no = ttInvItemLook.Invcust-no
                             AND usercust.company EQ prmComp
                             NO-LOCK NO-ERROR.
                         IF NOT AVAILABLE usercust THEN DELETE ttInvItemLook.
                     END. /*FOR EACH ttInvItemLook*/
                 END. /*IF AVAILABLE usercust*/
                 END. /*IF users.internal-user = YES*/
             END. /*IF AVAILABLE users*/
             ELSE DO:
                 FOR EACH ttInvItemLook:
                     DELETE ttInvItemLook.
                 END.
             END. /*IF NOT AVAILABLE users*/
END.  /* IF prmAction = search then do: */
