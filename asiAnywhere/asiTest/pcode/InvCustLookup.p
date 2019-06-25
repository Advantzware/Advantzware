
/*------------------------------------------------------------------------
    File         : CustomerLookup
    Purpose     : customer lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{InvCustLookup.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvCustomer.
	
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmOrderNum      = ? THEN ASSIGN prmOrderNum      = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FIND FIRST oe-ordl WHERE
         oe-ordl.company EQ prmComp AND
         oe-ordl.ord-no = INT(prmOrderNum) AND
         oe-ordl.company = prmComp
         NO-LOCK.
    
    FOR FIRST cust WHERE
        cust.company EQ prmComp AND
        cust.cust-no = oe-ordl.cust-no
        NO-LOCK:
        create ttInvCustomer.
            assign                                         
                ttInvCustomer.InvCustomer = cust.cust-no
                ttInvCustomer.InvName = cust.name 
                ttInvCustomer.Invcity = cust.city
                ttInvCustomer.Invstate = cust.state 
                ttInvCustomer.Invzip = cust.zip
                ttInvCustomer.Invtype = cust.type 
                ttInvCustomer.Invsman = cust.sman
                ttInvCustomer.Invterr = cust.terr
                .
    END.	 /* FOR EACH cust */


    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttInvCustomer:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttInvCustomer.InvCustomer
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttInvCustomer.
                END. /*FOR EACH ttInvCustomer*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                     usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttInvCustomer:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttInvCustomer.InvCustomer
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttInvCustomer.
                    END. /*FOR EACH ttInvCustomer*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttInvCustomer:
                DELETE ttInvCustomer.
            END.
        END. /*IF NOT AVAILABLE users*/

END.  /*if prmAction <> "search" then do*/ 
ELSE
/*IF prmAction = "search" then*/ do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:

            FOR EACH cust where cust.company EQ prmComp AND
                oe-ordl.ord-no = INT(prmOrderNum) AND oe-ordl.company = prmComp and ( cust.cust-no = prmText or cust.name = prmText)  no-lock:
                create ttInvCustomer.
                assign                                         
                ttInvCustomer.InvCustomer = cust.cust-no
                ttInvCustomer.InvName = cust.name 
                ttInvCustomer.Invcity = cust.city
                ttInvCustomer.Invstate = cust.state 
                ttInvCustomer.Invzip = cust.zip
                ttInvCustomer.Invtype = cust.type 
                ttInvCustomer.Invsman = cust.sman
                ttInvCustomer.Invterr = cust.terr
                .

             END. /*FOR EACH cust where cust.cust-no = prmText*/
           
        END.  /*IF prmCondition = EQUAL*/
IF prmCondition = "BEGIN" then do:
    FOR EACH cust where cust.company EQ prmComp AND
        oe-ordl.ord-no = INT(prmOrderNum) AND oe-ordl.company = prmComp AND (cust.cust-no begins prmText or cust.name begins prmText)  no-lock:
        create ttInvCustomer.
         assign                                         
                ttInvCustomer.InvCustomer = cust.cust-no
                ttInvCustomer.InvName = cust.name 
                ttInvCustomer.Invcity = cust.city
                ttInvCustomer.Invstate = cust.state 
                ttInvCustomer.Invzip = cust.zip
                ttInvCustomer.Invtype = cust.type 
                ttInvCustomer.Invsman = cust.sman
                ttInvCustomer.Invterr = cust.terr
                .
    END.  /*FOR EACH cust where */         
END. /*IF prmCondition = BEGIN then do:*/  
 END. /*IF prmField = ANY*/     

   

IF prmField = "cust-no" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH cust where cust.company EQ prmComp AND oe-ordl.ord-no = INT(prmOrderNum) AND cust.cust-no = prmText  AND oe-ordl.company = prmComp  no-lock:
            create ttInvCustomer.
             assign                                         
                ttInvCustomer.InvCustomer = cust.cust-no
                ttInvCustomer.InvName = cust.name 
                ttInvCustomer.Invcity = cust.city
                ttInvCustomer.Invstate = cust.state 
                ttInvCustomer.Invzip = cust.zip
                ttInvCustomer.Invtype = cust.type 
                ttInvCustomer.Invsman = cust.sman
                ttInvCustomer.Invterr = cust.terr
                .

        END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL*/
IF prmCondition = "BEGIN" then do:
    FOR EACH cust where cust.company EQ prmComp AND oe-ordl.ord-no = INT(prmOrderNum) AND cust.cust-no begins prmText  AND oe-ordl.company = prmComp no-lock:
        create ttInvCustomer.
         assign                                         
                ttInvCustomer.InvCustomer = cust.cust-no
                ttInvCustomer.InvName = cust.name 
                ttInvCustomer.Invcity = cust.city
                ttInvCustomer.Invstate = cust.state 
                ttInvCustomer.Invzip = cust.zip
                ttInvCustomer.Invtype = cust.type 
                ttInvCustomer.Invsman = cust.sman
                ttInvCustomer.Invterr = cust.terr
                .
     END. /*FOR EACH cust where*/
END.  /*if prmCondition = BEGIN*/
END.  /*IF prmField = cust-no */
if prmField = "name"  then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH cust where cust.company EQ prmComp AND oe-ordl.ord-no = INT(prmOrderNum) AND cust.name = prmText  AND oe-ordl.company = prmComp  no-lock:
            create ttInvCustomer.
             assign                                         
                ttInvCustomer.InvCustomer = cust.cust-no
                ttInvCustomer.InvName = cust.name 
                ttInvCustomer.Invcity = cust.city
                ttInvCustomer.Invstate = cust.state 
                ttInvCustomer.Invzip = cust.zip
                ttInvCustomer.Invtype = cust.type 
                ttInvCustomer.Invsman = cust.sman
                ttInvCustomer.Invterr = cust.terr
                .
        END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL */
IF prmCondition = "BEGIN" then do:
    FOR EACH cust WHERE cust.company EQ prmComp AND oe-ordl.ord-no = INT(prmOrderNum) AND cust.name begins prmText  AND oe-ordl.company = prmComp  no-lock:
        create ttInvCustomer.
         assign                                         
                ttInvCustomer.InvCustomer = cust.cust-no
                ttInvCustomer.InvName = cust.name 
                ttInvCustomer.Invcity = cust.city
                ttInvCustomer.Invstate = cust.state 
                ttInvCustomer.Invzip = cust.zip
                ttInvCustomer.Invtype = cust.type 
                ttInvCustomer.Invsman = cust.sman
                ttInvCustomer.Invterr = cust.terr
                .
    end.  /*FOR EACH cust wher*/
end.    /*if prmCondition = BEGIN*/    
end.  /* if prmField = name  */
FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttInvCustomer:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttInvCustomer.InvCustomer
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttInvCustomer.
                END. /*FOR EACH ttInvCustomer*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                     usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttInvCustomer:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttInvCustomer.InvCustomer
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttInvCustomer.
                    END. /*FOR EACH ttInvCustomer*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttInvCustomer:
                DELETE ttInvCustomer.
            END.
        END. /*IF NOT AVAILABLE users*/
END.  /* IF prmAction = search then do: */
