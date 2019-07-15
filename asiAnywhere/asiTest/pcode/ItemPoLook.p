

/*------------------------------------------------------------------------
    File         : itemByLookup
    Purpose     : Po lookup

    Syntax      :

    Description : Return a Dataset of all Po Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Jan 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ItemPoLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemPoLook.

DEF VAR prmComp AS CHAR NO-UNDO.
       
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH po-ordl WHERE po-ordl.company EQ prmComp NO-LOCK:
        create ttItemPoLook.
            assign                                         
                ttItemPoLook.itemPo = po-ordl.i-no
                ttItemPoLook.ItemName = po-ordl.i-name 
                ttItemPoLook.po-no =  po-ordl.po-no
                ttItemPoLook.cust-num = po-ordl.cust-no

                .
    END.	 /* FOR EACH po-ordl */
    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttItemPoLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttItemPoLook.cust-num
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttItemPoLook.
                END. /*FOR EACH ttItemPoLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser 
                     AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttItemPoLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttItemPoLook.cust-num
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttItemPoLook.
                    END. /*FOR EACH ttItemPoLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttItemPoLook:
                DELETE ttItemPoLook.
            END.
        END. /*IF NOT AVAILABLE users*/



END.  /*if prmAction <> "search" then do*/ 

ELSE
do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp no-lock:

                IF (po-ordl.i-no = prmText or po-ordl.i-name = prmText or po-ordl.po-no = int(prmText)) THEN
                DO:
                   create ttItemPoLook.
                   assign                                                            
                    ttItemPoLook.itemPo = po-ordl.i-no
                    ttItemPoLook.ItemName = po-ordl.i-name 
                    ttItemPoLook.po-no = po-ordl.po-no
                    ttItemPoLook.cust-num = po-ordl.cust-no.
                END.
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp
                no-lock:

                IF (po-ordl.i-no begins prmText or po-ordl.i-name begins prmText or po-ordl.po-no = int(prmText)) THEN
                DO:
                   create ttItemPoLook.
                   assign                 
                       ttItemPoLook.itemPo = po-ordl.i-no
                   ttItemPoLook.ItemName = po-ordl.i-name 
                   ttItemPoLook.po-no = po-ordl.po-no
                   ttItemPoLook.cust-num = po-ordl.cust-no.
                END.
             END.  /*FOR EACH po-ordl where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/     
    IF prmField = "i-no" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp AND po-ordl.i-no = prmText no-lock:
                create ttItemPoLook.
                assign                 
                    ttItemPoLook.itemPo = po-ordl.i-no
                ttItemPoLook.ItemName = po-ordl.i-name 
                ttItemPoLook.po-no = po-ordl.po-no
                ttItemPoLook.cust-num = po-ordl.cust-no
                    .
            END.  /*FOR EACH po-ordl where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp AND po-ordl.i-no begins prmText no-lock:
                create ttItemPoLook.
                assign                                                      
                     ttItemPoLook.itemPo = po-ordl.i-no
                ttItemPoLook.ItemName = po-ordl.i-name 
                ttItemPoLook.po-no = po-ordl.po-no
                ttItemPoLook.cust-num = po-ordl.cust-no
                    .
            END. /*FOR EACH po-ordl where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = i-no */
    if prmField = "i-name"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp AND po-ordl.i-name = prmText no-lock:
                create ttItemPoLook.
                assign                                         
                    ttItemPoLook.itemPo = po-ordl.i-no
                ttItemPoLook.ItemName = po-ordl.i-name 
                ttItemPoLook.po-no = po-ordl.po-no
                ttItemPoLook.cust-num = po-ordl.cust-no
                    .
            end. /*FOR EACH item-fg where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp AND po-ordl.i-name begins prmText no-lock:
                create ttItemPoLook.
                assign                                          
                   ttItemPoLook.itemPo = po-ordl.i-no
                ttItemPoLook.ItemName = po-ordl.i-name 
                ttItemPoLook.po-no = po-ordl.po-no
                ttItemPoLook.cust-num = po-ordl.cust-no
                    .
            end.  /*FOR EACH item-fg wher*/
        end.    /*if prmCondition = BEGIN*/    
    end.  /* if prmField = name  */
    if prmField = "po-no"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp AND po-ordl.po-no = int(prmText) no-lock:
                create ttItemPoLook.
                assign                                         
                   ttItemPoLook.itemPo = po-ordl.i-no
                ttItemPoLook.ItemName = po-ordl.i-name 
                ttItemPoLook.po-no = po-ordl.po-no
                ttItemPoLook.cust-num = po-ordl.cust-no
                    .
            end. /*FOR EACH po-ordl where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp AND po-ordl.po-no = int(prmText) no-lock:
                create ttItemPoLook.
                assign                                          
                   ttItemPoLook.itemPo = po-ordl.i-no
                ttItemPoLook.ItemName = po-ordl.i-name 
                ttItemPoLook.po-no = po-ordl.po-no
                ttItemPoLook.cust-num = po-ordl.cust-no
                    .
            end.  /*FOR EACH po-ordl wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */
     FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
     IF AVAILABLE users THEN DO:
         IF users.internal-user = NO THEN DO:
             FOR EACH ttItemPoLook:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser 
                     AND usercust.cust-no = ttItemPoLook.cust-num
                     AND usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE usercust THEN DELETE ttItemPoLook.
             END. /*FOR EACH ttItemPoLook*/
             END. /*IF users.internal-user = NO*/
             IF users.internal-user = YES THEN DO:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                      usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF AVAILABLE usercust THEN DO:
                     FOR EACH ttItemPoLook:
                         FIND FIRST usercust WHERE usercust.user_id = prmUser 
                             AND usercust.cust-no = ttItemPoLook.cust-num
                             AND usercust.company EQ prmComp
                             NO-LOCK NO-ERROR.
                         IF NOT AVAILABLE usercust THEN DELETE ttItemPoLook.
                     END. /*FOR EACH ttItemPoLook*/
                 END. /*IF AVAILABLE usercust*/
                 END. /*IF users.internal-user = YES*/
             END. /*IF AVAILABLE users*/
             ELSE DO:
                 FOR EACH ttItemPoLook.
                     DELETE ttItemPoLook.
                 END.
             END. /*IF NOT AVAILABLE users*/
END.  /* IF prmAction = search then do: */

