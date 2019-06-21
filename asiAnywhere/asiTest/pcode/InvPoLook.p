
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
{InvPoLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvPoLook.

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
    FOR EACH ar-invl WHERE ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp NO-LOCK  :
              create ttInvPoLook.
                       assign                                     
                         ttInvPoLook.InvCustPo    = ar-invl.po-no
                         ttInvPoLook.InvNumber = ar-invl.cust-no .                       
      END.	 /* FOR EACH ar-invl */
      FIND FIRST users WHERE users.user_id = prmUser NO-LOCK NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttInvPoLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttInvPoLook.InvNumber
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttInvPoLook.
                END. /*FOR EACH ttInvPoLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser
                     AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttInvPoLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttInvPoLook.InvNumber
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttInvPoLook.
                    END. /*FOR EACH ttInvPoLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttInvPoLook:
                DELETE ttInvPoLook.
            END.
        END. /*IF NOT AVAILABLE users*/



END.  /*ifif prmAction <> "search" */
IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
           FOR EACH ar-invl WHERE ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp no-lock:

               IF ar-invl.po-no = prmText or ar-invl.cust-no = prmText THEN
               DO:
                  create ttInvPoLook.
                  assign                                     
                    ttInvPoLook.InvCustPo    = ar-invl.po-no
                    ttInvPoLook.InvNumber = ar-invl.cust-no.
               END.
           END.   /*FOR EACH ar-invl*/
    END.   /* IF prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH ar-invl where ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp no-lock:

               IF ar-invl.po-no begins prmText or ar-invl.cust-no begins prmText THEN
               DO:
                  create ttInvPoLook.
                  assign                                                              
                    ttInvPoLook.InvCustPo    = ar-invl.po-no
                    ttInvPoLook.InvNumber = ar-invl.cust-no .
               END.

           END.  /*FOR EACH ar-invl*/

    END.   /*if prmCondition = "BEGIN"if prmCondition = "BEGIN"*/     
END.   /*IF prmField = "ANY" then do:*/

if prmField = "po-no" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH ar-invl where ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp AND ar-invl.po-no = prmText no-lock:
                       create ttInvPoLook.
                       assign                                                                
                         ttInvPoLook.InvCustPo    = ar-invl.po-no
                         ttInvPoLook.InvNumber = ar-invl.cust-no . 
           END.  /*FOR EACH ar-invl*/
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH ar-invl where ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp AND ar-invl.po-no begins prmText no-lock:
                       create ttInvPoLook.
                       assign                                                                  
                         ttInvPoLook.InvCustPo    = ar-invl.po-no
                         ttInvPoLook.InvNumber = ar-invl.cust-no   .                

           END.  /*FOR EACH ar-invl*/
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "part-no" then do:*/
if prmField = "cust-no" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH ar-invl where ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.cust-no = prmText AND ar-invl.company = prmComp no-lock:
                       create ttInvPoLook.
                       assign                                                                
                         ttInvPoLook.InvCustPo    = ar-invl.po-no
                         ttInvPoLook.InvNumber = ar-invl.cust-no    .                

           END.
    END.
    if prmCondition = "BEGIN" then do:
           FOR EACH ar-invl where ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp AND ar-invl.cust-no begins prmText no-lock:
                       create ttInvPoLook.
                       assign                                                                  
                         ttInvPoLook.InvCustPo    = ar-invl.po-no
                         ttInvPoLook.InvNumber = ar-invl.cust-no    .                

           END.
    END.        
END.
 FIND FIRST users WHERE users.user_id = prmUser NO-LOCK NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttInvPoLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttInvPoLook.InvNumber
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttInvPoLook.
                END. /*FOR EACH ttInvPoLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser 
                     AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttInvPoLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttInvPoLook.InvNumber
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttInvPoLook.
                    END. /*FOR EACH ttInvPoLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttInvPoLook:
                DELETE ttInvPoLook.
            END.
        END. /*IF NOT AVAILABLE users*/

END. /* IF prmAction = search then do: */

