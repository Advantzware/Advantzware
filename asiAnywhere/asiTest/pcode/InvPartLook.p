

/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Invoice Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Feb 06 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{InvPartLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsInvPartLook.

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
    FOR EACH ar-invl WHERE ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp NO-LOCK  :
              create ttInvPartLook.
                       assign                                     
                         ttInvPartLook.InvPart    = ar-invl.part-no
                         ttInvPartLook.InvName = ar-invl.i-name 
                         ttInvPartLook.Invcust-no =  ar-invl.cust-no
                           .                       
      END.	 /* FOR EACH ar-invl */
      FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttInvPartLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttInvPartLook.Invcust-no
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttInvPartLook.
                END. /*FOR EACH ttInvPartLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser
                     AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttInvPartLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttInvPartLook.Invcust-no
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttInvPartLook.
                    END. /*FOR EACH ttInvPartLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttInvPartLook:
                DELETE ttInvPartLook.
            END.
END. /*IF NOT AVAILABLE users*/

END.  /*if prmAction <> "search" */
ELSE
/*IF prmAction = "search" then*/ do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
           FOR EACH ar-invl where ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp no-lock:

               IF ar-invl.part-no = prmText or ar-invl.i-name = prmText THEN
               DO:
                  create ttInvPartLook.
                  assign                                     
                    ttInvPartLook.InvPart    = ar-invl.part-no
                    ttInvPartLook.InvName = ar-invl.i-name 
                    ttInvPartLook.Invcust-no =  ar-invl.cust-no.
               END.

           END.   /*FOR EACH ar-invl*/
    END.   /* IF prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH ar-invl where ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp  no-lock:

               IF ar-invl.part-no begins prmText or ar-invl.i-name begins prmText THEN
               DO:
                  create ttInvPartLook.
                  assign                                                              
                    ttInvPartLook.InvPart    = ar-invl.part-no
                    ttInvPartLook.InvName = ar-invl.i-name  
                    ttInvPartLook.Invcust-no =  ar-invl.cust-no.
               END.

           END.  /*FOR EACH ar-invl*/

    END.   /*if prmCondition = "BEGIN"if prmCondition = "BEGIN"*/     
END.   /*IF prmField = "ANY" then do:*/

if prmField = "part-no" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH ar-invl where ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp AND  ar-invl.part-no = prmText no-lock:
                       create ttInvPartLook.
                       assign                                                                
                         ttInvPartLook.InvPart    = ar-invl.part-no
                         ttInvPartLook.InvName = ar-invl.i-name  
                         ttInvPartLook.Invcust-no =  ar-invl.cust-no
                           .                       

           END.  /*FOR EACH ar-invl*/
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
           FOR EACH ar-invl where ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp AND  ar-invl.part-no begins prmText no-lock:
                       create ttInvPartLook.
                       assign                                                                  
                         ttInvPartLook.InvPart    = ar-invl.part-no
                         ttInvPartLook.InvName = ar-invl.i-name  
                         ttInvPartLook.Invcust-no =  ar-invl.cust-no
                           .                       

           END.  /*FOR EACH ar-invl*/
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "part-no" then do:*/
if prmField = "i-name" then do:
     if prmCondition = "EQUAL" then do:
           FOR EACH ar-invl WHERE ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp AND  ar-invl.i-name = prmText no-lock:
                       create ttInvPartLook.
                       assign                                                                
                         ttInvPartLook.InvPart    = ar-invl.part-no
                         ttInvPartLook.InvName = ar-invl.i-name  
                         ttInvPartLook.Invcust-no =  ar-invl.cust-no
                           .                       
           END.  /*FOR EACH ar-invl*/

    END.   /*if prmCondition = "EQUAL" */
    if prmCondition = "BEGIN" then do:
           FOR EACH ar-invl where ar-invl.ord-no = INT(prmOrderNum) AND ar-invl.company = prmComp AND  ar-invl.i-name begins prmText no-lock:
                       create ttInvPartLook.
                       assign                                                                  
                         ttInvPartLook.InvPart    = ar-invl.part-no
                         ttInvPartLook.InvName = ar-invl.i-name 
                         ttInvPartLook.Invcust-no =  ar-invl.cust-no
                           .                       

           END. /*FOR EACH ar-invl*/
    END. /*if prmCondition = "BEGIN" */       
END.
FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttInvPartLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttInvPartLook.Invcust-no
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttInvPartLook.
                END. /*FOR EACH ttInvPartLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser 
                     AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttInvPartLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttInvPartLook.Invcust-no
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttInvPartLook.
                    END. /*FOR EACH ttInvPartLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttInvPartLook:
                DELETE ttInvPartLook.
            END.
END. /*IF NOT AVAILABLE users*/
END. /* IF prmAction = search then do: */


