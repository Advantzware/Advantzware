

/*------------------------------------------------------------------------
    File         : BolNumLook.p
    Purpose     : Invoice lookup

    Syntax      :

    Description : Return a Dataset of all ArInvoice Inquiry

    Author(s)   : 
    Created     : Feb 07, 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{BolNumLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmPosted    AS CHAR NO-UNDO .

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBolNumLook.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF VAR post AS LOG NO-UNDO.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmCustomer      = ? THEN ASSIGN prmCustomer      = "".
IF prmPosted        = ? THEN ASSIGN prmPosted        = "".
   


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.


prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN 
    post = IF prmPosted = "Yes" THEN TRUE ELSE FALSE .


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "BOLFMT" NO-LOCK NO-ERROR.

if prmAction <> "search" then do:
    
    FOR EACH oe-bolh WHERE  oe-bolh.company eq prmComp AND (oe-bolh.cust-no = prmCustomer OR prmCustomer = "") 
                               AND  LOOKUP(oe-bolh.cust-no, custcount) <> 0 AND oe-bolh.cust-no <> "" 
                               AND oe-bolh.posted = post  NO-LOCK BY oe-bolh.bol-no DESC :
        FIND FIRST oe-boll WHERE oe-boll.company = prmComp AND oe-boll.bol-no = oe-bolh.bol-no NO-LOCK NO-ERROR.
      
        IF oe-bolh.bol-no <> 0 THEN DO:
            create ttBolNumLook.
            assign                                         
                ttBolNumLook.ArBol        = oe-bolh.bol-no
                ttBolNumLook.ArBolItem    = oe-boll.i-no
                ttBolNumLook.vCust        = oe-bolh.cust-no
                ttBolNumLook.vDate        = oe-bolh.bol-date
                ttBolNumLook.vBolFmt      = sys-ctrl.char-fld
                .
           
        END.    /*if oe-bolh.bol-no <> 0*/
     
    END.	 /* FOR EACH ar-invl */


    FOR EACH ttBolNumLook:                    
           IF ttBolNumLook.vCust EQ "" THEN DELETE ttBolNumLook.
    END. /*FOR EACH ttBolNumLook*/

    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttBolNumLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                         usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttBolNumLook.
                END. /*FOR EACH ttBolNumLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                     usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttBolNumLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                             usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttBolNumLook.
                    END. /*FOR EACH ttBolNumLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttBolNumLook:
                DELETE ttBolNumLook.
            END.
        END. /*IF NOT AVAILABLE users*/



END.  /*if prmAction <> "search" then do*/ 

ELSE
/*IF prmAction = "search" then*/ do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            
    
    FOR EACH oe-bolh WHERE  oe-bolh.company eq prmComp AND (oe-bolh.cust-no = prmCustomer OR prmCustomer = "")
                 AND  LOOKUP(oe-bolh.cust-no, custcount) <> 0 AND oe-bolh.cust-no <> ""  AND oe-bolh.posted = post   NO-LOCK:
                 FIND FIRST oe-boll WHERE oe-boll.company = prmComp AND oe-boll.bol-no = oe-bolh.bol-no NO-LOCK NO-ERROR.
                IF (oe-bolh.bol-no = int(prmText) OR oe-bolh.bol-date = DATE(prmText)) THEN
                DO:
                   create ttBolNumLook.
                   assign
                       ttBolNumLook.ArBol        = oe-bolh.bol-no
                       ttBolNumLook.ArBolItem    = oe-boll.i-no
                       ttBolNumLook.vCust        = oe-bolh.cust-no
                       ttBolNumLook.vDate        = oe-bolh.bol-date
                       ttBolNumLook.vBolFmt      = sys-ctrl.char-fld
                       .
                END.
   
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
                  FOR EACH oe-bolh WHERE  oe-bolh.company eq prmComp AND (oe-bolh.cust-no = prmCustomer OR prmCustomer = "") 
                            AND  LOOKUP(oe-bolh.cust-no, custcount) <> 0 AND oe-bolh.cust-no <> ""  AND oe-bolh.posted = post  NO-LOCK:
                     FIND FIRST oe-boll WHERE oe-boll.company = prmComp AND oe-boll.bol-no = oe-bolh.bol-no NO-LOCK NO-ERROR.
                IF (oe-bolh.bol-no = int(prmText) OR oe-bolh.bol-date = DATE(prmText)) THEN
                DO:
                   create ttBolNumLook.
                   assign
                       ttBolNumLook.ArBol        = oe-bolh.bol-no
                       ttBolNumLook.ArBolItem    = oe-boll.i-no
                       ttBolNumLook.vCust        = oe-bolh.cust-no
                       ttBolNumLook.vDate        = oe-bolh.bol-date
                       ttBolNumLook.vBolFmt      = sys-ctrl.char-fld
                .
                END.
                 
             END.  /*FOR EACH ar-invl where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "bol-no"  then do:
        if prmCondition = "EQUAL" then do:
          
            FOR EACH oe-bolh WHERE  (oe-bolh.bol-no = int(prmText) OR prmText = "") AND oe-bolh.company eq prmComp AND (oe-bolh.cust-no = prmCustomer OR prmCustomer = "")
                        AND  LOOKUP(oe-bolh.cust-no, custcount) <> 0 AND oe-bolh.cust-no <> ""  AND oe-bolh.posted = post  NO-LOCK BY oe-bolh.bol-no DESC:
                 FIND FIRST oe-boll WHERE oe-boll.company = prmComp AND oe-boll.bol-no = oe-bolh.bol-no NO-LOCK NO-ERROR.
                create ttBolNumLook.
                assign 
                    ttBolNumLook.ArBol        = oe-bolh.bol-no
                    ttBolNumLook.ArBolItem    = oe-boll.i-no
                    ttBolNumLook.vCust        = oe-bolh.cust-no
                    ttBolNumLook.vDate        = oe-bolh.bol-date
                    ttBolNumLook.vBolFmt      = sys-ctrl.char-fld
                    
                    .
            end. /*FOR EACH ar-invl where*/
            
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            
            FOR EACH oe-bolh WHERE oe-bolh.bol-no = INT(prmText) AND oe-bolh.company eq prmComp AND (oe-bolh.cust-no = prmCustomer OR prmCustomer = "") 
                           AND  LOOKUP(oe-bolh.cust-no, custcount) <> 0 AND oe-bolh.cust-no <> ""  AND oe-bolh.posted = post no-lock:
                 FIND FIRST oe-boll WHERE oe-boll.company = prmComp AND oe-boll.bol-no = oe-bolh.bol-no NO-LOCK NO-ERROR.
                create ttBolNumLook.
                assign
                    ttBolNumLook.ArBol        = oe-bolh.bol-no
                ttBolNumLook.ArBolItem    = oe-boll.i-no
                ttBolNumLook.vCust        = oe-bolh.cust-no
                ttBolNumLook.vDate        = oe-bolh.bol-date
                    ttBolNumLook.vBolFmt      = sys-ctrl.char-fld
                .
                    .
            end.  /*FOR EACH ar-invl wher*/
          
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */


     if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:
          
            FOR EACH oe-bolh WHERE   oe-bolh.company eq prmComp AND (oe-bolh.cust-no = prmCustomer OR prmCustomer = "")
                        AND  LOOKUP(oe-bolh.cust-no, custcount) <> 0 AND oe-bolh.cust-no <> ""  AND oe-bolh.posted = post  NO-LOCK BY oe-bolh.bol-no DESC:
                 FIND FIRST oe-boll WHERE oe-boll.company = prmComp AND oe-boll.bol-no = oe-bolh.bol-no 
                     AND (oe-boll.i-no = (prmText) OR prmText = "") NO-LOCK NO-ERROR.
                create ttBolNumLook.
                assign 
                    ttBolNumLook.ArBol        = oe-bolh.bol-no
                    ttBolNumLook.ArBolItem    = oe-boll.i-no
                    ttBolNumLook.vCust        = oe-bolh.cust-no
                    ttBolNumLook.vDate        = oe-bolh.bol-date
                    ttBolNumLook.vBolFmt      = sys-ctrl.char-fld
                    
                    .
            end. /*FOR EACH ar-invl where*/
            
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            
            FOR EACH oe-bolh WHERE  oe-bolh.company eq prmComp AND (oe-bolh.cust-no = prmCustomer OR prmCustomer = "") 
                           AND  LOOKUP(oe-bolh.cust-no, custcount) <> 0 AND oe-bolh.cust-no <> ""  AND oe-bolh.posted = post no-lock:
                 FIND FIRST oe-boll WHERE oe-boll.company = prmComp AND oe-boll.bol-no = oe-bolh.bol-no 
                     AND (oe-boll.i-no BEGINS (prmText) OR prmText = "") NO-LOCK NO-ERROR.
                create ttBolNumLook.
                assign
                    ttBolNumLook.ArBol        = oe-bolh.bol-no
                ttBolNumLook.ArBolItem    = oe-boll.i-no
                ttBolNumLook.vCust        = oe-bolh.cust-no
                ttBolNumLook.vDate        = oe-bolh.bol-date
                    ttBolNumLook.vBolFmt      = sys-ctrl.char-fld
                .
                    .
            end.  /*FOR EACH ar-invl wher*/
          
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */




     FOR EACH ttBolNumLook:                    
           IF ttBolNumLook.vCust EQ "" THEN DELETE ttBolNumLook.
     END. /*FOR EACH ttBolNumLook*/   
   
     FIND FIRST users WHERE  users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
     IF AVAILABLE users THEN DO:
         IF users.internal-user = NO THEN DO:
             FOR EACH ttBolNumLook:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser 
                      AND usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE usercust THEN DELETE ttBolNumLook.
             END. /*FOR EACH ttBolNumLook*/
             END. /*IF users.internal-user = NO*/
             IF users.internal-user = YES THEN DO:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                      usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF AVAILABLE usercust THEN DO:
                     FOR EACH ttBolNumLook:
                         FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                             usercust.company EQ prmComp
                            
                             NO-LOCK NO-ERROR.
                         IF NOT AVAILABLE usercust THEN DELETE ttBolNumLook.
                     END. /*FOR EACH ttBolNumLook*/
                 END. /*IF AVAILABLE usercust*/
                 END. /*IF users.internal-user = YES*/
             END. /*IF AVAILABLE users*/
             ELSE DO:
                 FOR EACH ttBolNumLook:
                     DELETE ttBolNumLook.
                 END.
             END. /*IF NOT AVAILABLE users*/
END.  /* IF prmAction = search then do: */

