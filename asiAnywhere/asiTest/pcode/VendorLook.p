



/*------------------------------------------------------------------------
    File         : VendorLook.p
    Purpose     : Vend Item lookup

    Syntax      :

    Description : Return a Dataset of all Po Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Jan 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{VendorLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendorLook.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

DEF VAR prmComp AS CHARACTER  NO-UNDO.
       
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

/*prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".*/
prmComp = "001".
if prmAction <> "search" then do:
    MESSAGE "test"  prmAction  prmComp.
    FOR EACH vend NO-LOCK:
        create ttVendorLook.
            assign                                         
                ttVendorLook.vend-no = vend.vend-no
                ttVendorLook.Vname = vend.name 
                .
            MESSAGE "jyven"  vend.name vend.vend-no. 
    END.	 /* FOR EACH vend */
    /*FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttVendorLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                         usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttVendorLook.
                END. /*FOR EACH ttVendorLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                     usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttVendorLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                             usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttVendorLook.
                    END. /*FOR EACH ttVendorLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttVendorLook:
                DELETE ttVendorLook.
            END.
        END. /*IF NOT AVAILABLE users*/
*/


END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH vend /* where vend.company EQ prmComp*/ NO-LOCK:
                
                IF vend.vend-no = prmText or vend.NAME = prmText THEN
                DO:
                   create ttVendorLook.
                   assign                                                            
                     ttVendorLook.vend-no = vend.vend-no
                     ttVendorLook.Vname = vend.name.
                END.
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH vend where vend.company EQ prmComp no-lock:

                IF vend.vend-no begins prmText or vend.NAME begins prmText THEN
                DO:
                   create ttVendorLook.
                   assign                 
                      ttVendorLook.vend-no = vend.vend-no
                      ttVendorLook.Vname = vend.name.
                END.
             END.  /*FOR EACH vend where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/     
    IF prmField = "vend-no" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH vend where  /*vend.company EQ prmComp AND*/  
                vend.vend-no = prmText no-lock:
                create ttVendorLook.
                assign 
                   ttVendorLook.vend-no = vend.vend-no
                   ttVendorLook.Vname = vend.NAME.
            END.  /*FOR EACH vend where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH vend where /*vend.company EQ prmComp AND*/
                vend.vend-no begins prmText no-lock:
                create ttVendorLook.
                assign   
                  ttVendorLook.vend-no = vend.vend-no
                  ttVendorLook.Vname = vend.name.
            END. /*FOR EACH vend where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = i-no */
    if prmField = "name"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH vend where /*vend.company EQ prmComp AND*/
                vend.NAME = prmText no-lock:
                create ttVendorLook.
                assign                                         
                   ttVendorLook.vend-no = vend.vend-no
                   ttVendorLook.Vname = vend.name.
            end. /*FOR EACH item-fg where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH vend where /*vend.company EQ prmComp AND*/
                vend.NAME begins prmText no-lock:
                create ttVendorLook.
                assign                                          
                  ttVendorLook.vend-no = vend.vend-no
                  ttVendorLook.Vname = vend.name.
            end.  /*FOR EACH item-fg wher*/
        end.    /*if prmCondition = BEGIN*/    
    end.  /* if prmField = name  */
    
 /*    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
     IF AVAILABLE users THEN DO:
         IF users.internal-user = NO THEN DO:
             FOR EACH ttVendorLook:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                      usercust.company EQ prmComp
                      NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE usercust THEN DELETE ttVendorLook.
             END. /*FOR EACH ttVendorLook*/
             END. /*IF users.internal-user = NO*/
             IF users.internal-user = YES THEN DO:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                      usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF AVAILABLE usercust THEN DO:
                     FOR EACH ttVendorLook:
                         FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                              usercust.company EQ prmComp
                              NO-LOCK NO-ERROR.
                         IF NOT AVAILABLE usercust THEN DELETE ttVendorLook.
                     END. /*FOR EACH ttVendorLook*/
                 END. /*IF AVAILABLE usercust*/
                 END. /*IF users.internal-user = YES*/
             END. /*IF AVAILABLE users*/
             ELSE DO:
                 FOR EACH ttVendorLook.
                     DELETE ttVendorLook.
                 END.
             END. /*IF NOT AVAILABLE users*/*/
END.  /* IF prmAction = search then do: */

