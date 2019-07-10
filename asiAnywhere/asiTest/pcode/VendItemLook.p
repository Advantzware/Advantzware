


/*------------------------------------------------------------------------
    File         : VendItemLook.p
    Purpose     : Vend Item lookup

    Syntax      :

    Description : Return a Dataset of all Po Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Jan 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{VendItemLook.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendItemLook.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

DEF VAR prmComp AS CHAR NO-UNDO.       

IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
MESSAGE "prmAction  " prmAction prmComp.
if prmAction <> "search" then do:
    FOR EACH po-ordl WHERE po-ordl.company EQ prmComp NO-LOCK:
        create ttVendItemLook.
            assign                                         
                ttVendItemLook.vend-i-no = po-ordl.vend-i-no
                ttVendItemLook.i-no = po-ordl.i-no 
                ttVendItemLook.cust = po-ordl.cust-no

                .
    END.	 /* FOR EACH po-ordl */
    /*FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttVendItemLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.cust-no = ttVendItemLook.cust
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttVendItemLook.
                END. /*FOR EACH ttVendItemLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser
                     AND usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttVendItemLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.cust-no = ttVendItemLook.cust
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttVendItemLook.
                    END. /*FOR EACH ttVendItemLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttVendItemLook:
                DELETE ttVendItemLook.
            END.
        END. /*IF NOT AVAILABLE users*/*/



END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH po-ordl WHERE po-ordl.company EQ prmComp NO-LOCK:
                
                IF po-ordl.vend-i-no = prmText or po-ordl.i-no = prmText THEN
                DO:
                   create ttVendItemLook.
                   assign                                                            
                     ttVendItemLook.vend-i-no = po-ordl.vend-i-no
                     ttVendItemLook.i-no = po-ordl.i-no 
                     ttVendItemLook.cust = po-ordl.cust-no.
                END.
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH po-ordl where
                po-ordl.company EQ prmComp NO-LOCK:

                IF (po-ordl.vend-i-no begins prmText or
                    po-ordl.i-no begins prmText) THEN
                DO:
                   create ttVendItemLook.
                   assign                 
                      ttVendItemLook.vend-i-no = po-ordl.vend-i-no
                      ttVendItemLook.i-no = po-ordl.i-no 
                      ttVendItemLook.cust = po-ordl.cust-no.
                END.
             END.  /*FOR EACH po-ordl where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/     
    IF prmField = "vend-i-no" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp AND
                po-ordl.vend-i-no = prmText no-lock:
                create ttVendItemLook.
                assign 
                    ttVendItemLook.vend-i-no = po-ordl.vend-i-no
                    ttVendItemLook.i-no = po-ordl.i-no 
                    ttVendItemLook.cust = po-ordl.cust-no
                    .
            END.  /*FOR EACH po-ordl where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp AND
                po-ordl.vend-i-no begins prmText no-lock:
                create ttVendItemLook.
                assign   
                   ttVendItemLook.vend-i-no = po-ordl.vend-i-no
                    ttVendItemLook.i-no = po-ordl.i-no 
                    ttVendItemLook.cust = po-ordl.cust-no           
                    .
            END. /*FOR EACH po-ordl where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = i-no */
    if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH po-ordl where po-ordl.company EQ prmComp AND
                po-ordl.i-no = prmText no-lock:
                create ttVendItemLook.
                assign                                         
                    ttVendItemLook.vend-i-no = po-ordl.vend-i-no
                    ttVendItemLook.i-no = po-ordl.i-no 
                    ttVendItemLook.cust = po-ordl.cust-no
                    .
            end. /*FOR EACH item-fg where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH po-ordl where
                po-ordl.company EQ prmComp AND
                po-ordl.i-no begins prmText no-lock:
                create ttVendItemLook.
                assign                                          
                   ttVendItemLook.vend-i-no = po-ordl.vend-i-no
                ttVendItemLook.i-no = po-ordl.i-no 
                ttVendItemLook.cust = po-ordl.cust-no

                    .
            end.  /*FOR EACH item-fg wher*/
        end.    /*if prmCondition = BEGIN*/    
    end.  /* if prmField = name  */
    
    /* FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
     IF AVAILABLE users THEN DO:
         IF users.internal-user = NO THEN DO:
             FOR EACH ttVendItemLook:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser 
                     AND usercust.cust-no = ttVendItemLook.cust
                     AND usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE usercust THEN DELETE ttVendItemLook.
             END. /*FOR EACH ttVendItemLook*/
             END. /*IF users.internal-user = NO*/
             IF users.internal-user = YES THEN DO:
                 FIND FIRST usercust WHERE usercust.user_id = prmUser
                      AND usercust.company EQ prmComp
                     NO-LOCK NO-ERROR.
                 IF AVAILABLE usercust THEN DO:
                     FOR EACH ttVendItemLook:
                         FIND FIRST usercust WHERE usercust.user_id = prmUser 
                             AND usercust.cust-no = ttVendItemLook.cust
                             AND usercust.company EQ prmComp 
                             NO-LOCK NO-ERROR.
                         IF NOT AVAILABLE usercust THEN DELETE ttVendItemLook.
                     END. /*FOR EACH ttVendItemLook*/
                 END. /*IF AVAILABLE usercust*/
                 END. /*IF users.internal-user = YES*/
             END. /*IF AVAILABLE users*/
             ELSE DO:
                 FOR EACH ttVendItemLook.
                     DELETE ttVendItemLook.
                 END.
             END. /*IF NOT AVAILABLE users*/*/
END.  /* IF prmAction = search then do: */

