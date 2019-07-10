



/*------------------------------------------------------------------------
    File         : SalesRepLook.p
    Purpose     :  smanor lookup

    Syntax      :

    Description : Return a Dataset of all user maintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttUserSalesLook NO-UNDO 
    FIELD vSaleman      AS CHARACTER
    FIELD vSsname        AS CHARACTER
    .
                                           
    
DEFINE DATASET dsUserSalesLook FOR ttUserSalesLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.

DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserSalesLook.


IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

IF prmField      = ? THEN ASSIGN prmField      = "".
DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
     
prmComp =  "001".

/*prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".*/
MESSAGE "sales" prmAction prmUser prmCondition prmText prmField.
if prmAction <> "search" then do:
    FOR EACH sman WHERE sman.company = prmComp NO-LOCK:
       create ttUserSalesLook.
            assign                                         
                ttUserSalesLook.vSaleman = sman.sman
                ttUserSalesLook.vSsname   = sman.sname .
                
    END.	 /* FOR EACH cust */


    /*FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttUserSalesLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttUserSalesLook.
                END. /*FOR EACH ttUserSalesLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                     usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttUserSalesLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttUserSalesLook.
                    END. /*FOR EACH ttUserSalesLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttUserSalesLook:
                DELETE ttUserSalesLook.
            END.
        END. /*IF NOT AVAILABLE users*/*/

END.  /*if prmAction <> "search" then do*/ 

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH sman where sman.company = prmComp  AND sman.sman = prmText or sman.sname = prmText no-lock:
                
                 create ttUserSalesLook.
            assign                                         
                ttUserSalesLook.vSaleman = sman.sman
                ttUserSalesLook.vSsname   = sman.sname .
                
    END.	 /* FOR EACH cust */
            
           
        END.  /*IF prmCondition = EQUAL*/
IF prmCondition = "BEGIN" then do:
    FOR EACH sman where sman.company = prmComp  AND sman.sman BEGINS prmText or sman.sname BEGINS prmText no-lock:
        create ttUserSalesLook.
        assign                 
           ttUserSalesLook.vSaleman = sman.sman
                ttUserSalesLook.vSsname   = sman.sname .
            END.  /*FOR EACH cust where */         
END. /*IF prmCondition = BEGIN then do:*/  
 END. /*IF prmField = ANY*/     

   

IF prmField = "sman" then do:
    MESSAGE "jyoti".
    if prmCondition = "EQUAL" then do:
        FOR EACH sman where sman.company = prmComp  AND sman.sman = prmText   no-lock:
            create ttUserSalesLook.
            
                assign                                         
                ttUserSalesLook.vSaleman = sman.sman
                ttUserSalesLook.vSsname   = sman.sname .
                .
            
        END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL*/
IF prmCondition = "BEGIN" then do:
    FOR EACH sman where sman.company = prmComp  AND  sman.sman BEGINS prmText   no-lock:
        create ttUserSalesLook.
                                                              
            assign                                         
               ttUserSalesLook.vSaleman = sman.sman
                ttUserSalesLook.vSsname   = sman.sname .
     END. /*FOR EACH cust where*/
END.  /*if prmCondition = BEGIN*/
END.  /*IF prmField = cust-no */
if prmField = "sname"  then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH sman where sman.company = prmComp  AND sman.sname = prmText   NO-LOCK:
            create ttUserSalesLook.
            assign 
                ttUserSalesLook.vSaleman = sman.sman
                ttUserSalesLook.vSsname   = sman.sname .
        END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL */
IF prmCondition = "BEGIN" then do:
   FOR EACH sman where sman.company = prmComp  AND   sman.sname BEGINS prmText   no-lock:
        create ttUserSalesLook.
        assign                                          
              ttUserSalesLook.vSaleman = sman.sman
                ttUserSalesLook.vSsname   = sman.sname .
    end.  /*FOR EACH cust wher*/
end.    /*if prmCondition = BEGIN*/    
end.  /* if prmField = sname  */
/*
FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttUserSalesLook:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.company EQ prmComp NO-LOCK NO-ERROR.
            
                    IF NOT AVAILABLE usercust THEN DELETE ttUserSalesLook.
                END. /*FOR EACH ttUserSalesLook*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                     usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttUserSalesLook:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttUserSalesLook.
                    END. /*FOR EACH ttUserSalesLook*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttUserSalesLook:
                DELETE ttUserSalesLook.
            END.
        END. /*IF NOT AVAILABLE users*/*/
END.  /* IF prmAction = search then do: */

