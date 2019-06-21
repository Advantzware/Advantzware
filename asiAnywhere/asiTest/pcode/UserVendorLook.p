


/*------------------------------------------------------------------------
    File         : UserUserVendLook.p
    Purpose     :  Vendor lookup

    Syntax      :

    Description : Return a Dataset of all user maintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttUserVend NO-UNDO 
    FIELD vVendor      AS CHARACTER
    FIELD vName        AS CHARACTER
    FIELD vAddress     AS CHARACTER
    FIELD vCity        AS CHARACTER
    FIELD vState       AS CHARACTER
    FIELD vZip         AS CHARACTER
    FIELD vType        AS CHARACTER
    FIELD vBuyer       AS CHARACTER
    FIELD phone        AS CHAR
    FIELD arecode      AS CHAR
    .
                                           
    
DEFINE DATASET dsUserVendLook FOR ttUserVend .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUserVendLook.

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

if prmAction <> "search" then do:
    MESSAGE "TestVend"  prmAction prmComp.
    FOR EACH vend where vend.company = prmComp  NO-LOCK:
       create ttUserVend.
            assign                                         
                ttUserVend.vVendor  = vend.vend-no
                ttUserVend.vName    = vend.name 
                ttUserVend.vAddress = vend.add1
                ttUserVend.vCity    = vend.city
                ttUserVend.vState   = vend.state 
                ttUserVend.vZip     = vend.zip         
                ttUserVend.vType    = vend.type 
                ttUserVend.vBuyer   = vend.buyer
                ttUserVend.phone     = vend.phone 
                ttUserVend.arecode   = vend.area-code.
                
    END.	 /* FOR EACH cust */


    FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttUserVend:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.company EQ prmComp
                        NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE usercust THEN DELETE ttUserVend.
                END. /*FOR EACH ttUserVend*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                     usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttUserVend:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttUserVend.
                    END. /*FOR EACH ttUserVend*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttUserVend:
                DELETE ttUserVend.
            END.
        END. /*IF NOT AVAILABLE users*/

END.  /*if prmAction <> "search" then do*/ 

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH vend where vend.company = prmComp  AND (vend.vend-no = prmText or vend.name = prmText 
                                                             OR vend.city = prmText or vend.state = prmText
                                                             OR vend.add1 = prmText or vend.zip = prmText
                                                             OR vend.TYPE = prmText or vend.buyer = prmText)  no-lock:
                
                 create ttUserVend.
            assign                                         
                ttUserVend.vVendor  = vend.vend-no
                ttUserVend.vName    = vend.name 
                ttUserVend.vAddress = vend.add1
                ttUserVend.vCity    = vend.city
                ttUserVend.vState   = vend.state 
                ttUserVend.vZip     = vend.zip
                ttUserVend.vType    = vend.type 
                ttUserVend.vBuyer   = vend.buyer
                ttUserVend.phone     = vend.phone 
                ttUserVend.arecode   = vend.area-code.
                
    END.	 /* FOR EACH cust */
            
           
        END.  /*IF prmCondition = EQUAL*/
IF prmCondition = "BEGIN" then do:
    FOR EACH vend where vend.company = prmComp  AND (vend.vend-no BEGINS prmText or vend.name BEGINS prmText 
                                                             OR vend.city BEGINS prmText or vend.state BEGINS prmText
                                                             OR vend.add1 BEGINS prmText or vend.zip BEGINS prmText
                                                             OR vend.TYPE BEGINS prmText or vend.buyer BEGINS prmText)  no-lock:
        create ttUserVend.
        assign                 
            ttUserVend.vVendor  = vend.vend-no
            ttUserVend.vName    = vend.name 
            ttUserVend.vAddress = vend.add1

            ttUserVend.vCity    = vend.city
                ttUserVend.vState   = vend.state 
                ttUserVend.vZip     = vend.zip
                ttUserVend.vType    = vend.type 
                ttUserVend.vBuyer   = vend.buyer
                ttUserVend.phone     = vend.phone 
                ttUserVend.arecode   = vend.area-code.
            
       
    END.  /*FOR EACH cust where */         
END. /*IF prmCondition = BEGIN then do:*/  
 END. /*IF prmField = ANY*/     

   

IF prmField = "vend-no" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH vend where vend.company = prmComp  AND vend.vend-no = prmText   no-lock:
            create ttUserVend.
            
                assign                                         
                ttUserVend.vVendor  = vend.vend-no
                ttUserVend.vName    = vend.name 
                ttUserVend.vAddress = vend.add1
                ttUserVend.vCity    = vend.city
                ttUserVend.vState   = vend.state 
                ttUserVend.vZip     = vend.zip
                ttUserVend.vType    = vend.type 
                ttUserVend.vBuyer   = vend.buyer
                ttUserVend.phone     = vend.phone 
                ttUserVend.arecode   = vend.area-code.
                
            
        END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL*/
IF prmCondition = "BEGIN" then do:
    FOR EACH vend where vend.company = prmComp  AND  vend.vend-no BEGINS prmText   no-lock:
        create ttUserVend.
                                                              
            assign                                         
                ttUserVend.vVendor  = vend.vend-no
                ttUserVend.vName    = vend.name 
                ttUserVend.vAddress = vend.add1
                ttUserVend.vCity    = vend.city
                ttUserVend.vState   = vend.state 
                ttUserVend.vZip     = vend.zip
                ttUserVend.vType    = vend.type 
                ttUserVend.vBuyer   = vend.buyer
                ttUserVend.phone     = vend.phone 
                ttUserVend.arecode   = vend.area-code.
     END. /*FOR EACH cust where*/
END.  /*if prmCondition = BEGIN*/
END.  /*IF prmField = cust-no */
if prmField = "name"  then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH vend where vend.company = prmComp  AND vend.name = prmText   NO-LOCK:
            create ttUserVend.
            assign 
                                                        
                ttUserVend.vVendor  = vend.vend-no
               
                ttUserVend.vAddress = vend.add1
                ttUserVend.vCity    = vend.city
                ttUserVend.vState   = vend.state 
                ttUserVend.vZip     = vend.zip
                ttUserVend.vType    = vend.type 
                ttUserVend.vBuyer   = vend.buyer
                ttUserVend.phone     = vend.phone 
                ttUserVend.arecode   = vend.area-code.
        END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL */
IF prmCondition = "BEGIN" then do:
   FOR EACH vend where vend.company = prmComp  AND   vend.name BEGINS prmText   no-lock:
        create ttUserVend.
        assign                                          
                                                    
                ttUserVend.vVendor  = vend.vend-no
                ttUserVend.vName    = vend.name 
                ttUserVend.vAddress = vend.add1
                ttUserVend.vCity    = vend.city
                ttUserVend.vState   = vend.state 
                ttUserVend.vZip     = vend.zip
                ttUserVend.vType    = vend.type 
                ttUserVend.vBuyer   = vend.buyer
                ttUserVend.phone     = vend.phone 
                ttUserVend.arecode   = vend.area-code.
    end.  /*FOR EACH cust wher*/
end.    /*if prmCondition = BEGIN*/    
end.  /* if prmField = name  */
FIND FIRST users WHERE users.user_id = prmUser NO-LOCK USE-INDEX pi-users NO-ERROR.
        IF AVAILABLE users THEN DO:
            IF users.internal-user = NO THEN DO:
                FOR EACH ttUserVend:
                    FIND FIRST usercust WHERE usercust.user_id = prmUser 
                        AND usercust.company EQ prmComp NO-LOCK NO-ERROR.
            
                    IF NOT AVAILABLE usercust THEN DELETE ttUserVend.
                END. /*FOR EACH ttUserVend*/
            END. /*IF users.internal-user = NO*/
            IF users.internal-user = YES THEN DO:
                FIND FIRST usercust WHERE usercust.user_id = prmUser AND
                     usercust.company EQ prmComp
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usercust THEN DO:
                    FOR EACH ttUserVend:
                        FIND FIRST usercust WHERE usercust.user_id = prmUser 
                            AND usercust.company EQ prmComp
                            NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE usercust THEN DELETE ttUserVend.
                    END. /*FOR EACH ttUserVend*/
                END. /*IF AVAILABLE usercust*/
            END. /*IF users.internal-user = YES*/
        END. /*IF AVAILABLE users*/
        ELSE DO:
            FOR EACH ttUserVend:
                DELETE ttUserVend.
            END.
        END. /*IF NOT AVAILABLE users*/
END.  /* IF prmAction = search then do: */

