
/*------------------------------------------------------------------------
    File        : custshiptolook.p
    Purpose     : Cust Ship To lookup 

    Syntax      :

    Description : Return a Dataset of all AdderLook

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttShipIdLook NO-UNDO 
    FIELD custnum AS CHARACTER
    FIELD Shipid AS CHARACTER
    FIELD Name AS CHARACTER
    FIELD Addr1 AS CHARACTER
    FIELD Addr2 AS CHARACTER
    FIELD City1 AS CHARACTER
    FIELD State1 AS CHARACTER
    FIELD Zip1 AS CHARACTER
    FIELD carrier1 AS CHARACTER
    FIELD Loc1 AS CHARACTER .
                                           
    
DEFINE DATASET dscustShipIdLook FOR ttShipIdLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShip      AS CHARACTER  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dscustShipIdLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField    = "".
IF prmShip      = ? THEN ASSIGN prmShip      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
if prmAction <> "search" then do:   
    FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp AND (usercust.cust-no = prmShip OR prmShip="")  NO-LOCK:
    FOR EACH shipto WHERE 
        shipto.company = prmComp AND shipto.cust-no = usercust.cust-no  NO-LOCK:
        create ttShipIdLook.
        assign                                     
            ttShipIdLook.custnum =  shipto.cust-no
            ttShipIdLook.Shipid    = shipto.ship-id
            ttShipIdLook.Name = shipto.ship-name 
            ttShipIdLook.Addr1    = shipto.ship-addr[1]
            ttShipIdLook.Addr2    = shipto.ship-addr[2]
            ttShipIdLook.City1 = shipto.ship-City
            ttShipIdLook.State1    = shipto.ship-state
            ttShipIdLook.Zip1 = shipto.ship-zip
            ttShipIdLook.carrier1    = shipto.carrier
            ttShipIdLook.Loc1 = shipto.loc.
    END.  /*FOR EACH usercust */
    END.  /*FOR EACH shipto WHERE*/
END.  /*ifif prmAction <> "search" */


IF prmAction = "search" then do:             
    if prmField = "ship-id"  then do:      
        if prmCondition = "EQUAL" then do:     
            FOR EACH usercust WHERE usercust.user_id = prmUser 
                    AND usercust.company EQ prmComp AND (usercust.cust-no = prmShip OR prmShip="")  NO-LOCK:
                FOR EACH shipto WHERE  shipto.ship-id =  prmText AND shipto.company = prmComp AND shipto.cust-no = usercust.cust-no NO-LOCK:
                    create ttShipIdLook.
                    assign                                     
                        ttShipIdLook.custnum =  shipto.cust-no
                        ttShipIdLook.Shipid    = shipto.ship-id
                        ttShipIdLook.Name = shipto.ship-name 
                        ttShipIdLook.Addr1    = shipto.ship-addr[1]
                        ttShipIdLook.Addr2    = shipto.ship-addr[2]
                        ttShipIdLook.City1 = shipto.ship-City
                        ttShipIdLook.State1    = shipto.ship-state
                        ttShipIdLook.Zip1 = shipto.ship-zip
                        ttShipIdLook.carrier1    = shipto.carrier
                        ttShipIdLook.Loc1 = shipto.loc.                                         
                END. /*FOR EACH shipto where*/
            END.
        END. /*if prmCondition = EQUAL */
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp AND (usercust.cust-no = prmShip OR prmShip="")  NO-LOCK:
              FOR EACH shipto WHERE  shipto.ship-id BEGINS prmText AND shipto.company = prmComp AND shipto.cust-no = usercust.cust-no NO-LOCK :
                create ttShipIdLook.
                assign                                     
                    ttShipIdLook.custnum =  shipto.cust-no
                    ttShipIdLook.Shipid    = shipto.ship-id
                    ttShipIdLook.Name = shipto.ship-name 
                    ttShipIdLook.Addr1    = shipto.ship-addr[1]
                    ttShipIdLook.Addr2    = shipto.ship-addr[2]
                    ttShipIdLook.City1 = shipto.ship-City
                    ttShipIdLook.State1    = shipto.ship-state
                    ttShipIdLook.Zip1 = shipto.ship-zip
                    ttShipIdLook.carrier1    = shipto.carrier
                    ttShipIdLook.Loc1 = shipto.loc.            
             END.  /*FOR EACH item wher*/
            END.
        END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = ship-id  */
     
     
     IF prmField = "ship-name" then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH usercust WHERE usercust.user_id = prmUser 
                    AND usercust.company EQ prmComp AND (usercust.cust-no = prmShip OR prmShip="")  NO-LOCK:
                FOR EACH shipto WHERE  shipto.ship-name = prmText AND shipto.company = prmComp AND shipto.cust-no = usercust.cust-no NO-LOCK :
                create ttShipIdLook.
                assign                                     
                    ttShipIdLook.custnum =  shipto.cust-no
                    ttShipIdLook.Shipid    = shipto.ship-id
                    ttShipIdLook.Name = shipto.ship-name 
                    ttShipIdLook.Addr1    = shipto.ship-addr[1]
                    ttShipIdLook.Addr2    = shipto.ship-addr[2]
                    ttShipIdLook.City1 = shipto.ship-City
                    ttShipIdLook.State1    = shipto.ship-state
                    ttShipIdLook.Zip1 = shipto.ship-zip
                    ttShipIdLook.carrier1    = shipto.carrier
                    ttShipIdLook.Loc1 = shipto.loc.
                END. /*FOR EACH shipto where*/
             END.
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
             FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp AND (usercust.cust-no = prmShip OR prmShip="")  NO-LOCK:
               FOR EACH shipto WHERE  shipto.ship-name BEGINS prmText AND shipto.company = prmComp AND shipto.cust-no = usercust.cust-no NO-LOCK :
                create ttShipIdLook.
                assign                                     
                    ttShipIdLook.custnum =  shipto.cust-no
                    ttShipIdLook.Shipid    = shipto.ship-id
                    ttShipIdLook.Name = shipto.ship-name 
                    ttShipIdLook.Addr1    = shipto.ship-addr[1]
                    ttShipIdLook.Addr2    = shipto.ship-addr[2]
                    ttShipIdLook.City1 = shipto.ship-City
                    ttShipIdLook.State1    = shipto.ship-state
                    ttShipIdLook.Zip1 = shipto.ship-zip
                    ttShipIdLook.carrier1    = shipto.carrier
                    ttShipIdLook.Loc1 = shipto.loc.
               END. /*FOR EACH shipto where*/
             END.
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = ship-name */
     
     /*if prmField = "custnum"  then DO:
        FOR EACH shipto WHERE  shipto.cust-no =  prmText NO-LOCK:
                create ttShipIdLook.
                    assign                                     
                        ttShipIdLook.custnum =  shipto.cust-no
                        ttShipIdLook.Shipid    = shipto.ship-id
                        ttShipIdLook.Name = shipto.ship-name 
                        ttShipIdLook.Addr1    = shipto.ship-addr[1]
                        ttShipIdLook.Addr2    = shipto.ship-addr[2]
                        ttShipIdLook.City1 = shipto.ship-City
                        ttShipIdLook.State1    = shipto.ship-state
                        ttShipIdLook.Zip1 = shipto.ship-zip
                        ttShipIdLook.carrier1    = shipto.carrier
                        ttShipIdLook.Loc1 = shipto.loc.                                         
            END. /*FOR EACH shipto where*/
        END. /*if prmCondition = EQUAL */
        
        IF prmCondition = "BEGIN" then do:            
              FOR EACH shipto WHERE  shipto.cust-no BEGINS prmText NO-LOCK :
                create ttShipIdLook.
                assign                                     
                    ttShipIdLook.custnum =  shipto.cust-no
                    ttShipIdLook.Shipid    = shipto.ship-id
                    ttShipIdLook.Name = shipto.ship-name 
                    ttShipIdLook.Addr1    = shipto.ship-addr[1]
                    ttShipIdLook.Addr2    = shipto.ship-addr[2]
                    ttShipIdLook.City1 = shipto.ship-City
                    ttShipIdLook.State1    = shipto.ship-state
                    ttShipIdLook.Zip1 = shipto.ship-zip
                    ttShipIdLook.carrier1    = shipto.carrier
                    ttShipIdLook.Loc1 = shipto.loc.            
            END.  
        END.    /*if prmCondition = BEGIN*/ 
     END.
     */
     
END.
