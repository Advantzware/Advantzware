
/*------------------------------------------------------------------------
    File        : Shipping Look.p
    Purpose     : ShipIdLook

    Syntax      :

    Description : Return a Dataset of all ShipIdLook

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  **************************/


DEFINE TEMP-TABLE ttShipIdLook NO-UNDO 
    FIELD custnum AS CHARACTER
    FIELD Shipid AS CHARACTER
    FIELD Shipiddscr AS CHARACTER
    FIELD carrier AS CHARACTER
    FIELD carrierdscr AS CHARACTER
    FIELD cas-no AS CHARACTER
    FIELD cas-cnt LIKE item.box-case
    FIELD cas-len LIKE item.case-l
    FIELD cas-wid LIKE item.case-w
    FIELD cas-dep LIKE item.case-d
    FIELD cas-pal LIKE item.case-pall
    FIELD cas-paldscr LIKE item.i-name
    FIELD cas-wt LIKE item.avg-w
    FIELD tr-no LIKE cust.pallet
    FIELD tr-len LIKE item.case-l
    FIELD tr-dep LIKE item.case-w
    FIELD tr-wid LIKE item.case-d
    FIELD State1 AS CHARACTER
    FIELD Zip1 AS CHARACTER
    FIELD carrier1 AS CHARACTER
    FIELD Loc1 AS CHARACTER 
    FIELD adddr1 AS CHARACTER
    FIELD addr2  AS CHARACTER
    FIELD city   AS CHARACTER
    FIELD state  AS CHARACTER
    FIELD vZip   AS CHARACTER.
                                           
    
DEFINE DATASET dsShipIdLook FOR ttShipIdLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShip      AS CHARACTER  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsShipIdLook.
       
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
        AND usercust.company EQ prmComp  AND (usercust.cust-no = prmShip OR prmShip = ""  ) NO-LOCK:
        
    FIND FIRST cust WHERE cust.cust-no = usercust.cust-no   AND
        cust.company = prmComp   NO-LOCK NO-ERROR.                
    
    FOR EACH shipto WHERE shipto.cust-no = cust.cust-no   NO-LOCK :
        FIND FIRST ttShipIdLook  WHERE ttShipIdLook.Shipid = shipto.ship-id NO-LOCK NO-ERROR.
          IF AVAIL ttShipIdLook  THEN NEXT.
    create ttShipIdLook.
        assign
            ttShipIdLook.custnum   = cust.cust-no
            ttShipIdLook.Shipid    = shipto.ship-id
            ttShipIdLook.Shipiddscr = shipto.ship-name
            ttShipIdLook.carrier   = shipto.carrier 
            ttShipIdLook.cas-no    = cust.case-bundle
            ttShipIdLook.tr-no     = cust.pallet
            ttShipIdLook.adddr1    = shipto.ship-addr[1]  
            ttShipIdLook.addr2     = shipto.ship-addr[2]  
            ttShipIdLook.city      = shipto.ship-city     
            ttShipIdLook.state     = shipto.ship-state    
            ttShipIdLook.vZip      = shipto.ship-zip      
            .
    
        FIND FIRST carrier WHERE carrier.carrier = cust.carrier NO-LOCK NO-ERROR.
         ASSIGN
              ttShipIdLook.carrierdscr   = carrier.dscr. 

        find item where item.company = prmComp and
                item.i-no = cust.case-bundle no-lock no-error.
            if avail item then
                assign
            ttShipIdLook.cas-cnt    =  item.box-case
            ttShipIdLook.cas-len    = item.case-l
            ttShipIdLook.cas-wid    = item.case-w 
            ttShipIdLook.cas-dep    = item.case-d
            ttShipIdLook.cas-wt     = item.avg-w.
       find item where item.company = prmComp and
                item.i-no = cust.pallet no-lock no-error.          
            if avail item then 
                assign 
            ttShipIdLook.cas-paldscr    =  item.i-name
            ttShipIdLook.tr-len     = item.case-l
            ttShipIdLook.tr-dep     = item.case-d
            ttShipIdLook.tr-wid     = item.case-w.
    
        END.  /*FOR EACH usercust */
    END.
    END.  /*ifif prmAction <> "search" */

IF prmAction = "search" then do:
    if prmField = "ship-id"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH usercust WHERE usercust.user_id = prmUser 
                AND usercust.company EQ prmComp  NO-LOCK:
                FIND FIRST cust WHERE cust.cust-no = usercust.cust-no AND
                    cust.company = prmComp   NO-LOCK NO-ERROR.
                FIND FIRST shipto WHERE shipto.cust-no = cust.cust-no AND shipto.ship-id = prmText  NO-LOCK NO-ERROR.
                 FIND FIRST ttShipIdLook WHERE ttShipIdLook.Shipid = shipto.ship-id NO-LOCK NO-ERROR.
                     IF AVAIL ttShipIdLook THEN NEXT.
                IF AVAILABLE shipto THEN DO:
                    create ttShipIdLook.
                assign     
                    ttShipIdLook.custnum   =  cust.cust-no
                    ttShipIdLook.Shipid    = shipto.ship-id
                    ttShipIdLook.Shipiddscr    = shipto.ship-name
                    ttShipIdLook.carrier   = shipto.carrier 
                    ttShipIdLook.cas-no    = cust.case-bundle
                    ttShipIdLook.tr-no     = cust.pallet
                    ttShipIdLook.adddr1    = shipto.ship-addr[1]  
                    ttShipIdLook.addr2     = shipto.ship-addr[2]  
                    ttShipIdLook.city      = shipto.ship-city     
                    ttShipIdLook.state     = shipto.ship-state    
                    ttShipIdLook.vZip      = shipto.ship-zip .
                    
                FIND FIRST carrier WHERE carrier.carrier = cust.carrier NO-LOCK NO-ERROR.
         ASSIGN
              ttShipIdLook.carrierdscr   = carrier.dscr.

                find item where item.company = prmComp and
                    item.i-no = cust.case-bundle no-lock no-error.
                if avail item then
                    assign
                    ttShipIdLook.cas-cnt    =  item.box-case
                    ttShipIdLook.cas-len    = item.case-l
                    ttShipIdLook.cas-wid    = item.case-w 
                    ttShipIdLook.cas-dep    = item.case-d
                    ttShipIdLook.cas-wt     = item.avg-w.
                find item where item.company = prmComp and
                    item.i-no = cust.pallet no-lock no-error.          
                if avail item then 
                    assign
                    ttShipIdLook.cas-paldscr    =  item.i-name
                    ttShipIdLook.tr-len     = item.case-l
                    ttShipIdLook.tr-dep     = item.case-d
                    ttShipIdLook.tr-wid     = item.case-w.
                 END.
             END.
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH usercust WHERE usercust.user_id = prmUser 
                AND usercust.company EQ prmComp  NO-LOCK:
                FIND FIRST cust WHERE cust.cust-no = usercust.cust-no AND
                    cust.company = prmComp   NO-LOCK NO-ERROR.
                FOR EACH shipto WHERE shipto.cust-no = cust.cust-no AND shipto.ship-id BEGINS prmText  NO-LOCK :
                 FIND FIRST ttShipIdLook WHERE ttShipIdLook.Shipid = shipto.ship-id NO-LOCK NO-ERROR.
                 IF AVAIL ttShipIdLook THEN NEXT.
                create ttShipIdLook.
                assign     
                    ttShipIdLook.custnum   =  cust.cust-no
                    ttShipIdLook.Shipid    = shipto.ship-id
                     ttShipIdLook.Shipiddscr    = shipto.ship-name
                    ttShipIdLook.carrier   = shipto.carrier 
                    ttShipIdLook.cas-no    = cust.case-bundle
                    ttShipIdLook.tr-no     = cust.pallet
                    ttShipIdLook.adddr1    = shipto.ship-addr[1]
                    ttShipIdLook.addr2     = shipto.ship-addr[2]  
                    ttShipIdLook.city      = shipto.ship-city     
                    ttShipIdLook.state     = shipto.ship-state    
                    ttShipIdLook.vZip      = shipto.ship-zip 
                    .
                FIND FIRST carrier WHERE carrier.carrier = cust.carrier NO-LOCK NO-ERROR.
         ASSIGN
              ttShipIdLook.carrierdscr   = carrier.dscr.

                find item where item.company = prmComp and
                    item.i-no = cust.case-bundle no-lock no-error.
                if avail item then
                    assign
                    ttShipIdLook.cas-cnt    =  item.box-case
                    ttShipIdLook.cas-len    = item.case-l
                    ttShipIdLook.cas-wid    = item.case-w 
                    ttShipIdLook.cas-dep    = item.case-d
                    ttShipIdLook.cas-wt     = item.avg-w.
                find item where item.company = prmComp and
                    item.i-no = cust.pallet no-lock no-error.          
                if avail item then 
                    assign
                    ttShipIdLook.cas-paldscr    =  item.i-name
                    ttShipIdLook.tr-len     = item.case-l
                    ttShipIdLook.tr-dep     = item.case-d
                    ttShipIdLook.tr-wid     = item.case-w.
                
             END.
            END.
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = "ship-id"  */



      if prmField = "custnum"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH usercust WHERE usercust.user_id = prmUser 
                AND usercust.company EQ prmComp  NO-LOCK:
                FIND FIRST cust WHERE cust.cust-no = usercust.cust-no AND cust.cust-no = prmText AND
                    cust.company = prmComp   NO-LOCK NO-ERROR.
               FOR EACH shipto WHERE shipto.cust-no = cust.cust-no   NO-LOCK:
                 FIND FIRST ttShipIdLook WHERE ttShipIdLook.Shipid = shipto.ship-id NO-LOCK NO-ERROR.
                     IF AVAIL ttShipIdLook THEN NEXT.
                IF AVAILABLE shipto THEN DO:
                    create ttShipIdLook.
                assign     
                    ttShipIdLook.custnum   =  cust.cust-no
                    ttShipIdLook.Shipid    = shipto.ship-id
                    ttShipIdLook.Shipiddscr    = shipto.ship-name
                    ttShipIdLook.carrier   = shipto.carrier 
                    ttShipIdLook.cas-no    = cust.case-bundle
                    ttShipIdLook.tr-no     = cust.pallet
                    ttShipIdLook.adddr1    = shipto.ship-addr[1]  
                    ttShipIdLook.addr2     = shipto.ship-addr[2]  
                    ttShipIdLook.city      = shipto.ship-city     
                    ttShipIdLook.state     = shipto.ship-state    
                    ttShipIdLook.vZip      = shipto.ship-zip .
                    
                FIND FIRST carrier WHERE carrier.carrier = cust.carrier NO-LOCK NO-ERROR.
         ASSIGN
              ttShipIdLook.carrierdscr   = carrier.dscr.

                find item where item.company = prmComp and
                    item.i-no = cust.case-bundle no-lock no-error.
                if avail item then
                    assign
                    ttShipIdLook.cas-cnt    =  item.box-case
                    ttShipIdLook.cas-len    = item.case-l
                    ttShipIdLook.cas-wid    = item.case-w 
                    ttShipIdLook.cas-dep    = item.case-d
                    ttShipIdLook.cas-wt     = item.avg-w.
                find item where item.company = prmComp and
                    item.i-no = cust.pallet no-lock no-error.          
                if avail item then 
                    assign
                    ttShipIdLook.cas-paldscr    =  item.i-name
                    ttShipIdLook.tr-len     = item.case-l
                    ttShipIdLook.tr-dep     = item.case-d
                    ttShipIdLook.tr-wid     = item.case-w.
                 END.
             END.
            END.
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH usercust WHERE usercust.user_id = prmUser 
                AND usercust.company EQ prmComp  NO-LOCK:
              FIND FIRST cust WHERE cust.cust-no = usercust.cust-no AND cust.cust-no BEGINS prmText AND
                    cust.company = prmComp   NO-LOCK NO-ERROR.
                FOR EACH shipto WHERE shipto.cust-no = cust.cust-no  NO-LOCK :
                 FIND FIRST ttShipIdLook WHERE  ttShipIdLook.custnum = shipto.ship-id NO-LOCK NO-ERROR.
                 IF AVAIL ttShipIdLook THEN NEXT.
                create ttShipIdLook.
                assign     
                    ttShipIdLook.custnum   =  cust.cust-no
                    ttShipIdLook.Shipid    = shipto.ship-id
                     ttShipIdLook.Shipiddscr    = shipto.ship-name
                    ttShipIdLook.carrier   = shipto.carrier 
                    ttShipIdLook.cas-no    = cust.case-bundle
                    ttShipIdLook.tr-no     = cust.pallet
                    ttShipIdLook.adddr1    = shipto.ship-addr[1]
                    ttShipIdLook.addr2     = shipto.ship-addr[2]  
                    ttShipIdLook.city      = shipto.ship-city     
                    ttShipIdLook.state     = shipto.ship-state    
                    ttShipIdLook.vZip      = shipto.ship-zip 
                    .
                FIND FIRST carrier WHERE carrier.carrier = cust.carrier NO-LOCK NO-ERROR.
         ASSIGN
              ttShipIdLook.carrierdscr   = carrier.dscr.

                find item where item.company = prmComp and
                    item.i-no = cust.case-bundle no-lock no-error.
                if avail item then
                    assign
                    ttShipIdLook.cas-cnt    =  item.box-case
                    ttShipIdLook.cas-len    = item.case-l
                    ttShipIdLook.cas-wid    = item.case-w 
                    ttShipIdLook.cas-dep    = item.case-d
                    ttShipIdLook.cas-wt     = item.avg-w.
                find item where item.company = prmComp and
                    item.i-no = cust.pallet no-lock no-error.          
                if avail item then 
                    assign
                    ttShipIdLook.cas-paldscr    =  item.i-name
                    ttShipIdLook.tr-len     = item.case-l
                    ttShipIdLook.tr-dep     = item.case-d
                    ttShipIdLook.tr-wid     = item.case-w.
                
             END.
            END.
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = "ship-id"  */



    END.
    


