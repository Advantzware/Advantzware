/*------------------------------------------------------------------------
    File         : ShipId Lookup
    Purpose     :  ShipId lookup

    Syntax      :

    Description : Return a Dataset of all ShipId

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttShipIdVal 
    FIELD vCust AS  CHAR
    FIELD vAddress2 AS  CHAR
    FIELD vshipid AS CHAR 
    FIELD vshiploc AS CHAR
    FIELD vshipaddress AS CHAR
    FIELD vshipstate AS CHAR
    FIELD vshipcity AS CHAR
    FIELD vshipzip AS CHAR.
DEFINE DATASET dsShipIdVal FOR ttShipIdVal.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsShipIdVal.

    IF prmAction  = ?    THEN ASSIGN prmAction = "".
    IF prmUser  = ?      THEN ASSIGN prmUser = "".
    IF prmField  = ?     THEN ASSIGN prmField = "".
    IF prmCondition  = ? THEN ASSIGN prmCondition = "".
    IF prmText  = ?      THEN ASSIGN prmText = "".

    DEF VAR prmComp AS CHAR NO-UNDO.

    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction <> "search"  THEN DO:

    FOR EACH usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id  NO-LOCK:
        FOR EACH shipto WHERE shipto.company = prmComp 
            AND shipto.cust-no = usercust.cust-no NO-LOCK:
            CREATE ttShipIdVal.
            ASSIGN 

           ttShipIdVal.vshipid = shipto.ship-id
           ttShipIdVal.vCust = shipto.cust-no
           ttShipIdVal.vAddress2 = shipto.ship-addr[2]
           ttShipIdVal.vshiploc = shipto.ship-name
           ttShipIdVal.vshipaddress = shipto.ship-addr[1]
           ttShipIdVal.vshipstate = shipto.ship-state
           ttShipIdVal.vshipcity = shipto.ship-city
           ttShipIdVal.vshipzip = shipto.ship-zip.
        END.
     END.
END.


IF prmAction = "search" then do:
     if prmField = "vshipid"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id   NO-LOCK:
        FOR EACH shipto WHERE shipto.company = prmComp 
            AND shipto.cust-no = usercust.cust-no AND shipto.ship-id = prmText NO-LOCK:
            CREATE ttShipIdVal.
            ASSIGN 
           ttShipIdVal.vshipid = shipto.ship-id
           ttShipIdVal.vCust = shipto.cust-no
           ttShipIdVal.vAddress2 = shipto.ship-addr[2]
           ttShipIdVal.vshiploc = shipto.ship-name
           ttShipIdVal.vshipaddress = shipto.ship-addr[1]
           ttShipIdVal.vshipstate = shipto.ship-state
           ttShipIdVal.vshipcity = shipto.ship-city
           ttShipIdVal.vshipzip = shipto.ship-zip.
        END.
    END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id   NO-LOCK:
        FOR EACH shipto WHERE shipto.company = prmComp 
            AND shipto.cust-no = usercust.cust-no AND shipto.ship-id BEGINS prmText NO-LOCK:
            CREATE ttShipIdVal.
            ASSIGN 
           ttShipIdVal.vshipid = shipto.ship-id
            ttShipIdVal.vCust = shipto.cust-no
           ttShipIdVal.vAddress2 = shipto.ship-addr[2]
           ttShipIdVal.vshiploc = shipto.ship-name
           ttShipIdVal.vshipaddress = shipto.ship-addr[1]
           ttShipIdVal.vshipstate = shipto.ship-state
           ttShipIdVal.vshipcity = shipto.ship-city
           ttShipIdVal.vshipzip = shipto.ship-zip.
        END.
    END.
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */







         if prmField = "vshiploc"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH usercust WHERE usercust.company = prmComp AND usercust.USER_id = usercomp.USER_id  NO-LOCK:
        FOR EACH shipto WHERE shipto.company = prmComp 
            AND shipto.cust-no = usercust.cust-no AND shipto.ship-name = prmText NO-LOCK:
            CREATE ttShipIdVal.
            ASSIGN 
           ttShipIdVal.vshipid = shipto.ship-id
                ttShipIdVal.vCust = shipto.cust-no
           ttShipIdVal.vAddress2 = shipto.ship-addr[2]
           ttShipIdVal.vshiploc = shipto.ship-name
           ttShipIdVal.vshipaddress = shipto.ship-addr[1]
           ttShipIdVal.vshipstate = shipto.ship-state
           ttShipIdVal.vshipcity = shipto.ship-city
           ttShipIdVal.vshipzip = shipto.ship-zip.
        END.
    END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH usercust WHERE usercust.company = prmComp  AND usercust.USER_id = usercomp.USER_id  NO-LOCK:
        FOR EACH shipto WHERE shipto.company = prmComp 
            AND shipto.cust-no = usercust.cust-no AND shipto.ship-name BEGINS prmText NO-LOCK:
            CREATE ttShipIdVal.
            ASSIGN 
           ttShipIdVal.vshipid = shipto.ship-id
           ttShipIdVal.vCust = shipto.cust-no
           ttShipIdVal.vAddress2 = shipto.ship-addr[2]
           ttShipIdVal.vshiploc = shipto.ship-name
           ttShipIdVal.vshipaddress = shipto.ship-addr[1]
           ttShipIdVal.vshipstate = shipto.ship-state
           ttShipIdVal.vshipcity = shipto.ship-city
           ttShipIdVal.vshipzip = shipto.ship-zip.
        END.
    END.
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */

