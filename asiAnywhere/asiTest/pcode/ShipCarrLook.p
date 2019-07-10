

/*------------------------------------------------------------------------
    File         :ShipCarrLookup.p
    Purpose     :  ShipCarr Lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttShipCarLook NO-UNDO 
    FIELD Shipcarr       AS CHAR FORMAT "x(5)"
    FIELD Cardscr  AS CHAR FORMAT "x(30)"
   
    .
 DEFINE DATASET dsShipCarLook FOR ttShipCarLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmShip   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsShipCarLook.

IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmAction    = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN  prmUser      = "".
IF prmShip   = ? THEN ASSIGN prmShip      = "".
IF prmField     = ? THEN ASSIGN prmField      = "".
IF prmCondition = ? THEN ASSIGN prmCondition      = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    MESSAGE "carrier" prmAction prmComp prmShip prmUser.
    FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp  NO-LOCK:
    FIND FIRST shipto WHERE shipto.cust-no = usercust.cust-no  NO-LOCK NO-ERROR.
    OE-LOOP :
    FOR EACH carrier WHERE shipto.carrier= carrier.carrier NO-LOCK:
        FIND FIRST ttShipCarLook WHERE ttShipCarLook.Shipcarr = carrier.carrier NO-LOCK NO-ERROR.
            IF AVAIL ttShipCarLook THEN NEXT OE-LOOP.
        create ttShipCarLook.
        assign                                     
            ttShipCarLook.Shipcarr    = carrier.carrier
            ttShipCarLook.Cardscr       = carrier.dscr.  
    END.
    
   END.
END.  /*ifif prmAction <> "search" */

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
           FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp  NO-LOCK:
    FIND FIRST shipto WHERE shipto.cust-no = usercust.cust-no  NO-LOCK NO-ERROR.
    OE-LOOP :
    FOR EACH carrier WHERE shipto.carrier = carrier.carrier no-lock:
                IF carrier.carrier = prmText OR
                   carrier.dscr = prmText THEN
                   DO:
                      create ttShipCarLook.
                      assign                                     
                        ttShipCarLook.Shipcarr = carrier.carrier
                        ttShipCarLook.Cardscr = carrier.dscr.
                   END.
    END.
            END.    /*for each itemfg*/
    END.   /* IF prmCondition = "EQUAL"*/

    if prmCondition = "BEGIN" then do:
        FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp  NO-LOCK:
    FIND FIRST shipto WHERE shipto.cust-no = usercust.cust-no  NO-LOCK NO-ERROR.
    OE-LOOP :
    FOR EACH carrier WHERE shipto.carrier= carrier.carrier no-lock:
            IF carrier.carrier begins prmText OR
               carrier.dscr begins prmText THEN
                DO:

                create ttShipCarLook.
                assign                                                              
                    ttShipCarLook.Shipcarr = carrier.carrier
                    ttShipCarLook.Cardscr = carrier.dscr.
                END.
                
    END.
        END.
    END.   /*if prmCondition = "BEGIN"if prmCondition = "BEGIN"*/     
END.   /*IF prmField = "ANY" then do:*/

if prmField = "carrier" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp  NO-LOCK:
    FIND FIRST shipto WHERE shipto.cust-no = usercust.cust-no  NO-LOCK NO-ERROR.
    OE-LOOP :
    FOR EACH carrier WHERE shipto.carrier= carrier.carrier AND carrier.carrier = prmText no-lock:
                create ttShipCarLook.
                assign                                                                
                    ttShipCarLook.Shipcarr    = carrier.carrier
                    ttShipCarLook.Cardscr = carrier.dscr.     
    END.
            
        END. /*for each itemfg*/
    END.   /*if prmCondition = "EQUAL"*/

    if prmCondition = "BEGIN" then do:
        FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp  NO-LOCK:
    FIND FIRST shipto WHERE shipto.cust-no = usercust.cust-no  NO-LOCK NO-ERROR.
    OE-LOOP :
    FOR EACH carrier WHERE shipto.carrier= carrier.carrier AND carrier.carrier begins prmText no-lock:
                create ttShipCarLook.
                assign                                                                  
                    ttShipCarLook.Shipcarr    = carrier.carrier
                    ttShipCarLook.Cardscr = carrier.dscr.
    END.
        END.
    END.  /*if prmCondition = "BEGIN"*/       
END.   /*  if prmField = "carrier" then do:*/
if prmField = "dscr" then do:
     if prmCondition = "EQUAL" then do:
         
              FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp  NO-LOCK:
    FIND FIRST shipto WHERE shipto.cust-no = usercust.cust-no  NO-LOCK NO-ERROR.
    OE-LOOP :
    FOR EACH carrier WHERE shipto.carrier= carrier.carrier AND carrier.dscr = prmText no-lock:
                    create ttShipCarLook.
                    assign                                                                
                        ttShipCarLook.Shipcarr    = carrier.carrier
                        ttShipCarLook.Cardscr = carrier.dscr.
    END.
          END.
    END.   /*if prmCondition = "EQUAL" */
    if prmCondition = "BEGIN" then do:
         
              FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp  NO-LOCK:
    FIND FIRST shipto WHERE shipto.cust-no = usercust.cust-no  NO-LOCK NO-ERROR.
    OE-LOOP :
    FOR EACH carrier WHERE shipto.carrier= carrier.carrier AND carrier.dscr begins prmText no-lock:
                  create ttShipCarLook.
                  assign                                                                  
                      ttShipCarLook.Shipcarr    = carrier.carrier
                      ttShipCarLook.Cardscr = carrier.dscr. 
    END.
              
          END.
    END. /*if prmCondition = "BEGIN" */       
END.
END. /* IF prmAction = search then do: */



