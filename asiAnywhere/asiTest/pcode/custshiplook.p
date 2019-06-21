

/*------------------------------------------------------------------------
    File         : Cust Part Lookup
    Purpose     :  Cust Part lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Oct 01 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCustShipLook NO-UNDO 
    FIELD Cust-no AS CHARACTER
    FIELD CName AS CHARACTER
    FIELD Addr AS CHARACTER
    FIELD Addr2 AS CHAR 
    FIELD City AS CHARACTER
    FIELD State AS CHARACTER
    FIELD Zip AS CHARACTER
    FIELD carrier AS CHARACTER
    FIELD carrdesc AS CHAR
    FIELD Sman     AS CHAR  
    FIELD sname    AS CHAR 
    FIELD terms    AS CHAR
    FIELD termsdesc AS CHAR
    FIELD zone AS CHAR
    FIELD zonedesc AS CHAR 
    FIELD shipid AS CHAR
    FIELD shipname AS CHAR
    FIELD shipadd1 AS CHAR
    FIELD shipadd2 AS CHAR
    FIELD shipcity AS CHAR
    FIELD shipstat AS CHAR
    FIELD shipzip AS CHAR

    FIELD vsoldid AS CHAR
    FIELD vsoldname AS CHAR
    FIELD vsoldadd1 AS CHAR
    FIELD vsoldadd2 AS CHAR
    FIELD vsoldcity AS CHAR
    FIELD vsoldstat AS CHAR
    FIELD vsoldzip AS CHAR
    FIELD vcontact AS CHAR 
    FIELD hghjj AS CHAR

    .
                                           
    
DEFINE DATASET dsCustShipLook FOR ttCustShipLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmItemNum   AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustShipLook.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmItemNum      = ? THEN ASSIGN prmItemNum      = "".
IF prmField      = ? THEN ASSIGN prmField      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

IF prmAction <> "search" then do:
     FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
         FOR EACH cust WHERE  cust.company = prmComp AND cust.cust-no = usercust.cust-no NO-LOCK:
             create ttCustShipLook.
             assign                                     
                 ttCustShipLook.Cust-no = cust.cust-no
                 ttCustShipLook.CName   = cust.name
                 ttCustShipLook.Addr    = cust.addr[1] 
                 ttCustShipLook.Addr2    = cust.addr[2]
                 ttCustShipLook.City    = cust.city
                 ttCustShipLook.State   = cust.state
                 ttCustShipLook.Zip     = cust.zip
                 ttCustShipLook.carrier = cust.carrier
                 ttCustShipLook.Sman     = cust.sman
                 ttCustShipLook.terms    = cust.terms
                 ttCustShipLook.vcontact  = cust.contact
                 ttCustShipLook.zone     = cust.del-zone
                 .
            
                 FIND FIRST shipto  WHERE shipto.company EQ prmComp 
                 AND shipto.cust-no EQ cust.cust-no NO-LOCK NO-ERROR.
             IF AVAIL shipto THEN
                 ASSIGN
                    ttCustShipLook.shipid    = shipto.ship-id
                    ttCustShipLook.shipname  = shipto.ship-name
                    ttCustShipLook.shipadd1  = shipto.ship-addr[1]
                    ttCustShipLook.shipadd2   = shipto.ship-addr[2]
                    ttCustShipLook.shipcity  = shipto.ship-city
                    ttCustShipLook.shipstat  = shipto.ship-state
                    ttCustShipLook.shipzip   = shipto.ship-zip .

        
            FIND FIRST soldto  WHERE soldto.company EQ prmComp 
                    AND soldto.cust-no EQ cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL soldto THEN
                ASSIGN
                    ttCustShipLook.vsoldid     = soldto.sold-id
                    ttCustShipLook.vsoldname   = soldto.sold-name
                    ttCustShipLook.vsoldadd1   = soldto.sold-addr[1]
                    ttCustShipLook.vsoldadd2    = soldto.sold-addr[2]
                    ttCustShipLook.vsoldcity   = soldto.sold-city
                    ttCustShipLook.vsoldstat   = soldto.sold-state
                    ttCustShipLook.vsoldzip    = soldto.sold-zip .

        FIND FIRST carrier  WHERE carrier.company = prmComp AND carrier.loc = cust.loc
                          AND carrier.carrier EQ cust.carrier NO-LOCK NO-ERROR.
    IF  AVAILABLE carrier THEN DO:
        ttCustShipLook.carrdesc = carrier.dscr.
    END.   /*IF  AVAILABLE carrier */
       FIND FIRST terms WHERE terms.company = cust.company and terms.t-code  eq cust.terms  NO-LOCK NO-ERROR.
     IF  AVAILABLE terms THEN DO:
         ASSIGN
             ttCustShipLook.termsdesc     = terms.dscr.
     END.
     FIND FIRST sman WHERE sman.company = prmComp AND sman.sman= cust.sman  NO-LOCK NO-ERROR.
     IF  AVAILABLE sman THEN DO:
         ASSIGN
             ttCustShipLook.sname        = sman.sname.
     END.
     FIND FIRST carr-mtx where carr-mtx.company = cust.company and 
                               carr-mtx.loc = cust.loc and
                               carr-mtx.carrier = cust.carrier and
                               carr-mtx.del-zone = cust.del-zone 
         no-lock NO-ERROR.
     IF AVAILABLE carr-mtx THEN ASSIGN
         ttCustShipLook.zonedesc = carr-mtx.del-dscr.

         
         

         END.  /*FOR EACH cust*/
     END. /*user cust*/
 END.  /*ifif prmAction <> "search" */


IF prmAction = "search" then do:
    
if prmField = "cust" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
         FOR EACH cust WHERE  cust.company = prmComp AND cust.cust-no = usercust.cust-no AND cust.cust-no = prmText NO-LOCK:
             create ttCustShipLook.
             assign                                     
                 ttCustShipLook.Cust-no = cust.cust-no
                 ttCustShipLook.CName   = cust.name
                 ttCustShipLook.Addr    = cust.addr[1] 
                 ttCustShipLook.Addr2    = cust.addr[2]
                 ttCustShipLook.City    = cust.city
                 ttCustShipLook.State   = cust.state
                 ttCustShipLook.Zip     = cust.zip
                 ttCustShipLook.carrier = cust.carrier
                 ttCustShipLook.Sman     = cust.sman
                 ttCustShipLook.terms    = cust.terms
                 ttCustShipLook.vcontact  = cust.contact
                 ttCustShipLook.zone     = cust.del-zone
                 .
            
                 FIND FIRST shipto  WHERE shipto.company EQ prmComp 
                 AND shipto.cust-no EQ cust.cust-no NO-LOCK NO-ERROR.
             IF AVAIL shipto THEN
                 ASSIGN
                    ttCustShipLook.shipid    = shipto.ship-id
                    ttCustShipLook.shipname  = shipto.ship-name
                    ttCustShipLook.shipadd1  = shipto.ship-addr[1]
                    ttCustShipLook.shipadd2   = shipto.ship-addr[2]
                    ttCustShipLook.shipcity  = shipto.ship-city
                    ttCustShipLook.shipstat  = shipto.ship-state
                    ttCustShipLook.shipzip   = shipto.ship-zip .

        
            FIND FIRST soldto  WHERE soldto.company EQ prmComp 
                    AND soldto.cust-no EQ cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL soldto THEN
                ASSIGN
                    ttCustShipLook.vsoldid     = soldto.sold-id
                    ttCustShipLook.vsoldname   = soldto.sold-name
                    ttCustShipLook.vsoldadd1   = soldto.sold-addr[1]
                    ttCustShipLook.vsoldadd2    = soldto.sold-addr[2]
                    ttCustShipLook.vsoldcity   = soldto.sold-city
                    ttCustShipLook.vsoldstat   = soldto.sold-state
                    ttCustShipLook.vsoldzip    = soldto.sold-zip .

        FIND FIRST carrier  WHERE carrier.company = prmComp AND carrier.loc = cust.loc
                          AND carrier.carrier EQ cust.carrier NO-LOCK NO-ERROR.
    IF  AVAILABLE carrier THEN DO:
        ttCustShipLook.carrdesc = carrier.dscr.
    END.   /*IF  AVAILABLE carrier */
       FIND FIRST terms WHERE terms.company = cust.company and terms.t-code  eq cust.terms  NO-LOCK NO-ERROR.
     IF  AVAILABLE terms THEN DO:
         ASSIGN
             ttCustShipLook.termsdesc     = terms.dscr.
     END.
     FIND FIRST sman WHERE sman.company = prmComp AND sman.sman= cust.sman  NO-LOCK NO-ERROR.
     IF  AVAILABLE sman THEN DO:
         ASSIGN
             ttCustShipLook.sname        = sman.sname.
     END.
     FIND FIRST carr-mtx where carr-mtx.company = cust.company and 
                               carr-mtx.loc = cust.loc and
                               carr-mtx.carrier = cust.carrier and
                               carr-mtx.del-zone = cust.del-zone 
         no-lock NO-ERROR.
     IF AVAILABLE carr-mtx THEN ASSIGN
         ttCustShipLook.zonedesc = carr-mtx.del-dscr.

         
         

         END.  /*FOR EACH cust*/
     END. /*user cust*/
    END.   /*if prmCondition = "EQUAL"*/
    if prmCondition = "BEGIN" then do:
        FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
         FOR EACH cust WHERE  cust.company = prmComp AND cust.cust-no = usercust.cust-no AND cust.cust-no BEGINS prmText NO-LOCK:
             create ttCustShipLook.
             assign                                     
                 ttCustShipLook.Cust-no = cust.cust-no
                 ttCustShipLook.CName   = cust.name
                 ttCustShipLook.Addr    = cust.addr[1] 
                 ttCustShipLook.Addr2    = cust.addr[2]
                 ttCustShipLook.City    = cust.city
                 ttCustShipLook.State   = cust.state
                 ttCustShipLook.Zip     = cust.zip
                 ttCustShipLook.carrier = cust.carrier
                 ttCustShipLook.Sman     = cust.sman
                 ttCustShipLook.terms    = cust.terms
                 ttCustShipLook.vcontact  = cust.contact
                 ttCustShipLook.zone     = cust.del-zone
                 .
            
                 FIND FIRST shipto  WHERE shipto.company EQ prmComp 
                 AND shipto.cust-no EQ cust.cust-no NO-LOCK NO-ERROR.
             IF AVAIL shipto THEN
                 ASSIGN
                    ttCustShipLook.shipid    = shipto.ship-id
                    ttCustShipLook.shipname  = shipto.ship-name
                    ttCustShipLook.shipadd1  = shipto.ship-addr[1]
                    ttCustShipLook.shipadd2   = shipto.ship-addr[2]
                    ttCustShipLook.shipcity  = shipto.ship-city
                    ttCustShipLook.shipstat  = shipto.ship-state
                    ttCustShipLook.shipzip   = shipto.ship-zip .

        
            FIND FIRST soldto  WHERE soldto.company EQ prmComp 
                    AND soldto.cust-no EQ cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL soldto THEN
                ASSIGN
                    ttCustShipLook.vsoldid     = soldto.sold-id
                    ttCustShipLook.vsoldname   = soldto.sold-name
                    ttCustShipLook.vsoldadd1   = soldto.sold-addr[1]
                    ttCustShipLook.vsoldadd2    = soldto.sold-addr[2]
                    ttCustShipLook.vsoldcity   = soldto.sold-city
                    ttCustShipLook.vsoldstat   = soldto.sold-state
                    ttCustShipLook.vsoldzip    = soldto.sold-zip .

        FIND FIRST carrier  WHERE carrier.company = prmComp AND carrier.loc = cust.loc
                          AND carrier.carrier EQ cust.carrier NO-LOCK NO-ERROR.
    IF  AVAILABLE carrier THEN DO:
        ttCustShipLook.carrdesc = carrier.dscr.
    END.   /*IF  AVAILABLE carrier */
       FIND FIRST terms WHERE terms.company = cust.company and terms.t-code  eq cust.terms  NO-LOCK NO-ERROR.
     IF  AVAILABLE terms THEN DO:
         ASSIGN
             ttCustShipLook.termsdesc     = terms.dscr.
     END.
     FIND FIRST sman WHERE sman.company = prmComp AND sman.sman= cust.sman  NO-LOCK NO-ERROR.
     IF  AVAILABLE sman THEN DO:
         ASSIGN
             ttCustShipLook.sname        = sman.sname.
     END.
     FIND FIRST carr-mtx where carr-mtx.company = cust.company and 
                               carr-mtx.loc = cust.loc and
                               carr-mtx.carrier = cust.carrier and
                               carr-mtx.del-zone = cust.del-zone 
         no-lock NO-ERROR.
     IF AVAILABLE carr-mtx THEN ASSIGN
         ttCustShipLook.zonedesc = carr-mtx.del-dscr.

         
         

         END.  /*FOR EACH cust*/
     END. /*user cust*/
    END.  /*if prmCondition = "BEGIN"*/  
END.   /*  if prmField = "part-no" then do:*/

if prmField = "name" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
         FOR EACH cust WHERE  cust.company = prmComp AND cust.cust-no = usercust.cust-no AND cust.name = prmText NO-LOCK:
             create ttCustShipLook.
             assign                                     
                 ttCustShipLook.Cust-no = cust.cust-no
                 ttCustShipLook.CName   = cust.name
                 ttCustShipLook.Addr    = cust.addr[1] 
                 ttCustShipLook.Addr2    = cust.addr[2]
                 ttCustShipLook.City    = cust.city
                 ttCustShipLook.State   = cust.state
                 ttCustShipLook.Zip     = cust.zip
                 ttCustShipLook.carrier = cust.carrier
                 ttCustShipLook.Sman     = cust.sman
                 ttCustShipLook.terms    = cust.terms
                 ttCustShipLook.vcontact  = cust.contact
                 ttCustShipLook.zone     = cust.del-zone
                 .
            
                 FIND FIRST shipto  WHERE shipto.company EQ prmComp 
                 AND shipto.cust-no EQ cust.cust-no NO-LOCK NO-ERROR.
             IF AVAIL shipto THEN
                 ASSIGN
                    ttCustShipLook.shipid    = shipto.ship-id
                    ttCustShipLook.shipname  = shipto.ship-name
                    ttCustShipLook.shipadd1  = shipto.ship-addr[1]
                    ttCustShipLook.shipadd2   = shipto.ship-addr[2]
                    ttCustShipLook.shipcity  = shipto.ship-city
                    ttCustShipLook.shipstat  = shipto.ship-state
                    ttCustShipLook.shipzip   = shipto.ship-zip .

        
            FIND FIRST soldto  WHERE soldto.company EQ prmComp 
                    AND soldto.cust-no EQ cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL soldto THEN
                ASSIGN
                    ttCustShipLook.vsoldid     = soldto.sold-id
                    ttCustShipLook.vsoldname   = soldto.sold-name
                    ttCustShipLook.vsoldadd1   = soldto.sold-addr[1]
                    ttCustShipLook.vsoldadd2    = soldto.sold-addr[2]
                    ttCustShipLook.vsoldcity   = soldto.sold-city
                    ttCustShipLook.vsoldstat   = soldto.sold-state
                    ttCustShipLook.vsoldzip    = soldto.sold-zip .

        FIND FIRST carrier  WHERE carrier.company = prmComp AND carrier.loc = cust.loc
                          AND carrier.carrier EQ cust.carrier NO-LOCK NO-ERROR.
    IF  AVAILABLE carrier THEN DO:
        ttCustShipLook.carrdesc = carrier.dscr.
    END.   /*IF  AVAILABLE carrier */
       FIND FIRST terms WHERE terms.company = cust.company and terms.t-code  eq cust.terms  NO-LOCK NO-ERROR.
     IF  AVAILABLE terms THEN DO:
         ASSIGN
             ttCustShipLook.termsdesc     = terms.dscr.
     END.
     FIND FIRST sman WHERE sman.company = prmComp AND sman.sman= cust.sman  NO-LOCK NO-ERROR.
     IF  AVAILABLE sman THEN DO:
         ASSIGN
             ttCustShipLook.sname        = sman.sname.
     END.
     FIND FIRST carr-mtx where carr-mtx.company = cust.company and 
                               carr-mtx.loc = cust.loc and
                               carr-mtx.carrier = cust.carrier and
                               carr-mtx.del-zone = cust.del-zone 
         no-lock NO-ERROR.
     IF AVAILABLE carr-mtx THEN ASSIGN
         ttCustShipLook.zonedesc = carr-mtx.del-dscr.

         
         

         END.  /*FOR EACH cust*/
     END. /*user cust*/
    END.   /*if prmCondition = "EQUAL" */
    
    if prmCondition = "BEGIN" then do:
        FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
         FOR EACH cust WHERE  cust.company = prmComp AND cust.cust-no = usercust.cust-no AND cust.name BEGINS prmText NO-LOCK:
             create ttCustShipLook.
             assign                                     
                 ttCustShipLook.Cust-no = cust.cust-no
                 ttCustShipLook.CName   = cust.name
                 ttCustShipLook.Addr    = cust.addr[1] 
                 ttCustShipLook.Addr2    = cust.addr[2]
                 ttCustShipLook.City    = cust.city
                 ttCustShipLook.State   = cust.state
                 ttCustShipLook.Zip     = cust.zip
                 ttCustShipLook.carrier = cust.carrier
                 ttCustShipLook.Sman     = cust.sman
                 ttCustShipLook.terms    = cust.terms
                 ttCustShipLook.vcontact  = cust.contact
                 ttCustShipLook.zone     = cust.del-zone
                 .
            
                 FIND FIRST shipto  WHERE shipto.company EQ prmComp 
                 AND shipto.cust-no EQ cust.cust-no NO-LOCK NO-ERROR.
             IF AVAIL shipto THEN
                 ASSIGN
                    ttCustShipLook.shipid    = shipto.ship-id
                    ttCustShipLook.shipname  = shipto.ship-name
                    ttCustShipLook.shipadd1  = shipto.ship-addr[1]
                    ttCustShipLook.shipadd2   = shipto.ship-addr[2]
                    ttCustShipLook.shipcity  = shipto.ship-city
                    ttCustShipLook.shipstat  = shipto.ship-state
                    ttCustShipLook.shipzip   = shipto.ship-zip .

        
            FIND FIRST soldto  WHERE soldto.company EQ prmComp 
                    AND soldto.cust-no EQ cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL soldto THEN
                ASSIGN
                    ttCustShipLook.vsoldid     = soldto.sold-id
                    ttCustShipLook.vsoldname   = soldto.sold-name
                    ttCustShipLook.vsoldadd1   = soldto.sold-addr[1]
                    ttCustShipLook.vsoldadd2    = soldto.sold-addr[2]
                    ttCustShipLook.vsoldcity   = soldto.sold-city
                    ttCustShipLook.vsoldstat   = soldto.sold-state
                    ttCustShipLook.vsoldzip    = soldto.sold-zip .

        FIND FIRST carrier  WHERE carrier.company = prmComp AND carrier.loc = cust.loc
                          AND carrier.carrier EQ cust.carrier NO-LOCK NO-ERROR.
    IF  AVAILABLE carrier THEN DO:
        ttCustShipLook.carrdesc = carrier.dscr.
    END.   /*IF  AVAILABLE carrier */
       FIND FIRST terms WHERE terms.company = cust.company and terms.t-code  eq cust.terms  NO-LOCK NO-ERROR.
     IF  AVAILABLE terms THEN DO:
         ASSIGN
             ttCustShipLook.termsdesc     = terms.dscr.
     END.
     FIND FIRST sman WHERE sman.company = prmComp AND sman.sman= cust.sman  NO-LOCK NO-ERROR.
     IF  AVAILABLE sman THEN DO:
         ASSIGN
             ttCustShipLook.sname        = sman.sname.
     END.
     FIND FIRST carr-mtx where carr-mtx.company = cust.company and 
                               carr-mtx.loc = cust.loc and
                               carr-mtx.carrier = cust.carrier and
                               carr-mtx.del-zone = cust.del-zone 
         no-lock NO-ERROR.
     IF AVAILABLE carr-mtx THEN ASSIGN
         ttCustShipLook.zonedesc = carr-mtx.del-dscr.

         
         

         END.  /*FOR EACH cust*/
     END. /*user cust*/
    END.  /*if prmCondition = "BEGIN" */     
END.   /*if prmField = "ship-name"*/
END. /* IF prmAction = search then do: */


  
