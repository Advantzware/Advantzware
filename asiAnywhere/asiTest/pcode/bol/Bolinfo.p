/*------------------------------------------------------------------------
    File        : bolInvPrint.p
    Purpose     : BOL Print

    Syntax      :

    Description : Return a Dataset of BOL

    Author(s)   : Kuldeep
    Created     : FEB 08 2010
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttBolInfo NO-UNDO
    
    FIELD vBolNo AS INTEGER
    FIELD vBillNo AS INTEGER
    FIELD vshipNo AS INTEGER
    FIELD vBolDate AS CHARACTER    
    FIELD vPoNo   AS CHARACTER
    FIELD vCarrier     AS CHARACTER
    FIELD vFreight    AS DECIMAL
    FIELD vTotWt     AS INTEGER     
    FIELD vCustNo   AS CHARACTER
    FIELD vShipId   AS CHARACTER  
    FIELD vShipName   AS CHARACTER
    FIELD vShipAddr1   AS CHARACTER
    FIELD vShipAddr2   AS CHARACTER
    FIELD vShipAddr3   AS CHARACTER
    FIELD vCustName   AS CHARACTER
    FIELD vCustAddr1   AS CHARACTER
    FIELD vCustAddr2   AS CHARACTER
    FIELD vCustAddr3   AS CHARACTER
    FIELD vFob   AS CHARACTER
    FIELD vDescr   AS CHARACTER
    FIELD vFrtTerms   AS CHARACTER    
    .
DEFINE DATASET dsBolInfo FOR ttBolInfo.

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustomer      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmBol      AS INTEGER  NO-UNDO.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBolInfo.

IF prmAction     = ? THEN ASSIGN prmAction     = "".
IF prmUser       = ? THEN ASSIGN prmUser       = "".
IF prmCustomer   = ? THEN ASSIGN prmCustomer   = "".
IF prmBol        = ? THEN ASSIGN prmBol        = 0.

DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.

def var v-frt-terms as char format "x(10)" no-undo.
def var v-fob               as   char format "x(12)".
def var v-comp-Id    like cust.cust-no.
def var v-comp-name  like company.name.
def var v-comp-addr  like company.addr.
def var v-comp-addr3 as   char format "x(30)".

def var v-ship-id    like shipto.ship-id.
def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-addr3 as   char format "x(30)".


DEF VAR prmComp AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
prmComp = "001".
FIND FIRST usercust NO-LOCK WHERE usercust.company EQ "001"
    AND usercust.user_id = prmUser NO-ERROR.

assign
  cocode = prmComp
  locode = usercomp.loc.





/* ********************  Preprocessor Definitions  ******************** */
IF prmAction = "BolInfo" THEN do:
FIND oe-boll WHERE oe-boll.company = prmComp AND oe-boll.bol-no = prmBol NO-LOCK.
    IF AVAILABLE oe-boll THEN 
    FIND FIRST oe-bolh OF oe-boll NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN DO:
    
        CREATE ttBolInfo.
        ASSIGN                       
            ttBolInfo.vBolNo        =  oe-boll.bol-no
            ttBolInfo.vBillNo       =  oe-bolh.b-no
            ttBolInfo.vshipNo       =  oe-bolh.ship-no            
            ttBolInfo.vBolDate      =  STRING(oe-bolh.bol-date)
            ttBolInfo.vPoNo         =  oe-bolh.po-no
            ttBolInfo.vCarrier      =  oe-bolh.carrier
            ttBolInfo.vFreight      =  oe-bolh.freight
            ttBolInfo.vTotWt        =  oe-bolh.tot-wt      
        . 

    find first carrier
        where carrier.company eq oe-bolh.company
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.
    FIND first oe-ord WHERE
	          oe-ord.company eq oe-boll.company AND
	          oe-ord.ord-no  eq oe-boll.ord-no
	          NO-LOCK NO-ERROR.

      if not available carrier then
      find first carrier where carrier.company = oe-ord.company
        and carrier.carrier = oe-ord.carrier no-lock no-error.

     FIND first cust where cust.company eq cocode and cust.cust-no eq oe-bolh.cust-no NO-LOCK NO-ERROR.
     IF AVAIL cust THEN DO:     
     assign
       v-comp-Id      = cust.cust-no
       v-comp-name    = cust.name
       v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
       v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.
       ASSIGN
           ttBolInfo.vCustNo = v-comp-Id
           ttBolInfo.vCustName = v-comp-name
           ttBolInfo.vCustAddr1 = v-comp-addr[1]
           ttBolInfo.vCustAddr2 = v-comp-addr[2]
           ttBolInfo.vCustAddr3 = v-comp-addr3
           .
     END.

     FIND FIRST shipto WHERE shipto.ship-id = oe-bolh.ship-id NO-LOCK NO-ERROR.
        IF AVAIL shipto THEN DO:  
            assign
                v-ship-id      = shipto.ship-id
                v-ship-name    = shipto.ship-name
                v-ship-addr[1] = shipto.ship-addr[1]
                v-ship-addr[2] = shipto.ship-addr[2]
                v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip.
            ASSIGN
           ttBolInfo.vShipId = v-ship-id
           ttBolInfo.vShipName = v-ship-name
           ttBolInfo.vShipAddr1 = v-ship-addr[1]
           ttBolInfo.vShipAddr2 = v-ship-addr[2]
           ttBolInfo.vShipAddr3 = v-ship-addr3
           .
        END. 

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".     
      v-frt-terms = if oe-bolh.frt-pay eq "P" then "Prepaid"
                           else if oe-bolh.frt-pay eq "B" then "Bill"
                           else if oe-bolh.frt-pay eq "C" then "Collect"
                           else if oe-bolh.frt-pay eq "T" then "Third Party"
                           else "".
        
        ASSIGN
           ttBolInfo.vFob = v-fob
           ttBolInfo.vDescr = carrier.dscr
           ttBolInfo.vFrtTerms = v-frt-terms
           .
                                              
    END.
END.  /*IF prmAction = "BolInfo" THEN do:*/

