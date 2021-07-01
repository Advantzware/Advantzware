USING system.SharedConfig.
DISABLE TRIGGERS FOR LOAD OF oe-boll.
DISABLE TRIGGERS FOR LOAD OF oe-bolh.

DEF INPUT PARAMETER iprBolhRow AS ROWID NO-UNDO.
DEF INPUT PARAMETER iplFreightCostCalc AS LOGICAL NO-UNDO.
DEF OUTPUT PARAMETER opdFreight AS DECIMAL DECIMALS 6 NO-UNDO.


DEF VAR dFreight AS DEC DECIMALS 6 NO-UNDO.
DEF VAR v-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS DEC NO-UNDO.
DEF VAR v-frt-chg AS DEC NO-UNDO.
DEF VAR v-del-zone LIKE oe-ordl.del-zone NO-UNDO.
DEF VAR v-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR dTotBasis AS DECIMAL NO-UNDO DECIMALS 10.
DEF VAR tot-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR ldMinRate AS DEC NO-UNDO.
DEF VAR dTotFreight AS DEC NO-UNDO DECIMALS 10.
DEFINE VARIABLE cCustID AS CHARACTER NO-UNDO.
DEFINE VARIABLE dRatePerPallet AS DECIMAL NO-UNDO.
DEFINE VARIABLE cTagDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lForceFreight AS LOGICAL NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCarrier AS CHARACTER NO-UNDO.
DEFINE VARIABLE scInstance AS CLASS system.SharedConfig NO-UNDO.
DEF BUFFER bf-oe-boll FOR oe-boll.


 FIND  oe-bolh WHERE ROWID(oe-bolh) EQ iprBolhRow EXCLUSIVE-LOCK NO-ERROR.
 IF NOT AVAIL oe-bolh THEN
   RETURN.
   
 ASSIGN 
      scInstance  = SharedConfig:instance
      cLocation   =  STRING(scInstance:GetValue("BolScreenValueOfLocation")) 
      cCarrier    =  STRING(scInstance:GetValue("BolScreenValueOfCarrier")) NO-ERROR.  
        
 ASSIGN oe-bolh.tot-pallets = 0
        dTotFreight         = 0
        tot-other-freight   = 0 .
 IF cLocation EQ "" THEN
    ASSIGN
        cLocation           = oe-bolh.loc
        cCarrier            = oe-bolh.carrier.
           
IF oe-bolh.cwt NE 0 THEN
DO:
  lForceFreight = YES.
  opdFreight =  oe-bolh.cwt / 100 * oe-bolh.tot-wt . 
  cTagDescription = "Freight cost forced at $" + string(oe-bolh.cwt) + "/ 100 lbs x " + string(oe-bolh.tot-wt) + " lbs = $" + string(opdFreight). 
END.

IF NOT lForceFreight THEN
DO:    

 /* Obtain total basis weight for all lines on the BOL */
 FOR EACH bf-oe-boll
     WHERE bf-oe-boll.company EQ oe-bolh.company
       AND bf-oe-boll.b-no    EQ oe-bolh.b-no:
       
       cCustID = oe-bolh.cust-no.
       RUN pGetCustID(BUFFER bf-oe-boll, INPUT-OUTPUT cCustID).
   RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
               INPUT cCustID,
               INPUT oe-bolh.ship-id,
               INPUT cCarrier,
               INPUT rowid(bf-oe-boll), 
               OUTPUT v-other-freight). 
   tot-other-freight = tot-other-freight + v-other-freight.
 END.


 /* Obtain the total freight for all lines on BOL */
 FOR EACH bf-oe-boll
     WHERE bf-oe-boll.company EQ oe-bolh.company
       AND bf-oe-boll.b-no    EQ oe-bolh.b-no:
       cCustID = oe-bolh.cust-no.
       RUN pGetCustID(BUFFER bf-oe-boll, INPUT-OUTPUT cCustID).
   FIND FIRST shipto
         WHERE shipto.company EQ oe-bolh.company
           AND shipto.cust-no EQ cCustID
           AND shipto.ship-id EQ oe-bolh.ship-id
         NO-LOCK NO-ERROR.
   IF NOT AVAIL shipto THEN
     NEXT.

   RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
               INPUT cCustID,
               INPUT oe-bolh.ship-id,
               INPUT cCarrier,
               INPUT rowid(bf-oe-boll), 
               OUTPUT v-other-freight). 
  
  FIND FIRST ITEMfg WHERE itemfg.company = oe-bolh.company
      AND itemfg.i-no EQ bf-oe-boll.i-no
    NO-LOCK NO-ERROR.
  v-del-zone = shipto.dest-code.
  /* Indicates to always copy BOL freight class from item */
  find first sys-ctrl where sys-ctrl.company eq oe-bolh.company
                        and sys-ctrl.name    eq "BOLFreight" 
                      no-lock no-error.
  IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "FGFreightClass" THEN DO:
    IF AVAIL(itemfg) AND itemfg.frt-class GT "" THEN DO:
      FIND FIRST carr-mtx WHERE carr-mtx.company  EQ oe-bolh.company
                            AND carr-mtx.loc      EQ shipto.loc
                            AND carr-mtx.carrier  EQ cCarrier
                            AND carr-mtx.del-zone EQ itemfg.frt-class
                          NO-LOCK NO-ERROR.
      IF AVAIL carr-mtx THEN
        ASSIGN v-del-zone = itemfg.frt-class.    
    END.
  END.


   RUN oe/getLineFrt.p (oe-bolh.company, 
                        cLocation, 
                        cCarrier, 
                        v-del-zone,
                        shipto.ship-zip, 
                        v-other-freight, 
                        tot-other-freight, 
                        1, 
                        OUTPUT dFreight, 
                        OUTPUT ldMinRate,
                        OUTPUT dRatePerPallet).
   
   IF iplFreightCostCalc THEN
   ASSIGN bf-oe-boll.freight = dFreight. 
   
          dTotFreight = dTotFreight + dFreight.
          dTotBasis = dTotBasis + v-other-freight.
          oe-bolh.tot-pallets = oe-bolh.tot-pallets + bf-oe-boll.tot-pallets.
 END. /* each oe-boll */

 /* If the total freight calculated is below minimum, distribute minimum to lines */
 IF dTotFreight LT ldMinRate THEN DO:

   dTotFreight = ldMinRate.

   /* distribute total to lines */
   FOR EACH bf-oe-boll
       WHERE bf-oe-boll.company EQ oe-bolh.company
         AND bf-oe-boll.b-no    EQ oe-bolh.b-no
       EXCLUSIVE-LOCK:
       
       cCustID = oe-bolh.cust-no.
       RUN pGetCustID(BUFFER bf-oe-boll, INPUT-OUTPUT cCustID).
     RUN oe/getLineFrtBasis.p (INPUT oe-bolh.company,
               INPUT cCustID,
               INPUT oe-bolh.ship-id,
               INPUT cCarrier,
               INPUT rowid(bf-oe-boll), 
               OUTPUT v-other-freight). 
 
     /* line freight is total / (basis  / total basis */
     IF iplFreightCostCalc THEN
     bf-oe-boll.freight = dTotFreight * v-other-freight / dTotBasis.

   END. /* each bol */

 END. /* if below minimum */

 /*oe-bolh.freight = dTotFreight.*/ /* task NO 5051503 */
 oe-bolh.freightCalculationAmount = dTotFreight.
 FIND CURRENT oe-bolh NO-LOCK.
 opdFreight = dTotFreight.

 
 FIND FIRST carrier NO-LOCK
      WHERE carrier.company EQ oe-bolh.company
      AND carrier.carrier EQ cCarrier
      AND carrier.loc EQ cLocation NO-ERROR .
 IF AVAIL carrier THEN
 DO:    
   IF carrier.chg-method EQ "P" THEN
   cTagDescription = "Loc = " + cLocation + ", Carrier = Pallet, Zone = " + v-del-zone + ", Pallet = " +  string(oe-bolh.tot-pallet) + " @ $" + string(dRatePerPallet) + "/Pallet".
   ELSE IF carrier.chg-method EQ "W" THEN
   cTagDescription = "Loc = " + cLocation + ", Carrier = Weight, Zone = " + v-del-zone + ", Weight = " +  string(oe-bolh.tot-wt) + " @ $" + string(dRatePerPallet) + "/100 Lbs".
   ELSE IF carrier.chg-method EQ "M" THEN
   cTagDescription = "Loc = " + cLocation + ", Carrier = Msf, Zone = " + v-del-zone + ", MSF = @ $" + string(dRatePerPallet) + "/Msf".
   
   IF dTotFreight LT ldMinRate THEN 
    cTagDescription = cTagDescription + ", Minimum applied @ $" + STRING(ldMinRate) .
 END.  
 ELSE cTagDescription = "Carrier not found".
END.
ELSE DO:

 IF iplFreightCostCalc THEN
 RUN oe/bolfrteq.p (BUFFER oe-bolh, opdFreight, 0). 
 
 oe-bolh.freightCalculationAmount = opdFreight.   
 /* Obtain the total freight for all lines on BOL */
 FOR EACH bf-oe-boll
     WHERE bf-oe-boll.company EQ oe-bolh.company
       AND bf-oe-boll.b-no    EQ oe-bolh.b-no:
          oe-bolh.tot-pallets = oe-bolh.tot-pallets + bf-oe-boll.tot-pallets.           
 END. /* each oe-boll */
END.
      
   IF iplFreightCostCalc AND opdFreight GT 0 THEN
   DO:
       RUN ClearTagsForGroup(oe-bolh.rec_key,"FreightCost" ).  /*Clear all hold tags - TagProcs.p*/
       RUN AddTagInfoForGroup(
           INPUT oe-bolh.rec_key,
           INPUT "oe-bolh",
           INPUT cTagDescription,
           INPUT "",
           INPUT "FreightCost"
           ). /*From TagProcs Super Proc*/      
   END.
      
   RUN ClearTagsForGroup(oe-bolh.rec_key , "CalcFreight").  /*Clear all hold tags - TagProcs.p*/  
   RUN AddTagInfoForGroup(
    INPUT oe-bolh.rec_key,
    INPUT "oe-bolh",
    INPUT cTagDescription,
    INPUT "",
    INPUT "CalcFreight"
    ). /*From TagProcs Super Proc*/   
  



/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetCustID:
/*------------------------------------------------------------------------------
 Purpose: Returns the appropriate cust ID given an oe-boll buffer
 Notes:
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-oe-boll FOR oe-boll.
DEFINE INPUT-OUTPUT PARAMETER iopcCustID AS CHARACTER NO-UNDO.

IF ipbf-oe-boll.s-code EQ 'T' THEN DO:
    FIND FIRST cust NO-LOCK 
        WHERE cust.company EQ ipbf-oe-boll.company
        AND cust.active EQ "X"
        NO-ERROR.
    IF AVAILABLE cust 
        THEN iopcCustID = cust.cust-no.
 
END.

END PROCEDURE.
