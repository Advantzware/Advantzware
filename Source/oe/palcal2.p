/*-----------------------------------------------------oe/palcalc.p 03/99 RLL-*/
/* Calculate the number of pallets needed.                                    */
/*----------------------------------------------------------------------------*/

DEF INPUT  PARAM ip-rowid   AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-pallets AS INT NO-UNDO.


DEF VAR v-int AS DEC NO-UNDO.
DEFINE VARIABLE hdInventoryProcs AS HANDLE NO-UNDO.
DEFINE VARIABLE dPartial AS DECIMAL NO-UNDO.
DEFINE VARIABLE iPallet AS INTEGER NO-UNDO.
DEFINE VARIABLE dSubUnits AS DECIMAL NO-UNDO.
DEFINE VARIABLE iQuantityPerSubUnit AS INTEGER NO-UNDO.
DEFINE VARIABLE iQuantitySubUnitsPerUnit AS INTEGER NO-UNDO.
RUN inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs.

FUNCTION fgBin RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

FIND oe-bolh WHERE ROWID(oe-bolh) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-bolh THEN
FOR EACH oe-boll
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no:   
 
  iQuantityPerSubUnit = oe-boll.qty-case .
  iQuantitySubUnitsPerUnit = fgbin() .
  RUN RecalcQuantityUnits IN hdInventoryProcs (oe-boll.qty, INPUT-OUTPUT iQuantityPerSubUnit, INPUT-OUTPUT iQuantitySubUnitsPerUnit,
                                             OUTPUT dSubUnits, OUTPUT iPallet, OUTPUT dPartial).
 
  oe-boll.tot-pallets = iPallet + (IF dPartial GT 0 THEN 1 ELSE 0).
  op-pallets = op-pallets + oe-boll.tot-pallets.
END.

IF VALID-HANDLE(hdInventoryProcs) THEN
DELETE PROCEDURE hdInventoryProcs.

FUNCTION fgBin RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iUnitPallet AS INTEGER NO-UNDO .
  FIND FIRST fg-bin NO-LOCK WHERE fg-bin.company EQ oe-boll.company
                              AND fg-bin.job-no EQ oe-boll.job-no
                              AND fg-bin.job-no2 EQ oe-boll.job-no2
                              AND fg-bin.i-no EQ oe-boll.i-no
                              AND fg-bin.loc EQ oe-boll.loc
                              AND fg-bin.loc-bin EQ oe-boll.loc-bin
                              AND fg-bin.tag EQ oe-boll.tag NO-ERROR.

  iUnitPallet = IF AVAILABLE fg-bin THEN fg-bin.cases-unit ELSE 0 .
  IF iUnitPallet EQ 0 THEN DO:
     FIND FIRST oe-ordl NO-LOCK
           WHERE oe-ordl.company EQ oe-boll.company 
             AND oe-ordl.ord-no EQ oe-boll.ord-no 
             AND oe-ordl.i-no EQ oe-boll.i-no NO-ERROR.
      IF AVAIL oe-ordl THEN
          ASSIGN
          iUnitPallet = oe-ordl.cases-unit .
  END.
  RETURN iUnitPallet.

END FUNCTION.

