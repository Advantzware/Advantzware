/*----------------------------------------------------oe/pallcalc.p 05/05 JLF */
/* Calculate the number of pallets needed.                                    */
/*----------------------------------------------------------------------------*/

DEF INPUT  PARAM ip-rowid   AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-pallets AS INT NO-UNDO.

DEF VAR v-dec AS DEC NO-UNDO.
DEF VAR v-int AS DEC NO-UNDO.
DEF VAR v-cases-unit AS INT INIT 1 NO-UNDO.
DEF VAR v-units-pallet AS INT INIT 1 NO-UNDO.

FIND oe-boll WHERE ROWID(oe-boll) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-boll THEN DO:
  v-dec = 1.

  FIND FIRST fg-bin
      WHERE fg-bin.company EQ oe-boll.company
        AND fg-bin.i-no    EQ oe-boll.i-no
        AND fg-bin.job-no  EQ oe-boll.job-no
        AND fg-bin.job-no2 EQ oe-boll.job-no2
        AND fg-bin.loc     EQ oe-boll.loc
        AND fg-bin.loc-bin EQ oe-boll.loc-bin
        AND fg-bin.tag     EQ oe-boll.tag
        AND fg-bin.cust-no EQ oe-boll.cust-no
      NO-LOCK NO-ERROR.

  IF AVAIL fg-bin THEN
     ASSIGN
        v-cases-unit = fg-bin.cases-unit
        v-units-pallet = fg-bin.units-pallet.
  ELSE
  DO:
     FOR EACH fg-rcpth FIELDS(r-no rita-code) WHERE
         fg-rcpth.company EQ oe-boll.company AND
         fg-rcpth.i-no EQ oe-boll.i-no AND
         fg-rcpth.rita-code EQ "R"
         NO-LOCK,
         FIRST fg-rdtlh FIELDS(stacks-unit units-pallet) WHERE
               fg-rdtlh.r-no EQ fg-rcpth.r-no AND
               fg-rdtlh.rita-code EQ fg-rcpth.rita-code
               NO-LOCK
               BY fg-rcpth.r-no DESC:

         ASSIGN
            v-cases-unit = fg-rdtlh.stacks-unit
            v-units-pallet = fg-rdtlh.units-pallet.

         LEAVE.
     END.
  END.

  v-dec = v-dec  *
          (IF v-cases-unit   EQ 0 THEN 1 ELSE v-cases-unit)   *
          (IF v-units-pallet EQ 0 THEN 1 ELSE v-units-pallet).
  
  IF v-dec LE 1 THEN DO:
     v-int = INT(oe-boll.partial NE 0).

     IF oe-boll.qty LT 0 THEN v-int = v-int * -1.
  END.

  ELSE v-int = 0.

  v-dec = (oe-boll.cases + v-int) / v-dec.
     
  {sys/inc/roundup.i v-dec}

  IF v-dec LT 0 THEN v-dec = v-dec * -1.

  op-pallets = v-dec.
END.
