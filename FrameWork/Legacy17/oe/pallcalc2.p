/*----------------------------------------------------oe/pallcalc2.p  */
/* Calculate the number of pallets needed.                                    */
/*----------------------------------------------------------------------------*/

  DEF INPUT PARAM ip-company AS CHAR NO-UNDO.
  DEF INPUT PARAM ip-i-no AS CHAR NO-UNDO.
  DEF INPUT PARAM ip-job-no AS CHAR NO-UNDO.
  DEF INPUT PARAM ip-job-no2 AS INT NO-UNDO.
  DEF INPUT PARAM ip-loc AS CHAR NO-UNDO.
  DEF INPUT PARAM ip-loc-bin AS CHAR NO-UNDO.
  DEF INPUT PARAM ip-tag AS CHAR NO-UNDO.
  DEF INPUT PARAM ip-cust-no AS CHAR NO-UNDO.
  DEF INPUT PARAM ip-partial AS INT NO-UNDO.
  DEF INPUT PARAM ip-qty AS INT NO-UNDO.
  DEF INPUT PARAM ip-cases AS INT NO-UNDO.

  DEF OUTPUT PARAM op-pallets AS INT NO-UNDO.

  DEF VAR v-dec AS DEC NO-UNDO.
  DEF VAR v-int AS DEC NO-UNDO.

  v-dec = 1.

  FIND FIRST fg-bin
      WHERE fg-bin.company EQ ip-company
        AND fg-bin.i-no    EQ ip-i-no
        AND fg-bin.job-no  EQ ip-job-no
        AND fg-bin.job-no2 EQ ip-job-no2
        AND fg-bin.loc     EQ ip-loc
        AND fg-bin.loc-bin EQ ip-loc-bin
        AND fg-bin.tag     EQ ip-tag
        AND fg-bin.cust-no EQ ip-cust-no
      NO-LOCK NO-ERROR. 

  IF AVAIL fg-bin THEN
    v-dec = v-dec                                                         *
            (IF fg-bin.cases-unit   EQ 0 THEN 1 ELSE fg-bin.cases-unit)   *
            (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet).
  
  IF v-dec LE 1 THEN DO:
    v-int = INT(ip-partial NE 0).

    IF ip-qty LT 0 THEN v-int = v-int * -1.
  END.

  ELSE v-int = 0.

  v-dec = (ip-cases + v-int) / v-dec.
     
  {sys/inc/roundup.i v-dec}

  IF v-dec LT 0 THEN v-dec = v-dec * -1.

  op-pallets = v-dec.

