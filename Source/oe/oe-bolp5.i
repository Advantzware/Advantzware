/* -------------------------------------------------- oe/oe-bolp5.i 02/98 JLF */
/* Bill Of Lading and Invoice Posting - CUST Warehouse Process                */
/* -------------------------------------------------------------------------- */

DEF BUFFER b-fg-bin-{2} FOR fg-bin.                              
                              
DEF VAR li-{2} AS INT NO-UNDO.
DEF VAR lv-{2}-cust-no LIKE oe-bolh.cust-no NO-UNDO.
DEF VAR ll-{2}-set-hdr AS LOG NO-UNDO.
DEF VAR lv-{2}-save-bin AS ROWID NO-UNDO.

ll-{2}-set-hdr = AVAIL oe-ordl AND CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}).
lv-{2}-save-bin = ?.
RELEASE fg-bin.
/* CREATE CUSTOMER WAREHOUSE FG-BIN RECORDS FOR CUSTOMER QUANTITY */
IF tt-boll.s-code EQ "I" THEN DO:
  /* find fg-bin, create fg-rdtlh, fg-rcpth */
  {oe/custbin1.i "{1}" "{2}" "v-bol-qty" "v-partial"}
END.

lv-{2}-cust-no = IF tt-boll.s-code EQ "I" THEN "" ELSE tt-bolh.cust-no.

DO li-{2} = 1 TO 5:
  RELEASE fg-bin.

  NEXT-BIN:
  DO WHILE v-bol-qty GT 0:
    IF li-{2} EQ 1 AND tt-boll.s-code EQ "S" AND tt-boll.tag NE "" THEN DO:
        FIND NEXT fg-bin
            WHERE fg-bin.company EQ tt-bolh.company
              AND fg-bin.i-no    EQ tt-boll.i-no
              AND fg-bin.job-no  EQ tt-boll.job-no
              AND fg-bin.job-no2 EQ tt-boll.job-no2
              AND fg-bin.loc     EQ tt-boll.loc
              AND fg-bin.loc-bin EQ tt-boll.loc-bin
              AND fg-bin.tag     EQ tt-boll.tag
              AND fg-bin.cust-no EQ tt-bolh.cust-no
            NO-ERROR.
    END.

    ELSE
    IF li-{2} EQ 2 AND tt-boll.s-code EQ "S" THEN
    FIND NEXT fg-bin
        WHERE fg-bin.company EQ tt-bolh.company
          AND fg-bin.i-no    EQ tt-boll.i-no
          AND fg-bin.job-no  EQ tt-boll.job-no
          AND fg-bin.job-no2 EQ tt-boll.job-no2
          AND fg-bin.cust-no EQ tt-bolh.cust-no
        NO-ERROR.

    IF li-{2} EQ 3 AND tt-boll.tag NE "" THEN
    FIND NEXT fg-bin
        WHERE fg-bin.company EQ tt-bolh.company
          AND fg-bin.job-no  EQ tt-boll.job-no
          AND fg-bin.job-no2 EQ tt-boll.job-no2
          AND fg-bin.i-no    EQ tt-boll.i-no
          AND fg-bin.tag     EQ tt-boll.tag
          AND fg-bin.cust-no EQ lv-{2}-cust-no
        USE-INDEX job NO-ERROR.

    ELSE
    IF li-{2} EQ 4 THEN
    FIND NEXT fg-bin
        WHERE fg-bin.company EQ tt-bolh.company
          AND fg-bin.job-no  EQ tt-boll.job-no
          AND fg-bin.job-no2 EQ tt-boll.job-no2
          AND fg-bin.i-no    EQ tt-boll.i-no
          AND fg-bin.cust-no EQ lv-{2}-cust-no
        USE-INDEX job NO-ERROR.

    ELSE
    IF li-{2} EQ 5 AND itemfg.pur-man THEN
    FIND NEXT fg-bin
        WHERE fg-bin.company EQ tt-bolh.company
          AND fg-bin.i-no    EQ tt-boll.i-no
          AND fg-bin.tag     EQ tt-boll.tag
          AND fg-bin.cust-no EQ lv-{2}-cust-no
        NO-ERROR.

    /* Save so that remain on the same record for subsequent find next */
    IF AVAIL(fg-bin) THEN
      lv-{2}-save-bin = ROWID(fg-bin).

    IF AVAIL fg-bin AND tt-boll.s-code NE "I" THEN DO:
      ASSIGN
       lv-qty     = MIN(v-bol-qty,fg-bin.qty)
       lv-partial = MIN(v-partial,fg-bin.partial-count)
       li-stupid  = lv-partial.  /* This is really stupid, but it must be done */

      /* Create  fg-rcpth fg-rdtlh */
      {oe/shiphist.i "{1}" "{2}" "lv-qty"}

      FIND FIRST itemfg WHERE itemfg.company EQ tt-boll.company
                          AND itemfg.i-no = tt-boll.i-no
                        NO-LOCK NO-ERROR.
      IF AVAIL itemfg THEN
          FIND FIRST fg-set WHERE fg-set.company EQ itemfg.company
                             AND fg-set.part-no EQ itemfg.i-no
                           NO-LOCK NO-ERROR.
    
      IF (tt-boll.s-code = "S" AND NOT 
           (AVAIL(fg-set) 
             AND 
            CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company EQ tt-boll.company
                                              AND oe-ordl.ord-no  EQ tt-boll.ord-no
                                              AND oe-ordl.i-no    EQ tt-boll.i-no
                                              AND oe-ordl.LINE    EQ tt-boll.LINE
                                              AND oe-ordl.is-a-component))) THEN DO:
  
         
        /* Reduce placeholder customer fg-bin record created for invoice-only */
        /* find fg-bin, create fg-rdtlh, fg-rcpth */
        {oe/custbin2.i "{1}" "{2}" "- lv-qty" "- v-partial" "-"}
  
        /* Reduce negative placeholder customer fg-bin record created for invoice-only */
        /* find fg-bin, create fg-rdtlh, fg-rcpth */
        {oe/custbin2.i "{1}" "{2}" "lv-qty" "v-partial" "+"}
  
      END. /* s-code is S and not SET */

      ASSIGN
       v-bol-qty            = v-bol-qty - lv-qty
       fg-bin.qty           = fg-bin.qty - lv-qty
       v-partial            = v-partial - lv-partial
       fg-bin.partial-count = fg-bin.partial-count - lv-partial.

/*removed for task 09241404 - reason is that there is now a 0 bin filter 
and the goal is to preserve original bin cost*/
/*       IF fg-bin.qty LE 0 THEN DELETE fg-bin. */
      
      IF lv-{2}-save-bin NE ? THEN
        FIND fg-bin WHERE ROWID(fg-bin) EQ lv-{2}-save-bin NO-LOCK NO-ERROR.
    END. /* avail bin and s-code ne I */

    ELSE LEAVE.
  END. /* Do while v-bol-qty gt 0 */
END. /* Do li 1 to 5 */


/* end ---------------------------------- copr. 1999  advanced software, inc. */
