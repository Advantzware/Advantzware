/* -------------------------------------------------- oe/oe-bolp5.i 02/98 JLF */
/* Bill Of Lading and Invoice Posting - CUST Warehouse Process                */
/* -------------------------------------------------------------------------- */

DEF BUFFER b-fg-bin-{2} FOR fg-bin.                              
                              
DEF VAR li-{2} AS INT NO-UNDO.
DEF VAR lv-{2}-cust-no LIKE oe-bolh.cust-no NO-UNDO.
DEF VAR ll-{2}-set-hdr AS LOG NO-UNDO.
DEF VAR lv-{2}-save-bin AS ROWID NO-UNDO.
DEF VAR ll-{2}-found-first AS LOG NO-UNDO.

ll-{2}-set-hdr = AVAIL oe-ordl AND CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}).
lv-{2}-save-bin = ?.
RELEASE fg-bin.
/* CREATE CUSTOMER WAREHOUSE FG-BIN RECORDS FOR CUSTOMER QUANTITY */
IF tt-boll.s-code EQ "I" THEN DO:
  /* find fg-bin, create fg-rdtlh, fg-rcpth */
  {oe/custbin1.i "{1}" "{2}" "v-bol-qty" "v-partial"}
END.

lv-{2}-cust-no = IF tt-boll.s-code EQ "I" THEN "" ELSE tt-bolh.cust-no.
ll-{2}-found-first = NO.
DO li-{2} = 1 TO 5:
  /* Find fg-bin of tt-boll, but don't pick up any customer owned placeholder records */
  /* i.e. where the loc-bin is blank                                                  */
  RELEASE fg-bin.

  NEXT-BIN:
  DO WHILE v-bol-qty GT 0:
    IF li-{2} EQ 1 AND tt-boll.s-code EQ "S" AND tt-boll.tag NE "" THEN DO:
        
        /* WFK - cust-no should not have to match */
        FIND NEXT fg-bin
            WHERE fg-bin.company EQ tt-bolh.company
              AND fg-bin.i-no    EQ tt-boll.i-no
              AND fg-bin.job-no  EQ tt-boll.job-no
              AND fg-bin.job-no2 EQ tt-boll.job-no2
              AND fg-bin.loc     EQ tt-boll.loc
              AND fg-bin.loc-bin EQ tt-boll.loc-bin
              AND fg-bin.tag     EQ tt-boll.tag
              AND (fg-bin.cust-no EQ tt-bolh.cust-no or fg-bin.cust-no eq "")
            NO-ERROR.

    END.

    ELSE 
    IF li-{2} EQ 2 AND tt-boll.s-code EQ "S" THEN DO:
  
      FIND NEXT fg-bin
          WHERE fg-bin.company EQ tt-bolh.company
            AND fg-bin.i-no    EQ tt-boll.i-no
            AND fg-bin.job-no  EQ tt-boll.job-no
            AND fg-bin.job-no2 EQ tt-boll.job-no2
            AND fg-bin.cust-no EQ tt-bolh.cust-no
            AND fg-bin.loc-bin GT ""
          NO-ERROR.
        IF NOT AVAIL fg-bin AND NOT ll-{2}-found-first THEN DO:
          /* Start at top with a find first if not found, but */
          /* do this only once to prevent infinite loop       */
          FIND FIRST fg-bin
              WHERE fg-bin.company EQ tt-bolh.company
                AND fg-bin.i-no    EQ tt-boll.i-no
                AND fg-bin.job-no  EQ tt-boll.job-no
                AND fg-bin.job-no2 EQ tt-boll.job-no2
                AND fg-bin.cust-no EQ ""
                AND fg-bin.loc-bin GT ""
              NO-ERROR.
          ll-{2}-found-first = YES.
        END.
  
    END.
    IF li-{2} EQ 3 AND tt-boll.tag NE "" THEN
    FIND NEXT fg-bin
        WHERE fg-bin.company EQ tt-bolh.company
          AND fg-bin.job-no  EQ tt-boll.job-no
          AND fg-bin.job-no2 EQ tt-boll.job-no2
          AND fg-bin.i-no    EQ tt-boll.i-no
          AND fg-bin.tag     EQ tt-boll.tag
          AND fg-bin.cust-no EQ lv-{2}-cust-no
          AND fg-bin.loc-bin GT ""
        USE-INDEX job NO-ERROR.

    ELSE
    IF li-{2} EQ 4 THEN
    FIND NEXT fg-bin
        WHERE fg-bin.company EQ tt-bolh.company
          AND fg-bin.job-no  EQ tt-boll.job-no
          AND fg-bin.job-no2 EQ tt-boll.job-no2
          AND fg-bin.i-no    EQ tt-boll.i-no
          AND fg-bin.cust-no EQ lv-{2}-cust-no
          AND fg-bin.loc-bin GT ""
        USE-INDEX job NO-ERROR.

    ELSE
    IF li-{2} EQ 5 AND itemfg.pur-man THEN
    FIND NEXT fg-bin
        WHERE fg-bin.company EQ tt-bolh.company
          AND fg-bin.i-no    EQ tt-boll.i-no
          AND fg-bin.tag     EQ tt-boll.tag
          AND fg-bin.cust-no EQ lv-{2}-cust-no
          AND fg-bin.loc-bin GT ""
        NO-ERROR.

    /* Save so that remain on the same record for subsequent find next */
    IF AVAIL(fg-bin) THEN
      lv-{2}-save-bin = ROWID(fg-bin).
    else
      lv-{2}-save-bin = ?.
      
    IF AVAIL fg-bin AND tt-boll.s-code NE "I" THEN DO:
     
      ASSIGN
       lv-qty     = MIN(v-bol-qty,fg-bin.qty)
       lv-partial = MIN(v-partial,fg-bin.partial-count)
       li-stupid  = lv-partial.  /* This is really stupid, but it must be done */
      
      if lv-qty NE 0 then do:
          /* WFK - no reason to do this if the quantity is zero */
          /* Create  fg-rcpth fg-rdtlh */
          {oe/shiphist.i "{1}" "{2}" "lv-qty"}
      end.
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
       
      /* If a non-customer owned fg-bin records was found, reduce fg-bin.qty by lv-qty */
      IF lv-{2}-save-bin NE ? THEN DO:
              
        FIND fg-bin WHERE ROWID(fg-bin) EQ lv-{2}-save-bin EXCLUSIVE-LOCK NO-ERROR.
        
        IF AVAIL fg-bin THEN DO:
       
               
          ASSIGN
           v-bol-qty            = v-bol-qty - lv-qty
           fg-bin.qty           = fg-bin.qty - lv-qty
           v-partial            = v-partial - lv-partial
           fg-bin.partial-count = fg-bin.partial-count - lv-partial.
           
           /* Delete zero bins unless FgKeepZeroBin is selected */
           IF NOT FgKeepZeroBin-log AND fg-bin.qty EQ 0 AND fg-bin.partial-count EQ 0 THEN 
              DELETE fg-bin.
              
        END. /* avail fg-bin */
      END. /* if save bin is not blank */


      /*removed for task 09241404 - reason is that there is now a 0 bin filter 
      and the goal is to preserve original bin cost*/
      /*       IF fg-bin.qty LE 0 THEN DELETE fg-bin. */
      
      IF lv-{2}-save-bin NE ? THEN 
        FIND fg-bin WHERE ROWID(fg-bin) EQ lv-{2}-save-bin EXCLUSIVE-LOCK NO-ERROR.

    END. /* avail bin and s-code ne I */

    
    /* If fg-bin was found, go on to next iteration and find next */
    IF NOT AVAIL fg-bin THEN
       LEAVE NEXT-BIN.
              

  END. /* Do while v-bol-qty gt 0 */


END. /* Do li 1 to 5 */

/* end ---------------------------------- copr. 1999  advanced software, inc. */
