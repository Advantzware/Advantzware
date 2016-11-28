
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM v-term LIKE report.term-id NO-UNDO.

{oe/closchk.i}

DEF BUFFER b-fg-bin FOR fg-bin.
DEF BUFFER b-oe-boll FOR oe-boll.

DEF VAR li AS INT NO-UNDO.
DEF VAR v-tag2 AS CHAR NO-UNDO.

FOR EACH oe-boll WHERE ROWID(oe-boll) EQ ip-rowid,
    FIRST oe-bolh
    WHERE oe-bolh.b-no EQ oe-boll.b-no
      AND CAN-FIND(FIRST b-oe-boll
                   WHERE b-oe-boll.b-no EQ oe-bolh.b-no
                     AND b-oe-boll.qty  NE 0),
    FIRST cust NO-LOCK
    WHERE cust.company EQ oe-bolh.company
      AND cust.cust-no EQ oe-bolh.cust-no,
    FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ oe-boll.company,
    FIRST oe-ord NO-LOCK
    WHERE oe-ord.company EQ oe-boll.company
      AND oe-ord.ord-no  EQ oe-boll.ord-no,
    FIRST oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ oe-boll.company
      AND oe-ordl.ord-no  EQ oe-boll.ord-no
      AND oe-ordl.line    EQ oe-boll.line
      AND oe-ordl.i-no    EQ oe-boll.i-no
    USE-INDEX ord-no,
    FIRST itemfg NO-LOCK
    WHERE itemfg.company EQ oe-boll.company
      AND itemfg.i-no    EQ oe-boll.i-no:

  RUN oe/custxship.p (oe-bolh.company,
                      oe-bolh.cust-no,
                      oe-bolh.ship-id,
                      BUFFER shipto).

  IF oe-ord.type EQ "T" OR oe-boll.s-code EQ "T" THEN DO: /* Process in-house transfer */
    IF AVAIL shipto AND CAN-FIND(FIRST fg-bin
                                 WHERE fg-bin.company EQ shipto.company
                                   AND fg-bin.i-no    EQ ""
                                   AND fg-bin.loc     EQ shipto.loc
                                   AND fg-bin.loc-bin EQ shipto.loc-bin) THEN DO:

      FIND FIRST sys-ctrl WHERE
           sys-ctrl.company EQ oe-bolh.company AND
           sys-ctrl.NAME EQ "BOLPOST"
           NO-LOCK NO-ERROR.

      IF AVAIL sys-ctrl AND sys-ctrl.int-fld EQ 0 THEN
      DO:
         li = 0.
        
         FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
         IF AVAIL fg-rctd AND fg-rctd.r-no GT li THEN li = fg-rctd.r-no.
        
         FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
         IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
                 
         CREATE fg-rctd.
         ASSIGN
          fg-rctd.r-no      = li + 1
          fg-rctd.company   = oe-boll.company
          fg-rctd.bol-no    = oe-bolh.bol-no
          fg-rctd.rct-date  = oe-bolh.bol-date
          fg-rctd.trans-time = TIME
          fg-rctd.i-no      = oe-boll.i-no
          fg-rctd.rita-code = "T"
          fg-rctd.job-no    = oe-boll.job-no
          fg-rctd.job-no2   = oe-boll.job-no2
          fg-rctd.loc       = oe-boll.loc
          fg-rctd.loc-bin   = oe-boll.loc-bin
          fg-rctd.tag       = oe-boll.tag
          fg-rctd.cust-no   = oe-boll.cust-no
          fg-rctd.partial   = oe-boll.partial
          fg-rctd.cases     = oe-boll.cases
          fg-rctd.qty-case  = oe-boll.qty-case
          fg-rctd.t-qty     = oe-boll.qty
          fg-rctd.loc2      = shipto.loc
          fg-rctd.loc-bin2  = shipto.loc-bin
          fg-rctd.tag2      = fg-rctd.tag.
      END. /* IF AVAIL sys-ctrl */

      FIND FIRST fg-bin NO-LOCK      /* Make sure we have a bin to relieve */
          WHERE fg-bin.company  EQ oe-boll.company
            AND fg-bin.i-no     EQ oe-boll.i-no
            AND fg-bin.job-no   EQ oe-boll.job-no
            AND fg-bin.job-no2  EQ oe-boll.job-no2
            AND fg-bin.loc      EQ oe-boll.loc
            AND fg-bin.loc-bin  EQ oe-boll.loc-bin
            AND fg-bin.tag      EQ oe-boll.tag
            AND fg-bin.cust-no  EQ oe-boll.cust-no
          NO-ERROR.

      IF NOT AVAIL fg-bin THEN DO:
        CREATE fg-bin.
        ASSIGN
         fg-bin.company      = oe-boll.company
         fg-bin.i-no         = oe-boll.i-no
         fg-bin.job-no       = oe-boll.job-no
         fg-bin.job-no2      = oe-boll.job-no2
         fg-bin.loc          = oe-boll.loc
         fg-bin.loc-bin      = oe-boll.loc-bin
         fg-bin.tag          = oe-boll.tag
         fg-bin.cust-no      = oe-boll.cust-no
         fg-bin.case-count   = oe-boll.qty-case
         fg-bin.pur-uom      = itemfg.prod-uom
         fg-bin.std-tot-cost = itemfg.std-tot-cost
         fg-bin.std-mat-cost = itemfg.std-mat-cost
         fg-bin.std-lab-cost = itemfg.std-lab-cost
         fg-bin.std-var-cost = itemfg.std-var-cost
         fg-bin.std-fix-cost = itemfg.std-fix-cost.
      END. /* IF NOT AVAIL fg-bin */
             
      v-tag2 = oe-boll.tag.

      IF AVAIL fg-rctd AND
         fg-bin.qty GT oe-boll.qty AND
         fg-bin.tag NE ""            AND
         fg-bin.tag EQ oe-boll.tag  
         AND AVAIL sys-ctrl AND sys-ctrl.int-fld EQ 0 THEN /*11181321 - BOLPOST int field = 1 should not create loadtag*/ 
         DO:
            RUN fg/mkloadtg.p (ROWID(fg-rctd), 0, INPUT-OUTPUT v-tag2).
            fg-rctd.tag2 = v-tag2.
         END.
      ELSE
         IF NOT AVAIL fg-rctd AND
            fg-bin.qty GT oe-boll.qty AND
            fg-bin.tag NE "" AND
            fg-bin.tag EQ oe-boll.tag  
            AND AVAIL sys-ctrl AND sys-ctrl.int-fld EQ 0 THEN /*11181321 - BOLPOST int field = 1 should not create loadtag*/ 
            RUN fg/mkloadtg2.p (ROWID(oe-boll), shipto.loc, shipto.loc-bin, 0, INPUT-OUTPUT v-tag2).
            

      IF fg-bin.loc     NE shipto.loc     OR
         fg-bin.loc-bin NE shipto.loc-bin OR
         fg-bin.tag     NE v-tag2 OR
         v-tag2 EQ "" THEN DO:
          RELEASE fg-bin .
         FIND FIRST b-fg-bin NO-LOCK     /* Make sure we have a bin to receive */
             WHERE b-fg-bin.company EQ oe-boll.company
               AND b-fg-bin.i-no    EQ oe-boll.i-no
               AND trim(b-fg-bin.job-no)  EQ trim(oe-boll.job-no)
               AND b-fg-bin.job-no2 EQ oe-boll.job-no2
               AND b-fg-bin.loc     EQ trim(shipto.loc)
               AND b-fg-bin.loc-bin EQ trim(shipto.loc-bin)
               AND b-fg-bin.tag     EQ v-tag2
               AND b-fg-bin.cust-no EQ oe-boll.cust-no
             NO-ERROR.
        
         IF NOT AVAIL b-fg-bin THEN DO:
            
           CREATE b-fg-bin.
           ASSIGN
            b-fg-bin.company      = oe-boll.company
            b-fg-bin.i-no         = oe-boll.i-no
            b-fg-bin.job-no       = oe-boll.job-no
            b-fg-bin.job-no2      = oe-boll.job-no2
            b-fg-bin.loc          = shipto.loc
            b-fg-bin.loc-bin      = shipto.loc-bin
            b-fg-bin.tag          = v-tag2
            b-fg-bin.cust-no      = oe-boll.cust-no
            b-fg-bin.case-count   = oe-boll.qty-case
            b-fg-bin.pur-uom      = /*fg-bin.pur-uom */      itemfg.prod-uom    
            b-fg-bin.std-tot-cost = /*fg-bin.std-tot-cost*/  itemfg.std-tot-cost
            b-fg-bin.std-mat-cost = /*fg-bin.std-mat-cost*/  itemfg.std-mat-cost
            b-fg-bin.std-lab-cost = /*fg-bin.std-lab-cost*/  itemfg.std-lab-cost
            b-fg-bin.std-var-cost = /*fg-bin.std-var-cost*/  itemfg.std-var-cost
            b-fg-bin.std-fix-cost = /*fg-bin.std-fix-cost*/  itemfg.std-fix-cost   .
         END.
           
         IF AVAIL fg-rctd THEN
         DO:
            ASSIGN 
             fg-rctd.pur-uom  = itemfg.prod-uom
             fg-rctd.std-cost = b-fg-bin.std-tot-cost.
           
            IF fg-rctd.pur-uom EQ "EA" THEN
               fg-rctd.ext-cost = fg-rctd.std-cost.
            ELSE
               RUN sys/ref/convcuom.p(fg-rctd.pur-uom, "EA", 0, 0, 0, 0,
                                      fg-rctd.std-cost, OUTPUT fg-rctd.ext-cost).
           
            fg-rctd.ext-cost = fg-rctd.ext-cost * fg-rctd.t-qty.

            RELEASE fg-rctd.
         END.
        
         ASSIGN
           oe-bolh.posted = YES
           oe-boll.posted = YES.
      END.  /* IF fg-bin.loc     NE shipto.loc */

      ELSE
         IF AVAIL fg-rctd THEN
            DELETE fg-rctd.
    END. /* IF AVAIL shipto AND CAN-FIND(FIRST fg-bin */

    /* to close order (do not close order for transfers) */
    IF oe-boll.s-code <> "T" THEN DO:
        CREATE w-ord.
        ASSIGN
         w-ord.ord-no = oe-ordl.ord-no
         w-ord.rec-id = RECID(oe-ord).
    END.
  END. /*  IF oe-ord.type EQ "T" OR oe-boll.s-code EQ "T" */

  {oe/seq-bolh.i}
END.
