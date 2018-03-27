DEF VAR li AS INT NO-UNDO.
DEF VAR v-uom LIKE itemfg.prod-uom.
DEF VAR v-qty LIKE oe-boll.qty.
DEF VAR v-cost LIKE ar-invl.cost EXTENT 4.
DEF VAR v-cost-m LIKE ar-invl.cost EXTENT 4.

DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.


SESSION:SET-WAIT-STATE ("general").
    
FOR EACH oe-bolh WHERE oe-bolh.posted EQ YES NO-LOCK TRANSACTION:
  FIND FIRST fg-rcpth WHERE fg-rcpth.b-no EQ oe-bolh.b-no NO-LOCK NO-ERROR.

  IF NOT AVAIL fg-rcpth THEN
  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no
        AND oe-boll.qty     NE 0
      NO-LOCK,
      FIRST itemfg
      WHERE itemfg.company EQ oe-boll.company
        AND itemfg.i-no    EQ oe-boll.i-no
      NO-LOCK:

    li = 0.

    FOR EACH fg-rctd WHERE fg-rctd.company EQ oe-bolh.company NO-LOCK
        BY fg-rctd.r-no DESC:
      li = fg-rctd.r-no.
      LEAVE.
    END.

    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
  
    CREATE fg-rcpth.
    ASSIGN
     fg-rcpth.r-no       = li + 1
     fg-rcpth.b-no       = oe-bolh.b-no
     fg-rcpth.company    = oe-bolh.company
     fg-rcpth.loc        = oe-boll.loc
     fg-rcpth.trans-date = oe-bolh.bol-date
     fg-rcpth.post-date  = oe-bolh.bol-date
     fg-rcpth.po-no      = oe-bolh.po-no
     fg-rcpth.i-no       = itemfg.i-no
     fg-rcpth.i-name     = itemfg.i-name
     fg-rcpth.job-no     = oe-boll.job-no
     fg-rcpth.job-no2    = oe-boll.job-no2
     fg-rcpth.pur-uom    = IF AVAIL itemfg THEN itemfg.pur-uom ELSE "M"
     fg-rcpth.rita-code  = IF oe-boll.s-code EQ "I" THEN "T" ELSE "S".

    CREATE fg-rdtlh.
    ASSIGN
     fg-rdtlh.r-no      = fg-rcpth.r-no
     fg-rdtlh.company   = oe-bolh.company
     fg-rdtlh.loc       = oe-boll.loc
     fg-rdtlh.loc-bin   = oe-boll.loc-bin
     fg-rdtlh.tag       = oe-boll.tag
     fg-rdtlh.qty       = oe-boll.qty * IF oe-boll.s-code EQ "I" THEN -1 ELSE 1
     fg-rdtlh.rita-code = fg-rcpth.rita-code.

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ oe-boll.company
          AND fg-bin.i-no    EQ oe-boll.i-no
          AND fg-bin.loc     EQ oe-boll.loc
          AND fg-bin.loc-bin EQ oe-boll.loc-bin
          AND fg-bin.tag     EQ oe-boll.tag
          AND fg-bin.job-no  EQ oe-boll.job-no
          AND fg-bin.job-no2 EQ oe-boll.job-no2
        NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
      CREATE fg-bin.
      ASSIGN
       fg-bin.company    = oe-boll.company
       fg-bin.i-no       = oe-boll.i-no
       fg-bin.loc        = oe-boll.loc
       fg-bin.loc-bin    = oe-boll.loc-bin
       fg-bin.tag        = oe-boll.tag
       fg-bin.case-count = IF itemfg.case-count NE 0 THEN
                             itemfg.case-count
                           ELSE
                             oe-boll.qty-case
       fg-bin.ord-no     = oe-boll.ord-no
       fg-bin.job-no     = oe-boll.job-no
       fg-bin.job-no2    = oe-boll.job-no2
       fg-bin.pur-uom    = itemfg.prod-uom
       fg-bin.aging-date = TODAY.

      FIND FIRST job-hdr
          WHERE job-hdr.company EQ oe-boll.company
            AND job-hdr.ord-no  EQ oe-boll.ord-no
            AND job-hdr.job-no  EQ oe-boll.job-no
            AND job-hdr.job-no2 EQ oe-boll.job-no2
            AND job-hdr.i-no    EQ oe-boll.i-no
          USE-INDEX ord-no NO-LOCK NO-ERROR.

      ASSIGN
       fg-bin.std-lab-cost = IF AVAIL job-hdr THEN job-hdr.std-lab-cost
                             ELSE itemfg.std-lab-cost
       fg-bin.std-fix-cost = IF AVAIL job-hdr THEN job-hdr.std-fix-cost
                             ELSE itemfg.std-fix-cost
       fg-bin.std-var-cost = IF AVAIL job-hdr then job-hdr.std-var-cost
                             ELSE itemfg.std-var-cost
       fg-bin.std-mat-cost = IF AVAIL job-hdr THEN job-hdr.std-mat-cost
                             ELSE itemfg.std-mat-cost
       fg-bin.std-tot-cost = fg-bin.std-lab-cost + fg-bin.std-fix-cost +
                             fg-bin.std-var-cost + fg-bin.std-mat-cost

       fg-bin.avg-cost  = fg-bin.std-tot-cost
       fg-bin.last-cost = fg-bin.std-tot-cost.
    END.

    ASSIGN
     fg-bin.qty    = fg-bin.qty - oe-boll.qty
     fg-rdtlh.cost = fg-bin.std-tot-cost.
  END.

  FOR EACH ar-invl
      WHERE ar-invl.company EQ oe-bolh.company
        AND ar-invl.bol-no  EQ oe-bolh.bol-no,
      FIRST ar-inv WHERE ar-inv.x-no EQ ar-invl.x-no:

    ASSIGN
     v-cost = 0
     v-qty  = 0.

    FOR EACH oe-boll
        WHERE oe-boll.company eq ar-invl.company
          AND oe-boll.b-no    eq ar-invl.b-no
          AND oe-boll.ord-no  eq ar-invl.ord-no
          AND oe-boll.i-no    eq ar-invl.i-no
          AND oe-boll.po-no   eq ar-invl.po-no
          AND oe-boll.qty     NE 0
        NO-LOCK,
        FIRST fg-bin
        WHERE fg-bin.company EQ oe-bolh.company
          AND fg-bin.i-no    EQ oe-boll.i-no
          AND fg-bin.tag     EQ oe-boll.tag
          AND fg-bin.loc     EQ oe-boll.loc
          AND fg-bin.loc-bin EQ oe-boll.loc-bin
          AND fg-bin.job-no  eq oe-boll.job-no
          AND fg-bin.job-no2 eq oe-boll.job-no2
        NO-LOCK:
                
      ASSIGN
       v-qty       = v-qty + oe-boll.qty
       v-cost-m[1] = fg-bin.std-lab-cost
       v-cost-m[2] = fg-bin.std-fix-cost
       v-cost-m[3] = fg-bin.std-var-cost
       v-cost-m[4] = fg-bin.std-mat-cost
       v-uom       = fg-bin.pur-uom
       cocode      = fg-bin.company.

      DO li = 1 TO 4:
        IF v-uom NE "M" THEN
          RUN sys/ref/convcuom.p(v-uom, "M", 0, 0, 0, 0,
                                 v-cost-m[li], OUTPUT v-cost-m[li]).
                                       
         v-cost[li] = v-cost[li] + (v-cost-m[li] * oe-boll.qty / 1000).
      END.                           
    END.
  
    DO li = 1 TO 4:
      v-cost[li] = v-cost[li] / (v-qty / 1000).
    
      IF v-cost[li] EQ ? THEN v-cost[li] = 0.
    END.
   
    ASSIGN
       ar-invl.cost = v-cost[1] + v-cost[2] + v-cost[3] + v-cost[4]
       ar-invl.dscr[1] = "M".
          
    IF ar-invl.cost EQ ? THEN ar-invl.cost = 0.

    ASSIGN
       ar-inv.t-cost = ar-inv.t-cost - ar-invl.t-cost
       ar-invl.t-cost = ar-invl.cost * ar-invl.inv-qty / 1000.

    IF ar-invl.t-cost EQ ? THEN ar-invl.t-cost = 0.

    ar-inv.t-cost = ar-inv.t-cost + ar-invl.t-cost.
  END.
END.

SESSION:SET-WAIT-STATE ("").

MESSAGE "Create missing shipments complete..." VIEW-AS ALERT-BOX.
