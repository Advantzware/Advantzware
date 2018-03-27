/* -------------------------------------------------- oe/custbin2.i 06/99 JLF */
/* Bill Of Lading and Invoice Posting - CUST Warehouse Process                */
/* -------------------------------------------------------------------------- */
lv-{2}-cust-no = IF "{5}" = "+" THEN "" ELSE tt-bolh.cust-no.

IF tt-boll.s-code EQ "S"  THEN DO:
   FOR EACH  oe-rel WHERE oe-rel.company EQ tt-boll.company
      AND oe-rel.ord-no EQ tt-boll.ord-no
      AND oe-rel.i-no   EQ tt-boll.i-no 
      AND oe-rel.s-code EQ "I"
        NO-LOCK:
         FIND FIRST oe-rell
            WHERE oe-rell.company  EQ oe-rel.company
              AND oe-rell.r-no     EQ oe-rel.link-no
              AND oe-rell.ord-no   EQ oe-rel.ord-no
              AND oe-rell.rel-no   EQ oe-rel.rel-no
              AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
              AND oe-rell.i-no     EQ oe-rel.i-no
              AND oe-rell.line     EQ oe-rel.line
              AND oe-rell.po-no    EQ oe-rel.po-no
              AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
            USE-INDEX r-no NO-LOCK NO-ERROR.
        IF NOT AVAIL oe-rell THEN
          FIND FIRST oe-rell
              WHERE oe-rell.company  EQ oe-rel.company
                AND oe-rell.link-no  EQ oe-rel.r-no
                AND oe-rell.ord-no   EQ oe-rel.ord-no
                AND oe-rell.rel-no   EQ oe-rel.rel-no
                AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
                AND oe-rell.i-no     EQ oe-rel.i-no
                AND oe-rell.line     EQ oe-rel.line
                AND oe-rell.po-no    EQ oe-rel.po-no
                AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
              NO-LOCK NO-ERROR.

      IF AVAIL oe-rell THEN DO:
      
        ASSIGN lcShipJobNo  = oe-rell.job-no
               liShipJobNo2 = oe-rell.job-no2.
  
         FIND FIRST fg-rcpth 
            WHERE fg-rcpth.company EQ oe-rell.company
              AND fg-rcpth.i-no    EQ oe-rell.i-no
              AND fg-rcpth.job-no  EQ oe-rell.job-no
              AND fg-rcpth.job-no2 EQ oe-rell.job-no2
              AND fg-rcpth.rita-code EQ "T" 
            USE-INDEX tdate
            NO-LOCK NO-ERROR .
        IF AVAIL fg-rcpth THEN
          lcShipLoc = fg-rcpth.loc.                 
          
      END. /* Avail oe-rell */
      LEAVE.
   END. /* Each oe-rel */

END. /* If S-code = "S" */


FIND FIRST b-fg-bin
    WHERE b-fg-bin.company EQ tt-boll.company
      AND b-fg-bin.job-no  EQ tt-boll.job-no
      AND b-fg-bin.job-no2 EQ tt-boll.job-no2
      AND b-fg-bin.i-no    EQ tt-boll.i-no
      AND b-fg-bin.cust-no EQ tt-boll.cust-no
      AND b-fg-bin.loc     EQ tt-boll.loc
      AND b-fg-bin.loc-bin EQ tt-boll.loc-bin
      AND b-fg-bin.tag     EQ tt-boll.tag
    USE-INDEX JOB  NO-ERROR.

/* wfk - Intended to handle the 'S' case, otherwise, don't know why loc-bin would be blank */

CASE tt-boll.s-code:
    WHEN "I" THEN 
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ tt-boll.company
              AND fg-bin.job-no  EQ tt-boll.job-no
              AND fg-bin.job-no2 EQ tt-boll.job-no2
              AND fg-bin.i-no    EQ tt-boll.i-no
              AND fg-bin.cust-no EQ lv-{2}-cust-no
              AND fg-bin.loc     EQ tt-boll.loc
              AND fg-bin.loc-bin EQ tt-boll.loc-bin
              AND fg-bin.tag     EQ ""
            USE-INDEX job NO-ERROR.
    WHEN "T" THEN
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ tt-boll.company
              AND fg-bin.job-no  EQ tt-boll.job-no
              AND fg-bin.job-no2 EQ tt-boll.job-no2
              AND fg-bin.i-no    EQ tt-boll.i-no
              AND fg-bin.cust-no EQ lv-{2}-cust-no
              AND fg-bin.loc     EQ tt-boll.loc
/*              AND fg-bin.loc-bin EQ tt-boll.loc-bin */
              AND fg-bin.tag     EQ ""
            USE-INDEX job NO-ERROR.
    WHEN "S" THEN
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ tt-boll.company
              AND fg-bin.job-no  EQ tt-boll.job-no
              AND fg-bin.job-no2 EQ tt-boll.job-no2 
              AND fg-bin.i-no    EQ tt-boll.i-no
              /* AND fg-bin.cust-no EQ lv-{2}-cust-no */
              AND fg-bin.loc     EQ tt-boll.loc
              AND fg-bin.loc-bin EQ "" /* tt-boll.loc-bin */
              AND fg-bin.tag     EQ tt-boll.tag
              AND (IF "{5}" = "+" THEN fg-bin.qty LT 0
                                 ELSE fg-bin.qty GE 0) 
            USE-INDEX job NO-ERROR.

    OTHERWISE 
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ tt-boll.company
              AND fg-bin.job-no  EQ tt-boll.job-no
              AND fg-bin.job-no2 EQ tt-boll.job-no2 
              AND fg-bin.i-no    EQ tt-boll.i-no
              AND fg-bin.cust-no EQ lv-{2}-cust-no
              AND fg-bin.loc     EQ tt-boll.loc
              AND fg-bin.loc-bin EQ ""
              AND fg-bin.tag     EQ "" 
              AND (IF "{5}" = "+" THEN fg-bin.qty LT 0
                                 ELSE fg-bin.qty GE 0) 
            USE-INDEX job NO-ERROR.

END CASE.



  /* WFK - I'm handling the S case, otherwise don't know why this is here */
  IF NOT AVAIL fg-bin AND tt-boll.s-code NE "S" THEN
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ tt-boll.company
            AND fg-bin.i-no    EQ tt-boll.i-no
            AND fg-bin.loc-bin EQ ""
          USE-INDEX job NO-ERROR.


/*IF NOT AVAIL fg-bin AND itemfg.pur-man THEN
FIND FIRST fg-bin
    WHERE fg-bin.company EQ tt-boll.company
      AND fg-bin.i-no    EQ tt-boll.i-no
      AND fg-bin.cust-no EQ lv-{2}-cust-no
    NO-ERROR.*/

IF AVAIL itemfg THEN
    FIND FIRST fg-set WHERE fg-set.company EQ itemfg.company
                       AND fg-set.part-no EQ itemfg.i-no
                     NO-LOCK NO-ERROR.

IF NOT AVAIL fg-bin 
    AND (tt-boll.s-code = "S" AND NOT 
         (AVAIL(fg-set) 
           AND 
          CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company EQ tt-boll.company
                                            AND oe-ordl.ord-no  EQ tt-boll.ord-no
                                            AND oe-ordl.i-no    EQ tt-boll.i-no
                                            AND oe-ordl.LINE    EQ tt-boll.LINE
                                            AND oe-ordl.is-a-component))) THEN DO:
  
    /* This find is here to prevent error related to the unique index */
    /* in case the record was not found above 08211403                */
    IF tt-boll.s-code EQ "S" THEN DO:
      /* No match on loc since placeholder always has default loc */
      FIND FIRST fg-bin 
        WHERE fg-bin.company    EQ cocode          
          AND fg-bin.i-no       EQ tt-boll.i-no
          AND fg-bin.job-no     EQ tt-boll.job-no
          AND fg-bin.job-no2    EQ tt-boll.job-no2
          AND fg-bin.tag        EQ "" /* tt-boll.tag */
          AND fg-bin.loc-bin    EQ "" /* tt-boll.loc-bin */
          AND fg-bin.cust-no    EQ lv-{2}-cust-no
          AND ((fg-bin.qty LT 0 AND fg-bin.cust-no EQ "") OR (fg-bin.qty GT 0 AND fg-bin.cust-no GT "")) 
        NO-ERROR.

      /* Try without job-no and ord-no */
      IF NOT AVAIL fg-bin THEN
      FIND FIRST fg-bin 
        WHERE fg-bin.company    EQ cocode
          AND fg-bin.i-no       EQ tt-boll.i-no
          AND fg-bin.tag        EQ "" /* tt-boll.tag */
          AND fg-bin.loc-bin    EQ "" /* tt-boll.loc-bin */
          AND fg-bin.cust-no    EQ lv-{2}-cust-no
          AND ((fg-bin.qty LT 0 AND fg-bin.cust-no EQ "") OR (fg-bin.qty GT 0 AND fg-bin.cust-no GT "")) 
        NO-ERROR.

     /* Try without cust-no */
     IF NOT AVAIL fg-bin THEN         
     FIND FIRST fg-bin 
        WHERE fg-bin.company    EQ cocode
          AND fg-bin.i-no       EQ tt-boll.i-no
          AND fg-bin.tag        EQ "" /* tt-boll.tag */
          AND fg-bin.loc-bin    EQ "" /* tt-boll.loc-bin */
          AND fg-bin.cust-no    EQ ""
          AND ((fg-bin.qty LT 0 AND fg-bin.cust-no EQ "") OR (fg-bin.qty GT 0 AND fg-bin.cust-no GT "")) 
        NO-ERROR.

 
    END. /* s-code eq 'S' */
    
    
    /* Make sure no duplicate with unique index */
    IF NOT AVAIL fg-bin THEN 
        FIND FIRST fg-bin 
            WHERE fg-bin.company    EQ cocode                                                    
            AND fg-bin.i-no       EQ tt-boll.i-no
            AND fg-bin.loc        EQ tt-boll.loc
            /* This is wrong, the placeholder record never has a loc-bin */
            /* AND fg-bin.loc-bin    EQ (IF tt-boll.s-code EQ "S" THEN tt-boll.loc-bin ELSE  "") */
            AND fg-bin.loc-bin    EQ ""
            /* This is wrong, placeholder record should never have a tag */
            /*  AND fg-bin.tag        EQ (IF tt-boll.s-code EQ "S" THEN tt-boll.tag ELSE "") */
            AND fg-bin.job-no     EQ (IF AVAIL b-fg-bin THEN  b-fg-bin.job-no ELSE tt-boll.job-no)
            AND fg-bin.job-no2    EQ (IF AVAIL b-fg-bin THEN  b-fg-bin.job-no2 ELSE tt-boll.job-no2)
            AND fg-bin.cust-no    EQ (IF tt-boll.s-code EQ "S" THEN "" ELSE lv-{2}-cust-no)
            /* Placeholder records should never have a bol-no */
            /* AND fg-bin.bol-no     EQ tt-bolh.bol-no */
            AND fg-bin.inv-no     EQ 0
            AND fg-bin.po-no      EQ ""
            NO-ERROR.
            
    IF NOT AVAIL fg-bin THEN DO:
      
      CREATE fg-bin.
      ASSIGN
       fg-bin.company    = cocode
       fg-bin.cust-no    = lv-{2}-cust-no
       /* fg-bin.bol-no     = tt-bolh.bol-no */
       fg-bin.i-no       = tt-boll.i-no
       fg-bin.loc        = tt-boll.loc
       fg-bin.loc-bin    = ""
       fg-bin.job-no     = tt-boll.job-no
       fg-bin.job-no2    = tt-boll.job-no2
       fg-bin.last-count = fg-bin.qty
       fg-bin.pur-uom    = IF AVAIL b-fg-bin THEN b-fg-bin.pur-uom 
                           ELSE
                           IF AVAIL itemfg THEN itemfg.prod-uom
                           ELSE "M"
       fg-bin.case-count = tt-boll.qty-case
       fg-bin.aging-date = TODAY
       fg-bin.ord-no     = {1}.ord-no
       fg-bin.tag        = "".
    
       IF tt-boll.s-code EQ "S" THEN DO:
         
         ASSIGN
         /* This is wrong, placeholder record should never have a tag */
         /* fg-bin.tag        = tt-boll.tag */
         fg-bin.tag           = ""
         /* This is wrong, the placeholder record never has a bin */
         /*  fg-bin.loc-bin    = tt-boll.loc-bin */
         fg-bin.loc-bin    = ""
         fg-bin.loc        = tt-boll.loc
         fg-bin.cust-no    = "".
         IF AVAIL b-fg-bin THEN
           ASSIGN fg-bin.job-no  = b-fg-bin.job-no
                  fg-bin.job-no2 = b-fg-bin.job-no2.

           END.
       
    END.
END.

 
IF AVAIL(fg-bin) THEN DO:
  IF AVAIL b-fg-bin AND ROWID(b-fg-bin) NE ROWID(fg-bin) THEN
    ASSIGN
     fg-bin.std-lab-cost = (({3}        * b-fg-bin.std-lab-cost) + 
                            (fg-bin.qty * fg-bin.std-lab-cost)) /
                           ({3} + fg-bin.qty)
     fg-bin.std-mat-cost = (({3}        * b-fg-bin.std-mat-cost) + 
                            (fg-bin.qty * fg-bin.std-mat-cost)) /
                           ({3} + fg-bin.qty)
     fg-bin.std-var-cost = (({3}        * b-fg-bin.std-var-cost) + 
                            (fg-bin.qty * fg-bin.std-var-cost)) /
                           ({3} + fg-bin.qty)
     fg-bin.std-fix-cost = (({3}        * b-fg-bin.std-fix-cost) + 
                            (fg-bin.qty * fg-bin.std-fix-cost)) /
                           ({3} + fg-bin.qty).

  IF fg-bin.std-lab-cost EQ ? THEN
    ASSIGN
     fg-bin.std-tot-cost = 0
     fg-bin.std-lab-cost = 0
     fg-bin.std-mat-cost = 0
     fg-bin.std-var-cost = 0
     fg-bin.std-fix-cost = 0.

  IF tt-boll.s-code EQ "S" AND lcShipJobNo GT "" THEN DO:
      /* Special override for ship only in the case where the job # */
      /* Is different from the invoice only. In this case, take the costs */
      /* from the invoice only related bins are not available here, so    */
      /* using the job-hdr                                                */
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ fg-bin.company
            AND job-hdr.job-no  EQ lcShipJobNo
            AND job-hdr.job-no2 EQ liShipJobNo2
            AND job-hdr.i-no    EQ fg-bin.i-no
          NO-ERROR.
     
      IF AVAIL job-hdr THEN
        ASSIGN
         fg-bin.std-lab-cost = job-hdr.std-lab-cost
         fg-bin.std-mat-cost = job-hdr.std-mat-cost
         fg-bin.std-var-cost = job-hdr.std-var-cost
         fg-bin.std-fix-cost = job-hdr.std-fix-cost
         fg-bin.pur-uom      = "M".
  END.
  ELSE DO:
  
    IF fg-bin.job-no NE ""                            AND
       fg-bin.std-lab-cost + fg-bin.std-mat-cost +
       fg-bin.std-var-cost + fg-bin.std-fix-cost EQ 0 THEN DO:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ fg-bin.company
            AND job-hdr.job-no  EQ fg-bin.job-no
            AND job-hdr.job-no2 EQ fg-bin.job-no2
            AND job-hdr.i-no    EQ fg-bin.i-no
          NO-ERROR.
      IF AVAIL job-hdr THEN
        ASSIGN
         fg-bin.std-lab-cost = job-hdr.std-lab-cost
         fg-bin.std-mat-cost = job-hdr.std-mat-cost
         fg-bin.std-var-cost = job-hdr.std-var-cost
         fg-bin.std-fix-cost = job-hdr.std-fix-cost
         fg-bin.pur-uom      = "M".
    END.
 END.
          
  fg-bin.std-tot-cost = fg-bin.std-lab-cost + fg-bin.std-mat-cost +
                        fg-bin.std-var-cost + fg-bin.std-fix-cost.

  IF fg-bin.std-lab-cost EQ ? THEN
    ASSIGN
     fg-bin.std-tot-cost = 0
     fg-bin.std-lab-cost = 0
     fg-bin.std-mat-cost = 0
     fg-bin.std-var-cost = 0
     fg-bin.std-fix-cost = 0.

  IF fg-bin.case-count   LE 0 THEN fg-bin.case-count   = tt-boll.qty-case.
  IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
  IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
  
  
  ASSIGN
   fg-bin.last-date     = TODAY
   fg-bin.qty           = fg-bin.qty + {3}
   fg-bin.partial-count = fg-bin.partial-count + {4}.
  

  v-rcpth-no = 0.
  FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL fg-rctd AND fg-rctd.r-no GT v-rcpth-no THEN v-rcpth-no = fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT v-rcpth-no THEN v-rcpth-no = fg-rcpth.r-no.

  DO WHILE TRUE:
    v-rcpth-no = v-rcpth-no + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ v-rcpth-no USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ v-rcpth-no USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL fg-rctd THEN NEXT.
    LEAVE.
  END.

  CREATE fg-rcpth.
  ASSIGN
   fg-rcpth.r-no       = v-rcpth-no
   fg-rcpth.b-no       = tt-bolh.b-no
   fg-rcpth.company    = fg-bin.company
   fg-rcpth.loc        = fg-bin.loc
   fg-rcpth.trans-date = tt-bolh.bol-date
   fg-rcpth.post-date  = TODAY 
   fg-rcpth.po-no      = tt-bolh.po-no
   fg-rcpth.i-no       = itemfg.i-no
   fg-rcpth.i-name     = IF AVAIL {1} THEN {1}.i-name ELSE itemfg.i-name
   fg-rcpth.job-no     = fg-bin.job-no
   fg-rcpth.job-no2    = fg-bin.job-no2
   fg-rcpth.pur-uom    = fg-bin.pur-uom
   fg-rcpth.rita-code  = IF ll-{2}-set-hdr THEN "R" ELSE "T".
   
  FIND CURRENT fg-rcpth.
  CREATE fg-rdtlh.
  ASSIGN
   fg-rdtlh.r-no         = fg-rcpth.r-no 
   fg-rdtlh.company      = fg-bin.company
   fg-rdtlh.loc          = fg-bin.loc
   fg-rdtlh.loc-bin      = fg-bin.loc-bin
   fg-rdtlh.tag          = fg-bin.tag
   fg-rdtlh.cust-no      = lv-{2}-cust-no
   fg-rdtlh.rita-code    = fg-rcpth.rita-code
   fg-rdtlh.cost         = fg-bin.std-tot-cost
   fg-rdtlh.qty-case     = tt-boll.qty-case
   fg-rdtlh.stacks-unit  = fg-bin.cases-unit
   fg-rdtlh.units-pallet = fg-bin.units-pallet
   fg-rdtlh.qty          = {3}
   fg-rdtlh.partial      = {4}
   fg-rdtlh.trans-time   = TIME
   fg-rdtlh.cases        = TRUNC((fg-rdtlh.qty - fg-rdtlh.partial) / 
                               fg-rdtlh.qty-case,0).
   IF fg-rcpth.rita-code EQ "S" THEN
     ASSIGN fg-rdtlh.tag = tt-boll.tag
            fg-rdtlh.cust-no = "".

   IF fg-rcpth.rita-code EQ "T" THEN DO:  
     ASSIGN fg-rdtlh.tag = ""
            fg-rdtlh.loc-bin = "".
     IF lcShipLoc GT "" THEN
            ASSIGN fg-rdtlh.loc = lcShipLoc
                   fg-rcpth.loc = lcShipLoc.
     
     IF lcShipJobNo GT "" THEN
            ASSIGN fg-rdtlh.job-no  = lcShipJobNo
                   fg-rdtlh.job-no2 = liShipJobNo2
                   fg-rcpth.job-no  = lcShipJobNo
                   fg-rcpth.job-no2 = liShipJobNo2.
   END.

   IF fg-bin.qty = 0 AND tt-boll.s-code NE "S" THEN
     DELETE fg-bin. 
 END.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

