/* -------------------------------------------------- oe/oe-bolp6.i 02/99 JLF */
/* Bill Of Lading and Invoice Posting - relieve bins                          */
/* -------------------------------------------------------------------------- */

DEF VAR ll-{2}-set-hdr AS LOG NO-UNDO.
DEF VAR ll-{2}-zero-bin AS LOG NO-UNDO.

ll-{2}-zero-bin = NO.
ll-{2}-set-hdr = AVAIL oe-ordl AND CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}).
IF NOT AVAIL fg-bin THEN DO:
  ASSIGN
   v-fifo    = NO
   v-loc     = tt-boll.loc
   v-loc-bin = tt-boll.loc-bin
   v-tag     = tt-boll.tag.
 
  IF fgsetrec EQ "Item" AND "{2}" EQ "Part" THEN DO:
    ASSIGN
     v-loc     = ""
     v-loc-bin = ""
     v-tag     = "".
         
    IF autopost EQ "ShipTo" THEN DO:
      /*get customer file from estimate blank file*/
      FIND FIRST cust
          WHERE cust.company EQ cocode
            AND cust.cust-no EQ itemfg.cust-no
          NO-LOCK NO-ERROR.
      IF AVAIL cust THEN DO:              
        FIND FIRST shipto
            WHERE shipto.company EQ cocode
              AND shipto.cust-no EQ cust.cust-no
            NO-LOCK NO-ERROR.
        IF AVAIL shipto THEN DO:
          FIND FIRST fg-bin
              WHERE fg-bin.company EQ cocode
                AND fg-bin.loc     EQ shipto.loc
                AND fg-bin.loc-bin EQ shipto.loc-bin
                AND fg-bin.i-no    EQ ""
              NO-LOCK NO-ERROR.
          IF AVAIL fg-bin THEN 
            ASSIGN
             v-loc     = shipto.loc
             v-loc-bin = shipto.loc-bin.
        END.
      END. /*if avail cust*/
                           
      IF v-loc EQ "" AND v-loc-bin EQ "" THEN DO:
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ cocode
              AND fg-bin.loc     EQ itemfg.def-loc
              AND fg-bin.loc-bin EQ itemfg.def-loc-bin
              AND fg-bin.i-no    EQ ""
            NO-LOCK NO-ERROR.
        IF AVAIL fg-bin THEN 
          ASSIGN
           v-loc     = itemfg.def-loc
           v-loc-bin = itemfg.def-loc-bin.
      END.
    END. /*if system default is shipto*/
          
    ELSE DO:
      FIND FIRST fg-bin
          WHERE fg-bin.company EQ cocode
            AND fg-bin.loc     EQ itemfg.def-loc
            AND fg-bin.loc-bin EQ itemfg.def-loc-bin
            AND fg-bin.i-no    EQ ""
          NO-LOCK NO-ERROR.
      IF AVAIL fg-bin THEN 
        ASSIGN
         v-loc     = itemfg.def-loc
         v-loc-bin = itemfg.def-loc-bin.
    END. /*else FGFILE*/
    IF tt-boll.s-code = "I" THEN
        v-loc-bin = "".

    /*if bin and warehouse are blank, goto cust "X" shipto file*/
    IF v-loc EQ "" AND v-loc-bin EQ "" THEN DO:
      FIND FIRST cust
          WHERE cust.company EQ cocode
            AND cust.active  EQ "X"
          NO-LOCK NO-ERROR.
                                
      IF AVAIL cust THEN DO:
        FIND FIRST shipto
            WHERE shipto.company EQ cocode
              AND shipto.cust-no EQ cust.cust-no  
            NO-LOCK NO-ERROR.
        IF AVAIL shipto THEN DO:
          FIND FIRST fg-bin
              WHERE fg-bin.company EQ cocode
                AND fg-bin.loc     EQ shipto.loc
                AND fg-bin.loc-bin EQ shipto.loc-bin
                AND fg-bin.i-no    EQ ""
              NO-LOCK NO-ERROR.
          IF AVAIL fg-bin THEN
            ASSIGN
             v-loc     = shipto.loc
             v-loc-bin = shipto.loc-bin.
        END.                                  
      END.
    END.
  END.

  FIND FIRST fg-bin
      WHERE fg-bin.company  EQ cocode
        AND fg-bin.job-no   EQ tt-boll.job-no
        AND fg-bin.job-no2  EQ tt-boll.job-no2
        AND fg-bin.i-no     EQ tt-boll.i-no
        AND fg-bin.loc      EQ v-loc
        AND fg-bin.loc-bin  EQ v-loc-bin
        AND fg-bin.tag      EQ v-tag
        AND (fg-bin.cust-no EQ "" OR tt-boll.s-code NE "I")
      USE-INDEX i-no NO-ERROR.

  IF NOT AVAIL fg-bin AND itemfg.pur-man THEN
  FIND FIRST fg-bin
      WHERE fg-bin.company  EQ cocode
        AND fg-bin.tag      EQ v-tag
        AND fg-bin.loc      EQ v-loc
        AND fg-bin.loc-bin  EQ v-loc-bin
        AND fg-bin.i-no     EQ tt-boll.i-no
        AND (fg-bin.cust-no EQ "" OR tt-boll.s-code NE "I")
      USE-INDEX tag NO-ERROR.
      
   
    /* find exactly as below to ensure unique index not violated */
    IF NOT AVAIL fg-bin THEN 
        FIND FIRST fg-bin
            WHERE fg-bin.company  EQ cocode
            AND fg-bin.i-no     EQ tt-boll.i-no
            AND fg-bin.loc      EQ v-loc
            AND fg-bin.loc-bin  EQ v-loc-bin
            AND fg-bin.tag      EQ v-tag
            AND fg-bin.job-no   EQ tt-boll.job-no
            AND fg-bin.job-no2  EQ tt-boll.job-no2                                                            
            AND fg-bin.cust-no  EQ "" 
            /* AND fg-bin.bol-no   EQ 0 */
            AND fg-bin.inv-no   EQ 0
            AND fg-bin.po-no    EQ ""
            USE-INDEX i-no NO-ERROR.   
  
  IF NOT AVAIL fg-bin THEN DO:
    CREATE fg-bin.
    ASSIGN
     fg-bin.company    = cocode
     fg-bin.i-no       = tt-boll.i-no
     fg-bin.loc        = v-loc
     fg-bin.loc-bin    = v-loc-bin
     fg-bin.tag        = v-tag
     fg-bin.ord-no     = tt-boll.ord-no
     fg-bin.job-no     = tt-boll.job-no
     fg-bin.job-no2    = tt-boll.job-no2
     fg-bin.pur-uom    = itemfg.prod-uom
     fg-bin.aging-date = TODAY.
  END. /* Not avail fg-bin */
  
END. /* Not avail fg-bin */

IF fg-bin.std-lab-cost EQ ? THEN
  ASSIGN
   fg-bin.std-tot-cost = 0
   fg-bin.std-lab-cost = 0
   fg-bin.std-mat-cost = 0
   fg-bin.std-var-cost = 0
   fg-bin.std-fix-cost = 0.

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
IF fg-bin.case-count   LE 0 THEN fg-bin.case-count   = itemfg.case-count.
IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.


IF v-bol-qty GT 0 AND v-bol-qty LE fg-bin.partial-count THEN
  fg-bin.partial-count = fg-bin.partial-count - v-bol-qty.
ELSE
  fg-bin.partial-count = fg-bin.partial-count - v-partial.

IF v-bol-qty GT 0                              OR
   NOT CAN-FIND(FIRST sys-ctrl
                WHERE sys-ctrl.company EQ fg-bin.company
                  AND sys-ctrl.name    EQ "BOLPOST"
                  AND sys-ctrl.log-fld EQ YES) THEN DO:    
    fg-bin.qty = fg-bin.qty - v-bol-qty.
    ll-{2}-zero-bin = YES.

END.
 
/* wfk - 11051313 moved this to before v-partial is zeroed out so that it updates the fg-rdtlh.partial field */
li-stupid = v-partial  /* This is really stupid, but it must be done */.
ASSIGN
 v-fg-qty   = v-bol-qty
 v-bol-qty  = 0
 v-partial  = 0.

IF fg-bin.partial-count EQ fg-bin.case-count THEN fg-bin.partial-count = 0.
  
IF v-fifo AND fg-bin.qty LT 0 THEN  DO:
  ASSIGN
   v-fg-qty   = v-fg-qty + fg-bin.qty
   v-bol-qty  = fg-bin.qty * -1
   fg-bin.qty = 0.
   

   
END.

/* To delete record after not referenced */
rSaveFgBinRow = ROWID(fg-bin).

IF v-fg-qty NE 0 THEN DO:

    {oe/shiphist.i "{1}" "{2}" "v-fg-qty"}
    
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
      {oe/custbin2.i "{1}" "{2}" "- v-fg-qty" "- v-partial" "-"}

      /* Reduce negative placeholder customer fg-bin record created for invoice-only */
      {oe/custbin2.i "{1}" "{2}" "v-fg-qty" "v-partial" "+"}

    END.
END.



/* Delete zero bins unless FgKeepZeroBin is selected */
IF NOT FgKeepZeroBin-log AND AVAILABLE fg-bin AND fg-bin.qty EQ 0 AND fg-bin.partial-count EQ 0 THEN 
    
    
FIND CURRENT fg-bin NO-LOCK NO-ERROR.
IF NOT FgKeepZeroBin-log AND CAN-FIND(fg-bin WHERE
                                               ROWID(fg-bin) = rSaveFgBinRow 
                                               AND fg-bin.qty EQ 0 
                                               AND fg-bin.partial-count EQ 0) THEN 
DO:
  
   FIND FIRST fg-bin WHERE ROWID(fg-bin) EQ rSaveFgBinRow EXCLUSIVE-LOCK NO-ERROR.
   DELETE fg-bin.
   
END.    
/*removed for task 09241404 - reason is that there is now a 0 bin filter 
and the goal is to preserve original bin cost*/
/* /* 03301201 - Per Joe, tags should be removed */ */
/* IF ll-{2}-zero-bin AND fg-bin.qty EQ 0 THEN      */
/*   DELETE fg-bin.                                 */

/* end ---------------------------------- copr. 1999  advanced software, inc. */
