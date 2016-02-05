
DEF PARAM BUFFER io-itemfg FOR itemfg.

DEF INPUT PARAM ip-date AS   DATE NO-UNDO.
DEF INPUT PARAM ip-fjob LIKE fg-bin.job-no NO-UNDO.
DEF INPUT PARAM ip-tjob LIKE fg-bin.job-no NO-UNDO.
DEF INPUT PARAM ip-floc LIKE fg-bin.loc NO-UNDO.
DEF INPUT PARAM ip-tloc LIKE fg-bin.loc NO-UNDO.
DEF INPUT PARAM ip-fbin LIKE fg-bin.loc-bin NO-UNDO.
DEF INPUT PARAM ip-tbin LIKE fg-bin.loc-bin NO-UNDO.
DEF INPUT PARAM ip-zbal AS   LOG NO-UNDO.
DEF INPUT PARAM ip-ager AS   INT NO-UNDO.
DEF INPUT PARAM ip-curr AS   LOG NO-UNDO.
DEF INPUT PARAM ip-cust AS   LOG NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF VAR vdat          AS   DATE NO-UNDO.
DEF VAR v-curr        AS   LOG NO-UNDO.
DEF VAR v-qohj        AS   DEC EXTENT 6 NO-UNDO.
DEF VAR v-qohi        LIKE v-qohj NO-UNDO.
DEF VAR v-qty         AS   INT NO-UNDO.
DEF VAR v-qty1        LIKE v-qty NO-UNDO.
DEF VAR v-qtyc        LIKE v-qty NO-UNDO.
DEF VAR v-red         LIKE v-qty NO-UNDO.
DEF VAR v             AS   INT NO-UNDO.
DEF VAR v-date        AS   DATE NO-UNDO.
DEF VAR lv-tag        LIKE fg-rdtlh.tag NO-UNDO.
DEF VAR ld-last       AS   DATE NO-UNDO.
DEF VAR v-rec-date    AS   DATE NO-UNDO.
DEF BUFFER b-f-rc for fg-rcpth.
DEF BUFFER b-f-rd for fg-rdtlh.  

{fg/rep/tt-fgbin.i SHARED}


STATUS DEFAULT "Processing FG Item#: " + TRIM(io-itemfg.i-no).

ASSIGN
 vdat   = ip-date
 v-curr = ip-curr.

IF NOT AVAIL io-itemfg THEN LEAVE.

cocode = io-itemfg.company.

IF ip-date GE TODAY THEN
FOR EACH fg-bin NO-LOCK
    WHERE fg-bin.company   EQ io-itemfg.company
      AND fg-bin.i-no      EQ io-itemfg.i-no
      AND STRING(FILL(" ",6 - LENGTH(TRIM(fg-bin.job-no))) +
                 TRIM(fg-bin.job-no) + STRING(fg-bin.job-no2,"99"))
                           GE ip-fjob
      AND STRING(FILL(" ",6 - LENGTH(TRIM(fg-bin.job-no))) +
                 TRIM(fg-bin.job-no) + STRING(fg-bin.job-no2,"99"))
                           LE ip-tjob
      AND fg-bin.loc       GE ip-floc
      AND fg-bin.loc       LE ip-tloc
      AND fg-bin.loc-bin   GE ip-fbin
      AND fg-bin.loc-bin   LE ip-tbin
      AND ((fg-bin.cust-no EQ "" AND fg-bin.loc NE "CUST") OR ip-cust)
      AND (fg-bin.qty      NE 0 OR ip-zbal):

  CREATE tt-fg-bin.
  BUFFER-COPY fg-bin TO tt-fg-bin.
   
  FOR EACH fg-rcpth FIELDS(r-no rita-code po-no) WHERE
      fg-rcpth.company EQ io-itemfg.company AND
      fg-rcpth.i-no EQ io-itemfg.i-no AND
      fg-rcpth.job-no EQ fg-bin.job-no AND
      fg-rcpth.job-no2 EQ fg-bin.job-no2 AND
      fg-rcpth.po-no NE "" AND
      fg-rcpth.rita-code EQ "R"
      NO-LOCK,
      FIRST fg-rdtlh fields() WHERE
            fg-rdtlh.r-no EQ fg-rcpth.r-no AND
            fg-rdtlh.rita-code EQ fg-rcpth.rita-code AND
            fg-rdtlh.loc EQ fg-bin.loc AND
            fg-rdtlh.loc-bin EQ fg-bin.loc-bin AND
            fg-rdtlh.tag EQ fg-bin.tag AND
            fg-rdtlh.cust-no EQ fg-bin.cust-no AND
            fg-rdtlh.bol-no EQ fg-bin.bol-no AND
            fg-rdtlh.inv-no EQ fg-bin.inv-no
            NO-LOCK
      BY fg-rcpth.trans-date DESC
      BY fg-rdtlh.trans-time DESC:

      tt-fg-bin.po-no = fg-rcpth.po-no.
  END.

  /*tt-fg-bin.first-date = tt-fg-bin.aging-date.*/
  
/*   IF TRIM(tt-fg-bin.tag) EQ "" THEN                          */
/*      FOR EACH fg-rcpth NO-LOCK                               */
/*          WHERE fg-rcpth.company      EQ io-itemfg.company    */
/*            AND fg-rcpth.i-no         EQ io-itemfg.i-no       */
/*            AND fg-rcpth.job-no       EQ tt-fg-bin.job-no     */
/*            AND fg-rcpth.job-no2      EQ tt-fg-bin.job-no2    */
/*          USE-INDEX tran,                                     */
/*                                                              */
/*          EACH fg-rdtlh NO-LOCK                               */
/*          WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no        */
/*            /*AND fg-rdtlh.loc          EQ tt-fg-bin.loc      */
/*            AND fg-rdtlh.loc-bin      EQ tt-fg-bin.loc-bin*/  */
/*            AND fg-rdtlh.tag          EQ tt-fg-bin.tag        */
/*            AND fg-rdtlh.cust-no      EQ tt-fg-bin.cust-no    */
/*            AND fg-rdtlh.rita-code    EQ "R"                  */
/*          USE-INDEX rm-rdtl                                   */
/*                                                              */
/*          BREAK BY fg-rcpth.trans-date                        */
/*                BY fg-rdtlh.trans-time                        */
/*                BY fg-rcpth.r-no:                             */
/*                                                              */
/*          IF FIRST(fg-rcpth.trans-date) THEN                  */
/*             tt-fg-bin.first-date = fg-rcpth.trans-date.      */
/*                                                              */
/*          LEAVE.                                              */
/*      END.                                                    */
/*   ELSE                                                       */
/*      FOR EACH fg-rdtlh                                       */
/*          WHERE fg-rdtlh.company      EQ tt-fg-bin.company    */
/*            AND fg-rdtlh.tag          EQ tt-fg-bin.tag        */
/*            /*AND fg-rdtlh.loc          EQ tt-fg-bin.loc      */
/*            AND fg-rdtlh.loc-bin      EQ tt-fg-bin.loc-bin*/  */
/*            AND fg-rdtlh.cust-no      EQ tt-fg-bin.cust-no    */
/*          USE-INDEX tag NO-LOCK,                              */
/*                                                              */
/*          FIRST fg-rcpth NO-LOCK                              */
/*          WHERE fg-rcpth.r-no         EQ fg-rdtlh.r-no        */
/*            AND fg-rcpth.i-no         EQ tt-fg-bin.i-no       */
/*            AND fg-rcpth.job-no       EQ tt-fg-bin.job-no     */
/*            AND fg-rcpth.job-no2      EQ tt-fg-bin.job-no2    */
/*            AND fg-rcpth.rita-code    EQ "R"                  */
/*          USE-INDEX r-no                                      */
/*                                                              */
/*          BREAK BY fg-rcpth.trans-date                        */
/*                BY fg-rdtlh.trans-time                        */
/*                BY fg-rcpth.r-no:                             */
/*                                                              */
/*         IF FIRST(fg-rcpth.trans-date) THEN                   */
/*            tt-fg-bin.first-date = fg-rcpth.trans-date.       */
/*      END.                                                    */
  /* At this point, we know the inventory value for the as-of date,
     so need to backtrack through the receipts only until the
     quantity is covered by the receipts */

  RUN find-receipt-date (INPUT rowid(tt-fg-bin), OUTPUT v-rec-date).

  IF v-rec-date NE ? THEN
    tt-fg-bin.first-date = v-rec-date.
  IF tt-fg-bin.first-date EQ ? THEN
     tt-fg-bin.first-date = DATE(SUBSTR(fg-bin.rec_key,1,8)).
END.

ELSE
FOR EACH fg-rcpth NO-LOCK
    WHERE fg-rcpth.company    EQ io-itemfg.company
      AND fg-rcpth.i-no       EQ io-itemfg.i-no
      AND fg-rcpth.trans-date LE vdat
      AND STRING(FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                 TRIM(fg-rcpth.job-no) + STRING(fg-rcpth.job-no2,"99"))
                              GE ip-fjob
      AND STRING(FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                 TRIM(fg-rcpth.job-no) + STRING(fg-rcpth.job-no2,"99"))
                              LE ip-tjob
    USE-INDEX tran,

    EACH fg-rdtlh NO-LOCK
    WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
      AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
      AND fg-rdtlh.loc       GE ip-floc
      AND fg-rdtlh.loc       LE ip-tloc
      AND fg-rdtlh.loc-bin   GE ip-fbin
      AND fg-rdtlh.loc-bin   LE ip-tbin
      AND ((fg-rdtlh.cust-no EQ "" AND fg-rdtlh.loc NE "CUST") OR ip-cust)
    BY fg-rcpth.trans-date
    BY fg-rdtlh.trans-time:

  IF NOT CAN-FIND(FIRST tt-fg-bin
                  WHERE tt-fg-bin.company EQ fg-rcpth.company
                    AND tt-fg-bin.i-no    EQ fg-rcpth.i-no
                    AND tt-fg-bin.job-no  EQ fg-rcpth.job-no
                    AND tt-fg-bin.job-no2 EQ fg-rcpth.job-no2
                    AND tt-fg-bin.loc     EQ fg-rdtlh.loc
                    AND tt-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                    AND tt-fg-bin.tag     EQ fg-rdtlh.tag
                    AND tt-fg-bin.cust-no EQ fg-rdtlh.cust-no) THEN DO:

    CREATE tt-fg-bin.

    ASSIGN
     tt-fg-bin.company      = fg-rcpth.company
     tt-fg-bin.job-no       = fg-rcpth.job-no
     tt-fg-bin.job-no2      = fg-rcpth.job-no2
     tt-fg-bin.loc          = fg-rdtlh.loc
     tt-fg-bin.loc-bin      = fg-rdtlh.loc-bin
     tt-fg-bin.tag          = fg-rdtlh.tag
     tt-fg-bin.cust-no      = fg-rdtlh.cust-no
     tt-fg-bin.i-no         = fg-rcpth.i-no
     tt-fg-bin.po-no        = fg-rcpth.po-no
     tt-fg-bin.aging-date   = fg-rcpth.trans-date
     tt-fg-bin.pur-uom      = io-itemfg.prod-uom
     tt-fg-bin.std-tot-cost = io-itemfg.total-std-cost
     tt-fg-bin.std-mat-cost = io-itemfg.std-mat-cost
     tt-fg-bin.std-lab-cost = io-itemfg.std-lab-cost
     tt-fg-bin.std-var-cost = io-itemfg.std-var-cost
     tt-fg-bin.std-fix-cost = io-itemfg.std-fix-cost.

    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company      EQ tt-fg-bin.company
          AND fg-bin.i-no         EQ tt-fg-bin.i-no
          AND fg-bin.job-no       EQ tt-fg-bin.job-no
          AND fg-bin.job-no2      EQ tt-fg-bin.job-no2
          AND fg-bin.loc          EQ tt-fg-bin.loc
          AND fg-bin.loc-bin      EQ tt-fg-bin.loc-bin
          AND fg-bin.tag          EQ tt-fg-bin.tag
          AND fg-bin.cust-no      EQ tt-fg-bin.cust-no
        NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       tt-fg-bin.std-tot-cost = fg-bin.std-tot-cost
       tt-fg-bin.std-mat-cost = fg-bin.std-mat-cost
       tt-fg-bin.std-lab-cost = fg-bin.std-lab-cost
       tt-fg-bin.std-var-cost = fg-bin.std-var-cost
       tt-fg-bin.std-fix-cost = fg-bin.std-fix-cost.
  END.

  IF tt-fg-bin.case-count   LE 0 AND fg-rdtlh.qty-case     GT 0 THEN
    tt-fg-bin.case-count   = fg-rdtlh.qty-case.
  IF tt-fg-bin.units-pallet LE 0 AND fg-rdtlh.units-pallet GT 0 THEN
    tt-fg-bin.units-pallet = fg-rdtlh.units-pallet.
  IF tt-fg-bin.cases-unit   LE 0 AND fg-rdtlh.stacks-unit  GT 0 THEN
    tt-fg-bin.cases-unit   = fg-rdtlh.stacks-unit.
END.

IF ip-ager NE 0 OR ip-date LT TODAY THEN
FOR EACH tt-fg-bin
    WHERE tt-fg-bin.company EQ io-itemfg.company
      AND tt-fg-bin.i-no    EQ io-itemfg.i-no
    USE-INDEX co-ino:
    
  STATUS DEFAULT "Processing FG Item#/Whs/Bin/Tag: " +
                 TRIM(io-itemfg.i-no)    + "/" +
                 TRIM(tt-fg-bin.loc)     + "/" +
                 TRIM(tt-fg-bin.loc-bin) + "/" +
                 TRIM(tt-fg-bin.tag).

  v-qohj = 0.

  IF TRIM(tt-fg-bin.tag) EQ "" THEN
  FOR EACH fg-rcpth NO-LOCK
      WHERE fg-rcpth.company      EQ io-itemfg.company
        AND fg-rcpth.i-no         EQ io-itemfg.i-no
        AND fg-rcpth.job-no       EQ tt-fg-bin.job-no
        AND fg-rcpth.job-no2      EQ tt-fg-bin.job-no2
        AND fg-rcpth.trans-date   LE vdat
      USE-INDEX tran,

      EACH fg-rdtlh NO-LOCK
      WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
        AND fg-rdtlh.loc          EQ tt-fg-bin.loc
        AND fg-rdtlh.loc-bin      EQ tt-fg-bin.loc-bin
        AND fg-rdtlh.tag          EQ tt-fg-bin.tag
        AND fg-rdtlh.cust-no      EQ tt-fg-bin.cust-no
        AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code
      USE-INDEX rm-rdtl

      BREAK BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no:

    IF FIRST(fg-rcpth.trans-date) THEN
      tt-fg-bin.first-date = fg-rcpth.trans-date.

    {fg/rep/fg-aging.i ip-ager}

/*     IF v-date LT tt-fg-bin.first-date AND v-date NE ? THEN */
/*       tt-fg-bin.first-date = v-date.                       */
  END.

  ELSE
  FOR EACH fg-rdtlh
      WHERE fg-rdtlh.company      EQ tt-fg-bin.company
        AND fg-rdtlh.tag          EQ tt-fg-bin.tag
        AND fg-rdtlh.loc          EQ tt-fg-bin.loc
        AND fg-rdtlh.loc-bin      EQ tt-fg-bin.loc-bin
        AND fg-rdtlh.cust-no      EQ tt-fg-bin.cust-no
      USE-INDEX tag NO-LOCK,

      FIRST fg-rcpth NO-LOCK
      WHERE fg-rcpth.r-no         EQ fg-rdtlh.r-no
        AND fg-rcpth.i-no         EQ tt-fg-bin.i-no
        AND fg-rcpth.job-no       EQ tt-fg-bin.job-no
        AND fg-rcpth.job-no2      EQ tt-fg-bin.job-no2
        AND fg-rcpth.rita-code    EQ fg-rdtlh.rita-code
        AND fg-rcpth.trans-date   LE vdat 
      USE-INDEX r-no

      BREAK BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no:

    IF FIRST(fg-rcpth.trans-date) THEN
      tt-fg-bin.first-date = fg-rcpth.trans-date.

    {fg/rep/fg-aging.i ip-ager}

/*     IF v-date LT tt-fg-bin.first-date AND v-date NE ? THEN */
/*       tt-fg-bin.first-date = v-date.                       */
  END.

  IF v-qohj[6] LT 0 THEN DO:
    ASSIGN
     v-qty     = v-qohj[6] * -1
     v-qohj[6] = 0.
          
    DO v = 5 TO 1 BY -1:
      IF v-qohj[v] GT 0 THEN
        ASSIGN
         v-red     = MIN(v-qty,v-qohj[v])
         v-qohj[v] = v-qohj[v] - v-red
         v-qty     = v-qty     - v-red.
                
      IF v-qty LE 0 THEN LEAVE.
    END.
          
    IF v-qty GT 0 THEN v-qohj[6] = v-qohj[6] - v-qty.
  END.

  /* At this point, we know the inventory value for the as-of date,
     so need to backtrack through the receipts only until the
     quantity is covered by the receipts */
  IF v-qohj[1] + v-qohj[2] + v-qohj[3] + v-qohj[4] + v-qohj[5] + v-qohj[6] GT 0 THEN
    RUN find-receipt-date (INPUT rowid(tt-fg-bin), OUTPUT v-rec-date).
  IF v-rec-date NE ? THEN
    tt-fg-bin.first-date = v-rec-date.

  IF tt-fg-bin.first-date GT vdat - ip-ager THEN v-qohj = 0.

  IF NOT v-curr THEN
    ASSIGN
     v-qohj[1] = 0
     v-qohj[2] = 0
     v-qohj[3] = 0.

  ASSIGN
   tt-fg-bin.qty         = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                           v-qohj[4] + v-qohj[5] + v-qohj[6]
   tt-fg-bin.aged-qty[1] = tt-fg-bin.aged-qty[1] + v-qohj[1]
   tt-fg-bin.aged-qty[2] = tt-fg-bin.aged-qty[2] + v-qohj[2]
   tt-fg-bin.aged-qty[3] = tt-fg-bin.aged-qty[3] + v-qohj[3]
   tt-fg-bin.aged-qty[4] = tt-fg-bin.aged-qty[4] + v-qohj[4]
   tt-fg-bin.aged-qty[5] = tt-fg-bin.aged-qty[5] + v-qohj[5]
   tt-fg-bin.aged-qty[6] = tt-fg-bin.aged-qty[6] + v-qohj[6].

  IF tt-fg-bin.qty NE 0 OR ip-zbal THEN RUN tt-bin-cost.

  ELSE DELETE tt-fg-bin.
END.
    
STATUS DEFAULT "Processing...".

RETURN.

PROCEDURE which-bucket:
  DEF INPUT PARAM ip-days AS INT NO-UNDO.
  DEF OUTPUT PARAM op-extent AS INT NO-UNDO.


  op-extent = IF ip-days LT age-days[1] THEN 1 ELSE
              IF ip-days LT age-days[2] THEN 2 ELSE
              IF ip-days LT age-days[3] THEN 3 ELSE
              IF ip-days LT age-days[4] THEN 4 ELSE 5.

END PROCEDURE.

PROCEDURE tt-bin-cost:
  DEF BUFFER b-fg-bin FOR fg-bin.


  IF tt-fg-bin.std-tot-cost EQ 0 THEN DO:
    RELEASE b-fg-bin.
    RELEASE job-hdr.
    RELEASE reftable.

    IF tt-fg-bin.tag NE "" THEN
    FIND FIRST b-fg-bin NO-LOCK
        WHERE b-fg-bin.company      EQ tt-fg-bin.company
          AND b-fg-bin.tag          EQ tt-fg-bin.tag
          AND b-fg-bin.i-no         EQ tt-fg-bin.i-no
          AND b-fg-bin.std-tot-cost NE 0
        USE-INDEX tag NO-ERROR.

    IF tt-fg-bin.job-no NE "" THEN DO:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ tt-fg-bin.company
            AND job-hdr.job-no  EQ tt-fg-bin.job-no
            AND job-hdr.job-no2 EQ tt-fg-bin.job-no2
            AND job-hdr.i-no    EQ tt-fg-bin.i-no
          USE-INDEX job-no NO-ERROR.

      IF NOT AVAIL job-hdr THEN DO:
        FIND FIRST job NO-LOCK
            WHERE job.company EQ tt-fg-bin.company
              AND job.job-no  EQ tt-fg-bin.job-no
              AND job.job-no2 EQ tt-fg-bin.job-no2
            USE-INDEX job-no NO-ERROR.
        IF AVAIL job THEN
        FIND FIRST reftable NO-LOCK
            WHERE reftable.reftable EQ "jc/jc-calc.p"
              AND reftable.company  EQ job.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ STRING(job.job,"999999999")
              AND reftable.code2    EQ tt-fg-bin.i-no
            USE-INDEX reftable NO-ERROR.
      END.
    END.

    IF AVAIL b-fg-bin THEN
      ASSIGN
       tt-fg-bin.std-tot-cost = b-fg-bin.std-tot-cost
       tt-fg-bin.std-mat-cost = b-fg-bin.std-mat-cost
       tt-fg-bin.std-lab-cost = b-fg-bin.std-lab-cost
       tt-fg-bin.std-var-cost = b-fg-bin.std-var-cost
       tt-fg-bin.std-fix-cost = b-fg-bin.std-fix-cost.
 
    ELSE
    IF AVAIL reftable AND reftable.val[5] NE 0 THEN
      ASSIGN
       tt-fg-bin.std-mat-cost = reftable.val[2]
       tt-fg-bin.std-lab-cost = reftable.val[1]
       tt-fg-bin.std-var-cost = reftable.val[3]
       tt-fg-bin.std-fix-cost = reftable.val[4].
 
    ELSE
    IF AVAIL job-hdr AND job-hdr.std-mat-cost + job-hdr.std-lab-cost +
                         job-hdr.std-var-cost + job-hdr.std-fix-cost NE 0 THEN
      ASSIGN
       tt-fg-bin.std-mat-cost = job-hdr.std-mat-cost
       tt-fg-bin.std-lab-cost = job-hdr.std-lab-cost
       tt-fg-bin.std-var-cost = job-hdr.std-var-cost
       tt-fg-bin.std-fix-cost = job-hdr.std-fix-cost.
 
    ELSE
      ASSIGN
       tt-fg-bin.std-tot-cost = io-itemfg.total-std-cost
       tt-fg-bin.std-mat-cost = io-itemfg.std-mat-cost
       tt-fg-bin.std-lab-cost = io-itemfg.std-lab-cost
       tt-fg-bin.std-var-cost = io-itemfg.std-var-cost
       tt-fg-bin.std-fix-cost = io-itemfg.std-fix-cost.

    tt-fg-bin.std-tot-cost = tt-fg-bin.std-mat-cost + tt-fg-bin.std-lab-cost +
                             tt-fg-bin.std-var-cost + tt-fg-bin.std-fix-cost.
  END.
END PROCEDURE.

PROCEDURE find-receipt-date:
DEF INPUT PARAMETER in-bin-row AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER out-rec-date AS DATE NO-UNDO.
DEF VAR v-last-date-found AS DATE NO-UNDO.
DEF BUFFER bf-fg-bin FOR tt-fg-bin.
FIND FIRST bf-fg-bin WHERE ROWID(bf-fg-bin) = in-bin-row NO-LOCK NO-ERROR.
IF NOT AVAIL bf-fg-bin THEN DO:
    MESSAGE "error - no bf-fg-bin"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.

END.
DEF VAR v-qty AS INT NO-UNDO.
  out-rec-date = ?.
  v-qty         = v-qohj[1] + v-qohj[2] + v-qohj[3] +
                  v-qohj[4] + v-qohj[5] + v-qohj[6].
  IF vdat GE TODAY AND  v-qty LE 0 THEN
    v-qty = bf-fg-bin.qty.

  IF TRIM(bf-fg-bin.tag) EQ "" THEN
  FOR EACH fg-rcpth NO-LOCK
      WHERE fg-rcpth.company      EQ io-itemfg.company
        AND fg-rcpth.i-no         EQ io-itemfg.i-no
        AND fg-rcpth.job-no       EQ bf-fg-bin.job-no
        AND fg-rcpth.job-no2      EQ bf-fg-bin.job-no2
        AND fg-rcpth.trans-date   LE vdat
        AND fg-rcpth.rita-code    EQ "R"
      USE-INDEX tran,

      EACH fg-rdtlh NO-LOCK
      WHERE fg-rdtlh.r-no         EQ fg-rcpth.r-no
        /*AND fg-rdtlh.loc          EQ bf-fg-bin.loc
        AND fg-rdtlh.loc-bin      EQ bf-fg-bin.loc-bin */
        AND fg-rdtlh.tag          EQ "" /* bf-fg-bin.tag  */
        AND fg-rdtlh.cust-no      EQ bf-fg-bin.cust-no
        AND fg-rdtlh.rita-code    EQ fg-rcpth.rita-code
      USE-INDEX rm-rdtl

      BREAK BY fg-rcpth.trans-date DESCENDING
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no:
    v-qty = v-qty - fg-rdtlh.qty.

    IF fg-rcpth.trans-date NE ? THEN
        v-last-date-found = fg-rcpth.trans-date.
    IF v-qty LE 0 THEN DO:
      out-rec-date = fg-rcpth.trans-date.
      LEAVE.
    END.
  END.

  ELSE
  FOR EACH fg-rdtlh
      WHERE fg-rdtlh.company      EQ bf-fg-bin.company
        AND fg-rdtlh.tag          EQ bf-fg-bin.tag
       /* AND fg-rdtlh.loc          EQ bf-fg-bin.loc
        AND fg-rdtlh.loc-bin      EQ bf-fg-bin.loc-bin
        AND fg-rdtlh.cust-no      EQ bf-fg-bin.cust-no */
        AND fg-rdtlh.rita-code    EQ "R"
      USE-INDEX tag NO-LOCK,

      FIRST fg-rcpth NO-LOCK
      WHERE fg-rcpth.r-no         EQ fg-rdtlh.r-no
        AND fg-rcpth.i-no         EQ bf-fg-bin.i-no
        AND fg-rcpth.job-no       EQ bf-fg-bin.job-no
        AND fg-rcpth.job-no2      EQ bf-fg-bin.job-no2
        AND fg-rcpth.rita-code    EQ fg-rdtlh.rita-code
        AND fg-rcpth.trans-date   LE vdat 
      USE-INDEX r-no

      BREAK BY fg-rcpth.trans-date DESCENDING
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no:

    v-qty = v-qty - fg-rdtlh.qty.
    IF fg-rcpth.trans-date NE ? THEN
        v-last-date-found = fg-rcpth.trans-date.

    IF v-qty LE 0 THEN DO:
      out-rec-date = fg-rcpth.trans-date.
      LEAVE.
    END.

  END.
  IF out-rec-date = ? AND v-last-date-found NE ? THEN
      out-rec-date = v-last-date-found.

  IF out-rec-date = ? AND bf-fg-bin.tag GT "" THEN DO:
 
      FOR EACH fg-rdtlh
          WHERE fg-rdtlh.company      EQ bf-fg-bin.company
            AND fg-rdtlh.tag          EQ bf-fg-bin.tag
            AND fg-rdtlh.rita-code    EQ "R"
          USE-INDEX tag NO-LOCK,
    
          FIRST fg-rcpth NO-LOCK
          WHERE fg-rcpth.r-no         EQ fg-rdtlh.r-no
            AND fg-rcpth.i-no         EQ bf-fg-bin.i-no
            AND fg-rcpth.job-no       EQ bf-fg-bin.job-no
            AND fg-rcpth.job-no2      EQ bf-fg-bin.job-no2
            AND fg-rcpth.rita-code    EQ fg-rdtlh.rita-code
            AND fg-rcpth.trans-date   LE vdat 
          USE-INDEX r-no
    
          BREAK BY fg-rcpth.trans-date
                BY fg-rdtlh.trans-time
                BY fg-rcpth.r-no:
    
        IF fg-rcpth.trans-date NE ? THEN DO:
          v-last-date-found = fg-rcpth.trans-date.
          LEAVE.
        END.
                
      END.
   
  END.

  IF out-rec-date = ? AND v-last-date-found NE ? THEN
    out-rec-date = v-last-date-found.

END PROCEDURE.
