/* -------------------------------------------------------------------------- */

/*----------------------------------------------------------------------
Program     : fg/fg-calcbcst.p

Description : Recalc bin costs for fg items                      

Copyright(c): Advanced Software Services Inc. 2013
Author      : Wade Kaldawi
Created     : 03/08/2013
Notes       : Taken from utility that processes all items

------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions     ----------------------------------------- */


DEF INPUT PARAMETER ipItemfgRow AS ROWID NO-UNDO.

/* Local Variable Definitions ----------------------------------------- */
DEF VAR lv-cost LIKE fg-rdtlh.cost NO-UNDO EXTENT 5.
DEF VAR lv-uom LIKE fg-rcpth.pur-uom NO-UNDO.
DEF VAR v-len LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-wid LIKE po-ordl.s-len NO-UNDO.
DEF VAR v-dep LIKE po-ordl.s-len NO-UNDO. 
DEF VAR v-bwt LIKE po-ordl.s-len NO-UNDO.
DEF VAR li AS INT NO-UNDO.

/* Include Files              ----------------------------------------- */

/* Function Forwards          ----------------------------------------- */

/* ***************************  Main Block  *************************** */


FIND itemfg WHERE ROWID(itemfg) EQ ipItemfgRow NO-LOCK.

FOR EACH fg-rcpth 
  WHERE fg-rcpth.i-no EQ itemfg.i-no
    AND fg-rcpth.company EQ itemfg.company
    AND  (rita-code EQ 'T'        OR
         (rita-code EQ 'R' AND INT(po-no) NE 0) OR
         TRIM(job-no) NE "")
         NO-LOCK,
    EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
    BREAK BY fg-rdtlh.r-no DESC BY fg-rdtlh.rec_key:

  IF FIRST-OF(fg-rdtlh.r-no) THEN DO:
    ASSIGN
     v-bwt   = 0
     v-len   = itemfg.t-len
     v-wid   = itemfg.t-wid
     v-dep   = 0
     lv-uom  = "M"
     lv-cost = 0.

    FIND FIRST fg-bin
        WHERE fg-bin.company EQ fg-rcpth.company
          AND fg-bin.i-no    EQ fg-rcpth.i-no
          AND fg-bin.job-no  EQ fg-rcpth.job-no
          AND fg-bin.job-no2 EQ fg-rcpth.job-no2
          AND fg-bin.loc     EQ fg-rdtlh.loc
          AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
          AND fg-bin.tag     EQ fg-rdtlh.tag
        NO-ERROR.
    IF AVAIL fg-bin AND (fg-bin.std-tot-cost EQ 0 OR fg-bin.std-tot-cost EQ ? OR
                         fg-bin.std-mat-cost +
                         lv-cost[1] +
                         fg-bin.std-var-cost +
                         fg-bin.std-fix-cost EQ 0 OR
                         fg-bin.std-mat-cost +
                         lv-cost[1] +
                         fg-bin.std-var-cost +
                         fg-bin.std-fix-cost EQ ?) THEN DO:

      RELEASE po-ordl.

      IF INT(fg-rcpth.po-no) NE 0 THEN DO:
        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ fg-rcpth.company
              AND po-ordl.po-no     EQ INT(fg-rcpth.po-no)
              AND po-ordl.item-type EQ NO
              AND po-ordl.i-no      EQ fg-rcpth.i-no
            NO-LOCK NO-ERROR.

        IF NOT AVAIL po-ordl THEN
        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ fg-rcpth.company
              AND po-ordl.po-no     EQ INT(fg-rcpth.po-no)
              AND po-ordl.item-type EQ NO
            NO-LOCK NO-ERROR.
      END. /* PO number not blank */

      IF AVAIL po-ordl THEN
        ASSIGN
         v-len      = po-ordl.s-len
         v-wid      = po-ordl.s-wid
         lv-cost[2] = po-ordl.cost
         lv-cost[5] = po-ordl.cost
         lv-uom     = po-ordl.pr-uom.

      ELSE DO:
        FIND FIRST job-hdr
            WHERE job-hdr.company EQ fg-rcpth.company
              AND job-hdr.i-no    EQ fg-rcpth.i-no
              AND job-hdr.job-no  EQ fg-rcpth.job-no
              AND job-hdr.job-no2 EQ fg-rcpth.job-no2
            NO-LOCK NO-ERROR.

        IF NOT AVAIL job-hdr THEN DO:
          FIND FIRST job
              WHERE job.company EQ fg-rcpth.company
                AND job.job-no  EQ fg-rcpth.job-no
                AND job.job-no2 EQ fg-rcpth.job-no2
            NO-LOCK NO-ERROR.
          IF AVAIL job THEN
          FIND FIRST reftable
              WHERE reftable.reftable EQ "jc/jc-calc.p"
                AND reftable.company  EQ job.company
                AND reftable.loc      EQ ""
                AND reftable.code     EQ STRING(job.job,"999999999")
                AND reftable.code2    EQ fg-rcpth.i-no
              NO-LOCK NO-ERROR.
        END.

        IF AVAIL job-hdr AND job-hdr.std-tot-cost GT 0 THEN
          ASSIGN
           lv-cost[5] = job-hdr.std-tot-cost
           lv-cost[1] = job-hdr.std-lab-cost
           lv-cost[2] = job-hdr.std-mat-cost
           lv-cost[3] = job-hdr.std-var-cost
           lv-cost[4] = job-hdr.std-fix-cost.
        ELSE
        IF AVAIL reftable AND reftable.val[5] GT 0 THEN
          ASSIGN
           lv-cost[5] = reftable.val[5]
           lv-cost[1] = reftable.val[1]
           lv-cost[2] = reftable.val[2]
           lv-cost[3] = reftable.val[3]
           lv-cost[4] = reftable.val[4].
      END. /* not avail po-ordl */
 
      IF lv-uom NE itemfg.prod-uom THEN DO li = 1 TO 4:
        RUN custom/convcuom.p(fg-rcpth.company,
                              lv-uom, itemfg.prod-uom,                   
                              v-bwt, v-len, v-wid, v-dep,
                              lv-cost[li], OUTPUT lv-cost[li]).
      END.

      ASSIGN
       fg-bin.pur-uom      = itemfg.prod-uom
       fg-bin.std-tot-cost = lv-cost[5]
       fg-bin.std-lab-cost = lv-cost[1]
       fg-bin.std-mat-cost = lv-cost[2]
       fg-bin.std-var-cost = lv-cost[3]
       fg-bin.std-fix-cost = lv-cost[4].
    END. /* if avail fg-bin ... */

    lv-cost[5] = IF AVAIL fg-bin THEN fg-bin.std-tot-cost ELSE ?.
  END. /* first of r-no */

  
END. /* each fg-rcpth */
