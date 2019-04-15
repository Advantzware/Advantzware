  
DEF INPUT-OUTPUT PARAM io-rowid AS ROWID NO-UNDO.
DEF INPUT PARAMETER iplCheckInventory AS LOG NO-UNDO.
DEF INPUT PARAMETER ipxRMINo LIKE job-mat.rm-i-no NO-UNDO.

def var k_frac     as   dec   init 6.25 no-undo.
{sys/inc/var.i SHARED}

DEF NEW SHARED BUFFER xest FOR est.
DEF NEW SHARED BUFFER xef  FOR ef.
DEF NEW SHARED BUFFER xeb  FOR eb.


DEF BUFFER b-item FOR item.
DEF BUFFER b-job-mat FOR job-mat.
DEF BUFFER bf-job-mch FOR job-mch.
DEF BUFFER bf-job-mch2 FOR job-mch.
DEF BUFFER bf-job-mat FOR job-mat.
DEF BUFFER bf-item FOR ITEM.
DEF BUFFER bf-mach FOR mach.
DEF BUFFER bff-item FOR ITEM.

DEF VAR lv-n-up LIKE job-mat.n-up NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR lInsufficientQty AS LOG NO-UNDO.
DEF VAR lv-est-type LIKE est.est-type NO-UNDO.
DEF VAR dNewSheetQty AS DECIMAL NO-UNDO.
DEF VAR dNewSheetTotQty AS DECIMAL NO-UNDO.
DEF VAR cRtnChar AS CHARACTER NO-UNDO.
DEF VAR lRecFound AS LOGICAL NO-UNDO .
DEF VAR lShtcalcWarm-log AS LOGICAL NO-UNDO .
DEFINE VARIABLE cRMino AS CHARACTER NO-UNDO .


{cec/bestfitc.i SHARED}
{sys/inc/f16to32.i}
  
FIND job-mat WHERE ROWID(job-mat) EQ io-rowid NO-LOCK NO-ERROR.

io-rowid = ?.
cRMino = IF AVAIL job-mat THEN job-mat.rm-i-no ELSE "" .

IF AVAIL job-mat THEN
FIND FIRST job NO-LOCK
    WHERE job.company      EQ job-mat.company
      AND job.job          EQ job-mat.job
      AND job.job-no       EQ job-mat.job-no
      AND job.job-no2      EQ job-mat.job-no2
      AND TRIM(job.est-no) NE ""
    NO-ERROR.
              
IF AVAIL job THEN DO:
  FOR EACH xest NO-LOCK
      WHERE xest.company EQ job.company
        AND xest.est-no  EQ job.est-no,
      FIRST xef NO-LOCK
      WHERE xef.company EQ xest.company
        AND xef.est-no  EQ xest.est-no
        AND xef.form-no EQ job-mat.frm
        AND xef.board   NE "",
      FIRST b-item NO-LOCK
      WHERE b-item.company EQ xef.company
        AND b-item.i-no    EQ xef.board
        AND CAN-FIND(FIRST item
                     WHERE item.company  EQ job-mat.company
                       AND item.i-no     EQ job-mat.rm-i-no
                       AND item.mat-type EQ b-item.mat-type),
      FIRST xeb NO-LOCK
      WHERE xeb.company EQ xef.company
        AND xeb.est-no  EQ xef.est-no
        AND xeb.form-no EQ xef.form-no:
   
    RUN cec/bestfitc.p ("", job-mat.qty * job-mat.n-up, job-mat.qty-uom, ipxRMINo, cRMino).

    FIND FIRST tt-ef NO-ERROR.
    FIND FIRST tt-eb NO-ERROR.

    RELEASE item.

    IF AVAIL tt-ef AND AVAIL tt-eb THEN
    FIND FIRST item
        WHERE item.company EQ job-mat.company
          AND item.i-no    EQ tt-ef.board
        NO-LOCK NO-ERROR.

    IF AVAIL item THEN DO:
      FIND FIRST tt-report
          WHERE tt-report.key-02 EQ item.i-no
          NO-ERROR.
      
      lv-n-up = 0.
      
      IF xest.est-type EQ 2 OR xest.est-type EQ 6 THEN
      FOR EACH reftable NO-LOCK
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.val[12]  EQ job-mat.frm:
        lv-n-up = lv-n-up + reftable.val[11].
      END.

      ELSE
      FOR EACH job-hdr
          WHERE job-hdr.company EQ job.company
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            AND job-hdr.frm     EQ job-mat.frm
          NO-LOCK:
        lv-n-up = lv-n-up + job-hdr.n-on.  
      END.

      IF lv-n-up EQ 0 THEN lv-n-up = 1.

      lv-n-up = lv-n-up * (IF tt-ef.n-out   EQ 0 THEN 1 ELSE tt-ef.n-out  ) *
                          (IF tt-ef.n-out-l EQ 0 THEN 1 ELSE tt-ef.n-out-l) *
                          (IF tt-ef.n-out-d EQ 0 THEN 1 ELSE tt-ef.n-out-d).
      lInsufficientQty = NO.
      IF AVAIL tt-report AND tt-reqs GT tt-onhs AND iplCheckInventory THEN
        MESSAGE "Material is insufficient for Job. " +
                "Import material to job with only Quantity On-hand?"
/*                 + "[Note: Not On-Hand]?" */
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE lInsufficientQty.

      IF lInsufficientQty AND iplCheckInventory THEN DO:
        /*IF b-item.i-code EQ "E" THEN*/  /* Ticket - 34285 */
        FIND b-job-mat EXCLUSIVE-LOCK WHERE ROWID(b-job-mat) EQ ROWID(job-mat) NO-ERROR.

        IF job-mat.qty-uom NE "EA" THEN
          RUN sys/ref/convquom.p("EA",
                                 job-mat.qty-uom,
                                 job-mat.basis-w,
                                 job-mat.len,
                                 job-mat.wid,
                                 item.s-dep,
                                 tt-onhs,
                                 OUTPUT tt-onhs).

        IF AVAIL b-job-mat THEN DO:
          ASSIGN
           b-job-mat.qty     = b-job-mat.qty - (tt-onhs * (lv-n-up / b-job-mat.n-up))
           b-job-mat.qty-all = b-job-mat.qty.
           {sys/inc/roundup.i b-job-mat.qty}
           {sys/inc/roundup.i b-job-mat.qty-all}
          CREATE job-mat.
          BUFFER-COPY b-job-mat EXCEPT rec_key TO job-mat.
        END.

        ASSIGN
         job-mat.qty     = tt-onhs
         job-mat.qty-all = tt-onhs.
      END.


      FIND CURRENT job-mat .
      IF job-mat.sc-uom EQ job-mat.qty-uom THEN
        ld = job-mat.std-cost.
      ELSE
        RUN sys/ref/convcuom.p(job-mat.sc-uom,
                               job-mat.qty-uom,
                               job-mat.basis-w,
                               job-mat.len,
                               job-mat.wid,
                               item.s-dep,
                               job-mat.std-cost,
                               OUTPUT ld).
                                         
      ld = ld * job-mat.qty.
                    
      ASSIGN                 
       job-mat.rm-i-no = item.i-no
       job-mat.i-no    = item.i-no
       job-mat.basis-w = item.basis-w
       job-mat.qty     = IF NOT lInsufficientQty THEN job-mat.qty * job-mat.n-up ELSE job-mat.qty
       job-mat.n-up    = lv-n-up
       job-mat.qty     = IF NOT lInsufficientQty THEN job-mat.qty / job-mat.n-up ELSE job-mat.qty
       job-mat.qty-all = job-mat.qty
       job-mat.sc-uom  = item.cons-uom .
     
      /* Task 10091315 */
/*       if index("BAP",item.mat-type) gt 0 then do:                                     */
/*           assign                                                                      */
/*               job-mat.wid = IF item.r-wid NE 0 THEN ({sys/inc/k16v.i item.r-wid})     */
/*                             ELSE ({sys/inc/k16v.i item.s-wid})                        */
/*              job-mat.len = IF item.r-wid NE 0 THEN                                    */
/*                             job-mat.len ELSE ({sys/inc/k16v.i item.s-len}) .          */
/*       END.                                                                            */
/*                                                                                       */
/*       ELSE if index("1234",item.mat-type) gt 0 then do:                               */
/*               assign                                                                  */
/*                   job-mat.wid = IF item.r-wid NE 0 THEN ({sys/inc/k16v.i item.r-wid}) */
/*                                 ELSE ({sys/inc/k16v.i item.s-wid})                    */
/*                   job-mat.len = IF item.r-wid NE 0 THEN                               */
/*                             job-mat.len ELSE ({sys/inc/k16v.i item.s-len}).           */
/*           end.                                                                        */
/*                                                                                       */
/*       ELSE do:                                                                        */
          ASSIGN
              job-mat.wid     = IF item.r-wid NE 0 THEN
                                item.r-wid ELSE item.s-wid
              job-mat.len     = IF item.r-wid NE 0 THEN
                                   job-mat.len ELSE item.s-len.
/*       END.  /* Task 10091315 */ */
                     
      {sys/inc/roundup.i job-mat.qty}
      {sys/inc/roundup.i job-mat.qty-all}
     /*11061306 - Update Job Machine (routing) with any new sheet calc qtys*/
     /*Find new board qty*/
    dNewSheetTotQty = 0.
    FOR EACH bf-job-mat
        WHERE bf-job-mat.company EQ job-mat.company
          AND bf-job-mat.job     EQ job-mat.job
          AND bf-job-mat.job-no  EQ job-mat.job-no
          AND bf-job-mat.job-no2 EQ job-mat.job-no2
          AND bf-job-mat.frm     EQ job-mat.frm
        NO-LOCK,
        FIRST bf-item 
            WHERE bf-item.company EQ job-mat.company
              AND bf-item.i-no    EQ bf-job-mat.rm-i-no
              AND LOOKUP(bf-item.mat-type,"1,2,3,4,B,P,R") > 0
        NO-LOCK:
        dNewSheetQty = bf-job-mat.qty.
        IF bf-job-mat.qty-uom NE "EA" THEN
            RUN sys/ref/convquom.p(bf-job-mat.qty-uom,
                                   "EA",
                                   bf-job-mat.basis-w,
                                   bf-job-mat.len,
                                   bf-job-mat.wid,
                                   bf-job-mat.dep,
                                   bf-job-mat.qty,
                                   OUTPUT dNewSheetQty).
        dNewSheetTotQty = dNewSheetTotQty + dNewSheetQty.
    END.
     
     /*Update Sheeters machine qtys and times*/
    FOR EACH bf-job-mch 
        WHERE bf-job-mch.company EQ job-mat.company
          AND bf-job-mch.job     EQ job-mat.job
          AND bf-job-mch.job-no  EQ job-mat.job-no
          AND bf-job-mch.job-no2 EQ job-mat.job-no2
          AND bf-job-mch.frm     EQ job-mat.frm
        EXCLUSIVE-LOCK,
        FIRST bf-mach 
            WHERE bf-mach.company EQ job-mat.company
              AND bf-mach.m-code  EQ bf-job-mch.m-code
              AND (LOOKUP(bf-mach.dept[1],"CR,RC,RS") > 0
                   OR LOOKUP(bf-mach.dept[2],"CR,RC,RS") > 0
                   OR LOOKUP(bf-mach.dept[3],"CR,RC,RS") > 0)
        NO-LOCK:
        /*update run qty and run hours based on new qty*/
        bf-job-mch.run-qty = dNewSheetTotQty.
        IF bf-mach.therm AND bf-mach.p-type EQ "R"  /*Use Lineal Feet in Run Matrix?*/ THEN
            bf-job-mch.run-hr = dNewSheetTotQty * (job-mat.len / 12) / bf-job-mch.speed.
        ELSE
            bf-job-mch.run-hr = dNewSheetTotQty / bf-job-mch.speed.
        /*update run qty on any machines that precede the sheeter*/
        FOR EACH bf-job-mch2 
            WHERE bf-job-mch2.company EQ bf-job-mch.company
              AND bf-job-mch2.job     EQ bf-job-mch.job
              AND bf-job-mch2.job-no  EQ bf-job-mch.job-no
              AND bf-job-mch2.job-no2 EQ bf-job-mch.job-no2
              AND bf-job-mch2.frm     EQ bf-job-mch.frm
              AND bf-job-mch2.LINE    LT bf-job-mch.LINE
            EXCLUSIVE-LOCK:
            bf-job-mch2.run-qty = dNewSheetTotQty.
        END.
        LEAVE.
     END.
                
      ld = ld / job-mat.qty.
      IF ld = ? THEN ld = 0.

      IF job-mat.qty-uom EQ job-mat.sc-uom THEN
        job-mat.std-cost = ld.
      ELSE
        RUN sys/ref/convcuom.p(job-mat.qty-uom,
                               job-mat.sc-uom,
                               job-mat.basis-w,
                               job-mat.len,
                               job-mat.wid,
                               item.s-dep,
                               ld,
                               OUTPUT job-mat.std-cost).

      io-rowid = ROWID(job-mat).
    END.

    LEAVE.
  END.

  RELEASE xest.
  RELEASE xef.
  RELEASE xeb.
END.
