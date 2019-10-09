
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


FIND fg-bin WHERE ROWID(fg-bin) EQ ip-rowid NO-ERROR.

IF NOT AVAIL fg-bin THEN RETURN.

IF fg-bin.std-tot-cost EQ ? THEN
  ASSIGN
   fg-bin.std-lab-cost = 0
   fg-bin.std-mat-cost = 0
   fg-bin.std-var-cost = 0
   fg-bin.std-fix-cost = 0.

FIND FIRST itemfg
    WHERE itemfg.company EQ fg-bin.company
      AND itemfg.i-no    EQ fg-bin.i-no
    NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN DO:
  RELEASE job-hdr.
  RELEASE reftable.

  IF fg-bin.job-no NE "" THEN DO:
    FIND FIRST job-hdr
        WHERE job-hdr.company EQ fg-bin.company
          AND job-hdr.job-no  EQ fg-bin.job-no
          AND job-hdr.job-no2 EQ fg-bin.job-no2
          AND job-hdr.i-no    EQ fg-bin.i-no
    USE-INDEX job-no NO-LOCK NO-ERROR.
    IF NOT AVAIL job-hdr OR 
       (AVAIL job-hdr AND (job-hdr.std-tot-cost = 0 OR job-hdr.std-tot-cost = ?)) 
    THEN DO:
      FIND FIRST job
          WHERE job.company EQ fg-bin.company
            AND job.job-no  EQ fg-bin.job-no
            AND job.job-no2 EQ fg-bin.job-no2
      NO-LOCK NO-ERROR.
      IF AVAIL job THEN
         FIND FIRST reftable
            WHERE reftable.reftable EQ "jc/jc-calc.p"
              AND reftable.company  EQ job.company
              AND reftable.loc      EQ ""
              AND reftable.code     EQ STRING(job.job,"999999999")
              AND reftable.code2    EQ fg-bin.i-no
         NO-LOCK NO-ERROR.
    END. /* IF NOT AVAIL job-hdr OR */
  END. /* IF fg-bin.job-no NE ""  */

  IF AVAIL job-hdr AND job-hdr.std-tot-cost GT 0 AND job-hdr.std-tot-cost NE ? THEN
  DO:
      IF AVAIL itemfg AND itemfg.prod-uom = "EA" THEN
         ASSIGN
            fg-bin.std-lab-cost = job-hdr.std-lab-cost / 1000
            fg-bin.std-mat-cost = job-hdr.std-mat-cost / 1000
            fg-bin.std-var-cost = job-hdr.std-var-cost / 1000
            fg-bin.std-fix-cost = job-hdr.std-fix-cost / 1000
            fg-bin.pur-uom      = itemfg.prod-uom.
      ELSE IF AVAIL itemfg AND itemfg.prod-uom = "M" THEN
         ASSIGN
            fg-bin.std-lab-cost = job-hdr.std-lab-cost
            fg-bin.std-mat-cost = job-hdr.std-mat-cost
            fg-bin.std-var-cost = job-hdr.std-var-cost
            fg-bin.std-fix-cost = job-hdr.std-fix-cost
            fg-bin.pur-uom      = itemfg.prod-uom.
  END. /* IF AVAIL job-hdr AND job-hdr.std-tot-cost GT 0 AND */
  ELSE IF AVAIL reftable AND reftable.val[5] GT 0 AND reftable.val[5] NE ? THEN DO:

     IF AVAIL itemfg AND itemfg.prod-uom = "EA" THEN
        ASSIGN
           fg-bin.std-lab-cost = reftable.val[1] / 1000
           fg-bin.std-mat-cost = reftable.val[2] / 1000
           fg-bin.std-var-cost = reftable.val[3] / 1000
           fg-bin.std-fix-cost = reftable.val[4] / 1000
           fg-bin.pur-uom      = itemfg.prod-uom.
     ELSE IF AVAIL itemfg AND itemfg.prod-uom = "M" THEN
        ASSIGN
           fg-bin.std-lab-cost = reftable.val[1]
           fg-bin.std-mat-cost = reftable.val[2]
           fg-bin.std-var-cost = reftable.val[3]
           fg-bin.std-fix-cost = reftable.val[4]
           fg-bin.pur-uom      = itemfg.prod-uom.
  END. /* ELSE IF AVAIL reftable AND reftable.val[5] GT 0 */
END. /* IF AVAIL itemfg THEN DO: */

IF fg-bin.std-tot-cost NE fg-bin.std-lab-cost + fg-bin.std-fix-cost +
                          fg-bin.std-var-cost + fg-bin.std-mat-cost 
THEN
   ASSIGN
      fg-bin.std-tot-cost = fg-bin.std-lab-cost + fg-bin.std-fix-cost +
                            fg-bin.std-var-cost + fg-bin.std-mat-cost
      fg-bin.avg-cost     = fg-bin.std-tot-cost
      fg-bin.last-cost    = fg-bin.std-tot-cost.
