/* -------------------------------------------------- pc/pcprdd4u.p 07/03 JLF */

DEF INPUT PARAM v-rowid AS ROWID.

{sys/inc/var.i SHARED}.
{sys/form/s-top.f}

DEF VAR v-est-type LIKE est.est-type NO-UNDO.
DEF VAR v-assembled AS LOG NO-UNDO.
DEF VAR ll-set-mach AS LOG NO-UNDO.
DEF VAR next-j-no   AS INT NO-UNDO.
DEF BUFFER b-mach FOR mach.

{pc/pcprdd4u.i}

FOR EACH tt-job-hdr:
  DELETE tt-job-hdr.
END.

FIND pc-prdd WHERE ROWID(pc-prdd) EQ v-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL pc-prdd THEN RETURN.    

{sys/inc/autopdc.i}
{sys/inc/tspostfg.i}
{sys/inc/fgrecpt.i}

IF fgrecpt-char EQ "AUTOPOST" THEN DO:
  FIND FIRST mach
      {sys/look/machW.i}
        AND mach.m-code EQ pc-prdd.m-code
      NO-LOCK.

  FIND FIRST job
      WHERE job.company EQ pc-prdd.company
        AND job.job-no  EQ pc-prdd.job-no
        AND job.job-no2 EQ pc-prdd.job-no2
      USE-INDEX job-no NO-LOCK NO-ERROR.

  FIND FIRST est
      WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no
      NO-LOCK NO-ERROR.
  v-est-type = IF AVAIL est THEN est.est-type ELSE 1.

  IF v-est-type GT 4 THEN v-est-type = v-est-type - 4.

  v-assembled = NO.

  IF v-est-type EQ 2 THEN
  FOR EACH job-hdr
      WHERE job-hdr.company  EQ cocode
        AND job-hdr.job      EQ pc-prdd.job
        AND job-hdr.job-no   EQ pc-prdd.job-no
        AND job-hdr.job-no2  EQ pc-prdd.job-no2
      NO-LOCK,
      FIRST itemfg
      WHERE itemfg.company   EQ cocode
        AND itemfg.i-no      EQ job-hdr.i-no
        AND itemfg.isaset
      NO-LOCK:

    ASSIGN
     v-assembled = YES
     v-est-type  = 4
     ll-set-mach = INDEX("AP",mach.p-type) GT 0.

    LEAVE.
  END.

  FOR EACH job-mch
      WHERE job-mch.company   EQ pc-prdd.company
        AND job-mch.job       EQ pc-prdd.job
        AND job-mch.job-no    EQ pc-prdd.job-no
        AND job-mch.job-no2   EQ pc-prdd.job-no2
        AND (job-mch.frm      EQ pc-prdd.frm OR v-est-type EQ 2)
        AND LOOKUP(job-mch.m-code,autopdc)
                              EQ 0
        AND autopdc           NE "*"
      USE-INDEX line-idx NO-LOCK,

      FIRST b-mach
      WHERE b-mach.company EQ job-mch.company
        AND b-mach.loc     EQ locode
        AND b-mach.m-code  EQ job-mch.m-code
        AND ((INDEX("AP",b-mach.p-type) LE 0 AND NOT ll-set-mach) OR
             (INDEX("AP",b-mach.p-type) GT 0 AND ll-set-mach))
      NO-LOCK

      BY job-mch.line DESC:
    LEAVE.
  END.

  IF AVAIL job-mch AND job-mch.blank-no NE 0 THEN
  FOR EACH job-mch
      WHERE job-mch.company   EQ pc-prdd.company
        AND job-mch.job       EQ pc-prdd.job
        AND job-mch.job-no    EQ pc-prdd.job-no
        AND job-mch.job-no2   EQ pc-prdd.job-no2
        AND (job-mch.frm      EQ pc-prdd.frm OR v-est-type EQ 2)
        AND LOOKUP(job-mch.m-code,autopdc)
                              EQ 0
        AND autopdc           NE "*"
        AND (job-mch.blank-no EQ pc-prdd.blank-no OR
             mach.p-type      NE "B"              OR
             v-est-type       EQ 1)
      USE-INDEX line-idx NO-LOCK,

      FIRST b-mach
      WHERE b-mach.company EQ job-mch.company
        AND b-mach.loc     EQ locode
        AND b-mach.m-code  EQ job-mch.m-code
        AND ((INDEX("AP",b-mach.p-type) LE 0 AND NOT ll-set-mach) OR
             (INDEX("AP",b-mach.p-type) GT 0 AND ll-set-mach))
      NO-LOCK

      BY job-mch.line DESC:
    LEAVE.
  END.

  IF AVAIL job-mch                              AND
            /*pc-prdd.code     EQ "RUN"                */
      CAN-FIND(FIRST job-code WHERE job-code.CODE = pc-prdd.CODE AND job-code.cat = "RUN")
       AND
     ((job-mch.m-code EQ pc-prdd.m-code AND
       job-mch.pass   EQ pc-prdd.pass)      OR
      (AVAIL mach                       AND
       (job-mch.dept  EQ mach.dept[1] OR
        job-mch.dept  EQ mach.dept[2] OR
        job-mch.dept  EQ mach.dept[3] OR
        job-mch.dept  EQ mach.dept[4])))        THEN DO:
     
    IF v-assembled AND NOT ll-set-mach THEN
    FOR EACH reftable
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job-mch.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job-mch.job,"999999999")
          AND reftable.val[12]  EQ job-mch.frm
          AND (reftable.val[13] EQ job-mch.blank-no OR
               job-mch.blank-no EQ 0)
        NO-LOCK:
      FIND LAST tt-job-hdr NO-ERROR.
      IF AVAIL tt-job-hdr THEN
          next-j-no = tt-job-hdr.j-no + 1.
      
      CREATE tt-job-hdr.
      ASSIGN
       tt-job-hdr.company      = reftable.company
       tt-job-hdr.job          = job-mch.job
       tt-job-hdr.j-no         = next-j-no
       tt-job-hdr.job-no       = job-mch.job-no
       tt-job-hdr.job-no2      = job-mch.job-no2
       tt-job-hdr.frm          = reftable.val[12]
       tt-job-hdr.blank-no     = reftable.val[13]
       tt-job-hdr.i-no         = reftable.code2
       tt-job-hdr.std-lab-cost = reftable.val[1]
       tt-job-hdr.std-mat-cost = reftable.val[2]
       tt-job-hdr.std-var-cost = reftable.val[3]
       tt-job-hdr.std-fix-cost = reftable.val[4]
       tt-job-hdr.std-tot-cost = reftable.val[5]
       tt-job-hdr.last-mach    = job-mch.m-code EQ pc-prdd.m-code AND
                                 job-mch.pass   EQ pc-prdd.pass.
    END.

    FIND FIRST tt-job-hdr NO-ERROR.
    IF NOT AVAIL tt-job-hdr THEN
    FOR EACH job-hdr
        WHERE job-hdr.company   EQ cocode
          AND job-hdr.job-no    EQ job-mch.job-no
          AND job-hdr.job-no2   EQ job-mch.job-no2
          AND (job-hdr.frm      EQ pc-prdd.frm OR
               v-est-type       EQ 2           OR
               ll-set-mach)
          AND (job-hdr.blank-no EQ job-mch.blank-no OR
               job-mch.blank-no EQ 0                OR
               ll-set-mach)
        NO-LOCK:
      CREATE tt-job-hdr.
      BUFFER-COPY job-hdr TO tt-job-hdr
      ASSIGN
       tt-job-hdr.last-mach = job-mch.m-code EQ pc-prdd.m-code AND
                              job-mch.pass   EQ pc-prdd.pass.
    END.
  END.
END.
