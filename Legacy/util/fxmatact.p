
{sys/inc/var.i NEW SHARED}

DEF VAR out-cost AS DEC DECIMALS 10 NO-UNDO.


SESSION:SET-WAIT-STATE ("general").

FOR EACH mat-act,
    
    FIRST item
    WHERE item.company  EQ mat-act.company
      AND item.i-no     EQ mat-act.i-no
      AND item.mat-type EQ "B"
    NO-LOCK,

    FIRST job
    WHERE job.company EQ mat-act.company
      AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(mat-act.job-no))) +
                         TRIM(mat-act.job-no)
      AND job.job-no2 EQ mat-act.job-no2
    NO-LOCK,

    FIRST job-mat
    WHERE job-mat.company  EQ mat-act.company
      AND job-mat.job      EQ job.job
      AND job-mat.job-no   EQ mat-act.job-no
      AND job-mat.job-no2  EQ mat-act.job-no2
      AND job-mat.frm      EQ mat-act.s-num
      AND job-mat.blank-no EQ mat-act.b-num
      AND job-mat.i-no     EQ mat-act.i-no
    NO-LOCK:

  FOR EACH rm-rcpth
      WHERE rm-rcpth.company   EQ mat-act.company
        AND rm-rcpth.i-no      EQ mat-act.i-no
        AND rm-rcpth.job-no    EQ mat-act.job-no
        AND rm-rcpth.job-no2   EQ mat-act.job-no2
        AND rm-rcpth.post-date EQ mat-act.mat-date
        AND rm-rcpth.rita-code EQ "I"
      NO-LOCK,

      EACH rm-rdtlh
      WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
        AND rm-rdtlh.s-num     EQ mat-act.s-num
        AND rm-rdtlh.tag       EQ mat-act.tag
        AND rm-rdtlh.loc       EQ mat-act.loc
        AND rm-rdtlh.loc-bin   EQ mat-act.loc-bin
      NO-LOCK
      
      BREAK BY rm-rdtlh.b-num DESC:

    IF rm-rdtlh.b-num EQ mat-act.b-num OR LAST(rm-rdtlh.b-num) THEN DO:
      cocode = mat-act.company.

      RUN sys/ref/convcuom.p(rm-rcpth.pur-uom, job-mat.sc-uom,
                             job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                             rm-rdtlh.cost, OUTPUT out-cost).

      mat-act.cost = out-cost.
            
      RUN sys/ref/convcuom.p(rm-rcpth.pur-uom, job-mat.qty-uom,
                             job-mat.basis-w, job-mat.len, job-mat.wid, item.s-dep,
                             rm-rdtlh.cost, OUTPUT out-cost).

      mat-act.ext-cost = mat-act.qty * out-cost.

      LEAVE.
    END.
  END.
END.

SESSION:SET-WAIT-STATE ("").
