/* ------------------------------------------------- cec/pr42-ink.i 07/96 JLF */

      do i = 1 to 10:
        if xeb.i-code[i] ne ""                           and
           can-find(first item
                    where item.company eq xeb.company
                      and item.i-no    eq xeb.i-code[i]) then do:
          
          find first ink
              where ink.i-code eq xeb.i-code[i]
                and ink.i-dscr eq xeb.i-dscr[i]
                and ink.snum   eq xeb.form-no
                and ink.bnum   eq xeb.blank-no
              no-error.

          if not avail ink then do:
            create ink.
            assign
             ink.id     = xeb.part-no
             ink.snum   = xeb.form-no
             ink.bnum   = xeb.blank-no
             ink.i-code = xeb.i-code[i]
             ink.i-dscr = xeb.i-dscr[i].
          end.
        end.
      end.

      for each ink
          where ink.snum eq xeb.form-no
            and ink.bnum eq xeb.blank-no,
      
          first item
          where item.company eq cocode
            and item.i-no    eq ink.i-code
          no-lock:
          
        do i = 1 to 10:
          ASSIGN
             j = 0
             v-first-pass = YES.

          if xeb.i-code[i] eq ink.i-code and
             xeb.i-dscr[i] eq ink.i-dscr then
          for each est-op
              where est-op.company eq xeb.company
                and est-op.est-no  eq xeb.est-no
                and (est-op.qty    eq v-op-qty or xeb.est-type ge 7)
                and est-op.s-num   eq xeb.form-no
                and est-op.line    ge 500
                and (est-op.dept   eq "PR" or
                     (est-op.dept  eq "CT" and item.mat-type eq "V"))
              no-lock,
              first mach
              {sys/ref/machW.i}
                and mach.m-code eq est-op.m-code
              no-lock
              by est-op.line:
            j = j + 1.
            if xeb.i-ps[i] eq j then do:
              FIND FIRST tt-ink
                  WHERE tt-ink.i-code EQ ink.i-code
                    AND tt-ink.i-dscr EQ ink.i-dscr
                    AND tt-ink.pass   EQ j
                  NO-ERROR.

              IF NOT AVAIL tt-ink THEN DO:
                CREATE tt-ink.
                ASSIGN
                 tt-ink.i-code = ink.i-code
                 tt-ink.i-dscr = ink.i-dscr
                 tt-ink.pass   = j.
              END.

              assign
               ink.i-%    = xeb.i-%[i]
               ink.i-qty  = ink.i-qty + (IF v-first-pass THEN mach.col-wastelb ELSE 0) +
                            (((xeb.t-sqin - xeb.t-win) *
                              (est-op.num-sh * xeb.num-up * v-n-out) *
                              (xeb.i-%[i] / 100)) / item.yield)
               v-first-pass = NO.
            end.
          end.
        end.
      end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
