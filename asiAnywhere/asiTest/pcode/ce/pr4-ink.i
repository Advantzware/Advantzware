/* --------------------------------------------------- ce/pr4-ink.i 07/96 JLF */

  /*j = 0.
  for each est-op
      where est-op.company = xest.company
        AND est-op.est-no eq xest.est-no
        and (est-op.qty   eq v-op-qty OR xest.est-type NE 1)
        and est-op.s-num eq {1}.form-no
        and ((est-op.b-num eq {1}.blank-no and xest.est-type eq 3) or
             xest.est-type ne 3)
        and est-op.line  ge 500
        and est-op.dept  eq "PR"
      no-lock,
      first mach
      {sys/ref/mach.w}
        and mach.m-code eq est-op.m-code
      no-lock
      by est-op.line:

    assign
     v-col-p = 0
     j       = j + 1.

    do i = 1 to 20:
      if {1}.i-code2[i] ne "" and {1}.i-ps2[i] eq j then do:
        find first item
            where item.company eq {1}.company
              and item.i-no    eq {1}.i-code2[i]
            no-lock no-error.
        if avail item then do:
          find first ink
              where ink.i-code eq {1}.i-code2[i]
                and ink.i-dscr eq {1}.i-dscr2[i]
                and ink.snum   eq {1}.form-no
                and ink.bnum   eq {1}.blank-no
              no-error.

          if not avail ink then do:
            create ink.
            assign
             ink.id     = {1}.part-no
             ink.snum   = {1}.form-no
             ink.bnum   = {1}.blank-no
             ink.i-code = {1}.i-code2[i]
             ink.i-dscr = {1}.i-dscr2[i]
             ink.i-%    = {1}.i-%2[i]
             ink.i-qty  = mach.col-wastelb.
          end.

          v-col-p = v-col-p + 1.
        end.
      end.
    end.

    if v-col-p gt 0 then do:
      do i = 1 to 20:
        if {1}.i-code2[i] ne "" and {1}.i-ps2[i] eq j then do:
          find first item no-lock
              where item.company eq {1}.company
                and item.i-no    eq {1}.i-code2[i]
              no-error.

          find first ink
              where ink.i-code eq {1}.i-code2[i]
                and ink.i-dscr eq {1}.i-dscr2[i]
                and ink.snum   eq {1}.form-no
                and ink.bnum   eq {1}.blank-no
              no-error.
           
          if avail item and avail ink then
            ink.i-qty  = ink.i-qty + (mach.ink-waste / v-col-p) +
                         ((({1}.t-sqin - {1}.t-win) *
                           (est-op.num-sh * v-num-up) *
                           (if xef.n-out   eq 0 then 1 else xef.n-out) *
                           (if xef.n-out-l eq 0 then 1 else xef.n-out-l) *
                           ({1}.i-%2[i] / 100)) / item.yield).
        end.
      end.
    end.
  end.*/
  
DEF VAR v-t-win AS DEC DECIMALS 4 NO-UNDO.

DEF BUFFER b-item FOR ITEM.
DEF VAR v-li AS INT NO-UNDO.

      IF {1}.est-type EQ 1 THEN
      do v-li = 1 TO 4:
         find first b-item WHERE
              b-item.company EQ xef.company and
              b-item.i-no eq xef.leaf[v-li]
              no-lock no-error.
        
         if avail b-item and b-item.mat-type eq "W" and
            (xef.leaf-l[v-li] ne 0 and xef.leaf-w[v-li] ne 0) then
            DO:
               IF xef.leaf-bnum[v-li] EQ 0 THEN
                  v-t-win = v-t-win + (xef.leaf-l[v-li] * xef.leaf-w[v-li] / {1}.num-up).
               ELSE
                  v-t-win = v-t-win + (xef.leaf-l[v-li] * xef.leaf-w[v-li]).
            END.
      end.
      ELSE
         v-t-win = {1}.t-win.

      do i = 1 to 20:
        if {1}.i-code2[i] ne ""                           and
           can-find(first item
                    where item.company eq {1}.company
                      and item.i-no    eq {1}.i-code2[i]) then do:
          
          find first ink
              where ink.i-code eq {1}.i-code2[i]
                and ink.i-dscr eq {1}.i-dscr2[i]
                and ink.snum   eq {1}.form-no
                and ink.bnum   eq {1}.blank-no
              no-error.

          if not avail ink then do:
            create ink.
            assign
             ink.id     = {1}.part-no
             ink.snum   = {1}.form-no
             ink.bnum   = {1}.blank-no
             ink.i-code = {1}.i-code2[i]
             ink.i-dscr = {1}.i-dscr2[i].
          end.
        end.
      end.

      for each ink
          where ink.snum eq {1}.form-no
            and ink.bnum eq {1}.blank-no,
      
          first item
          where item.company eq {1}.company
            and item.i-no    eq ink.i-code
            and item.yield   gt 0
          no-lock:
          
        do i = 1 to 20:
          ASSIGN
             j = 0
             v-first-pass = YES.

          if {1}.i-code2[i] eq ink.i-code and
             {1}.i-dscr2[i] eq ink.i-dscr then
          for each est-op
              where est-op.company eq {1}.company
                and est-op.est-no  eq {1}.est-no
                and (est-op.qty    eq v-op-qty OR xest.est-type NE 1)
                and est-op.s-num   eq {1}.form-no
                and ((est-op.b-num eq {1}.blank-no and xest.est-type eq 3) or
                     xest.est-type ne 3)
                and est-op.line    ge 500
                and est-op.dept    eq "PR"
              no-lock,
              first mach
              where (mach.company = cocode and
       mach.loc     = locode)
                and mach.m-code eq est-op.m-code
              no-lock
              by est-op.line:
            j = j + 1.

            if {1}.i-ps2[i] eq j then do:
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
               ink.i-%    = {1}.i-%2[i]
               ink.i-qty  = ink.i-qty + (IF FIRST-OF(xeb.form-no) AND v-first-pass THEN mach.col-wastelb ELSE 0) +
                            ((({1}.t-sqin - v-t-win) *
                              (est-op.num-sh * v-num-up) *
                              (if xef.n-out   eq 0 then 1 else xef.n-out) *
                              (if xef.n-out-l eq 0 then 1 else xef.n-out-l) *
                              ({1}.i-%2[i] / 100)) / item.yield)
               v-first-pass = NO.
            end.
          end.
        end.
      end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
