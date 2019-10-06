
          assign
           v-sman-no = ""
           v-misc    = no.

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq {2}ar-invl.i-no
              no-lock no-error.

          if not avail itemfg then do:
            find first fgcat
                where fgcat.company eq cocode
                  and fgcat.glacc   eq {2}ar-invl.actnum
                no-lock no-error.
            v-misc = not avail fgcat.
          end.
          
          do i = 1 to 3:
            v-sman-no = if {2}ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                        else {2}ar-invl.sman[i].

            if v-sman-no   lt fsman                         or
               v-sman-no   gt tsman                         or
               (i ne 1 and
                (v-sman-no eq "" or {2}ar-invl.s-pct[i] eq 0)) then next.

            create b-tt-report.

            assign
             b-tt-report.term-id = ""
             b-tt-report.rec-id  = recid({1})
             b-tt-report.row-id  = ROWID({2}ar-invl)
             b-tt-report.key-01  = if avail itemfg then "1"
                                   else
                                   if v-misc then "2" else "3"
             b-tt-report.key-02  = v-sman-no.
          end.
          
