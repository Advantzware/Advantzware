
          assign
           v-sman-no = "".

          do i = 1 to 3:
            v-sman-no = if {3}ar-invl.sman[i] eq "" and i eq 1 then cust.sman
                        else {3}ar-invl.sman[i].

            if v-sman-no   lt fsman                         or
               v-sman-no   gt tsman                         or
               (i ne 1 and
                (v-sman-no eq "" or {3}ar-invl.s-pct[i] eq 0)) then next.

            create b-tt-report.

            assign
             b-tt-report.rec-id  = recid({2})
             b-tt-report.row-id  = ROWID({3}ar-invl)
             b-tt-report.key-02  = v-sman-no
             b-tt-report.DATE    = {1}.
          end.
          
