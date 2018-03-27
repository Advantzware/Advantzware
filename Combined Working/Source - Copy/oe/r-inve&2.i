    
    ld-t[2] = 0.
    
    for each tt-report
        where tt-report.term-id eq ""
          and tt-report.key-01  eq "{1}"
        no-lock
        break by tt-report.key-02:

      find first account
          where account.company eq cocode
            and account.actnum  eq tt-report.key-02
          no-lock no-error.
      v-dscr = if avail account then account.dscr
               else "ACCOUNT NOT FOUND - " + TRIM("{2}").

      accumulate dec(tt-report.key-05) (total by tt-report.key-02).
      assign
       ld-t[1] = tt-report.weight / 2000
       ld-t[2] = ld-t[2] + ld-t[1].

      if v-gldetail then do:
        assign
         v-tmp-amt = dec(tt-report.key-05) * -1
         ld-t[1]   = ld-t[1] * -1
         ld-pton   = v-tmp-amt / ld-t[1].

        IF ld-pton EQ ? THEN ld-pton = 0.

        display tt-report.key-02       @ account.actnum
                v-dscr
                int(tt-report.key-03)  @ inv-head.inv-no
                tt-report.key-04       @ inv-line.i-no
                v-tmp-amt
                ld-pton FORMAT "->>>>>>9.999" WHEN tb_ton 
                ld-t[1] WHEN tb_ton
            with frame gl-det.
        down with frame gl-det.
      end.

      if last-of(tt-report.key-02) then do:
        assign
          v-disp-actnum = tt-report.key-02
          v-disp-amt    =
                    (accumulate total by tt-report.key-02 dec(tt-report.key-05)) * -1
          v-gl-sales    =
                    (accumulate total by tt-report.key-02 dec(tt-report.key-05)) * -1
          ld-t[2]       = ld-t[2] * -1
          ld-pton       = v-disp-amt / ld-t[2].

        IF ld-pton EQ ? THEN ld-pton = 0.

        if v-gldetail then do:
          put v-disp-amt to 128.
          IF tb_ton THEN DO:
            ld-pton = v-disp-amt / ld-t[2].
            IF ld-pton EQ ? THEN ld-pton = 0.
            PUT ld-pton FORMAT "->>>>>>9.999" TO 141 ld-t[2] TO 151 SKIP(1).
          END.
          ELSE PUT skip.
        END.

        else do:
          display v-disp-actnum
                  v-dscr
                  tran-date
                  v-disp-amt
                  ld-pton FORMAT "->>>>>>9.999" WHEN tb_ton 
                  ld-t[2] WHEN tb_ton
              with frame gl-sum.
          down with frame gl-sum.
        end.

        assign
         v-balance = v-balance + v-gl-sales
         ld-t[3]   = ld-t[3] + ld-t[2]
         ld-t[2]   = 0.
      end. /* last actnum */
    end. /* each tt-report */
