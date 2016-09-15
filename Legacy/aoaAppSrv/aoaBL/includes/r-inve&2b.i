    
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
      cAccountDscr = if avail account then account.dscr
               else "ACCOUNT NOT FOUND - " + TRIM("{2}").

      accumulate dec(tt-report.key-05) (total by tt-report.key-02).
      assign
       ld-t[1] = tt-report.weight / 2000
       ld-t[2] = ld-t[2] + ld-t[1].

      if lGLReportDetail then do:
        assign
         v-tmp-amt = dec(tt-report.key-05) * -1
         ld-t[1]   = ld-t[1] * -1
         ld-pton   = v-tmp-amt / ld-t[1].

        IF ld-pton EQ ? THEN ld-pton = 0.

/*        display tt-report.key-02       @ account.actnum */
/*                cAccountDscr                                  */
/*                int(tt-report.key-03)  @ inv-head.inv-no*/
/*                tt-report.key-04       @ inv-line.i-no  */
/*                v-tmp-amt                               */
/*                ld-pton WHEN lPrintTon                     */
/*                ld-t[1] WHEN lPrintTon                     */
/*            with frame gl-det.                          */
      end.

      if last-of(tt-report.key-02) then do:
        assign
          v-disp-actnum = tt-report.key-02
          v-disp-amt    =
                    (accumulate total by tt-report.key-02 dec(tt-report.key-05)) * -1
          dGLSales    =
                    (accumulate total by tt-report.key-02 dec(tt-report.key-05)) * -1
          ld-t[2]       = ld-t[2] * -1
          ld-pton       = v-disp-amt / ld-t[2].

        IF ld-pton EQ ? THEN ld-pton = 0.

        if lGLReportDetail then do:
/*          put v-disp-amt to 128.*/

          IF lPrintTon THEN DO:
            ld-pton = v-disp-amt / ld-t[2].
            IF ld-pton EQ ? THEN ld-pton = 0.
/*            PUT ld-pton TO 138 ld-t[2] TO 148 SKIP(1).*/
          END.
        END.

        else do:
/*          display v-disp-actnum      */
/*                  cAccountDscr             */
/*                  tran-date          */
/*                  v-disp-amt         */
/*                  ld-pton WHEN lPrintTon*/
/*                  ld-t[2] WHEN lPrintTon*/
/*              with frame gl-sum.     */
        end.

        assign
         v-balance = v-balance + dGLSales
         ld-t[3]   = ld-t[3] + ld-t[2]
         ld-t[2]   = 0.
      end. /* last actnum */
    end. /* each tt-report */
