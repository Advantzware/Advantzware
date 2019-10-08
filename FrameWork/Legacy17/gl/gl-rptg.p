/* --------------------------------------------------- gl/gl-rptg.p  8/94 Rd  */
/* g/l report maintenance                                                     */
/* -------------------------------------------------------------------------- */

define input parameter v-ip-recid as recid.
define input parameter v-ip-disp as log.

{sys/inc/var.i shared}

{gl/gl-fs.i}

DEF VAR month-hdr AS LOG NO-UNDO.
DEF VAR months-of-year AS CHAR NO-UNDO.
months-of-year = "JAN,FEB,MAR,APR,MAY,JUN,JUL,AUG,SEP,OCT,NOV,DEC".
DEF VAR li-month AS INT NO-UNDO.

def frame gl-rpth.
{gl/gl-rpth.i}

FOR EACH company
    WHERE LOOKUP(company.company,company-list) GT 0
       OR company.company EQ cocode
    NO-LOCK
    BY company.num-per DESC:
  LEAVE.
END.

find gl-rpt where recid(gl-rpt) = v-ip-recid no-lock no-error.
if not avail gl-rpt then return.
else assign v-rpt = gl-rpt.rpt.

ASSIGN
 tot-format  = "->>>,>>>,>>9.99"
 pct-format  = "->>,>>>,>>9.99%"
 pct-formats = "->>>9.99%"
 sul-format  = " --------------"
 sul-formats = " --------"
 dul-format  = " =============="
 dul-formats = " ========".

							  /* LINE 1 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 1 no-error.
   if avail gl-rpt then
    assign v-rpt-name = gl-rpt.dscr.
							  /* LINE 2 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 2 no-error.
   if avail gl-rpt then do i = 1 to 9:
      if substring(gl-rpt.flag,i,1) = "Y" then v-per[i] = true.
   end.

							  /* LINES 3 - 7 */
   do i = 1 to 5:
      find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
      gl-rpt.line = i + 2 no-lock no-error.
      if avail gl-rpt then v-hdr[i] = gl-rpt.dscr.
   end.

							  /* LINES 8 - 17 */
   do i = 1 to 9:
      find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
      gl-rpt.line = i + 7 no-error.
      if avail gl-rpt then v-ch[i] = gl-rpt.dscr.
   end.

   all-per = NO.
							  /* LINES 17 - 26 */
   do i = 1 to 9:
      find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
      gl-rpt.line = i + 16 no-error.
      if avail gl-rpt then v-ct[i] = gl-rpt.dscr.
      if v-onevar = 0 then
         v-onevar = (if input v-ct[i] = "Variance" then i else 0).
      ELSE if v-onevar ne 0 and v-twovar = 0 then
         v-twovar = (if input v-ct[i] = "Variance" and v-onevar ne 0
		             then i else 0).

      /* Is it the All Periods & YTD report */
      IF v-ct[i] EQ "All Per & YTD"                       AND
         PROGRAM-NAME(2) BEGINS "run-report gl/r-fnstmt." THEN DO:
        all-per = YES.
        LEAVE.
      END.
   end.

                              /* LINES 26 - 32 */
   do i = 1 to 6:
     find gl-rpt where gl-rpt.company = cocode and
	 gl-rpt.rpt = v-rpt and
	 gl-rpt.line = i + 25 no-error.
     if avail gl-rpt then v-vcol[i] = integer (substring (gl-rpt.flag,1,1)).
   end.

							  /* LINE 32 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 32 no-error.
   if avail gl-rpt then do:
     if gl-rpt.flag = "Y" then v-c-bs = true.
      else v-c-bs = false.
   end.

							  /* LINE 33 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 33 no-error.
   if avail gl-rpt then v-p-w = integer (gl-rpt.flag).

							  /* LINE 34 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 34 no-error.
   if avail gl-rpt then v-col-used = integer (gl-rpt.flag).

							  /* LINE 35 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 35 no-error.
   if avail gl-rpt then v-no-col = integer (gl-rpt.flag).

							  /* LINE 36 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 36 no-error.
   if avail gl-rpt then v-d-wid = integer(gl-rpt.flag).

							  /* LINES 37 - 49 */
   do i = 1 to 12:
      find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
      gl-rpt.line = i + 36 no-error.
      if avail gl-rpt then v-sub[i] = gl-rpt.dscr.
      else v-sub[i] = "".
   end.

							  /* LINE 49 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 49 no-error.
   if avail gl-rpt then do i = 1 to 12:
      v-sdol[i] = false.
      if substring(gl-rpt.flag,i,1) = "Y" then v-sdol[i] = true.
   end.

							  /* LINE 50 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 50 no-error.
   if avail gl-rpt then do i = 1 to 12:
      v-sper[i] = false.
      if substring(gl-rpt.flag,i,1) = "Y" then v-sper[i] = true.
   end.

							  /* LINE 51 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 51 no-error.
   if avail gl-rpt then do i = 1 to 12:
      v-shun[i] = false.
      if substring(gl-rpt.flag,i,1) = "Y" then v-shun[i] = true.
   end.

							  /* LINE 52 */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 52 no-error.
   if avail gl-rpt then v-page-length = integer(gl-rpt.flag).

  assign v-onevar = 0
	 v-twovar = 0.
   do i = 1 to 9:
     if v-onevar = 0 then
       v-onevar = (if v-ct[i] = "Variance" then i else 0).
     else if v-onevar ne 0 and v-twovar = 0 then
	   v-twovar = (if v-ct[i] = "Variance" and v-onevar ne 0 then
	    		   i else 0).
   end.

   IF all-per THEN DO:
     ASSIGN
      v-no-col       = company.num-per + 1
      v-d-wid        = IF v-d-wid GT 32 THEN 32 ELSE v-d-wid
      v-col-used     = v-d-wid + (v-no-col * 12)
      v-p-w          = 200
      tot-format     = "(>>,>>>,>>>)"
      pct-format     = "(>>,>>9.99)%"
      sul-format     = " ---------- "
      dul-format     = " ========== "
      month-hdr      = v-ch[1] BEGINS "Month"
      v-ch[v-no-col] = "YTD"
      v-ct[v-no-col] = "Curr YTD"
      v-vcol         = 0
      v-onevar       = 0
      v-twovar       = 0
      v-per          = NO.

     DO i = 1 TO v-no-col - 1:
       IF v-no-col EQ 13 AND month-hdr THEN DO:
         li-month = i.
         FIND FIRST period
             WHERE period.company EQ cocode
               AND period.yr      EQ YEAR(udate)
               AND period.pnum    EQ i
             NO-LOCK NO-ERROR.
         IF AVAIL period THEN DO:
           li-month = MONTH(period.pst) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN li-month = i.
         END.
         v-ch[i] = ENTRY(li-month,months-of-year).
       END.
       ELSE
         v-ch[i] = "Period " + TRIM(STRING(i,">>")).
       v-ct[i] = "Curr Per".
     END.
   END.

pause 0.

if v-ip-disp then do:
  find gl-rpt where recid(gl-rpt) = v-ip-recid no-lock no-error.
  display
     gl-rpt.rpt
     v-rpt-name
     v-hdr[1 for 5]
     v-c-bs
     v-p-w
     v-col-used
     v-no-col
     v-page-length
     v-d-wid
     v-ch[1 for 9]
     v-ct[1 for 9]
     v-onevar v-twovar
     v-vcol[1 for 6]  with frame gl-rpth.
end.


         
/* -------------------------------------------------------------------------- */
