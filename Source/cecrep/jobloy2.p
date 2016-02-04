/* ---------------------------------------------*/
/* cecrep/jobloy2.p                             */
/* ---------------------------------------------*/

  def input parameter v-recid  as   recid.
  DEF INPUT PARAMETER ip-frm-no AS INT NO-UNDO.

  DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.

  {sys/inc/VAR.i SHARED}
  {cecrep/jobtick.i "shared"}

  DEF VAR v-bn-note AS CHAR FORMAT "X(35)" NO-UNDO.
  DEF VAR v-notes AS CHAR EXTENT 8 NO-UNDO.

  DEF BUFFER b-job FOR job.

  find job-hdr where recid(job-hdr) eq v-recid no-lock no-error.

  FIND FIRST b-job WHERE
       b-job.company eq cocode AND
       b-job.job eq job-hdr.job AND
       b-job.job-no eq job-hdr.job-no AND
       b-job.job-no2 eq job-hdr.job-no2
       NO-LOCK NO-ERROR.

  IF AVAIL b-job THEN
  DO:
     FIND FIRST notes WHERE
          notes.rec_key = b-job.rec_key AND
          (notes.note_form_no = ip-frm-no OR notes.note_form_no = 0) AND
          notes.note_code EQ "BN"
          NO-LOCK NO-ERROR.
         
     IF AVAIL notes THEN
        v-bn-note = SUBSTRING(notes.note_text,1,35).
     ELSE
     DO:
        FIND FIRST reftable WHERE
             reftable.reftable = "STACK" and
             reftable.company = "" AND
             reftable.loc = "" AND
             reftable.CODE = xeb.stack-code
             NO-LOCK NO-ERROR.

        IF AVAIL reftable THEN
        DO:
           FIND FIRST notes WHERE
                notes.rec_key = reftable.rec_key
                NO-LOCK NO-ERROR.

           IF AVAIL notes THEN
              v-bn-note = SUBSTRING(notes.note_text,1,35).
        END.
     END.
  END.

/* gdm - 11180902 */
  FIND FIRST item NO-LOCK
    WHERE ITEM.company EQ xeb.company
      AND ITEM.i-no    EQ xeb.cas-no NO-ERROR.
/* gdm - 11180902 end */

  display "<P9><B>Stacking:</B><P8>" AT 1 SKIP
          "Pallet ID:" xeb.tr-no when avail xeb SKIP
          "# Per Bndl:"                 AT 4
/* gdm - 11180902 */
/*           xoe-ordl.cas-cnt when avail xoe-ordl @ xeb.cas-cnt */
/*           xeb.cas-cnt when avail xeb                         */
          IF AVAIL ITEM THEN ITEM.i-name ELSE "" FORMAT "x(25)"
/* gdm - 11180902 end */
          SKIP
          "# Per Unit:" AT 4 xeb.tr-cnt when avail xeb
          SKIP
          v-stackcode          AT 4    format "x(28)"  
          SKIP
          "Pallet:" AT 4
          trim(string({sys/inc/k16v.i xeb.tr-len},">,>>9")) + " x " +
          trim(string({sys/inc/k16v.i xeb.tr-wid},">,>>9"))
                              when avail xeb format "x(15)" SKIP
         v-bn-note AT 4 FORMAT "X(35)"
         with no-box no-labels frame m6 width 200 NO-ATTR-SPACE STREAM-IO.
