/* util/delapchk.p  Delete ap-chk for blank vend-no and 0 check amount */

DEF VAR v-vend-no LIKE ap-chk.vend-no NO-UNDO.
DEF VAR v-chkno LIKE ap-chk.check-no NO-UNDO.
DEF STREAM st-save .
DEF VAR lv-got-chk AS LOG NO-UNDO.

lv-got-chk = NO.
upd:
REPEAT:
   UPDATE /*v-vend-no */ SKIP
          v-chkno 
          WITH FRAME upd SIDE-LABEL CENTERED TITLE " Delete AP Check ".

   FIND FIRST ap-chk WHERE ap-chk.company = "001" AND
                        /*ap-chk.vend-no = v-vend-no AND */
                        ap-chk.check-no = v-chkno NO-LOCK NO-ERROR.
   IF NOT AVAIL ap-chk THEN DO:
      MESSAGE "Invalid Check# for the vendor entered." VIEW-AS ALERT-BOX ERROR.
      NEXT upd.
   END.
   ELSE do:
       lv-got-chk = YES.
       DISP ap-chk.check-no ap-chk.check-date
            ap-chk.vend-no LABEL "Vend#" ap-chk.check-amt.
       LEAVE upd.
   END.
END.
IF NOT lv-got-chk THEN RETURN.

MESSAGE "Are you sure you want to delete check without Vendor# and 0 Check amount?"
         VIEW-AS ALERT-BOX WARNING BUTTON YES-NO UPDATE ll-ans AS LOG.

IF ll-ans THEN DO:
   OUTPUT STREAM st-save TO value("c:\tmp\apchk.d" + STRING(TIME)).
   FOR EACH ap-chk /*WHERE vend-no = "" AND check-amt = 0 */
                  WHERE ap-chk.company = "001" /*ap-chk.vend-no = v-vend-no */
                        AND ap-chk.check-no = v-chkno:
       EXPORT STREAM st-save ap-chk.
       DISP ap-chk.check-no ap-chk.check-date
            ap-chk.vend-no ap-chk.check-amt.
       PAUSE 0.     
       DELETE ap-chk.
   END.

   OUTPUT CLOSE.
   MESSAGE "Process completed." VIEW-AS ALERT-BOX.
END.
