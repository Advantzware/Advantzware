/* updapchk.p  Update ap-chk values */

{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

DEF VAR v-vend-no LIKE vend.vend-no NO-UNDO.
DEF VAR v-chkno LIKE ap-chk.check-no NO-UNDO.

REPEAT:

upd:
REPEAT:

  UPDATE v-vend-no SKIP
         v-chkno 
         WITH FRAME upd SIDE-LABEL CENTERED TITLE " Update AP Check ".

   FIND FIRST ap-chk WHERE ap-chk.company = gcompany AND
                        ap-chk.vend-no = v-vend-no AND
                        ap-chk.check-no = v-chkno NO-LOCK NO-ERROR.
   IF NOT AVAIL ap-chk THEN DO:
      MESSAGE "Invalid Check# for the vendor entered." VIEW-AS ALERT-BOX ERROR.
      NEXT upd.
   END.
   ELSE LEAVE upd.
END.

IF NOT AVAIL ap-chk THEN RETURN.

FIND CURRENT ap-chk EXCLUSIVE-LOCK.
DISPLAY ap-chk.check-no ap-chk.check-date
        ap-chk.check-amt
    WITH CENTERED FRAME det 1 COL ROW 7 TITLE " Check Information ".
UPDATE ap-chk.check-date ap-chk.check-amt WITH FRAME det.
MESSAGE "Are you sure you want to change Check values? " VIEW-AS ALERT-BOX WARNING
        BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN next.
ELSE do:
    DISPLAY ap-chk.check-no ap-chk.check-date
            ap-chk.check-amt WITH  FRAME det.
    UNDO, RETRY.
END.

END.

hide  ALL NO-PAUSE.
