/* updapinv.p  Update ap-inv values */
DEF VAR v-comp LIKE ap-inv.company INIT "001" NO-UNDO.
DEF VAR v-vend-no LIKE vend.vend-no NO-UNDO.
DEF VAR v-invno LIKE ap-inv.inv-no NO-UNDO.

REPEAT:

upd:
REPEAT:

  UPDATE v-comp skip
         v-vend-no SKIP
         v-invno 
         WITH FRAME upd SIDE-LABEL CENTERED TITLE " Update AP Invoice" .

   FIND FIRST ap-inv WHERE ap-inv.company = v-comp AND
                        ap-inv.vend-no = v-vend-no AND
                        ap-inv.inv-no = v-invno NO-LOCK NO-ERROR.
   IF NOT AVAIL ap-inv THEN DO:
      MESSAGE "Invalid Invoice# for the vendor entered." VIEW-AS ALERT-BOX ERROR.
      NEXT upd.
   END.
   ELSE LEAVE upd.
END.

IF NOT AVAIL ap-inv THEN RETURN.

FIND CURRENT ap-inv EXCLUSIVE-LOCK.
DISPLAY ap-inv.inv-no ap-inv.inv-date ap-inv.due-date
        ap-inv.gross ap-inv.net ap-inv.paid ap-inv.due
    WITH CENTERED FRAME det 1 COL ROW 7 TITLE " Check Information ".
UPDATE ap-inv.inv-date ap-inv.due-date
        ap-inv.gross ap-inv.net ap-inv.paid ap-inv.due
       WITH FRAME det.
MESSAGE "Are you sure you want to change Check values? " VIEW-AS ALERT-BOX WARNING
        BUTTON YES-NO UPDATE ll-ans AS LOG.
IF ll-ans THEN next.
ELSE do:
    DISPLAY ap-inv.inv-no ap-inv.inv-date ap-inv.due-date
            ap-inv.gross ap-inv.net ap-inv.paid ap-inv.due
            WITH  FRAME det.
    UNDO, RETRY.
END.

END.

hide  ALL NO-PAUSE.
