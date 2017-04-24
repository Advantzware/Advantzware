/* sys/inc/tssecure.i */
DEF VAR lv-tssecure AS LOG NO-UNDO.
DEF VAR lv-tssecure-val AS cha NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "TSSECURE"
      no-lock no-error.
if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "TSSECURE"
     sys-ctrl.log-fld  = yes
     sys-ctrl.char-fld = "password"
     sys-ctrl.descrip  = "Password Option to Show Labor Rates".

   /* MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld. */
end.
ASSIGN lv-tssecure = sys-ctrl.log-fld
       lv-tssecure-val = sys-ctrl.char-fld.
