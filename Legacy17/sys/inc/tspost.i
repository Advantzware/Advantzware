/* sys/inc/tspost.i */
  
DEF VAR v-tspost AS LOG NO-UNDO.
DEF VAR v-tspost-val AS cha NO-UNDO.

find first sys-ctrl
      where sys-ctrl.company eq g_company
        and sys-ctrl.name    eq "TSPOST"
      no-lock no-error.
if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = g_company
     sys-ctrl.name     = "TSPOST"
     sys-ctrl.log-fld  = no
     sys-ctrl.char-fld = "STANDARD"
     sys-ctrl.descrip  = "Post ACTUAL or STANDARD Direct Labor Costs?".

   /* MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld. */
end.
ASSIGN v-tspost = sys-ctrl.log-fld
       v-tspost-val = sys-ctrl.char-fld.
