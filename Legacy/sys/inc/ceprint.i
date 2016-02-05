/* sys/inc/ceprint.i*/
def var ceprint-log like sys-ctrl.log-fld NO-UNDO.
def var ceprint-int like sys-ctrl.int-fld NO-UNDO.
DEF VAR ceprint-char LIKE sys-ctrl.char-fld NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq g_company
                      and sys-ctrl.name    eq "CEPRINT" no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  ASSIGN sys-ctrl.company  = g_company
         sys-ctrl.name     = "CEPRINT"
         sys-ctrl.char-fld = "Segment"
         sys-ctrl.log-fld  = no
         sys-ctrl.descrip  = "Print Hardcopy is Segments?".
/*
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
*/      
end.
ASSIGN ceprint-log = sys-ctrl.log-fld
       ceprint-int = sys-ctrl.int-fld
       ceprint-char = sys-ctrl.char-fld.

