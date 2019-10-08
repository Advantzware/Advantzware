/*sys/inc/addnk1.p*/
DEF INPUT PARAMETER cocode     AS   CHAR              NO-UNDO.
DEF INPUT PARAMETER ip-name    LIKE sys-ctrl.NAME     NO-UNDO.
DEF INPUT PARAMETER ip-prompt  AS   LOG               NO-UNDO.
DEF INPUT PARAMETER ip-descrip LIKE sys-ctrl.descrip  NO-UNDO.
DEF INPUT PARAMETER v-nk1-cha like sys-ctrl.char-fld  NO-UNDO.
DEF INPUT PARAMETER v-nk1-int LIKE sys-ctrl.int-fld   NO-UNDO.
DEF INPUT PARAMETER v-nk1-log like sys-ctrl.log-fld   NO-UNDO.


find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq ip-name no-lock no-error.
                      
                      
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = ip-name
   sys-ctrl.log-fld  = v-nk1-log
   sys-ctrl.int-fld  = v-nk1-int
   sys-ctrl.char-fld = v-nk1-cha
   sys-ctrl.descrip  = ip-descrip.
  if ip-prompt then
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.



assign
 v-nk1-cha = sys-ctrl.char-fld
 v-nk1-log = sys-ctrl.log-fld
 v-nk1-int = sys-ctrl.int-fld.

