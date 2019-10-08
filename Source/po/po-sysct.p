/* -------------------------------------------------------po/po-sysct.p */
/* po system control                                                   */
/* ---------------------------------------------------------------------------*/

def shared var cocode as char format "x(3)" no-undo.

def shared var factor# as decimal no-undo.
def shared var v-default-gl-log as log no-undo.
def shared var v-default-gl-cha as cha no-undo.
def shared var v-po-qty as log initial true no-undo.
def shared var v-po-msf like sys-ctrl.int-fld no-undo.


{sys/inc/ap-gl#.i}
assign
 v-default-gl-log = ap-gl#-log
 v-default-gl-cha = ap-gl#-cha.

find sys-ctrl where sys-ctrl.company = cocode
                  and sys-ctrl.name = "poprint" 
              no-lock no-error.

factor# = if avail sys-ctrl and can-do("Premier,Middlesx,16th's",sys-ctrl.char-fld)
  then .16 else 1.
  
v-po-msf = if avail sys-ctrl then sys-ctrl.int-fld else 0.

{sys/inc/poqty.i}

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGPOCOST"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign sys-ctrl.company = cocode
   sys-ctrl.name     = "FGPOCOST"
   sys-ctrl.descrip  = "Update order entry cost for purchased finished goods?"
   sys-ctrl.log-fld    = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */

