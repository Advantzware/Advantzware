
def var v-label  as char format "x(12)" extent 6.
def var v-label1 as char format "x(41)".

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "MACHFILE"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "MACHFILE"
   sys-ctrl.descrip = "Reverse W & L Labels in Machine File?".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.

assign
 v-label[1] = trim(string(sys-ctrl.log-fld,"Width/Length")) + ":"
 v-label[2] = trim(string(sys-ctrl.log-fld,"Length/Width")) + ":"
 v-label[3] = "Trim " + string(sys-ctrl.log-fld,"Wid/Len") + ":"
 v-label[4] = "Trim " + string(sys-ctrl.log-fld,"Len/Wid") + ":"
 v-label[5] = "Panel " + string(sys-ctrl.log-fld,"Wid/Len") + ":"
 v-label[6] = "Panel " + string(sys-ctrl.log-fld,"Len/Wid") + ":".

do i = 1 to 6:
  v-label[i] = fill(" ",12 - length(trim(v-label[i]))) + trim(v-label[i]).
end.
