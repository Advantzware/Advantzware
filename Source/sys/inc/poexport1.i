
if poexport-cha eq "CorrTrim" then do:
  {sys/inc/corrtrim.i}
end.
else
if poexport-cha eq "Alliance" then do:
  {sys/inc/alliance.i}
end.
else
if poexport-cha eq "HRMS" then do:
  {sys/inc/hrms.i}
end.
else
if poexport-cha eq "CorSuply" then do:
  {sys/inc/corsuply.i}
end.
else
if poexport-cha eq "Corr-U-Kraft II" then do:
  {sys/inc/corkraft.i}
end.
else
if poexport-cha eq "Kiwi" then do:
  {sys/inc/kiwi.i}
end.
ELSE
if poexport-cha eq "CorrChoice" then do:
  {sys/inc/corrchoice.i}
end.

if avail sys-ctrl then
  assign
   v-out        = sys-ctrl.descrip
   poexport-log = sys-ctrl.log-fld and v-out ne "".
