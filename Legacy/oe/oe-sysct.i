/* oe/oe-sysct.i */

DO TRANSACTION:
  {sys/inc/lastship.i}
  ASSIGN
   ld-lastship-dec = lastship-dec
   ld-lastship-cha = lastship-cha.
END.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "JOB#"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "JOB#"
   sys-ctrl.descrip = "Job Number Creation Method from Estimate or Order"
   sys-ctrl.log-fld = no.
  message "Sys-ctrl record NOT found. " sys-ctrl.descrip
          update sys-ctrl.char-fld.
end.
v-job-meth = sys-ctrl.char-fld.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "OECOUNT"
          no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OECOUNT"
   sys-ctrl.descrip = "Default Order Entry Count to Case/Bundle Count?"
   sys-ctrl.log-fld = yes.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-oecount = sys-ctrl.log-fld.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "FGOECOST"
     no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "FGOECOST"
   sys-ctrl.descrip = "Order Entry FG Item Cost? Yes=Full No=Direct Factory"
   sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-full-cost = sys-ctrl.log-fld.

find first sys-ctrl where sys-ctrl.company eq cocode
       and sys-ctrl.name    eq "QUOPRICE"
       no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "QUOPRICE"
   sys-ctrl.descrip = "Default Price for Order from Last Quote"
   sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-quo-price = sys-ctrl.log-fld.

find first sys-ctrl where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGITEM#"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "FGITEM#"
   sys-ctrl.descrip = "Order Entry default FG Item Number from Estimate?"
   sys-ctrl.log-fld = yes.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 v-est-fg  = sys-ctrl.log-fld
 v-est-fg1 = sys-ctrl.char-fld.
