/* ------------------------------------------------- oe/oe-sysct.p 08/99 FWK  */
/* Order entry lines - System Control Finds                                   */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{oe/oe-sysct1.i}


{sys/inc/job#.i}
v-job-meth = job#-cha.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "QUOPRICE"
    no-lock no-error.
if not avail sys-ctrl then do:
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
assign
 v-quo-price-log = sys-ctrl.log-fld
 v-quo-price-int = sys-ctrl.int-fld
 v-quo-price-dec = sys-ctrl.dec-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGITEM#"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "FGITEM#"
   sys-ctrl.descrip = "Order entry default FG Item Number from Estimate?"
   sys-ctrl.log-fld = yes.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 v-est-fg  = sys-ctrl.log-fld
 v-est-fg1 = sys-ctrl.char-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OECOMM"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OECOMM"
   sys-ctrl.descrip = "Order entry default FG Item Number from Estimate?"
   sys-ctrl.log-fld = yes.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 v-oecomm-log = sys-ctrl.log-fld
 v-oecomm-cha = sys-ctrl.char-fld
 v-oecomm-int = sys-ctrl.int-fld.

find first sys-ctrl where                                        /* begin ekw01070001 */
      sys-ctrl.company = cocode and
      sys-ctrl.name      = "FGPOCOST" 
      no-lock no-error.
if not avail sys-ctrl then do:
   create sys-ctrl.
   assign sys-ctrl.company = cocode
              sys-ctrl.name     = "FGPOCOST"
              sys-ctrl.descrip  = "Update order entry cost for purchased finished goods?"
              sys-ctrl.log-fld    = no.
   MESSAGE sys-ctrl.descrip
       VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
       UPDATE sys-ctrl.log-fld.  
end. /* if not avail sys-ctrl */                                 /* end ekw01070001 */

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FOAMDATE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "FOAMDATE"
   sys-ctrl.descrip = "Add Manufacturing Days to Order Date to calculate Due Date for Foam"
   sys-ctrl.log-fld = no.

  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
assign
 v-foamdate-log = sys-ctrl.log-fld
 v-foamdate-int = sys-ctrl.int-fld.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
