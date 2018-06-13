ASSIGN
 {1}.case-count = {2}.cas-cnt
 {1}.case-pall  = {2}.cas-pal.

find first sys-ctrl
    where sys-ctrl.company eq {2}.company
      and sys-ctrl.name    eq "OECOUNT"
    no-lock no-error.
if avail sys-ctrl and not sys-ctrl.log-fld then
  {1}.case-count = if {2}.tr-cnt ne 0 then {2}.tr-cnt
                   else ({2}.cas-cnt * {2}.cas-pal).

DEF BUFFER b-eiv FOR e-itemfg-vend.

IF {2}.pur-man THEN
FOR EACH e-itemfg-vend NO-LOCK
    WHERE e-itemfg-vend.company  EQ {2}.company
      AND e-itemfg-vend.est-no   EQ {2}.est-no
      AND e-itemfg-vend.form-no  EQ {2}.form-no
      AND e-itemfg-vend.blank-no EQ {2}.blank-no
    BREAK BY e-itemfg-vend.vend-no:

  IF FIRST(e-itemfg-vend.vend-no) THEN DO:
    CREATE e-itemfg.
    BUFFER-COPY e-itemfg-vend EXCEPT rec_key TO e-itemfg.
    ASSIGN
        e-itemfg.i-no    = {1}.i-no
        .
    IF e-itemfg-vend.std-uom = "" THEN
        ASSIGN e-itemfg.std-uom = "EA".


  END.

  CREATE b-eiv.
  BUFFER-COPY e-itemfg-vend EXCEPT rec_key TO b-eiv
  ASSIGN
   b-eiv.i-no     = {1}.i-no
   b-eiv.est-no   = ""
   b-eiv.eqty     = 0
   b-eiv.form-no  = 0
   b-eiv.blank-no = 0.
END.

