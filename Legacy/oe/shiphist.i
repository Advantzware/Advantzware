/* -------------------------------------------------- oe/shiphist.i 08/98 JLF */
/* Create FG History records for relief of a bin record                       */
/* -------------------------------------------------------------------------- */

v-rcpth-no = 0.
FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
IF AVAIL fg-rctd AND fg-rctd.r-no GT v-rcpth-no THEN v-rcpth-no = fg-rctd.r-no.

FIND LAST b-fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
IF AVAIL b-fg-rcpth AND b-fg-rcpth.r-no GT v-rcpth-no THEN v-rcpth-no = b-fg-rcpth.r-no.

DO WHILE TRUE:
  v-rcpth-no = v-rcpth-no + 1.
  FIND FIRST b-fg-rcpth WHERE b-fg-rcpth.r-no EQ v-rcpth-no USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rcpth THEN NEXT.
  FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ v-rcpth-no USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL fg-rctd THEN NEXT.
  LEAVE.
END.

CREATE fg-rcpth.
ASSIGN
 fg-rcpth.r-no       = v-rcpth-no
 fg-rcpth.b-no       = tt-bolh.b-no
 fg-rcpth.company    = tt-bolh.company
 fg-rcpth.loc        = fg-bin.loc
 fg-rcpth.trans-date = tt-bolh.bol-date
 fg-rcpth.post-date  = TODAY
 fg-rcpth.po-no      = tt-bolh.po-no
 fg-rcpth.i-no       = tt-boll.i-no
 fg-rcpth.i-name     = IF AVAIL {1} THEN {1}.i-name ELSE itemfg.i-name
 fg-rcpth.job-no     = fg-bin.job-no
 fg-rcpth.job-no2    = fg-bin.job-no2
 fg-rcpth.pur-uom    = IF fg-bin.pur-uom EQ "" THEN "M" ELSE fg-bin.pur-uom
 fg-rcpth.rita-code  = IF tt-boll.s-code EQ "I" AND NOT ll-{2}-set-hdr THEN "T" ELSE "S"
 fg-rcpth.job-no-to  = tt-boll.po-no.
FIND CURRENT fg-rcpth.


CREATE fg-rdtlh.
ASSIGN
 fg-rdtlh.r-no         = fg-rcpth.r-no
 fg-rdtlh.company      = tt-bolh.company
 fg-rdtlh.loc          = fg-bin.loc
 fg-rdtlh.loc-bin      = fg-bin.loc-bin
 fg-rdtlh.tag          = fg-bin.tag
 fg-rdtlh.cust-no      = fg-bin.cust-no
 fg-rdtlh.rita-code    = fg-rcpth.rita-code
 fg-rdtlh.cost         = fg-bin.std-tot-cost                          
 fg-rdtlh.qty-case     = tt-boll.qty-case
 fg-rdtlh.stacks-unit  = fg-bin.cases-unit
 fg-rdtlh.units-pallet = fg-bin.units-pallet
 fg-rdtlh.qty          = {3}
 fg-rdtlh.partial      = li-stupid
 fg-rdtlh.cases        = TRUNC((fg-rdtlh.qty - fg-rdtlh.partial) /
                               fg-rdtlh.qty-case,0) 
 fg-rdtlh.avg-cost     = fg-bin.avg-cost 
 fg-rdtlh.last-cost    = fg-bin.last-cost
 fg-rdtlh.std-fix-cost = fg-bin.std-fix-cost 
 fg-rdtlh.std-lab-cost = fg-bin.std-lab-cost 
 fg-rdtlh.std-mat-cost = fg-bin.std-mat-cost 
 fg-rdtlh.std-tot-cost = fg-bin.std-tot-cost 
 fg-rdtlh.std-var-cost = fg-bin.std-var-cost.                              
 .
 IF fg-rcpth.rita-code EQ "S" THEN
   ASSIGN fg-rdtlh.tag = tt-boll.tag
          fg-rdtlh.loc-bin = tt-boll.loc-bin
          fg-rdtlh.loc = tt-boll.loc
          fg-rdtlh.tag = tt-boll.tag
          fg-rdtlh.cust-no = "".

     
 /* gdm - 10260912 */
find first sys-ctrl
      where sys-ctrl.company eq tt-bolh.company
        and sys-ctrl.name    eq "BolPostTime"
      no-lock no-error.

IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "Fixed Time" THEN
    fg-rdtlh.trans-time   = INT(int(SUBSTRING(string(sys-ctrl.dec-fld),1,2)) * 3600 + int(SUBSTRING(string(sys-ctrl.dec-fld),3,4)) * 60 ).
ELSE
    fg-rdtlh.trans-time   = TIME.

 IF tt-boll.s-code = "I" THEN
      fg-rdtlh.tag = "".

IF fg-rcpth.rita-code EQ "T" THEN
  ASSIGN
   fg-rdtlh.qty     = fg-rdtlh.qty * -1
   fg-rdtlh.cases   = fg-rdtlh.cases * -1
   fg-rdtlh.partial = fg-rdtlh.partial * -1.

ELSE
IF fg-rdtlh.qty LT 0                       AND
   CAN-FIND(FIRST sys-ctrl
            WHERE sys-ctrl.company EQ fg-rcpth.company
              AND sys-ctrl.name    EQ "BOLPOST"
              AND sys-ctrl.log-fld EQ YES) THEN DO:

  v-rcpth-no = 0.
  FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL fg-rctd AND fg-rctd.r-no GT v-rcpth-no THEN v-rcpth-no = fg-rctd.r-no.

  FIND LAST b-fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rcpth AND b-fg-rcpth.r-no GT v-rcpth-no THEN v-rcpth-no = b-fg-rcpth.r-no.

  DO WHILE TRUE:
    v-rcpth-no = v-rcpth-no + 1.
    FIND FIRST b-fg-rcpth WHERE b-fg-rcpth.r-no EQ v-rcpth-no USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rcpth THEN NEXT.
    FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ v-rcpth-no USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL fg-rctd THEN NEXT.
    LEAVE.
  END.

  CREATE b-fg-rcpth.
  BUFFER-COPY fg-rcpth TO b-fg-rcpth
  ASSIGN
   b-fg-rcpth.r-no      = v-rcpth-no
   b-fg-rcpth.rita-code = "R".

  CREATE b-fg-rdtlh.
  BUFFER-COPY fg-rdtlh TO b-fg-rdtlh
  ASSIGN
   b-fg-rdtlh.r-no      = b-fg-rcpth.r-no
   b-fg-rdtlh.rita-code = b-fg-rcpth.rita-code.
END.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
