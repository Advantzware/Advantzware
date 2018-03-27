
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i NEW SHARED}

DEF VAR v-binqty AS INT NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR v-tagcost AS DEC NO-UNDO.
DEF VAR ld-cvt-cost AS DEC NO-UNDO.
DEF VAR lv-uom LIKE fg-rctd.cost-uom NO-UNDO.
DEF VAR fg-uom-list AS CHAR NO-UNDO.

DEF VAR lvdRctdT-qty     LIKE fg-rctd.t-qty     NO-UNDO.
DEF VAR lvdRctdPartial   LIKE fg-rctd.partial   NO-UNDO.
DEF VAR lvcRctdJob-no    LIKE fg-rctd.job-no    NO-UNDO.
DEF VAR lvcRctdCompany   LIKE fg-rctd.company   NO-UNDO.
DEF VAR lviRctdJob-no2   LIKE fg-rctd.job-no2   NO-UNDO.
DEF VAR lvcRctdI-no      LIKE fg-rctd.i-no      NO-UNDO.
DEF VAR lvcRctdRita-code LIKE fg-rctd.rita-code NO-UNDO.
DEF VAR lvdExtCost       LIKE fg-rctd.ext-cost NO-UNDO.

DEF BUFFER b-fg-rcpth FOR fg-rcpth.
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh.

DEF TEMP-TABLE tt-fg-bin NO-UNDO  LIKE fg-bin.
DEF TEMP-TABLE tt-fg-rctd NO-UNDO LIKE fg-rctd.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

FIND fg-bin WHERE ROWID(fg-bin) EQ ip-rowid NO-ERROR.

IF AVAIL fg-bin THEN
FIND FIRST itemfg
    WHERE itemfg.company EQ fg-bin.company
      AND itemfg.i-no    EQ fg-bin.i-no
    NO-LOCK NO-ERROR.

IF AVAIL itemfg THEN DO:
  cocode = itemfg.company.

  CREATE tt-fg-bin.
  ASSIGN
   tt-fg-bin.company      = fg-bin.company
   tt-fg-bin.job-no       = fg-bin.job-no
   tt-fg-bin.job-no2      = fg-bin.job-no2
   tt-fg-bin.loc          = fg-bin.loc
   tt-fg-bin.loc-bin      = fg-bin.loc-bin
   tt-fg-bin.tag          = fg-bin.tag
   tt-fg-bin.cust-no      = fg-bin.cust-no
   tt-fg-bin.i-no         = fg-bin.i-no
   tt-fg-bin.pur-uom      = fg-bin.pur-uom
   tt-fg-bin.qty          = 0
   tt-fg-bin.std-tot-cost = 0
   tt-fg-bin.std-mat-cost = 0
   tt-fg-bin.std-lab-cost = 0
   tt-fg-bin.std-fix-cost = 0
   tt-fg-bin.std-var-cost = 0.

  IF TRIM(fg-bin.tag) EQ "" THEN
  FOR EACH fg-rcpth
      WHERE fg-rcpth.company EQ fg-bin.company
        AND fg-rcpth.i-no    EQ fg-bin.i-no
        AND fg-rcpth.job-no  EQ fg-bin.job-no
        AND fg-rcpth.job-no2 EQ fg-bin.job-no2
      USE-INDEX tran NO-LOCK,

      EACH fg-rdtlh
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        AND fg-rdtlh.loc       EQ fg-bin.loc
        AND fg-rdtlh.loc-bin   EQ fg-bin.loc-bin
        AND fg-rdtlh.tag       EQ fg-bin.tag
        AND fg-rdtlh.cust-no   EQ fg-bin.cust-no
      USE-INDEX rm-rdtl NO-LOCK

      BREAK BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no: 

    {fg/upfgbinc.i}
  END. /* each fg-rcpth */

  ELSE
  FOR EACH fg-rdtlh
      WHERE fg-rdtlh.company  EQ fg-bin.company
        AND fg-rdtlh.tag      EQ fg-bin.tag
        AND fg-rdtlh.loc      EQ fg-bin.loc
        AND fg-rdtlh.loc-bin  EQ fg-bin.loc-bin
        AND fg-rdtlh.cust-no  EQ fg-bin.cust-no
      USE-INDEX tag NO-LOCK,

      FIRST fg-rcpth
      WHERE fg-rcpth.r-no      EQ fg-rdtlh.r-no
        AND fg-rcpth.i-no      EQ fg-bin.i-no
        AND fg-rcpth.job-no    EQ fg-bin.job-no
        AND fg-rcpth.job-no2   EQ fg-bin.job-no2
        AND fg-rcpth.rita-code EQ fg-rdtlh.rita-code
      USE-INDEX r-no NO-LOCK

      BREAK BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no: 

    {fg/upfgbinc.i}
  END. /* each fg-rdtlh */

  IF tt-fg-bin.std-tot-cost NE ? THEN
    ASSIGN
     fg-bin.std-tot-cost = tt-fg-bin.std-tot-cost
     fg-bin.std-lab-cost = tt-fg-bin.std-lab-cost
     fg-bin.std-mat-cost = tt-fg-bin.std-mat-cost
     fg-bin.std-var-cost = tt-fg-bin.std-var-cost
     fg-bin.std-fix-cost = tt-fg-bin.std-fix-cost.
END.

PROCEDURE upd-bin:
{fg/upd-bini.i}
END PROCEDURE /* upd-bin */.
