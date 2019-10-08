/* -------------------------------------------------- fg/fg-mkbin.p 11/97 JLF */
/* finished goods bin rebuild program                                         */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM rec-id AS RECID.

{sys/inc/var.i NEW SHARED}

DEF VAR li AS INT NO-UNDO.
DEF VAR lv AS CHAR NO-UNDO.

DEF BUFFER b-rdtlh FOR fg-rdtlh.


DISABLE TRIGGERS FOR LOAD OF fg-bin.

FIND itemfg WHERE RECID(itemfg) EQ rec-id NO-LOCK NO-ERROR.

IF NOT AVAIL itemfg THEN RETURN.

cocode = itemfg.company.

FOR EACH fg-bin
    WHERE fg-bin.company EQ itemfg.company
      AND fg-bin.i-no    EQ itemfg.i-no
    USE-INDEX co-ino:
  ASSIGN
   fg-bin.qty           = 0
   fg-bin.partial-count = 0.
END.

FOR EACH fg-rcpth
    WHERE fg-rcpth.company EQ itemfg.company
      AND fg-rcpth.i-no    EQ itemfg.i-no
    NO-LOCK USE-INDEX tran,

    EACH fg-rdtlh
    WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
      AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code

    BY fg-rcpth.trans-date
    BY fg-rdtlh.trans-time
    BY fg-rcpth.r-no
    BY fg-rdtlh.rec_key:

  FIND FIRST fg-bin
      WHERE fg-bin.company EQ fg-rcpth.company
        AND fg-bin.i-no    EQ fg-rcpth.i-no
        AND fg-bin.loc     EQ fg-rdtlh.loc
        AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
        AND fg-bin.tag     EQ fg-rdtlh.tag
        AND fg-bin.job-no  EQ fg-rcpth.job-no
        AND fg-bin.job-no2 EQ fg-rcpth.job-no2
        AND fg-bin.cust-no EQ fg-rdtlh.cust-no
      USE-INDEX co-ino NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
    CREATE fg-bin.
    ASSIGN
     fg-bin.company      = fg-rcpth.company
     fg-bin.job-no       = fg-rcpth.job-no
     fg-bin.job-no2      = fg-rcpth.job-no2
     fg-bin.loc          = fg-rdtlh.loc
     fg-bin.loc-bin      = fg-rdtlh.loc-bin
     fg-bin.tag          = fg-rdtlh.tag
     fg-bin.cust-no      = fg-rdtlh.cust-no
     fg-bin.i-no         = fg-rcpth.i-no
     fg-bin.aging-date   = fg-rcpth.trans-date
     fg-bin.pur-uom      = itemfg.prod-uom
     fg-bin.std-tot-cost = itemfg.total-std-cost
     fg-bin.std-mat-cost = itemfg.std-mat-cost
     fg-bin.std-lab-cost = itemfg.std-lab-cost
     fg-bin.std-var-cost = itemfg.std-var-cost
     fg-bin.std-fix-cost = itemfg.std-fix-cost
     fg-bin.case-count   = fg-rdtlh.qty-case
     fg-bin.units-pallet = fg-rdtlh.units-pallet
     fg-bin.cases-unit   = fg-rdtlh.stacks-unit .
  END.
  IF fg-bin.po-no EQ "" AND fg-rcpth.po-no NE "" THEN
      ASSIGN 
      fg-bin.po-no        = fg-rcpth.po-no  .  
  
  {fg/fgmkbin2.i}
END. /* EACH fg-rcpth */
