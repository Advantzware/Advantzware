/* -------------------------------------------------- fg/fg-mkbin.p 11/97 JLF */
/* finished goods bin rebuild program                                         */
/* -------------------------------------------------------------------------- */

def input parameter rec-id as recid.

{sys/inc/var.i shared}


find itemfg where recid(itemfg) eq rec-id no-lock.

for each fg-bin
    where fg-bin.company eq cocode
      and fg-bin.i-no    eq itemfg.i-no
    use-index co-ino:
  fg-bin.qty = 0.
end.

for each fg-rcpth
    where fg-rcpth.company eq cocode
      and fg-rcpth.i-no    eq itemfg.i-no
    no-lock use-index tran,

    each fg-rdtlh
    where fg-rdtlh.r-no      eq fg-rcpth.r-no
      and fg-rdtlh.rita-code eq fg-rcpth.rita-code
    no-lock

    by fg-rcpth.trans-date
    BY fg-rdtlh.trans-time
    by fg-rcpth.r-no:

  find first fg-bin
      where fg-bin.company eq fg-rcpth.company
        and fg-bin.i-no    eq fg-rcpth.i-no
        and fg-bin.job-no  eq fg-rcpth.job-no
        and fg-bin.job-no2 eq fg-rcpth.job-no2
        and fg-bin.loc     eq fg-rdtlh.loc
        and fg-bin.loc-bin eq fg-rdtlh.loc-bin
        and fg-bin.tag     eq fg-rdtlh.tag
      use-index co-ino no-error.
  if not avail fg-bin then do:
    create fg-bin.
    assign
     fg-bin.company      = fg-rdtlh.company
     fg-bin.job-no       = fg-rcpth.job-no
     fg-bin.job-no2      = fg-rcpth.job-no2
     fg-bin.loc          = fg-rdtlh.loc
     fg-bin.loc-bin      = fg-rdtlh.loc-bin
     fg-bin.tag          = fg-rdtlh.tag
     fg-bin.i-no         = fg-rcpth.i-no
     fg-bin.case-count   = 1
     fg-bin.cases-unit   = 1
     fg-bin.aging-date   = fg-rcpth.trans-date
     fg-bin.pur-uom      = itemfg.prod-uom
     fg-bin.std-tot-cost = itemfg.total-std-cost
     fg-bin.std-mat-cost = itemfg.std-mat-cost
     fg-bin.std-lab-cost = itemfg.std-lab-cost
     fg-bin.std-var-cost = itemfg.std-var-cost
     fg-bin.std-fix-cost = itemfg.std-fix-cost.
  end.

  {fg/fg-mkbin.i}
end. /* each fg-rcpth */

IF TRIM(itemfg.i-no) NE "" THEN
FOR EACH fg-bin
    WHERE fg-bin.company EQ itemfg.company
      AND fg-bin.i-no    EQ itemfg.i-no
      AND fg-bin.cust-no GT ""
      AND fg-bin.qty     LE 0
    USE-INDEX co-ino:
  DELETE fg-bin.
END.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
