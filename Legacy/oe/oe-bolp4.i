/* -------------------------------------------------- oe/oe-bolp4.i 02/99 JLF */
/* Bill Of Lading Posting - Check for CUST Warehouse                          */
/* -------------------------------------------------------------------------- */

release fg-bin.

/* CREATE TEMPORARY CUSTOMER WAREHOUSE FG-BIN */
if oe-boll.tag ne "" then
find first fg-bin
    where fg-bin.company eq cocode
      and fg-bin.tag     eq oe-boll.tag
      and fg-bin.i-no    eq itemfg.i-no
      and fg-bin.loc     eq "CUST"
    use-index tag exclusive-lock no-error.

if avail fg-bin then
  assign
   v-bol-qty = v-bol-qty - fg-bin.qty
   v-partial = v-partial - min(v-partial,fg-bin.partial-count).

else do:
  find first fg-bin
      where fg-bin.company eq cocode
        and fg-bin.i-no    eq itemfg.i-no
        and fg-bin.loc     eq "CUST"
        and fg-bin.loc-bin eq oe-ord.cust-no
        and fg-bin.tag     eq ""
        and fg-bin.job-no  eq oe-boll.job-no
        and fg-bin.job-no2 eq oe-boll.job-no2
        and fg-bin.qty     gt 0
      use-index co-ino exclusive-lock no-error.

  if avail fg-bin then do:
    find first xfg-bin
        where xfg-bin.company eq cocode
          and xfg-bin.i-no    eq itemfg.i-no
          and xfg-bin.loc     eq "CUST"
          and xfg-bin.loc-bin eq string(oe-boll.bol-no,"999999")
          and xfg-bin.tag     eq ""
          and xfg-bin.job-no  eq oe-boll.job-no
          and xfg-bin.job-no2 eq oe-boll.job-no2
        no-error.
    if not avail xfg-bin then do:
      create xfg-bin.
      assign
       xfg-bin.company = cocode
       xfg-bin.i-no    = itemfg.i-no
       xfg-bin.loc     = "CUST"
       xfg-bin.loc-bin = string(oe-boll.bol-no,"999999")
       xfg-bin.tag     = ""
       xfg-bin.job-no  = oe-boll.job-no
       xfg-bin.job-no2 = oe-boll.job-no2.
    end.

    IF xfg-bin.std-lab-cost EQ ? THEN
      ASSIGN
       xfg-bin.std-tot-cost = 0
       xfg-bin.std-lab-cost = 0
       xfg-bin.std-mat-cost = 0
       xfg-bin.std-var-cost = 0
       xfg-bin.std-fix-cost = 0.

    IF xfg-bin.case-count   LE 0 THEN xfg-bin.case-count   = oe-boll.qty-case.
    IF xfg-bin.units-pallet LE 0 THEN xfg-bin.units-pallet = 1.
    IF xfg-bin.cases-unit   LE 0 THEN xfg-bin.cases-unit   = 1.

    assign
     xfg-bin.qty           = xfg-bin.qty + min(fg-bin.qty,oe-boll.qty)
     xfg-bin.partial-count = xfg-bin.partial-count + oe-boll.partial
     v-bol-qty             = v-bol-qty   - min(fg-bin.qty,oe-boll.qty)
     fg-bin.qty            = fg-bin.qty  - min(fg-bin.qty,oe-boll.qty)
     fg-bin.partial-count  = fg-bin.partial-count - oe-boll.partial.

    if fg-bin.partial-count lt 0 then
      fg-bin.partial-count = if fg-bin.case-count gt 0 then
                               fg-bin.qty modulo fg-bin.case-count
                             else 0.
  end. /* if avail fg-bin record */
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */
