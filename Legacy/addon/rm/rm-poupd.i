/* -------------------------------------------------- rm/rm-poupd.i 03/97 JLF */
/* raw materials - post transactions - update po                              */
/* -------------------------------------------------------------------------- */

if rm-rctd.po-no ne "" then
update-po{1}: do.
  /* Check to see if po entered through the purchasing module. If
     so, verify all charaters are number type because po number
     in purchasing is a integer field. */
  do i = 1 to length(rm-rctd.po-no):
    if asc(substring(rm-rctd.po-no,i,1)) lt 48 or
       asc(substring(rm-rctd.po-no,i,1)) gt 57 then
      leave update-po{1}.
  end.

  find first po-ord
      where po-ord.company eq item.company
        and po-ord.po-no   eq int(rm-rctd.po-no)
      exclusive-lock no-wait no-error.
  if not avail po-ord and locked po-ord then do:
    message " Purchase Order Record " rm-rcpt.po-no
            "is in use.  Can Not Update.".
   /* undo transblok, next transblok. */
   undo, next.
  end.

  v-recid = ?.
  
  for each po-ordl
      where po-ordl.company   eq item.company
        and po-ordl.i-no      eq rm-rctd.i-no
        and po-ordl.po-no     eq int(rm-rctd.po-no)
        and po-ordl.deleted   eq no
        and po-ordl.item-type eq yes
        and po-ordl.job-no    eq rm-rctd.job-no
        and po-ordl.job-no2   eq rm-rctd.job-no2
      use-index item-ordno no-lock
      by po-ordl.s-num desc:
      
    v-recid = recid(po-ordl).  
      
    if po-ordl.s-num eq rm-rctd.s-num then leave.
  end.
  
  find po-ordl where recid(po-ordl) eq v-recid exclusive-lock no-error.
  
  if avail po-ordl then do:
    assign
     po-ord.received   = yes
     po-ordl.t-rec-qty = po-ordl.t-rec-qty + ld-cvt-qty /*rm-rctd.qty*/
     item.q-ono        = item.q-ono - ld-cvt-qty        /*rm-rctd.qty*/
     v-overrun-qty     = po-ordl.cons-qty *
                         (1 + (po-ordl.over-pct / 100))
     v-underrun-qty    = po-ordl.cons-qty *
                         (1 - (po-ordl.under-pct / 100)).

    if po-ordl.t-rec-qty gt po-ordl.cons-qty then
      item.q-ono = item.q-ono + (po-ordl.t-rec-qty - po-ordl.cons-qty).

    if item.q-ono lt 0 then item.q-ono = 0.

    if po-ordl.t-rec-qty ge v-underrun-qty then do:
      /* added to relieve on order qty */
      if po-ordl.cons-qty - po-ordl.t-rec-qty gt 0 then
        item.q-ono = item.q-ono - (po-ordl.cons-qty - po-ordl.t-rec-qty).

      po-ordl.stat = "C".    

      find first b-po-ordl
          where b-po-ordl.company eq po-ord.company
            and b-po-ordl.po-no   eq po-ord.po-no
            and b-po-ordl.stat    ne "C"
            and b-po-ordl.deleted eq no
            and recid(b-po-ordl)  ne recid(po-ordl)
          no-lock no-error.
      if not avail b-po-ordl then po-ord.stat = "C".
    end.

    else po-ordl.stat = "P".
  end.
  
  else do:
    message " Purchase Order Line Record is in use.  Can Not Update.".
/*    undo transblok, next transblok.*/
    undo, next.
  end.
end. /* update-po{1} */

/* end ---------------------------------- copr. 1997  advanced software, inc. */

