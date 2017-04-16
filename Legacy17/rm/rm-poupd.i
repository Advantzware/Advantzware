/* -------------------------------------------------- rm/rm-poupd.i 03/97 JLF */
/* raw materials - post transactions - update po                              */
/* -------------------------------------------------------------------------- */

if int(rm-rctd.po-no) ne 0 then
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
      exclusive-loc no-wait no-error.
  if not avail po-ord and locked po-ord then do:
    message " Purchase Order Record " rm-rctd.po-no
            "is in use.  Can Not Update..."
            VIEW-AS ALERT-BOX.
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
      break by po-ordl.s-num desc:
      
    v-recid = recid(po-ordl).  
      
    if last(po-ordl.s-num) or po-ordl.s-num eq rm-rctd.s-num then leave.
  end.
  
  find po-ordl where recid(po-ordl) eq v-recid exclusive-lock no-error.
  
  if avail po-ordl then do:

    ld-cvt-qty = rm-rctd.qty.

    IF rm-rctd.pur-uom NE po-ordl.cons-uom THEN
      RUN sys/ref/convquom.p (rm-rctd.pur-uom, po-ordl.cons-uom,
                            item.basis-w, po-ordl.s-len, po-ordl.s-wid, item.s-dep,
                            ld-cvt-qty, OUTPUT ld-cvt-qty).

    ASSIGN
     po-ord.received   = yes
     po-ordl.t-rec-qty = po-ordl.t-rec-qty + ld-cvt-qty.

    RUN rm/polclose.p (ROWID(po-ordl), rm-rctd.qty, rm-rctd.pur-uom).

    FIND CURRENT po-ordl EXCLUSIVE.
  end.
  
  else do:
    message " Purchase Order Line Record is in use.  Can Not Update..."
            VIEW-AS ALERT-BOX.
/*    undo transblok, next transblok.*/
    undo, next.
  end.
end. /* update-po{1} */

/* end ---------------------------------- copr. 1997  advanced software, inc. */
