/* company.i */

DISABLE TRIGGERS FOR LOAD OF loc.
DISABLE TRIGGERS FOR LOAD OF fgcat.

IF CAN-FIND(FIRST period OF company) THEN
FOR EACH period OF company EXCLUSIVE-LOCK:
  DELETE period.
END.

FOR EACH account EXCLUSIVE-LOCK WHERE account.company = company.company:
  DELETE account.
END.

FOR EACH ap-buy EXCLUSIVE-LOCK WHERE ap-buy.company = company.company:
  DELETE ap-buy.
END.

FOR EACH ap-chk EXCLUSIVE-LOCK WHERE ap-chk.company = company.company:
  DELETE ap-chk.
END.

FOR EACH ap-ctrl EXCLUSIVE-LOCK WHERE ap-ctrl.company = company.company:
  DELETE ap-ctrl.
END.

FOR EACH ap-dis EXCLUSIVE-LOCK WHERE ap-dis.company = company.company:
  DELETE ap-dis.
END.

FOR EACH ap-disl EXCLUSIVE-LOCK WHERE ap-disl.company = company.company:
  DELETE ap-disl.
END.

FOR EACH ap-inv EXCLUSIVE-LOCK WHERE ap-inv.company = company.company:
  DELETE ap-inv.
END.

FOR EACH ap-invl EXCLUSIVE-LOCK WHERE ap-invl.company = company.company:
  DELETE ap-invl.
END.

FOR EACH ap-ledger EXCLUSIVE-LOCK WHERE ap-ledger.company = company.company:
  DELETE ap-ledger.
END.

FOR EACH ap-pay EXCLUSIVE-LOCK WHERE ap-pay.company = company.company:
  DELETE ap-pay.
END.

FOR EACH ap-sel EXCLUSIVE-LOCK WHERE ap-sel.company = company.company:
  DELETE ap-sel.
END.

FOR EACH aphist EXCLUSIVE-LOCK WHERE aphist.company = company.company:
  DELETE aphist.
END.

FOR EACH ar-cash EXCLUSIVE-LOCK WHERE ar-cash.company = company.company:
  DELETE ar-cash.
END.

FOR EACH ar-cashl EXCLUSIVE-LOCK WHERE ar-cashl.company = company.company:
  DELETE ar-cashl.
END.

FOR EACH ar-ctrl EXCLUSIVE-LOCK WHERE ar-ctrl.company = company.company:
  DELETE ar-ctrl.
END.

FOR EACH ar-inv EXCLUSIVE-LOCK WHERE ar-inv.company = company.company:
  DELETE ar-inv.
END.

FOR EACH ar-invl EXCLUSIVE-LOCK WHERE ar-invl.company = company.company:
  DELETE ar-invl.
END.

FOR EACH ar-ledger EXCLUSIVE-LOCK WHERE ar-ledger.company = company.company:
  DELETE ar-ledger.
END.

FOR EACH ar-mcash EXCLUSIVE-LOCK WHERE ar-mcash.company = company.company:
  DELETE ar-mcash.
END.

FOR EACH bank EXCLUSIVE-LOCK WHERE bank.company = company.company:
  DELETE bank.
END.

FOR EACH buyer EXCLUSIVE-LOCK WHERE buyer.company = company.company:
  DELETE buyer.
END.

FOR EACH carr-mtx EXCLUSIVE-LOCK WHERE carr-mtx.company = company.company:
  DELETE carr-mtx.
END.

FOR EACH carrier EXCLUSIVE-LOCK WHERE carrier.company = company.company:
  DELETE carrier.
END.

FOR EACH ce-ctrl EXCLUSIVE-LOCK WHERE ce-ctrl.company = company.company:
  DELETE ce-ctrl.
END.

/**** DO LAST !!! *
FOR EACH company EXCLUSIVE-LOCK WHERE company.company = company.company:
DELETE company.
END.
*******************/

FOR EACH costtype EXCLUSIVE-LOCK WHERE costtype.company = company.company:
  DELETE costtype.
END.

FOR EACH cust EXCLUSIVE-LOCK WHERE cust.company = company.company:
  DELETE cust.
END.

FOR EACH custype EXCLUSIVE-LOCK WHERE custype.company = company.company:
  DELETE custype.
END.

/************* Not really by company, so DONT delete ********************
FOR EACH dept EXCLUSIVE-LOCK WHERE dept.company = company.company:
  DELETE dept.
END.
************************************************************************/

FOR EACH e-item EXCLUSIVE-LOCK WHERE e-item.company = company.company:
  DELETE e-item.
END.

FOR EACH e-itemfg EXCLUSIVE-LOCK WHERE e-itemfg.company = company.company:
  DELETE e-itemfg.
END.

FOR EACH eb EXCLUSIVE-LOCK WHERE eb.company = company.company:
  DELETE eb.
END.

FOR EACH ef EXCLUSIVE-LOCK WHERE ef.company = company.company:
  DELETE ef.
END.

FOR EACH est EXCLUSIVE-LOCK WHERE est.company = company.company:
  DELETE est.
END.

FOR EACH est-inst EXCLUSIVE-LOCK WHERE est-inst.company = company.company:
  DELETE est-inst.
END.

FOR EACH fg-act EXCLUSIVE-LOCK WHERE fg-act.company = company.company:
  DELETE fg-act.
END.

FOR EACH fg-bin EXCLUSIVE-LOCK WHERE fg-bin.company = company.company:
  DELETE fg-bin.
END.

FOR EACH fg-ctrl EXCLUSIVE-LOCK WHERE fg-ctrl.company = company.company:
  DELETE fg-ctrl.
END.

FOR EACH fg-hist EXCLUSIVE-LOCK WHERE fg-hist.company = company.company:
  DELETE fg-hist.
END.

FOR EACH fg-rcpth EXCLUSIVE-LOCK WHERE fg-rcpth.company = company.company:
  DELETE fg-rcpth.
END.

FOR EACH fg-rcpts EXCLUSIVE-LOCK WHERE fg-rcpts.company = company.company:
  DELETE fg-rcpts.
END.

FOR EACH fg-rdtl EXCLUSIVE-LOCK WHERE fg-rdtl.company = company.company:
  DELETE fg-rdtl.
END.

FOR EACH fg-rdtlh EXCLUSIVE-LOCK WHERE fg-rdtlh.company = company.company:
  DELETE fg-rdtlh.
END.

FOR EACH fg-set EXCLUSIVE-LOCK WHERE fg-set.company = company.company:
  DELETE fg-set.
END.

FOR EACH fgcat EXCLUSIVE-LOCK WHERE fgcat.company = company.company:
  DELETE fgcat.
END.

FOR EACH gl-ctrl EXCLUSIVE-LOCK WHERE gl-ctrl.company = company.company:
  DELETE gl-ctrl.
END.

FOR EACH gl-jrn EXCLUSIVE-LOCK WHERE gl-jrn.company = company.company:
  DELETE gl-jrn.
END.

FOR EACH gl-rpt EXCLUSIVE-LOCK WHERE gl-rpt.company = company.company:
  DELETE gl-rpt.
END.

FOR EACH glhist EXCLUSIVE-LOCK WHERE glhist.company = company.company:
  DELETE glhist.
END.

FOR EACH gltrans EXCLUSIVE-LOCK WHERE gltrans.company = company.company:
  DELETE gltrans.
END.

FOR EACH inv-head EXCLUSIVE-LOCK WHERE inv-head.company = company.company:
  DELETE inv-head.
END.

FOR EACH inv-line EXCLUSIVE-LOCK WHERE inv-line.company = company.company:
  DELETE inv-line.
END.

FOR EACH inv-misc EXCLUSIVE-LOCK WHERE inv-misc.company = company.company:
  DELETE inv-misc.
END.

FOR EACH item EXCLUSIVE-LOCK WHERE item.company = company.company:
  DELETE item.
END.

FOR EACH itemfg EXCLUSIVE-LOCK WHERE itemfg.company = company.company:
  DELETE itemfg.
END.

FOR EACH itemfgdtl EXCLUSIVE-LOCK WHERE itemfgdtl.company = company.company:
  DELETE itemfgdtl.
END.

FOR EACH jc-ctrl EXCLUSIVE-LOCK WHERE jc-ctrl.company = company.company:
  DELETE jc-ctrl.
END.

FOR EACH job EXCLUSIVE-LOCK WHERE job.company = company.company:
  if job.exported then do:
    job.stat = "X".
    run jc/kiwiexp2.p (recid(job)).
  end.
  
  DELETE job.
END.

FOR EACH job-all EXCLUSIVE-LOCK WHERE job-all.company = company.company:
  DELETE job-all.
END.

FOR EACH job-hdr EXCLUSIVE-LOCK WHERE job-hdr.company = company.company:
  DELETE job-hdr.
END.

FOR EACH job-mat EXCLUSIVE-LOCK WHERE job-mat.company = company.company:
  DELETE job-mat.
END.

FOR EACH job-mch EXCLUSIVE-LOCK WHERE job-mch.company = company.company:
  DELETE job-mch.
END.

FOR EACH job-prep EXCLUSIVE-LOCK WHERE job-prep.company = company.company:
  DELETE job-prep.
END.

FOR EACH loc EXCLUSIVE-LOCK WHERE loc.company = company.company:
  DELETE loc.
END.

/************* Not really by company, so DONT delete ********************
FOR EACH mach EXCLUSIVE-LOCK WHERE mach.company = company.company:
  DELETE mach.
END.
************************************************************************/

FOR EACH mat EXCLUSIVE-LOCK WHERE mat.company = company.company:
  DELETE mat.
END.

FOR EACH mat-act EXCLUSIVE-LOCK WHERE mat-act.company = company.company:
  DELETE mat-act.
END.

FOR EACH matprep EXCLUSIVE-LOCK WHERE matprep.company = company.company:
  DELETE matprep.
END.

FOR EACH mch-act EXCLUSIVE-LOCK WHERE mch-act.company = company.company:
  DELETE mch-act.
END.

FOR EACH misc-act EXCLUSIVE-LOCK WHERE misc-act.company = company.company:
  DELETE misc-act.
END.

/************* Not really by company, so DONT delete ********************
FOR EACH mmtx EXCLUSIVE-LOCK WHERE mmtx.company = company.company:
  DELETE mmtx.
END.

FOR EACH mmtx2 EXCLUSIVE-LOCK WHERE mmtx2.company = company.company:
  DELETE mmtx2.
END.

FOR EACH mmty EXCLUSIVE-LOCK WHERE mmty.company = company.company:
  DELETE mmty.
END.

FOR EACH mstd EXCLUSIVE-LOCK WHERE mstd.company = company.company:
  DELETE mstd.
END.
************************************************************************/

FOR EACH oe-bolh EXCLUSIVE-LOCK WHERE oe-bolh.company = company.company:
  DELETE oe-bolh.
END.

FOR EACH oe-boll EXCLUSIVE-LOCK WHERE oe-boll.company = company.company:
  DELETE oe-boll.
END.

FOR EACH oe-ctrl EXCLUSIVE-LOCK WHERE oe-ctrl.company = company.company:
  DELETE oe-ctrl.
END.

FOR EACH oe-ord EXCLUSIVE-LOCK WHERE oe-ord.company = company.company:
  DELETE oe-ord.
END.

FOR EACH oe-ordl EXCLUSIVE-LOCK WHERE oe-ordl.company = company.company:
  DELETE oe-ordl.
END.

FOR EACH oe-ordm EXCLUSIVE-LOCK WHERE oe-ordm.company = company.company:
  DELETE oe-ordm.
END.

FOR EACH oe-prmtx EXCLUSIVE-LOCK WHERE oe-prmtx.company = company.company:
  DELETE oe-prmtx.
END.

FOR EACH oe-rel EXCLUSIVE-LOCK WHERE oe-rel.company = company.company:
  DELETE oe-rel.
END.

FOR EACH oe-relh EXCLUSIVE-LOCK WHERE oe-relh.company = company.company:
  DELETE oe-relh.
END.

FOR EACH oe-rell EXCLUSIVE-LOCK WHERE oe-rell.company = company.company:
  DELETE oe-rell.
END.

FOR EACH oe-reth EXCLUSIVE-LOCK WHERE oe-reth.company = company.company:
  DELETE oe-reth.
END.

FOR EACH oe-retl EXCLUSIVE-LOCK WHERE oe-retl.company = company.company:
  DELETE oe-retl.
END.

FOR EACH oe-ship EXCLUSIVE-LOCK WHERE oe-ship.company = company.company:
  DELETE oe-ship.
END.

FOR EACH pc-misc EXCLUSIVE-LOCK WHERE pc-misc.company = company.company:
  DELETE pc-misc.
END.

FOR EACH pc-prdd EXCLUSIVE-LOCK WHERE pc-prdd.company = company.company:
  DELETE pc-prdd.
END.

FOR EACH pc-prdh EXCLUSIVE-LOCK WHERE pc-prdh.company = company.company:
  DELETE pc-prdh.
END.

FOR EACH period EXCLUSIVE-LOCK WHERE period.company = company.company:
  DELETE period.
END.

FOR EACH permg EXCLUSIVE-LOCK WHERE permg.company = company.company:
  DELETE permg.
END.

FOR EACH po-all EXCLUSIVE-LOCK WHERE po-all.company = company.company:
  DELETE po-all.
END.

FOR EACH po-ctrl EXCLUSIVE-LOCK WHERE po-ctrl.company = company.company:
  DELETE po-ctrl.
END.

FOR EACH po-ord EXCLUSIVE-LOCK WHERE po-ord.company = company.company:
  DELETE po-ord.
END.

FOR EACH po-ordl EXCLUSIVE-LOCK WHERE po-ordl.company = company.company:
  DELETE po-ordl.
END.

FOR EACH po-rcpts EXCLUSIVE-LOCK WHERE po-rcpts.company = company.company:
  DELETE po-rcpts.
END.

FOR EACH prep EXCLUSIVE-LOCK WHERE prep.company = company.company:
  DELETE prep.
END.

/************* Not really by company, so DONT delete ********************
FOR EACH printer EXCLUSIVE-LOCK WHERE printer.company = company.company:
  DELETE printer.
END.
************************************************************************/

FOR EACH procat EXCLUSIVE-LOCK WHERE procat.company = company.company:
  DELETE procat.
END.

FOR EACH prod EXCLUSIVE-LOCK WHERE prod.company = company.company:
  DELETE prod.
END.

FOR EACH prodl EXCLUSIVE-LOCK WHERE prodl.company = company.company:
  DELETE prodl.
END.

FOR EACH quote EXCLUSIVE-LOCK WHERE quote.company = company.company:
  DELETE quote.
END.

FOR EACH rm-bin EXCLUSIVE-LOCK WHERE rm-bin.company = company.company:
  DELETE rm-bin.
END.

FOR EACH rm-ctrl EXCLUSIVE-LOCK WHERE rm-ctrl.company = company.company:
  DELETE rm-ctrl.
END.

FOR EACH rm-rcpt EXCLUSIVE-LOCK WHERE rm-rcpt.company = company.company:
  DELETE rm-rcpt.
END.

FOR EACH rm-rcpth EXCLUSIVE-LOCK WHERE rm-rcpth.company = company.company:
  DELETE rm-rcpth.
END.

FOR EACH rm-rdtl EXCLUSIVE-LOCK WHERE rm-rdtl.company = company.company:
  DELETE rm-rdtl.
END.

FOR EACH rm-rdtlh EXCLUSIVE-LOCK WHERE rm-rdtlh.company = company.company:
  DELETE rm-rdtlh.
END.

FOR EACH rm-receipts EXCLUSIVE-LOCK WHERE rm-receipts.company = company.company:
  DELETE rm-receipts.
END.

FOR EACH shipto EXCLUSIVE-LOCK WHERE shipto.company = company.company:
  DELETE shipto.
END.

FOR EACH sman EXCLUSIVE-LOCK WHERE sman.company = company.company:
  DELETE sman.
END.

FOR EACH sman-mtx EXCLUSIVE-LOCK WHERE sman-mtx.company = company.company:
  DELETE sman-mtx.
END.

IF CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "smanmtrx") THEN
  RUN custom/delsmtrx.p ({&TABLENAME}.company, 1, "").

FOR EACH soldto EXCLUSIVE-LOCK WHERE soldto.company = company.company:
  DELETE soldto.
END.

FOR EACH stax EXCLUSIVE-LOCK WHERE stax.company = company.company:
  DELETE stax.
END.

FOR EACH style EXCLUSIVE-LOCK WHERE style.company = company.company:
  DELETE style.
END.

FOR EACH terms EXCLUSIVE-LOCK WHERE terms.company = company.company:
  DELETE terms.
END.

FOR EACH terr EXCLUSIVE-LOCK WHERE terr.company = company.company:
  DELETE terr.
END.

FOR EACH vend EXCLUSIVE-LOCK WHERE vend.company = company.company:
  DELETE vend.
END.

FOR EACH ventype EXCLUSIVE-LOCK WHERE ventype.company = company.company:
  DELETE ventype.
END.

FOR EACH rm-rctd WHERE rm-rctd.company = company.company:
    DELETE rm-rctd.
END.
FOR EACH fg-rctd WHERE fg-rctd.company = company.company:
    DELETE fg-rctd.
END.
FOR EACH quotehd WHERE quotehd.company = company.company:
    DELETE quotehd.
END.
FOR EACH probe WHERE probe.company = company.company:
    DELETE probe.
END.
