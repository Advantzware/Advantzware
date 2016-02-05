RUN accountCopy.
RUN acctcostCopy.
RUN ap-buyCopy.
RUN ap-chkCopy.
RUN ap-ctrlCopy.
RUN ap-disCopy.
RUN ap-dislCopy.
RUN ap-invCopy.
RUN ap-invlCopy.
RUN ap-invlrCopy.
RUN ap-ledgerCopy.
RUN ap-payCopy.
RUN ap-selCopy.
RUN aphistCopy.
RUN ar-cashCopy.
RUN ar-cashlCopy.
RUN ar-ctrlCopy.
RUN ar-invCopy.
RUN ar-invlCopy.
RUN ar-invmCopy.
RUN ar-ledgerCopy.
RUN ar-mcashCopy.
RUN asinotesCopy.
RUN bankCopy.
RUN box-design-hdrCopy.
RUN box-design-lineCopy.
RUN buyerCopy.
RUN carr-mtxCopy.
RUN carrierCopy.
RUN ce-ctrlCopy.
RUN companyCopy.
RUN costtypeCopy.
RUN crewCopy.
RUN currencyCopy.
RUN custCopy.
RUN cust-markupCopy.
RUN cust-partCopy.
RUN cust-prod-salesCopy.
RUN custypeCopy.
RUN db-ctrlCopy.
RUN deptCopy.
RUN e-itemCopy.
RUN e-item-custCopy.
RUN e-item-vendCopy.
RUN e-itemfgCopy.
RUN e-itemfg-vendCopy.
RUN ebCopy.
RUN EDAPCheckCopy.
RUN EDCoCopy.
RUN EDICXrefCopy.
RUN EDIVAddonCopy.
RUN EDIVLineCopy.
RUN EDIVTranCopy.
RUN efCopy.
RUN ef-nshCopy.
RUN empCopy.
RUN estCopy.
RUN est-flmCopy.
RUN est-instCopy.
RUN est-opCopy.
RUN est-pfCopy.
RUN est-prepCopy.
RUN est-qtyCopy.
RUN est-summCopy.
RUN fg-actCopy.
RUN fg-binCopy.
RUN fg-ctrlCopy.
RUN fg-histCopy.
RUN fg-rcpthCopy.
RUN fg-rcptsCopy.
RUN fg-rctdCopy.
RUN fg-rdtlCopy.
RUN fg-rdtlhCopy.
RUN fg-setCopy.
RUN fgcatCopy.
RUN fluteCopy.
RUN gl-ctrlCopy.
RUN gl-freqCopy.
RUN gl-jrnCopy.
RUN gl-rptCopy.
RUN gl-rptdCopy.
RUN glhistCopy.
RUN gltransCopy.
RUN inv-headCopy.
RUN inv-lineCopy.
RUN inv-miscCopy.
RUN itemCopy.
RUN item-bomCopy.
RUN item-specCopy.
RUN itemfgCopy.
RUN itemfg-bomCopy.
RUN itemfg-inkCopy.
RUN itemfg-locCopy.
RUN itemfgdtlCopy.
RUN jc-ctrlCopy.
RUN jobCopy.
RUN job-allCopy.
RUN job-brdCopy.
RUN job-hdrCopy.
RUN job-matCopy.
RUN job-mchCopy.
RUN job-prepCopy.
RUN job-schCopy.
RUN loadtagCopy.
RUN locCopy.
RUN machCopy.
RUN mach-adderCopy.
RUN mach-calendarCopy.
RUN matCopy.
RUN mat-actCopy.
RUN matprepCopy.
RUN mch-actCopy.
RUN misc-actCopy.
RUN mmtxCopy.
RUN mmtx2Copy.
RUN mmtyCopy.
RUN mstdCopy.
RUN oe-bolhCopy.
RUN oe-bollCopy.
RUN oe-boll-qtyCopy.
RUN oe-ctrlCopy.
RUN oe-ordCopy.
RUN oe-ordlCopy.
RUN oe-ordmCopy.
RUN oe-prmtxCopy.
RUN oe-relCopy.
RUN oe-relhCopy.
RUN oe-rellCopy.
RUN oe-rethCopy.
RUN oe-retlCopy.
RUN oe-shipCopy.
RUN pc-miscCopy.
RUN pc-prddCopy.
RUN pc-prdhCopy.
RUN periodCopy.
RUN permgCopy.
RUN po-allCopy.
RUN po-ctrlCopy.
RUN po-ordCopy.
RUN po-ordlCopy.
RUN po-rcptsCopy.
RUN prepCopy.
RUN printerCopy.
RUN probeCopy.
RUN probeitCopy.
RUN probeit-priceCopy.
RUN procatCopy.
RUN prodCopy.
RUN prodlCopy.
RUN quoteCopy.
RUN quotechgCopy.
RUN quotehdCopy.
RUN quoteitCopy.
RUN quoteitmCopy.
RUN quoteqtyCopy.
RUN reftableCopy.
RUN rm-binCopy.
RUN rm-ctrlCopy.
RUN rm-rcptCopy.
RUN rm-rcpthCopy.
RUN rm-rctdCopy.
RUN rm-rcthCopy.
RUN rm-rdtlCopy.
RUN rm-rdtlhCopy.
RUN rm-receiptsCopy.
RUN routingCopy.
RUN routing-mtxCopy.
RUN shiftCopy.
RUN shiptoCopy.
RUN smanCopy.
RUN sman-mtxCopy.
/*RUN smanmtrxCopy.*/
RUN soldtoCopy.
RUN stack-fluteCopy.
RUN stack-sizeCopy.
RUN staxCopy.
RUN stax-groupCopy.
RUN std-codeCopy.
RUN styleCopy.
RUN sys-ctrlCopy.
RUN termsCopy.
RUN terrCopy.
RUN test-redCopy.
RUN user-printCopy.
RUN usercompCopy.
RUN usrCopy.
RUN usr-grpCopy.
RUN usrxCopy.
RUN vendCopy.
RUN ventypeCopy.

PROCEDURE accountCopy:
  DEFINE BUFFER baccount FOR account.

  FOR EACH account NO-LOCK:
    CREATE baccount.
    BUFFER-COPY account EXCEPT company TO baccount
      ASSIGN baccount.company = "002".
  END.
END PROCEDURE.

PROCEDURE acctcostCopy:
  DEFINE BUFFER bacctcost FOR acctcost.

  FOR EACH acctcost NO-LOCK:
    CREATE bacctcost.
    BUFFER-COPY acctcost EXCEPT company TO bacctcost
      ASSIGN bacctcost.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-buyCopy:
  DEFINE BUFFER bap-buy FOR ap-buy.

  FOR EACH ap-buy NO-LOCK:
    CREATE bap-buy.
    BUFFER-COPY ap-buy EXCEPT company TO bap-buy
      ASSIGN bap-buy.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-chkCopy:
  DEFINE BUFFER bap-chk FOR ap-chk.

  FOR EACH ap-chk NO-LOCK:
    CREATE bap-chk.
    BUFFER-COPY ap-chk EXCEPT company TO bap-chk
      ASSIGN bap-chk.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-ctrlCopy:
  DEFINE BUFFER bap-ctrl FOR ap-ctrl.

  FOR EACH ap-ctrl NO-LOCK:
    CREATE bap-ctrl.
    BUFFER-COPY ap-ctrl EXCEPT company TO bap-ctrl
      ASSIGN bap-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-disCopy:
  DEFINE BUFFER bap-dis FOR ap-dis.

  FOR EACH ap-dis NO-LOCK:
    CREATE bap-dis.
    BUFFER-COPY ap-dis EXCEPT company TO bap-dis
      ASSIGN bap-dis.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-dislCopy:
  DEFINE BUFFER bap-disl FOR ap-disl.

  FOR EACH ap-disl NO-LOCK:
    CREATE bap-disl.
    BUFFER-COPY ap-disl EXCEPT company TO bap-disl
      ASSIGN bap-disl.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-invCopy:
  DEFINE BUFFER bap-inv FOR ap-inv.

  FOR EACH ap-inv NO-LOCK:
    CREATE bap-inv.
    BUFFER-COPY ap-inv EXCEPT company TO bap-inv
      ASSIGN bap-inv.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-invlCopy:
  DEFINE BUFFER bap-invl FOR ap-invl.

  FOR EACH ap-invl NO-LOCK:
    CREATE bap-invl.
    BUFFER-COPY ap-invl EXCEPT company TO bap-invl
      ASSIGN bap-invl.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-invlrCopy:
  DEFINE BUFFER bap-invlr FOR ap-invlr.

  FOR EACH ap-invlr NO-LOCK:
    CREATE bap-invlr.
    BUFFER-COPY ap-invlr EXCEPT company TO bap-invlr
      ASSIGN bap-invlr.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-ledgerCopy:
  DEFINE BUFFER bap-ledger FOR ap-ledger.

  FOR EACH ap-ledger NO-LOCK:
    CREATE bap-ledger.
    BUFFER-COPY ap-ledger EXCEPT company TO bap-ledger
      ASSIGN bap-ledger.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-payCopy:
  DEFINE BUFFER bap-pay FOR ap-pay.

  FOR EACH ap-pay NO-LOCK:
    CREATE bap-pay.
    BUFFER-COPY ap-pay EXCEPT company TO bap-pay
      ASSIGN bap-pay.company = "002".
  END.
END PROCEDURE.

PROCEDURE ap-selCopy:
  DEFINE BUFFER bap-sel FOR ap-sel.

  FOR EACH ap-sel NO-LOCK:
    CREATE bap-sel.
    BUFFER-COPY ap-sel EXCEPT company TO bap-sel
      ASSIGN bap-sel.company = "002".
  END.
END PROCEDURE.

PROCEDURE aphistCopy:
  DEFINE BUFFER baphist FOR aphist.

  FOR EACH aphist NO-LOCK:
    CREATE baphist.
    BUFFER-COPY aphist EXCEPT company TO baphist
      ASSIGN baphist.company = "002".
  END.
END PROCEDURE.

PROCEDURE ar-cashCopy:
  DEFINE BUFFER bar-cash FOR ar-cash.

  FOR EACH ar-cash NO-LOCK:
    CREATE bar-cash.
    BUFFER-COPY ar-cash EXCEPT company TO bar-cash
      ASSIGN bar-cash.company = "002".
  END.
END PROCEDURE.

PROCEDURE ar-cashlCopy:
  DEFINE BUFFER bar-cashl FOR ar-cashl.

  FOR EACH ar-cashl NO-LOCK:
    CREATE bar-cashl.
    BUFFER-COPY ar-cashl EXCEPT company TO bar-cashl
      ASSIGN bar-cashl.company = "002".
  END.
END PROCEDURE.

PROCEDURE ar-ctrlCopy:
  DEFINE BUFFER bar-ctrl FOR ar-ctrl.

  FOR EACH ar-ctrl NO-LOCK:
    CREATE bar-ctrl.
    BUFFER-COPY ar-ctrl EXCEPT company TO bar-ctrl
      ASSIGN bar-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE ar-invCopy:
  DEFINE BUFFER bar-inv FOR ar-inv.

  FOR EACH ar-inv NO-LOCK:
    CREATE bar-inv.
    BUFFER-COPY ar-inv EXCEPT company TO bar-inv
      ASSIGN bar-inv.company = "002".
  END.
END PROCEDURE.

PROCEDURE ar-invlCopy:
  DEFINE BUFFER bar-invl FOR ar-invl.

  FOR EACH ar-invl NO-LOCK:
    CREATE bar-invl.
    BUFFER-COPY ar-invl EXCEPT company TO bar-invl
      ASSIGN bar-invl.company = "002".
  END.
END PROCEDURE.

PROCEDURE ar-invmCopy:
  DEFINE BUFFER bar-invm FOR ar-invm.

  FOR EACH ar-invm NO-LOCK:
    CREATE bar-invm.
    BUFFER-COPY ar-invm EXCEPT company TO bar-invm
      ASSIGN bar-invm.company = "002".
  END.
END PROCEDURE.

PROCEDURE ar-ledgerCopy:
  DEFINE BUFFER bar-ledger FOR ar-ledger.

  FOR EACH ar-ledger NO-LOCK:
    CREATE bar-ledger.
    BUFFER-COPY ar-ledger EXCEPT company TO bar-ledger
      ASSIGN bar-ledger.company = "002".
  END.
END PROCEDURE.

PROCEDURE ar-mcashCopy:
  DEFINE BUFFER bar-mcash FOR ar-mcash.

  FOR EACH ar-mcash NO-LOCK:
    CREATE bar-mcash.
    BUFFER-COPY ar-mcash EXCEPT company TO bar-mcash
      ASSIGN bar-mcash.company = "002".
  END.
END PROCEDURE.

PROCEDURE asinotesCopy:
  DEFINE BUFFER basinotes FOR asinotes.

  FOR EACH asinotes NO-LOCK:
    CREATE basinotes.
    BUFFER-COPY asinotes EXCEPT company TO basinotes
      ASSIGN basinotes.company = "002".
  END.
END PROCEDURE.

PROCEDURE bankCopy:
  DEFINE BUFFER bbank FOR bank.

  FOR EACH bank NO-LOCK:
    CREATE bbank.
    BUFFER-COPY bank EXCEPT company TO bbank
      ASSIGN bbank.company = "002".
  END.
END PROCEDURE.

PROCEDURE box-design-hdrCopy:
  DEFINE BUFFER bbox-design-hdr FOR box-design-hdr.

  FOR EACH box-design-hdr NO-LOCK:
    CREATE bbox-design-hdr.
    BUFFER-COPY box-design-hdr EXCEPT company TO bbox-design-hdr
      ASSIGN bbox-design-hdr.company = "002".
  END.
END PROCEDURE.

PROCEDURE box-design-lineCopy:
  DEFINE BUFFER bbox-design-line FOR box-design-line.

  FOR EACH box-design-line NO-LOCK:
    CREATE bbox-design-line.
    BUFFER-COPY box-design-line EXCEPT company TO bbox-design-line
      ASSIGN bbox-design-line.company = "002".
  END.
END PROCEDURE.

PROCEDURE buyerCopy:
  DEFINE BUFFER bbuyer FOR buyer.

  FOR EACH buyer NO-LOCK:
    CREATE bbuyer.
    BUFFER-COPY buyer EXCEPT company TO bbuyer
      ASSIGN bbuyer.company = "002".
  END.
END PROCEDURE.

PROCEDURE carr-mtxCopy:
  DEFINE BUFFER bcarr-mtx FOR carr-mtx.

  FOR EACH carr-mtx NO-LOCK:
    CREATE bcarr-mtx.
    BUFFER-COPY carr-mtx EXCEPT company TO bcarr-mtx
      ASSIGN bcarr-mtx.company = "002".
  END.
END PROCEDURE.

PROCEDURE carrierCopy:
  DEFINE BUFFER bcarrier FOR carrier.

  FOR EACH carrier NO-LOCK:
    CREATE bcarrier.
    BUFFER-COPY carrier EXCEPT company TO bcarrier
      ASSIGN bcarrier.company = "002".
  END.
END PROCEDURE.

PROCEDURE ce-ctrlCopy:
  DEFINE BUFFER bce-ctrl FOR ce-ctrl.

  FOR EACH ce-ctrl NO-LOCK:
    CREATE bce-ctrl.
    BUFFER-COPY ce-ctrl EXCEPT company TO bce-ctrl
      ASSIGN bce-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE companyCopy:
  DEFINE BUFFER bcompany FOR company.

  FOR EACH company NO-LOCK:
    CREATE bcompany.
    BUFFER-COPY company EXCEPT company TO bcompany
      ASSIGN bcompany.company = "002".
  END.
END PROCEDURE.

PROCEDURE costtypeCopy:
  DEFINE BUFFER bcosttype FOR costtype.

  FOR EACH costtype NO-LOCK:
    CREATE bcosttype.
    BUFFER-COPY costtype EXCEPT company TO bcosttype
      ASSIGN bcosttype.company = "002".
  END.
END PROCEDURE.

PROCEDURE crewCopy:
  DEFINE BUFFER bcrew FOR crew.

  FOR EACH crew NO-LOCK:
    CREATE bcrew.
    BUFFER-COPY crew EXCEPT company TO bcrew
      ASSIGN bcrew.company = "002".
  END.
END PROCEDURE.

PROCEDURE currencyCopy:
  DEFINE BUFFER bcurrency FOR currency.

  FOR EACH currency NO-LOCK:
    CREATE bcurrency.
    BUFFER-COPY currency EXCEPT company TO bcurrency
      ASSIGN bcurrency.company = "002".
  END.
END PROCEDURE.

PROCEDURE custCopy:
  DEFINE BUFFER bcust FOR cust.

  FOR EACH cust NO-LOCK:
    CREATE bcust.
    BUFFER-COPY cust EXCEPT company TO bcust
      ASSIGN bcust.company = "002".
  END.
END PROCEDURE.

PROCEDURE cust-markupCopy:
  DEFINE BUFFER bcust-markup FOR cust-markup.

  FOR EACH cust-markup NO-LOCK:
    CREATE bcust-markup.
    BUFFER-COPY cust-markup EXCEPT company TO bcust-markup
      ASSIGN bcust-markup.company = "002".
  END.
END PROCEDURE.

PROCEDURE cust-partCopy:
  DEFINE BUFFER bcust-part FOR cust-part.

  FOR EACH cust-part NO-LOCK:
    CREATE bcust-part.
    BUFFER-COPY cust-part EXCEPT company TO bcust-part
      ASSIGN bcust-part.company = "002".
  END.
END PROCEDURE.

PROCEDURE cust-prod-salesCopy:
  DEFINE BUFFER bcust-prod-sales FOR cust-prod-sales.

  FOR EACH cust-prod-sales NO-LOCK:
    CREATE bcust-prod-sales.
    BUFFER-COPY cust-prod-sales EXCEPT company TO bcust-prod-sales
      ASSIGN bcust-prod-sales.company = "002".
  END.
END PROCEDURE.

PROCEDURE custypeCopy:
  DEFINE BUFFER bcustype FOR custype.

  FOR EACH custype NO-LOCK:
    CREATE bcustype.
    BUFFER-COPY custype EXCEPT company TO bcustype
      ASSIGN bcustype.company = "002".
  END.
END PROCEDURE.

PROCEDURE db-ctrlCopy:
  DEFINE BUFFER bdb-ctrl FOR db-ctrl.

  FOR EACH db-ctrl NO-LOCK:
    CREATE bdb-ctrl.
    BUFFER-COPY db-ctrl EXCEPT company TO bdb-ctrl
      ASSIGN bdb-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE deptCopy:
  DEFINE BUFFER bdept FOR dept.

  FOR EACH dept NO-LOCK:
    CREATE bdept.
    BUFFER-COPY dept EXCEPT company TO bdept
      ASSIGN bdept.company = "002".
  END.
END PROCEDURE.

PROCEDURE e-itemCopy:
  DEFINE BUFFER be-item FOR e-item.

  FOR EACH e-item NO-LOCK:
    CREATE be-item.
    BUFFER-COPY e-item EXCEPT company TO be-item
      ASSIGN be-item.company = "002".
  END.
END PROCEDURE.

PROCEDURE e-item-custCopy:
  DEFINE BUFFER be-item-cust FOR e-item-cust.

  FOR EACH e-item-cust NO-LOCK:
    CREATE be-item-cust.
    BUFFER-COPY e-item-cust EXCEPT company TO be-item-cust
      ASSIGN be-item-cust.company = "002".
  END.
END PROCEDURE.

PROCEDURE e-item-vendCopy:
  DEFINE BUFFER be-item-vend FOR e-item-vend.

  FOR EACH e-item-vend NO-LOCK:
    CREATE be-item-vend.
    BUFFER-COPY e-item-vend EXCEPT company TO be-item-vend
      ASSIGN be-item-vend.company = "002".
  END.
END PROCEDURE.

PROCEDURE e-itemfgCopy:
  DEFINE BUFFER be-itemfg FOR e-itemfg.

  FOR EACH e-itemfg NO-LOCK:
    CREATE be-itemfg.
    BUFFER-COPY e-itemfg EXCEPT company TO be-itemfg
      ASSIGN be-itemfg.company = "002".
  END.
END PROCEDURE.

PROCEDURE e-itemfg-vendCopy:
  DEFINE BUFFER be-itemfg-vend FOR e-itemfg-vend.

  FOR EACH e-itemfg-vend NO-LOCK:
    CREATE be-itemfg-vend.
    BUFFER-COPY e-itemfg-vend EXCEPT company TO be-itemfg-vend
      ASSIGN be-itemfg-vend.company = "002".
  END.
END PROCEDURE.

PROCEDURE ebCopy:
  DEFINE BUFFER beb FOR eb.

  FOR EACH eb NO-LOCK:
    CREATE beb.
    BUFFER-COPY eb EXCEPT company TO beb
      ASSIGN beb.company = "002".
  END.
END PROCEDURE.

PROCEDURE EDAPCheckCopy:
  DEFINE BUFFER bEDAPCheck FOR EDAPCheck.

  FOR EACH EDAPCheck NO-LOCK:
    CREATE bEDAPCheck.
    BUFFER-COPY EDAPCheck EXCEPT company TO bEDAPCheck
      ASSIGN bEDAPCheck.company = "002".
  END.
END PROCEDURE.

PROCEDURE EDCoCopy:
  DEFINE BUFFER bEDCo FOR EDCo.

  FOR EACH EDCo NO-LOCK:
    CREATE bEDCo.
    BUFFER-COPY EDCo EXCEPT company TO bEDCo
      ASSIGN bEDCo.company = "002".
  END.
END PROCEDURE.

PROCEDURE EDICXrefCopy:
  DEFINE BUFFER bEDICXref FOR EDICXref.

  FOR EACH EDICXref NO-LOCK:
    CREATE bEDICXref.
    BUFFER-COPY EDICXref EXCEPT company TO bEDICXref
      ASSIGN bEDICXref.company = "002".
  END.
END PROCEDURE.

PROCEDURE EDIVAddonCopy:
  DEFINE BUFFER bEDIVAddon FOR EDIVAddon.

  FOR EACH EDIVAddon NO-LOCK:
    CREATE bEDIVAddon.
    BUFFER-COPY EDIVAddon EXCEPT company TO bEDIVAddon
      ASSIGN bEDIVAddon.company = "002".
  END.
END PROCEDURE.

PROCEDURE EDIVLineCopy:
  DEFINE BUFFER bEDIVLine FOR EDIVLine.

  FOR EACH EDIVLine NO-LOCK:
    CREATE bEDIVLine.
    BUFFER-COPY EDIVLine EXCEPT company TO bEDIVLine
      ASSIGN bEDIVLine.company = "002".
  END.
END PROCEDURE.

PROCEDURE EDIVTranCopy:
  DEFINE BUFFER bEDIVTran FOR EDIVTran.

  FOR EACH EDIVTran NO-LOCK:
    CREATE bEDIVTran.
    BUFFER-COPY EDIVTran EXCEPT company TO bEDIVTran
      ASSIGN bEDIVTran.company = "002".
  END.
END PROCEDURE.

PROCEDURE efCopy:
  DEFINE BUFFER bef FOR ef.

  FOR EACH ef NO-LOCK:
    CREATE bef.
    BUFFER-COPY ef EXCEPT company TO bef
      ASSIGN bef.company = "002".
  END.
END PROCEDURE.

PROCEDURE ef-nshCopy:
  DEFINE BUFFER bef-nsh FOR ef-nsh.

  FOR EACH ef-nsh NO-LOCK:
    CREATE bef-nsh.
    BUFFER-COPY ef-nsh EXCEPT company TO bef-nsh
      ASSIGN bef-nsh.company = "002".
  END.
END PROCEDURE.

PROCEDURE empCopy:
  DEFINE BUFFER bemp FOR emp.

  FOR EACH emp NO-LOCK:
    CREATE bemp.
    BUFFER-COPY emp EXCEPT company TO bemp
      ASSIGN bemp.company = "002".
  END.
END PROCEDURE.

PROCEDURE estCopy:
  DEFINE BUFFER best FOR est.

  FOR EACH est NO-LOCK:
    CREATE best.
    BUFFER-COPY est EXCEPT company TO best
      ASSIGN best.company = "002".
  END.
END PROCEDURE.

PROCEDURE est-flmCopy:
  DEFINE BUFFER best-flm FOR est-flm.

  FOR EACH est-flm NO-LOCK:
    CREATE best-flm.
    BUFFER-COPY est-flm EXCEPT company TO best-flm
      ASSIGN best-flm.company = "002".
  END.
END PROCEDURE.

PROCEDURE est-instCopy:
  DEFINE BUFFER best-inst FOR est-inst.

  FOR EACH est-inst NO-LOCK:
    CREATE best-inst.
    BUFFER-COPY est-inst EXCEPT company TO best-inst
      ASSIGN best-inst.company = "002".
  END.
END PROCEDURE.

PROCEDURE est-opCopy:
  DEFINE BUFFER best-op FOR est-op.

  FOR EACH est-op NO-LOCK:
    CREATE best-op.
    BUFFER-COPY est-op EXCEPT company TO best-op
      ASSIGN best-op.company = "002".
  END.
END PROCEDURE.

PROCEDURE est-pfCopy:
  DEFINE BUFFER best-pf FOR est-pf.

  FOR EACH est-pf NO-LOCK:
    CREATE best-pf.
    BUFFER-COPY est-pf EXCEPT company TO best-pf
      ASSIGN best-pf.company = "002".
  END.
END PROCEDURE.

PROCEDURE est-prepCopy:
  DEFINE BUFFER best-prep FOR est-prep.

  FOR EACH est-prep NO-LOCK:
    CREATE best-prep.
    BUFFER-COPY est-prep EXCEPT company TO best-prep
      ASSIGN best-prep.company = "002".
  END.
END PROCEDURE.

PROCEDURE est-qtyCopy:
  DEFINE BUFFER best-qty FOR est-qty.

  FOR EACH est-qty NO-LOCK:
    CREATE best-qty.
    BUFFER-COPY est-qty EXCEPT company TO best-qty
      ASSIGN best-qty.company = "002".
  END.
END PROCEDURE.

PROCEDURE est-summCopy:
  DEFINE BUFFER best-summ FOR est-summ.

  FOR EACH est-summ NO-LOCK:
    CREATE best-summ.
    BUFFER-COPY est-summ EXCEPT company TO best-summ
      ASSIGN best-summ.company = "002".
  END.
END PROCEDURE.

PROCEDURE fg-actCopy:
  DEFINE BUFFER bfg-act FOR fg-act.

  FOR EACH fg-act NO-LOCK:
    CREATE bfg-act.
    BUFFER-COPY fg-act EXCEPT company TO bfg-act
      ASSIGN bfg-act.company = "002".
  END.
END PROCEDURE.

PROCEDURE fg-binCopy:
  DEFINE BUFFER bfg-bin FOR fg-bin.

  FOR EACH fg-bin NO-LOCK:
    CREATE bfg-bin.
    BUFFER-COPY fg-bin EXCEPT company TO bfg-bin
      ASSIGN bfg-bin.company = "002".
  END.
END PROCEDURE.

PROCEDURE fg-ctrlCopy:
  DEFINE BUFFER bfg-ctrl FOR fg-ctrl.

  FOR EACH fg-ctrl NO-LOCK:
    CREATE bfg-ctrl.
    BUFFER-COPY fg-ctrl EXCEPT company TO bfg-ctrl
      ASSIGN bfg-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE fg-histCopy:
  DEFINE BUFFER bfg-hist FOR fg-hist.

  FOR EACH fg-hist NO-LOCK:
    CREATE bfg-hist.
    BUFFER-COPY fg-hist EXCEPT company TO bfg-hist
      ASSIGN bfg-hist.company = "002".
  END.
END PROCEDURE.

PROCEDURE fg-rcpthCopy:
  DEFINE BUFFER bfg-rcpth FOR fg-rcpth.

  FOR EACH fg-rcpth NO-LOCK:
    CREATE bfg-rcpth.
    BUFFER-COPY fg-rcpth EXCEPT company TO bfg-rcpth
      ASSIGN bfg-rcpth.company = "002".
  END.
END PROCEDURE.

PROCEDURE fg-rcptsCopy:
  DEFINE BUFFER bfg-rcpts FOR fg-rcpts.

  FOR EACH fg-rcpts NO-LOCK:
    CREATE bfg-rcpts.
    BUFFER-COPY fg-rcpts EXCEPT company TO bfg-rcpts
      ASSIGN bfg-rcpts.company = "002".
  END.
END PROCEDURE.

PROCEDURE fg-rctdCopy:
  DEFINE BUFFER bfg-rctd FOR fg-rctd.

  FOR EACH fg-rctd NO-LOCK:
    CREATE bfg-rctd.
    BUFFER-COPY fg-rctd EXCEPT company TO bfg-rctd
      ASSIGN bfg-rctd.company = "002".
  END.
END PROCEDURE.

PROCEDURE fg-rdtlCopy:
  DEFINE BUFFER bfg-rdtl FOR fg-rdtl.

  FOR EACH fg-rdtl NO-LOCK:
    CREATE bfg-rdtl.
    BUFFER-COPY fg-rdtl EXCEPT company TO bfg-rdtl
      ASSIGN bfg-rdtl.company = "002".
  END.
END PROCEDURE.

PROCEDURE fg-rdtlhCopy:
  DEFINE BUFFER bfg-rdtlh FOR fg-rdtlh.

  FOR EACH fg-rdtlh NO-LOCK:
    CREATE bfg-rdtlh.
    BUFFER-COPY fg-rdtlh EXCEPT company TO bfg-rdtlh
      ASSIGN bfg-rdtlh.company = "002".
  END.
END PROCEDURE.

PROCEDURE fg-setCopy:
  DEFINE BUFFER bfg-set FOR fg-set.

  FOR EACH fg-set NO-LOCK:
    CREATE bfg-set.
    BUFFER-COPY fg-set EXCEPT company TO bfg-set
      ASSIGN bfg-set.company = "002".
  END.
END PROCEDURE.

PROCEDURE fgcatCopy:
  DEFINE BUFFER bfgcat FOR fgcat.

  FOR EACH fgcat NO-LOCK:
    CREATE bfgcat.
    BUFFER-COPY fgcat EXCEPT company TO bfgcat
      ASSIGN bfgcat.company = "002".
  END.
END PROCEDURE.

PROCEDURE fluteCopy:
  DEFINE BUFFER bflute FOR flute.

  FOR EACH flute NO-LOCK:
    CREATE bflute.
    BUFFER-COPY flute EXCEPT company TO bflute
      ASSIGN bflute.company = "002".
  END.
END PROCEDURE.

PROCEDURE gl-ctrlCopy:
  DEFINE BUFFER bgl-ctrl FOR gl-ctrl.

  FOR EACH gl-ctrl NO-LOCK:
    CREATE bgl-ctrl.
    BUFFER-COPY gl-ctrl EXCEPT company TO bgl-ctrl
      ASSIGN bgl-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE gl-freqCopy:
  DEFINE BUFFER bgl-freq FOR gl-freq.

  FOR EACH gl-freq NO-LOCK:
    CREATE bgl-freq.
    BUFFER-COPY gl-freq EXCEPT company TO bgl-freq
      ASSIGN bgl-freq.company = "002".
  END.
END PROCEDURE.

PROCEDURE gl-jrnCopy:
  DEFINE BUFFER bgl-jrn FOR gl-jrn.

  FOR EACH gl-jrn NO-LOCK:
    CREATE bgl-jrn.
    BUFFER-COPY gl-jrn EXCEPT company TO bgl-jrn
      ASSIGN bgl-jrn.company = "002".
  END.
END PROCEDURE.

PROCEDURE gl-rptCopy:
  DEFINE BUFFER bgl-rpt FOR gl-rpt.

  FOR EACH gl-rpt NO-LOCK:
    CREATE bgl-rpt.
    BUFFER-COPY gl-rpt EXCEPT company TO bgl-rpt
      ASSIGN bgl-rpt.company = "002".
  END.
END PROCEDURE.

PROCEDURE gl-rptdCopy:
  DEFINE BUFFER bgl-rptd FOR gl-rptd.

  FOR EACH gl-rptd NO-LOCK:
    CREATE bgl-rptd.
    BUFFER-COPY gl-rptd EXCEPT company TO bgl-rptd
      ASSIGN bgl-rptd.company = "002".
  END.
END PROCEDURE.

PROCEDURE glhistCopy:
  DEFINE BUFFER bglhist FOR glhist.

  FOR EACH glhist NO-LOCK:
    CREATE bglhist.
    BUFFER-COPY glhist EXCEPT company TO bglhist
      ASSIGN bglhist.company = "002".
  END.
END PROCEDURE.

PROCEDURE gltransCopy:
  DEFINE BUFFER bgltrans FOR gltrans.

  FOR EACH gltrans NO-LOCK:
    CREATE bgltrans.
    BUFFER-COPY gltrans EXCEPT company TO bgltrans
      ASSIGN bgltrans.company = "002".
  END.
END PROCEDURE.

PROCEDURE inv-headCopy:
  DEFINE BUFFER binv-head FOR inv-head.

  FOR EACH inv-head NO-LOCK:
    CREATE binv-head.
    BUFFER-COPY inv-head EXCEPT company TO binv-head
      ASSIGN binv-head.company = "002".
  END.
END PROCEDURE.

PROCEDURE inv-lineCopy:
  DEFINE BUFFER binv-line FOR inv-line.

  FOR EACH inv-line NO-LOCK:
    CREATE binv-line.
    BUFFER-COPY inv-line EXCEPT company TO binv-line
      ASSIGN binv-line.company = "002".
  END.
END PROCEDURE.

PROCEDURE inv-miscCopy:
  DEFINE BUFFER binv-misc FOR inv-misc.

  FOR EACH inv-misc NO-LOCK:
    CREATE binv-misc.
    BUFFER-COPY inv-misc EXCEPT company TO binv-misc
      ASSIGN binv-misc.company = "002".
  END.
END PROCEDURE.

PROCEDURE itemCopy:
  DEFINE BUFFER bitem FOR item.

  FOR EACH item NO-LOCK:
    CREATE bitem.
    BUFFER-COPY item EXCEPT company TO bitem
      ASSIGN bitem.company = "002".
  END.
END PROCEDURE.

PROCEDURE item-bomCopy:
  DEFINE BUFFER bitem-bom FOR item-bom.

  FOR EACH item-bom NO-LOCK:
    CREATE bitem-bom.
    BUFFER-COPY item-bom EXCEPT company TO bitem-bom
      ASSIGN bitem-bom.company = "002".
  END.
END PROCEDURE.

PROCEDURE item-specCopy:
  DEFINE BUFFER bitem-spec FOR item-spec.

  FOR EACH item-spec NO-LOCK:
    CREATE bitem-spec.
    BUFFER-COPY item-spec EXCEPT company TO bitem-spec
      ASSIGN bitem-spec.company = "002".
  END.
END PROCEDURE.

PROCEDURE itemfgCopy:
  DEFINE BUFFER bitemfg FOR itemfg.

  FOR EACH itemfg NO-LOCK:
    CREATE bitemfg.
    BUFFER-COPY itemfg EXCEPT company TO bitemfg
      ASSIGN bitemfg.company = "002".
  END.
END PROCEDURE.

PROCEDURE itemfg-bomCopy:
  DEFINE BUFFER bitemfg-bom FOR itemfg-bom.

  FOR EACH itemfg-bom NO-LOCK:
    CREATE bitemfg-bom.
    BUFFER-COPY itemfg-bom EXCEPT company TO bitemfg-bom
      ASSIGN bitemfg-bom.company = "002".
  END.
END PROCEDURE.

PROCEDURE itemfg-inkCopy:
  DEFINE BUFFER bitemfg-ink FOR itemfg-ink.

  FOR EACH itemfg-ink NO-LOCK:
    CREATE bitemfg-ink.
    BUFFER-COPY itemfg-ink EXCEPT company TO bitemfg-ink
      ASSIGN bitemfg-ink.company = "002".
  END.
END PROCEDURE.

PROCEDURE itemfg-locCopy:
  DEFINE BUFFER bitemfg-loc FOR itemfg-loc.

  FOR EACH itemfg-loc NO-LOCK:
    CREATE bitemfg-loc.
    BUFFER-COPY itemfg-loc EXCEPT company TO bitemfg-loc
      ASSIGN bitemfg-loc.company = "002".
  END.
END PROCEDURE.

PROCEDURE itemfgdtlCopy:
  DEFINE BUFFER bitemfgdtl FOR itemfgdtl.

  FOR EACH itemfgdtl NO-LOCK:
    CREATE bitemfgdtl.
    BUFFER-COPY itemfgdtl EXCEPT company TO bitemfgdtl
      ASSIGN bitemfgdtl.company = "002".
  END.
END PROCEDURE.

PROCEDURE jc-ctrlCopy:
  DEFINE BUFFER bjc-ctrl FOR jc-ctrl.

  FOR EACH jc-ctrl NO-LOCK:
    CREATE bjc-ctrl.
    BUFFER-COPY jc-ctrl EXCEPT company TO bjc-ctrl
      ASSIGN bjc-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE jobCopy:
  DEFINE BUFFER bjob FOR job.

  FOR EACH job NO-LOCK:
    CREATE bjob.
    BUFFER-COPY job EXCEPT company TO bjob
      ASSIGN bjob.company = "002".
  END.
END PROCEDURE.

PROCEDURE job-allCopy:
  DEFINE BUFFER bjob-all FOR job-all.

  FOR EACH job-all NO-LOCK:
    CREATE bjob-all.
    BUFFER-COPY job-all EXCEPT company TO bjob-all
      ASSIGN bjob-all.company = "002".
  END.
END PROCEDURE.

PROCEDURE job-brdCopy:
  DEFINE BUFFER bjob-brd FOR job-brd.

  FOR EACH job-brd NO-LOCK:
    CREATE bjob-brd.
    BUFFER-COPY job-brd EXCEPT company TO bjob-brd
      ASSIGN bjob-brd.company = "002".
  END.
END PROCEDURE.

PROCEDURE job-hdrCopy:
  DEFINE BUFFER bjob-hdr FOR job-hdr.

  FOR EACH job-hdr NO-LOCK:
    CREATE bjob-hdr.
    BUFFER-COPY job-hdr EXCEPT company TO bjob-hdr
      ASSIGN bjob-hdr.company = "002".
  END.
END PROCEDURE.

PROCEDURE job-matCopy:
  DEFINE BUFFER bjob-mat FOR job-mat.

  FOR EACH job-mat NO-LOCK:
    CREATE bjob-mat.
    BUFFER-COPY job-mat EXCEPT company TO bjob-mat
      ASSIGN bjob-mat.company = "002".
  END.
END PROCEDURE.

PROCEDURE job-mchCopy:
  DEFINE BUFFER bjob-mch FOR job-mch.

  FOR EACH job-mch NO-LOCK:
    CREATE bjob-mch.
    BUFFER-COPY job-mch EXCEPT company TO bjob-mch
      ASSIGN bjob-mch.company = "002".
  END.
END PROCEDURE.

PROCEDURE job-prepCopy:
  DEFINE BUFFER bjob-prep FOR job-prep.

  FOR EACH job-prep NO-LOCK:
    CREATE bjob-prep.
    BUFFER-COPY job-prep EXCEPT company TO bjob-prep
      ASSIGN bjob-prep.company = "002".
  END.
END PROCEDURE.

PROCEDURE job-schCopy:
  DEFINE BUFFER bjob-sch FOR job-sch.

  FOR EACH job-sch NO-LOCK:
    CREATE bjob-sch.
    BUFFER-COPY job-sch EXCEPT company TO bjob-sch
      ASSIGN bjob-sch.company = "002".
  END.
END PROCEDURE.

PROCEDURE loadtagCopy:
  DEFINE BUFFER bloadtag FOR loadtag.

  FOR EACH loadtag NO-LOCK:
    CREATE bloadtag.
    BUFFER-COPY loadtag EXCEPT company TO bloadtag
      ASSIGN bloadtag.company = "002".
  END.
END PROCEDURE.

PROCEDURE locCopy:
  DEFINE BUFFER bloc FOR loc.

  FOR EACH loc NO-LOCK:
    CREATE bloc.
    BUFFER-COPY loc EXCEPT company TO bloc
      ASSIGN bloc.company = "002".
  END.
END PROCEDURE.

PROCEDURE machCopy:
  DEFINE BUFFER bmach FOR mach.

  FOR EACH mach NO-LOCK:
    CREATE bmach.
    BUFFER-COPY mach EXCEPT company TO bmach
      ASSIGN bmach.company = "002".
  END.
END PROCEDURE.

PROCEDURE mach-adderCopy:
  DEFINE BUFFER bmach-adder FOR mach-adder.

  FOR EACH mach-adder NO-LOCK:
    CREATE bmach-adder.
    BUFFER-COPY mach-adder EXCEPT company TO bmach-adder
      ASSIGN bmach-adder.company = "002".
  END.
END PROCEDURE.

PROCEDURE mach-calendarCopy:
  DEFINE BUFFER bmach-calendar FOR mach-calendar.

  FOR EACH mach-calendar NO-LOCK:
    CREATE bmach-calendar.
    BUFFER-COPY mach-calendar EXCEPT company TO bmach-calendar
      ASSIGN bmach-calendar.company = "002".
  END.
END PROCEDURE.

PROCEDURE matCopy:
  DEFINE BUFFER bmat FOR mat.

  FOR EACH mat NO-LOCK:
    CREATE bmat.
    BUFFER-COPY mat EXCEPT company TO bmat
      ASSIGN bmat.company = "002".
  END.
END PROCEDURE.

PROCEDURE mat-actCopy:
  DEFINE BUFFER bmat-act FOR mat-act.

  FOR EACH mat-act NO-LOCK:
    CREATE bmat-act.
    BUFFER-COPY mat-act EXCEPT company TO bmat-act
      ASSIGN bmat-act.company = "002".
  END.
END PROCEDURE.

PROCEDURE matprepCopy:
  DEFINE BUFFER bmatprep FOR matprep.

  FOR EACH matprep NO-LOCK:
    CREATE bmatprep.
    BUFFER-COPY matprep EXCEPT company TO bmatprep
      ASSIGN bmatprep.company = "002".
  END.
END PROCEDURE.

PROCEDURE mch-actCopy:
  DEFINE BUFFER bmch-act FOR mch-act.

  FOR EACH mch-act NO-LOCK:
    CREATE bmch-act.
    BUFFER-COPY mch-act EXCEPT company TO bmch-act
      ASSIGN bmch-act.company = "002".
  END.
END PROCEDURE.

PROCEDURE misc-actCopy:
  DEFINE BUFFER bmisc-act FOR misc-act.

  FOR EACH misc-act NO-LOCK:
    CREATE bmisc-act.
    BUFFER-COPY misc-act EXCEPT company TO bmisc-act
      ASSIGN bmisc-act.company = "002".
  END.
END PROCEDURE.

PROCEDURE mmtxCopy:
  DEFINE BUFFER bmmtx FOR mmtx.

  FOR EACH mmtx NO-LOCK:
    CREATE bmmtx.
    BUFFER-COPY mmtx EXCEPT company TO bmmtx
      ASSIGN bmmtx.company = "002".
  END.
END PROCEDURE.

PROCEDURE mmtx2Copy:
  DEFINE BUFFER bmmtx2 FOR mmtx2.

  FOR EACH mmtx2 NO-LOCK:
    CREATE bmmtx2.
    BUFFER-COPY mmtx2 EXCEPT company TO bmmtx2
      ASSIGN bmmtx2.company = "002".
  END.
END PROCEDURE.

PROCEDURE mmtyCopy:
  DEFINE BUFFER bmmty FOR mmty.

  FOR EACH mmty NO-LOCK:
    CREATE bmmty.
    BUFFER-COPY mmty EXCEPT company TO bmmty
      ASSIGN bmmty.company = "002".
  END.
END PROCEDURE.

PROCEDURE mstdCopy:
  DEFINE BUFFER bmstd FOR mstd.

  FOR EACH mstd NO-LOCK:
    CREATE bmstd.
    BUFFER-COPY mstd EXCEPT company TO bmstd
      ASSIGN bmstd.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-bolhCopy:
  DEFINE BUFFER boe-bolh FOR oe-bolh.

  FOR EACH oe-bolh NO-LOCK:
    CREATE boe-bolh.
    BUFFER-COPY oe-bolh EXCEPT company TO boe-bolh
      ASSIGN boe-bolh.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-bollCopy:
  DEFINE BUFFER boe-boll FOR oe-boll.

  FOR EACH oe-boll NO-LOCK:
    CREATE boe-boll.
    BUFFER-COPY oe-boll EXCEPT company TO boe-boll
      ASSIGN boe-boll.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-boll-qtyCopy:
  DEFINE BUFFER boe-boll-qty FOR oe-boll-qty.

  FOR EACH oe-boll-qty NO-LOCK:
    CREATE boe-boll-qty.
    BUFFER-COPY oe-boll-qty EXCEPT company TO boe-boll-qty
      ASSIGN boe-boll-qty.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-ctrlCopy:
  DEFINE BUFFER boe-ctrl FOR oe-ctrl.

  FOR EACH oe-ctrl NO-LOCK:
    CREATE boe-ctrl.
    BUFFER-COPY oe-ctrl EXCEPT company TO boe-ctrl
      ASSIGN boe-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-ordCopy:
  DEFINE BUFFER boe-ord FOR oe-ord.

  FOR EACH oe-ord NO-LOCK:
    CREATE boe-ord.
    BUFFER-COPY oe-ord EXCEPT company TO boe-ord
      ASSIGN boe-ord.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-ordlCopy:
  DEFINE BUFFER boe-ordl FOR oe-ordl.

  FOR EACH oe-ordl NO-LOCK:
    CREATE boe-ordl.
    BUFFER-COPY oe-ordl EXCEPT company TO boe-ordl
      ASSIGN boe-ordl.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-ordmCopy:
  DEFINE BUFFER boe-ordm FOR oe-ordm.

  FOR EACH oe-ordm NO-LOCK:
    CREATE boe-ordm.
    BUFFER-COPY oe-ordm EXCEPT company TO boe-ordm
      ASSIGN boe-ordm.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-prmtxCopy:
  DEFINE BUFFER boe-prmtx FOR oe-prmtx.

  FOR EACH oe-prmtx NO-LOCK:
    CREATE boe-prmtx.
    BUFFER-COPY oe-prmtx EXCEPT company TO boe-prmtx
      ASSIGN boe-prmtx.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-relCopy:
  DEFINE BUFFER boe-rel FOR oe-rel.

  FOR EACH oe-rel NO-LOCK:
    CREATE boe-rel.
    BUFFER-COPY oe-rel EXCEPT company TO boe-rel
      ASSIGN boe-rel.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-relhCopy:
  DEFINE BUFFER boe-relh FOR oe-relh.

  FOR EACH oe-relh NO-LOCK:
    CREATE boe-relh.
    BUFFER-COPY oe-relh EXCEPT company TO boe-relh
      ASSIGN boe-relh.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-rellCopy:
  DEFINE BUFFER boe-rell FOR oe-rell.

  FOR EACH oe-rell NO-LOCK:
    CREATE boe-rell.
    BUFFER-COPY oe-rell EXCEPT company TO boe-rell
      ASSIGN boe-rell.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-rethCopy:
  DEFINE BUFFER boe-reth FOR oe-reth.

  FOR EACH oe-reth NO-LOCK:
    CREATE boe-reth.
    BUFFER-COPY oe-reth EXCEPT company TO boe-reth
      ASSIGN boe-reth.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-retlCopy:
  DEFINE BUFFER boe-retl FOR oe-retl.

  FOR EACH oe-retl NO-LOCK:
    CREATE boe-retl.
    BUFFER-COPY oe-retl EXCEPT company TO boe-retl
      ASSIGN boe-retl.company = "002".
  END.
END PROCEDURE.

PROCEDURE oe-shipCopy:
  DEFINE BUFFER boe-ship FOR oe-ship.

  FOR EACH oe-ship NO-LOCK:
    CREATE boe-ship.
    BUFFER-COPY oe-ship EXCEPT company TO boe-ship
      ASSIGN boe-ship.company = "002".
  END.
END PROCEDURE.

PROCEDURE pc-miscCopy:
  DEFINE BUFFER bpc-misc FOR pc-misc.

  FOR EACH pc-misc NO-LOCK:
    CREATE bpc-misc.
    BUFFER-COPY pc-misc EXCEPT company TO bpc-misc
      ASSIGN bpc-misc.company = "002".
  END.
END PROCEDURE.

PROCEDURE pc-prddCopy:
  DEFINE BUFFER bpc-prdd FOR pc-prdd.

  FOR EACH pc-prdd NO-LOCK:
    CREATE bpc-prdd.
    BUFFER-COPY pc-prdd EXCEPT company TO bpc-prdd
      ASSIGN bpc-prdd.company = "002".
  END.
END PROCEDURE.

PROCEDURE pc-prdhCopy:
  DEFINE BUFFER bpc-prdh FOR pc-prdh.

  FOR EACH pc-prdh NO-LOCK:
    CREATE bpc-prdh.
    BUFFER-COPY pc-prdh EXCEPT company TO bpc-prdh
      ASSIGN bpc-prdh.company = "002".
  END.
END PROCEDURE.

PROCEDURE periodCopy:
  DEFINE BUFFER bperiod FOR period.

  FOR EACH period NO-LOCK:
    CREATE bperiod.
    BUFFER-COPY period EXCEPT company TO bperiod
      ASSIGN bperiod.company = "002".
  END.
END PROCEDURE.

PROCEDURE permgCopy:
  DEFINE BUFFER bpermg FOR permg.

  FOR EACH permg NO-LOCK:
    CREATE bpermg.
    BUFFER-COPY permg EXCEPT company TO bpermg
      ASSIGN bpermg.company = "002".
  END.
END PROCEDURE.

PROCEDURE po-allCopy:
  DEFINE BUFFER bpo-all FOR po-all.

  FOR EACH po-all NO-LOCK:
    CREATE bpo-all.
    BUFFER-COPY po-all EXCEPT company TO bpo-all
      ASSIGN bpo-all.company = "002".
  END.
END PROCEDURE.

PROCEDURE po-ctrlCopy:
  DEFINE BUFFER bpo-ctrl FOR po-ctrl.

  FOR EACH po-ctrl NO-LOCK:
    CREATE bpo-ctrl.
    BUFFER-COPY po-ctrl EXCEPT company TO bpo-ctrl
      ASSIGN bpo-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE po-ordCopy:
  DEFINE BUFFER bpo-ord FOR po-ord.

  FOR EACH po-ord NO-LOCK:
    CREATE bpo-ord.
    BUFFER-COPY po-ord EXCEPT company TO bpo-ord
      ASSIGN bpo-ord.company = "002".
  END.
END PROCEDURE.

PROCEDURE po-ordlCopy:
  DEFINE BUFFER bpo-ordl FOR po-ordl.

  FOR EACH po-ordl NO-LOCK:
    CREATE bpo-ordl.
    BUFFER-COPY po-ordl EXCEPT company TO bpo-ordl
      ASSIGN bpo-ordl.company = "002".
  END.
END PROCEDURE.

PROCEDURE po-rcptsCopy:
  DEFINE BUFFER bpo-rcpts FOR po-rcpts.

  FOR EACH po-rcpts NO-LOCK:
    CREATE bpo-rcpts.
    BUFFER-COPY po-rcpts EXCEPT company TO bpo-rcpts
      ASSIGN bpo-rcpts.company = "002".
  END.
END PROCEDURE.

PROCEDURE prepCopy:
  DEFINE BUFFER bprep FOR prep.

  FOR EACH prep NO-LOCK:
    CREATE bprep.
    BUFFER-COPY prep EXCEPT company TO bprep
      ASSIGN bprep.company = "002".
  END.
END PROCEDURE.

PROCEDURE printerCopy:
  DEFINE BUFFER bprinter FOR printer.

  FOR EACH printer NO-LOCK:
    CREATE bprinter.
    BUFFER-COPY printer EXCEPT company TO bprinter
      ASSIGN bprinter.company = "002".
  END.
END PROCEDURE.

PROCEDURE probeCopy:
  DEFINE BUFFER bprobe FOR probe.

  FOR EACH probe NO-LOCK:
    CREATE bprobe.
    BUFFER-COPY probe EXCEPT company TO bprobe
      ASSIGN bprobe.company = "002".
  END.
END PROCEDURE.

PROCEDURE probeitCopy:
  DEFINE BUFFER bprobeit FOR probeit.

  FOR EACH probeit NO-LOCK:
    CREATE bprobeit.
    BUFFER-COPY probeit EXCEPT company TO bprobeit
      ASSIGN bprobeit.company = "002".
  END.
END PROCEDURE.

PROCEDURE probeit-priceCopy:
  DEFINE BUFFER bprobeit-price FOR probeit-price.

  FOR EACH probeit-price NO-LOCK:
    CREATE bprobeit-price.
    BUFFER-COPY probeit-price EXCEPT company TO bprobeit-price
      ASSIGN bprobeit-price.company = "002".
  END.
END PROCEDURE.

PROCEDURE procatCopy:
  DEFINE BUFFER bprocat FOR procat.

  FOR EACH procat NO-LOCK:
    CREATE bprocat.
    BUFFER-COPY procat EXCEPT company TO bprocat
      ASSIGN bprocat.company = "002".
  END.
END PROCEDURE.

PROCEDURE prodCopy:
  DEFINE BUFFER bprod FOR prod.

  FOR EACH prod NO-LOCK:
    CREATE bprod.
    BUFFER-COPY prod EXCEPT company TO bprod
      ASSIGN bprod.company = "002".
  END.
END PROCEDURE.

PROCEDURE prodlCopy:
  DEFINE BUFFER bprodl FOR prodl.

  FOR EACH prodl NO-LOCK:
    CREATE bprodl.
    BUFFER-COPY prodl EXCEPT company TO bprodl
      ASSIGN bprodl.company = "002".
  END.
END PROCEDURE.

PROCEDURE quoteCopy:
  DEFINE BUFFER bquote FOR quote.

  FOR EACH quote NO-LOCK:
    CREATE bquote.
    BUFFER-COPY quote EXCEPT company TO bquote
      ASSIGN bquote.company = "002".
  END.
END PROCEDURE.

PROCEDURE quotechgCopy:
  DEFINE BUFFER bquotechg FOR quotechg.

  FOR EACH quotechg NO-LOCK:
    CREATE bquotechg.
    BUFFER-COPY quotechg EXCEPT company TO bquotechg
      ASSIGN bquotechg.company = "002".
  END.
END PROCEDURE.

PROCEDURE quotehdCopy:
  DEFINE BUFFER bquotehd FOR quotehd.

  FOR EACH quotehd NO-LOCK:
    CREATE bquotehd.
    BUFFER-COPY quotehd EXCEPT company TO bquotehd
      ASSIGN bquotehd.company = "002".
  END.
END PROCEDURE.

PROCEDURE quoteitCopy:
  DEFINE BUFFER bquoteit FOR quoteit.

  FOR EACH quoteit NO-LOCK:
    CREATE bquoteit.
    BUFFER-COPY quoteit EXCEPT company TO bquoteit
      ASSIGN bquoteit.company = "002".
  END.
END PROCEDURE.

PROCEDURE quoteitmCopy:
  DEFINE BUFFER bquoteitm FOR quoteitm.

  FOR EACH quoteitm NO-LOCK:
    CREATE bquoteitm.
    BUFFER-COPY quoteitm EXCEPT company TO bquoteitm
      ASSIGN bquoteitm.company = "002".
  END.
END PROCEDURE.

PROCEDURE quoteqtyCopy:
  DEFINE BUFFER bquoteqty FOR quoteqty.

  FOR EACH quoteqty NO-LOCK:
    CREATE bquoteqty.
    BUFFER-COPY quoteqty EXCEPT company TO bquoteqty
      ASSIGN bquoteqty.company = "002".
  END.
END PROCEDURE.

PROCEDURE reftableCopy:
  DEFINE BUFFER breftable FOR reftable.

  FOR EACH reftable NO-LOCK:
    CREATE breftable.
    BUFFER-COPY reftable EXCEPT company TO breftable
      ASSIGN breftable.company = "002".
  END.
END PROCEDURE.

PROCEDURE rm-binCopy:
  DEFINE BUFFER brm-bin FOR rm-bin.

  FOR EACH rm-bin NO-LOCK:
    CREATE brm-bin.
    BUFFER-COPY rm-bin EXCEPT company TO brm-bin
      ASSIGN brm-bin.company = "002".
  END.
END PROCEDURE.

PROCEDURE rm-ctrlCopy:
  DEFINE BUFFER brm-ctrl FOR rm-ctrl.

  FOR EACH rm-ctrl NO-LOCK:
    CREATE brm-ctrl.
    BUFFER-COPY rm-ctrl EXCEPT company TO brm-ctrl
      ASSIGN brm-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE rm-rcptCopy:
  DEFINE BUFFER brm-rcpt FOR rm-rcpt.

  FOR EACH rm-rcpt NO-LOCK:
    CREATE brm-rcpt.
    BUFFER-COPY rm-rcpt EXCEPT company TO brm-rcpt
      ASSIGN brm-rcpt.company = "002".
  END.
END PROCEDURE.

PROCEDURE rm-rcpthCopy:
  DEFINE BUFFER brm-rcpth FOR rm-rcpth.

  FOR EACH rm-rcpth NO-LOCK:
    CREATE brm-rcpth.
    BUFFER-COPY rm-rcpth EXCEPT company TO brm-rcpth
      ASSIGN brm-rcpth.company = "002".
  END.
END PROCEDURE.

PROCEDURE rm-rctdCopy:
  DEFINE BUFFER brm-rctd FOR rm-rctd.

  FOR EACH rm-rctd NO-LOCK:
    CREATE brm-rctd.
    BUFFER-COPY rm-rctd EXCEPT company TO brm-rctd
      ASSIGN brm-rctd.company = "002".
  END.
END PROCEDURE.

PROCEDURE rm-rcthCopy:
  DEFINE BUFFER brm-rcth FOR rm-rcth.

  FOR EACH rm-rcth NO-LOCK:
    CREATE brm-rcth.
    BUFFER-COPY rm-rcth EXCEPT company TO brm-rcth
      ASSIGN brm-rcth.company = "002".
  END.
END PROCEDURE.

PROCEDURE rm-rdtlCopy:
  DEFINE BUFFER brm-rdtl FOR rm-rdtl.

  FOR EACH rm-rdtl NO-LOCK:
    CREATE brm-rdtl.
    BUFFER-COPY rm-rdtl EXCEPT company TO brm-rdtl
      ASSIGN brm-rdtl.company = "002".
  END.
END PROCEDURE.

PROCEDURE rm-rdtlhCopy:
  DEFINE BUFFER brm-rdtlh FOR rm-rdtlh.

  FOR EACH rm-rdtlh NO-LOCK:
    CREATE brm-rdtlh.
    BUFFER-COPY rm-rdtlh EXCEPT company TO brm-rdtlh
      ASSIGN brm-rdtlh.company = "002".
  END.
END PROCEDURE.

PROCEDURE rm-receiptsCopy:
  DEFINE BUFFER brm-receipts FOR rm-receipts.

  FOR EACH rm-receipts NO-LOCK:
    CREATE brm-receipts.
    BUFFER-COPY rm-receipts EXCEPT company TO brm-receipts
      ASSIGN brm-receipts.company = "002".
  END.
END PROCEDURE.

PROCEDURE routingCopy:
  DEFINE BUFFER brouting FOR routing.

  FOR EACH routing NO-LOCK:
    CREATE brouting.
    BUFFER-COPY routing EXCEPT company TO brouting
      ASSIGN brouting.company = "002".
  END.
END PROCEDURE.

PROCEDURE routing-mtxCopy:
  DEFINE BUFFER brouting-mtx FOR routing-mtx.

  FOR EACH routing-mtx NO-LOCK:
    CREATE brouting-mtx.
    BUFFER-COPY routing-mtx EXCEPT company TO brouting-mtx
      ASSIGN brouting-mtx.company = "002".
  END.
END PROCEDURE.

PROCEDURE shiftCopy:
  DEFINE BUFFER bshift FOR shift.

  FOR EACH shift NO-LOCK:
    CREATE bshift.
    BUFFER-COPY shift EXCEPT company TO bshift
      ASSIGN bshift.company = "002".
  END.
END PROCEDURE.

PROCEDURE shiptoCopy:
  DEFINE BUFFER bshipto FOR shipto.

  FOR EACH shipto NO-LOCK:
    CREATE bshipto.
    BUFFER-COPY shipto EXCEPT company TO bshipto
      ASSIGN bshipto.company = "002".
  END.
END PROCEDURE.

PROCEDURE smanCopy:
  DEFINE BUFFER bsman FOR sman.

  FOR EACH sman NO-LOCK:
    CREATE bsman.
    BUFFER-COPY sman EXCEPT company TO bsman
      ASSIGN bsman.company = "002".
  END.
END PROCEDURE.

PROCEDURE sman-mtxCopy:
  DEFINE BUFFER bsman-mtx FOR sman-mtx.

  FOR EACH sman-mtx NO-LOCK:
    CREATE bsman-mtx.
    BUFFER-COPY sman-mtx EXCEPT company TO bsman-mtx
      ASSIGN bsman-mtx.company = "002".
  END.
END PROCEDURE.
/*
PROCEDURE smanmtrxCopy:
  DEFINE BUFFER bsmanmtrx FOR smanmtrx.

  FOR EACH smanmtrx NO-LOCK:
    CREATE bsmanmtrx.
    BUFFER-COPY smanmtrx EXCEPT company TO bsmanmtrx
      ASSIGN bsmanmtrx.company = "002".
  END.
END PROCEDURE.
*/
PROCEDURE soldtoCopy:
  DEFINE BUFFER bsoldto FOR soldto.

  FOR EACH soldto NO-LOCK:
    CREATE bsoldto.
    BUFFER-COPY soldto EXCEPT company TO bsoldto
      ASSIGN bsoldto.company = "002".
  END.
END PROCEDURE.

PROCEDURE stack-fluteCopy:
  DEFINE BUFFER bstack-flute FOR stack-flute.

  FOR EACH stack-flute NO-LOCK:
    CREATE bstack-flute.
    BUFFER-COPY stack-flute EXCEPT company TO bstack-flute
      ASSIGN bstack-flute.company = "002".
  END.
END PROCEDURE.

PROCEDURE stack-sizeCopy:
  DEFINE BUFFER bstack-size FOR stack-size.

  FOR EACH stack-size NO-LOCK:
    CREATE bstack-size.
    BUFFER-COPY stack-size EXCEPT company TO bstack-size
      ASSIGN bstack-size.company = "002".
  END.
END PROCEDURE.

PROCEDURE staxCopy:
  DEFINE BUFFER bstax FOR stax.

  FOR EACH stax NO-LOCK:
    CREATE bstax.
    BUFFER-COPY stax EXCEPT company TO bstax
      ASSIGN bstax.company = "002".
  END.
END PROCEDURE.

PROCEDURE stax-groupCopy:
  DEFINE BUFFER bstax-group FOR stax-group.

  FOR EACH stax-group NO-LOCK:
    CREATE bstax-group.
    BUFFER-COPY stax-group EXCEPT company TO bstax-group
      ASSIGN bstax-group.company = "002".
  END.
END PROCEDURE.

PROCEDURE std-codeCopy:
  DEFINE BUFFER bstd-code FOR std-code.

  FOR EACH std-code NO-LOCK:
    CREATE bstd-code.
    BUFFER-COPY std-code EXCEPT company TO bstd-code
      ASSIGN bstd-code.company = "002".
  END.
END PROCEDURE.

PROCEDURE styleCopy:
  DEFINE BUFFER bstyle FOR style.

  FOR EACH style NO-LOCK:
    CREATE bstyle.
    BUFFER-COPY style EXCEPT company TO bstyle
      ASSIGN bstyle.company = "002".
  END.
END PROCEDURE.

PROCEDURE sys-ctrlCopy:
  DEFINE BUFFER bsys-ctrl FOR sys-ctrl.

  FOR EACH sys-ctrl NO-LOCK:
    CREATE bsys-ctrl.
    BUFFER-COPY sys-ctrl EXCEPT company TO bsys-ctrl
      ASSIGN bsys-ctrl.company = "002".
  END.
END PROCEDURE.

PROCEDURE termsCopy:
  DEFINE BUFFER bterms FOR terms.

  FOR EACH terms NO-LOCK:
    CREATE bterms.
    BUFFER-COPY terms EXCEPT company TO bterms
      ASSIGN bterms.company = "002".
  END.
END PROCEDURE.

PROCEDURE terrCopy:
  DEFINE BUFFER bterr FOR terr.

  FOR EACH terr NO-LOCK:
    CREATE bterr.
    BUFFER-COPY terr EXCEPT company TO bterr
      ASSIGN bterr.company = "002".
  END.
END PROCEDURE.

PROCEDURE test-redCopy:
  DEFINE BUFFER btest-red FOR test-red.

  FOR EACH test-red NO-LOCK:
    CREATE btest-red.
    BUFFER-COPY test-red EXCEPT company TO btest-red
      ASSIGN btest-red.company = "002".
  END.
END PROCEDURE.

PROCEDURE user-printCopy:
  DEFINE BUFFER buser-print FOR user-print.

  FOR EACH user-print NO-LOCK:
    CREATE buser-print.
    BUFFER-COPY user-print EXCEPT company TO buser-print
      ASSIGN buser-print.company = "002".
  END.
END PROCEDURE.

PROCEDURE usercompCopy:
  DEFINE BUFFER busercomp FOR usercomp.

  FOR EACH usercomp NO-LOCK:
    CREATE busercomp.
    BUFFER-COPY usercomp EXCEPT company TO busercomp
      ASSIGN busercomp.company = "002".
  END.
END PROCEDURE.

PROCEDURE usrCopy:
  DEFINE BUFFER busr FOR usr.

  FOR EACH usr NO-LOCK:
    CREATE busr.
    BUFFER-COPY usr EXCEPT company TO busr
      ASSIGN busr.company = "002".
  END.
END PROCEDURE.

PROCEDURE usr-grpCopy:
  DEFINE BUFFER busr-grp FOR usr-grp.

  FOR EACH usr-grp NO-LOCK:
    CREATE busr-grp.
    BUFFER-COPY usr-grp EXCEPT company TO busr-grp
      ASSIGN busr-grp.company = "002".
  END.
END PROCEDURE.

PROCEDURE usrxCopy:
  DEFINE BUFFER busrx FOR usrx.

  FOR EACH usrx NO-LOCK:
    CREATE busrx.
    BUFFER-COPY usrx EXCEPT company TO busrx
      ASSIGN busrx.company = "002".
  END.
END PROCEDURE.

PROCEDURE vendCopy:
  DEFINE BUFFER bvend FOR vend.

  FOR EACH vend NO-LOCK:
    CREATE bvend.
    BUFFER-COPY vend EXCEPT company TO bvend
      ASSIGN bvend.company = "002".
  END.
END PROCEDURE.

PROCEDURE ventypeCopy:
  DEFINE BUFFER bventype FOR ventype.

  FOR EACH ventype NO-LOCK:
    CREATE bventype.
    BUFFER-COPY ventype EXCEPT company TO bventype
      ASSIGN bventype.company = "002".
  END.
END PROCEDURE.
