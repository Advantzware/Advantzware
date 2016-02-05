/* custom/company.p */
/* auto generated from genCompanyCopy.p on 03/17/05 @  1:48:17 pm */

DEFINE INPUT PARAMETER ipCompanyFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipCompanyTo AS CHARACTER NO-UNDO.

DEFINE VARIABLE copyRecord AS LOGICAL NO-UNDO.

MESSAGE "Copy Master Table's Data to New Company?" SKIP(1)
   '** Warning: Depending on Data Volume,' SKIP
   '** this Process may take awhile.' VIEW-AS ALERT-BOX
  QUESTION BUTTONS YES-NO
  TITLE "Copy From Company " + ipCompanyFrom + " To Company " + ipCompanyTo
  UPDATE copyRecord.
IF NOT copyRecord THEN RETURN.

SESSION:SET-WAIT-STATE ("general").

RUN accountCopy.
RUN ap-buyCopy.
RUN ap-ctrlCopy.
RUN ap-ledgerCopy.
RUN ar-ctrlCopy.
RUN bankCopy.
RUN buyerCopy.
RUN carr-mtxCopy.
RUN carrierCopy.
RUN ce-ctrlCopy.
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
RUN empCopy.
RUN fg-actCopy.
RUN fg-binCopy.
RUN fg-ctrlCopy.
RUN fg-setCopy.
RUN fgcatCopy.
RUN fluteCopy.
RUN gl-ctrlCopy.
RUN gl-rptCopy.
RUN gl-rptdCopy.
RUN itemCopy.
RUN item-bomCopy.
RUN item-specCopy.
RUN itemfgCopy.
RUN itemfg-bomCopy.
RUN itemfg-inkCopy.
RUN itemfg-locCopy.
RUN itemfgdtlCopy.
RUN jc-ctrlCopy.
RUN loadtagCopy.
RUN locCopy.
RUN machCopy.
RUN mach-adderCopy.
RUN mach-calendarCopy.
RUN matCopy.
RUN mat-actCopy.
RUN matprepCopy.
RUN mmtxCopy.
RUN mmtx2Copy.
RUN mmtyCopy.
RUN mstdCopy.
RUN oe-ctrlCopy.
RUN oe-prmtxCopy.
RUN periodCopy.
RUN po-ctrlCopy.
RUN prepCopy.
RUN procatCopy.
RUN prodCopy.
RUN prodlCopy.
RUN reftableCopy.
RUN rm-binCopy.
RUN rm-ctrlCopy.
RUN routingCopy.
RUN routing-mtxCopy.
RUN shiftCopy.
RUN shiptoCopy.
RUN smanCopy.
RUN sman-mtxCopy.
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
RUN usercompCopy.
RUN vendCopy.
RUN ventypeCopy.

SESSION:SET-WAIT-STATE ("").

PROCEDURE accountCopy:
  DEFINE BUFFER baccount FOR account.

  FOR EACH account EXCLUSIVE-LOCK WHERE account.company EQ ipCompanyTo:
    DELETE account.
  END.
  FOR EACH account NO-LOCK WHERE account.company EQ ipCompanyFrom:
    CREATE baccount.
    BUFFER-COPY account EXCEPT company TO baccount
      ASSIGN baccount.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE ap-buyCopy:
  DEFINE BUFFER bap-buy FOR ap-buy.

  FOR EACH ap-buy EXCLUSIVE-LOCK WHERE ap-buy.company EQ ipCompanyTo:
    DELETE ap-buy.
  END.
  FOR EACH ap-buy NO-LOCK WHERE ap-buy.company EQ ipCompanyFrom:
    CREATE bap-buy.
    BUFFER-COPY ap-buy EXCEPT company TO bap-buy
      ASSIGN bap-buy.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE ap-ctrlCopy:
  DEFINE BUFFER bap-ctrl FOR ap-ctrl.

  FOR EACH ap-ctrl EXCLUSIVE-LOCK WHERE ap-ctrl.company EQ ipCompanyTo:
    DELETE ap-ctrl.
  END.
  FOR EACH ap-ctrl NO-LOCK WHERE ap-ctrl.company EQ ipCompanyFrom:
    CREATE bap-ctrl.
    BUFFER-COPY ap-ctrl EXCEPT company TO bap-ctrl
      ASSIGN bap-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE ap-ledgerCopy:
  DEFINE BUFFER bap-ledger FOR ap-ledger.

  FOR EACH ap-ledger EXCLUSIVE-LOCK WHERE ap-ledger.company EQ ipCompanyTo:
    DELETE ap-ledger.
  END.
  FOR EACH ap-ledger NO-LOCK WHERE ap-ledger.company EQ ipCompanyFrom:
    CREATE bap-ledger.
    BUFFER-COPY ap-ledger EXCEPT company TO bap-ledger
      ASSIGN bap-ledger.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE ar-ctrlCopy:
  DEFINE BUFFER bar-ctrl FOR ar-ctrl.

  FOR EACH ar-ctrl EXCLUSIVE-LOCK WHERE ar-ctrl.company EQ ipCompanyTo:
    DELETE ar-ctrl.
  END.
  FOR EACH ar-ctrl NO-LOCK WHERE ar-ctrl.company EQ ipCompanyFrom:
    CREATE bar-ctrl.
    BUFFER-COPY ar-ctrl EXCEPT company TO bar-ctrl
      ASSIGN bar-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE bankCopy:
  DEFINE BUFFER bbank FOR bank.

  FOR EACH bank EXCLUSIVE-LOCK WHERE bank.company EQ ipCompanyTo:
    DELETE bank.
  END.
  FOR EACH bank NO-LOCK WHERE bank.company EQ ipCompanyFrom:
    CREATE bbank.
    BUFFER-COPY bank EXCEPT company TO bbank
      ASSIGN bbank.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE buyerCopy:
  DEFINE BUFFER bbuyer FOR buyer.

  FOR EACH buyer EXCLUSIVE-LOCK WHERE buyer.company EQ ipCompanyTo:
    DELETE buyer.
  END.
  FOR EACH buyer NO-LOCK WHERE buyer.company EQ ipCompanyFrom:
    CREATE bbuyer.
    BUFFER-COPY buyer EXCEPT company TO bbuyer
      ASSIGN bbuyer.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE carr-mtxCopy:
  DEFINE BUFFER bcarr-mtx FOR carr-mtx.

  FOR EACH carr-mtx EXCLUSIVE-LOCK WHERE carr-mtx.company EQ ipCompanyTo:
    DELETE carr-mtx.
  END.
  FOR EACH carr-mtx NO-LOCK WHERE carr-mtx.company EQ ipCompanyFrom:
    CREATE bcarr-mtx.
    BUFFER-COPY carr-mtx EXCEPT company TO bcarr-mtx
      ASSIGN bcarr-mtx.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE carrierCopy:
  DEFINE BUFFER bcarrier FOR carrier.

  FOR EACH carrier EXCLUSIVE-LOCK WHERE carrier.company EQ ipCompanyTo:
    DELETE carrier.
  END.
  FOR EACH carrier NO-LOCK WHERE carrier.company EQ ipCompanyFrom:
    CREATE bcarrier.
    BUFFER-COPY carrier EXCEPT company TO bcarrier
      ASSIGN bcarrier.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE ce-ctrlCopy:
  DEFINE BUFFER bce-ctrl FOR ce-ctrl.

  FOR EACH ce-ctrl EXCLUSIVE-LOCK WHERE ce-ctrl.company EQ ipCompanyTo:
    DELETE ce-ctrl.
  END.
  FOR EACH ce-ctrl NO-LOCK WHERE ce-ctrl.company EQ ipCompanyFrom:
    CREATE bce-ctrl.
    BUFFER-COPY ce-ctrl EXCEPT company TO bce-ctrl
      ASSIGN bce-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE costtypeCopy:
  DEFINE BUFFER bcosttype FOR costtype.

  FOR EACH costtype EXCLUSIVE-LOCK WHERE costtype.company EQ ipCompanyTo:
    DELETE costtype.
  END.
  FOR EACH costtype NO-LOCK WHERE costtype.company EQ ipCompanyFrom:
    CREATE bcosttype.
    BUFFER-COPY costtype EXCEPT company TO bcosttype
      ASSIGN bcosttype.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE crewCopy:
  DEFINE BUFFER bcrew FOR crew.

  FOR EACH crew EXCLUSIVE-LOCK WHERE crew.company EQ ipCompanyTo:
    DELETE crew.
  END.
  FOR EACH crew NO-LOCK WHERE crew.company EQ ipCompanyFrom:
    CREATE bcrew.
    BUFFER-COPY crew EXCEPT company TO bcrew
      ASSIGN bcrew.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE currencyCopy:
  DEFINE BUFFER bcurrency FOR currency.

  FOR EACH currency EXCLUSIVE-LOCK WHERE currency.company EQ ipCompanyTo:
    DELETE currency.
  END.
  FOR EACH currency NO-LOCK WHERE currency.company EQ ipCompanyFrom:
    CREATE bcurrency.
    BUFFER-COPY currency EXCEPT company TO bcurrency
      ASSIGN bcurrency.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE custCopy:
  DEFINE BUFFER bcust FOR cust.

  FOR EACH cust EXCLUSIVE-LOCK WHERE cust.company EQ ipCompanyTo:
    DELETE cust.
  END.
  FOR EACH cust NO-LOCK WHERE cust.company EQ ipCompanyFrom:
    CREATE bcust.
    BUFFER-COPY cust EXCEPT company TO bcust
      ASSIGN bcust.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE cust-markupCopy:
  DEFINE BUFFER bcust-markup FOR cust-markup.

  FOR EACH cust-markup EXCLUSIVE-LOCK WHERE cust-markup.company EQ ipCompanyTo:
    DELETE cust-markup.
  END.
  FOR EACH cust-markup NO-LOCK WHERE cust-markup.company EQ ipCompanyFrom:
    CREATE bcust-markup.
    BUFFER-COPY cust-markup EXCEPT company TO bcust-markup
      ASSIGN bcust-markup.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE cust-partCopy:
  DEFINE BUFFER bcust-part FOR cust-part.

  FOR EACH cust-part EXCLUSIVE-LOCK WHERE cust-part.company EQ ipCompanyTo:
    DELETE cust-part.
  END.
  FOR EACH cust-part NO-LOCK WHERE cust-part.company EQ ipCompanyFrom:
    CREATE bcust-part.
    BUFFER-COPY cust-part EXCEPT company TO bcust-part
      ASSIGN bcust-part.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE cust-prod-salesCopy:
  DEFINE BUFFER bcust-prod-sales FOR cust-prod-sales.

  FOR EACH cust-prod-sales EXCLUSIVE-LOCK WHERE cust-prod-sales.company EQ ipCompanyTo:
    DELETE cust-prod-sales.
  END.
  FOR EACH cust-prod-sales NO-LOCK WHERE cust-prod-sales.company EQ ipCompanyFrom:
    CREATE bcust-prod-sales.
    BUFFER-COPY cust-prod-sales EXCEPT company TO bcust-prod-sales
      ASSIGN bcust-prod-sales.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE custypeCopy:
  DEFINE BUFFER bcustype FOR custype.

  FOR EACH custype EXCLUSIVE-LOCK WHERE custype.company EQ ipCompanyTo:
    DELETE custype.
  END.
  FOR EACH custype NO-LOCK WHERE custype.company EQ ipCompanyFrom:
    CREATE bcustype.
    BUFFER-COPY custype EXCEPT company TO bcustype
      ASSIGN bcustype.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE db-ctrlCopy:
  DEFINE BUFFER bdb-ctrl FOR db-ctrl.

  FOR EACH db-ctrl EXCLUSIVE-LOCK WHERE db-ctrl.company EQ ipCompanyTo:
    DELETE db-ctrl.
  END.
  FOR EACH db-ctrl NO-LOCK WHERE db-ctrl.company EQ ipCompanyFrom:
    CREATE bdb-ctrl.
    BUFFER-COPY db-ctrl EXCEPT company TO bdb-ctrl
      ASSIGN bdb-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE deptCopy:
  DEFINE BUFFER bdept FOR dept.

  FOR EACH dept EXCLUSIVE-LOCK WHERE dept.company EQ ipCompanyTo:
    DELETE dept.
  END.
  FOR EACH dept NO-LOCK WHERE dept.company EQ ipCompanyFrom:
    CREATE bdept.
    BUFFER-COPY dept EXCEPT company TO bdept
      ASSIGN bdept.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE e-itemCopy:
  DEFINE BUFFER be-item FOR e-item.

  FOR EACH e-item EXCLUSIVE-LOCK WHERE e-item.company EQ ipCompanyTo:
    DELETE e-item.
  END.
  FOR EACH e-item NO-LOCK WHERE e-item.company EQ ipCompanyFrom:
    CREATE be-item.
    BUFFER-COPY e-item EXCEPT company TO be-item
      ASSIGN be-item.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE e-item-custCopy:
  DEFINE BUFFER be-item-cust FOR e-item-cust.

  FOR EACH e-item-cust EXCLUSIVE-LOCK WHERE e-item-cust.company EQ ipCompanyTo:
    DELETE e-item-cust.
  END.
  FOR EACH e-item-cust NO-LOCK WHERE e-item-cust.company EQ ipCompanyFrom:
    CREATE be-item-cust.
    BUFFER-COPY e-item-cust EXCEPT company TO be-item-cust
      ASSIGN be-item-cust.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE e-item-vendCopy:
  DEFINE BUFFER be-item-vend FOR e-item-vend.

  FOR EACH e-item-vend EXCLUSIVE-LOCK WHERE e-item-vend.company EQ ipCompanyTo:
    DELETE e-item-vend.
  END.
  FOR EACH e-item-vend NO-LOCK WHERE e-item-vend.company EQ ipCompanyFrom:
    CREATE be-item-vend.
    BUFFER-COPY e-item-vend EXCEPT company TO be-item-vend
      ASSIGN be-item-vend.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE e-itemfgCopy:
  DEFINE BUFFER be-itemfg FOR e-itemfg.

  FOR EACH e-itemfg EXCLUSIVE-LOCK WHERE e-itemfg.company EQ ipCompanyTo:
    DELETE e-itemfg.
  END.
  FOR EACH e-itemfg NO-LOCK WHERE e-itemfg.company EQ ipCompanyFrom:
    CREATE be-itemfg.
    BUFFER-COPY e-itemfg EXCEPT company TO be-itemfg
      ASSIGN be-itemfg.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE e-itemfg-vendCopy:
  DEFINE BUFFER be-itemfg-vend FOR e-itemfg-vend.

  FOR EACH e-itemfg-vend EXCLUSIVE-LOCK WHERE e-itemfg-vend.company EQ ipCompanyTo:
    DELETE e-itemfg-vend.
  END.
  FOR EACH e-itemfg-vend NO-LOCK WHERE e-itemfg-vend.company EQ ipCompanyFrom:
    CREATE be-itemfg-vend.
    BUFFER-COPY e-itemfg-vend EXCEPT company TO be-itemfg-vend
      ASSIGN be-itemfg-vend.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE empCopy:
  DEFINE BUFFER bemp FOR emp.

  FOR EACH emp EXCLUSIVE-LOCK WHERE emp.company EQ ipCompanyTo:
    DELETE emp.
  END.
  FOR EACH emp NO-LOCK WHERE emp.company EQ ipCompanyFrom:
    CREATE bemp.
    BUFFER-COPY emp EXCEPT company TO bemp
      ASSIGN bemp.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE fg-actCopy:
  DEFINE BUFFER bfg-act FOR fg-act.

  FOR EACH fg-act EXCLUSIVE-LOCK WHERE fg-act.company EQ ipCompanyTo:
    DELETE fg-act.
  END.
  FOR EACH fg-act NO-LOCK WHERE fg-act.company EQ ipCompanyFrom:
    CREATE bfg-act.
    BUFFER-COPY fg-act EXCEPT company TO bfg-act
      ASSIGN bfg-act.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE fg-binCopy:
  DEFINE BUFFER bfg-bin FOR fg-bin.

  FOR EACH fg-bin EXCLUSIVE-LOCK WHERE fg-bin.company EQ ipCompanyTo:
    DELETE fg-bin.
  END.
  FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ ipCompanyFrom:
    CREATE bfg-bin.
    BUFFER-COPY fg-bin EXCEPT company TO bfg-bin
      ASSIGN bfg-bin.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE fg-ctrlCopy:
  DEFINE BUFFER bfg-ctrl FOR fg-ctrl.

  FOR EACH fg-ctrl EXCLUSIVE-LOCK WHERE fg-ctrl.company EQ ipCompanyTo:
    DELETE fg-ctrl.
  END.
  FOR EACH fg-ctrl NO-LOCK WHERE fg-ctrl.company EQ ipCompanyFrom:
    CREATE bfg-ctrl.
    BUFFER-COPY fg-ctrl EXCEPT company TO bfg-ctrl
      ASSIGN bfg-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE fg-setCopy:
  DEFINE BUFFER bfg-set FOR fg-set.

  FOR EACH fg-set EXCLUSIVE-LOCK WHERE fg-set.company EQ ipCompanyTo:
    DELETE fg-set.
  END.
  FOR EACH fg-set NO-LOCK WHERE fg-set.company EQ ipCompanyFrom:
    CREATE bfg-set.
    BUFFER-COPY fg-set EXCEPT company TO bfg-set
      ASSIGN bfg-set.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE fgcatCopy:
  DEFINE BUFFER bfgcat FOR fgcat.

  FOR EACH fgcat EXCLUSIVE-LOCK WHERE fgcat.company EQ ipCompanyTo:
    DELETE fgcat.
  END.
  FOR EACH fgcat NO-LOCK WHERE fgcat.company EQ ipCompanyFrom:
    CREATE bfgcat.
    BUFFER-COPY fgcat EXCEPT company TO bfgcat
      ASSIGN bfgcat.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE fluteCopy:
  DEFINE BUFFER bflute FOR flute.

  FOR EACH flute EXCLUSIVE-LOCK WHERE flute.company EQ ipCompanyTo:
    DELETE flute.
  END.
  FOR EACH flute NO-LOCK WHERE flute.company EQ ipCompanyFrom:
    CREATE bflute.
    BUFFER-COPY flute EXCEPT company TO bflute
      ASSIGN bflute.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE gl-ctrlCopy:
  DEFINE BUFFER bgl-ctrl FOR gl-ctrl.

  FOR EACH gl-ctrl EXCLUSIVE-LOCK WHERE gl-ctrl.company EQ ipCompanyTo:
    DELETE gl-ctrl.
  END.
  FOR EACH gl-ctrl NO-LOCK WHERE gl-ctrl.company EQ ipCompanyFrom:
    CREATE bgl-ctrl.
    BUFFER-COPY gl-ctrl EXCEPT company TO bgl-ctrl
      ASSIGN bgl-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE gl-rptCopy:
  DEFINE BUFFER bgl-rpt FOR gl-rpt.

  FOR EACH gl-rpt EXCLUSIVE-LOCK WHERE gl-rpt.company EQ ipCompanyTo:
    DELETE gl-rpt.
  END.
  FOR EACH gl-rpt NO-LOCK WHERE gl-rpt.company EQ ipCompanyFrom:
    CREATE bgl-rpt.
    BUFFER-COPY gl-rpt EXCEPT company TO bgl-rpt
      ASSIGN bgl-rpt.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE gl-rptdCopy:
  DEFINE BUFFER bgl-rptd FOR gl-rptd.

  FOR EACH gl-rptd EXCLUSIVE-LOCK WHERE gl-rptd.company EQ ipCompanyTo:
    DELETE gl-rptd.
  END.
  FOR EACH gl-rptd NO-LOCK WHERE gl-rptd.company EQ ipCompanyFrom:
    CREATE bgl-rptd.
    BUFFER-COPY gl-rptd EXCEPT company TO bgl-rptd
      ASSIGN bgl-rptd.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE itemCopy:
  DEFINE BUFFER bitem FOR item.

  FOR EACH item EXCLUSIVE-LOCK WHERE item.company EQ ipCompanyTo:
    DELETE item.
  END.
  FOR EACH item NO-LOCK WHERE item.company EQ ipCompanyFrom:
    CREATE bitem.
    BUFFER-COPY item EXCEPT company TO bitem
      ASSIGN bitem.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE item-bomCopy:
  DEFINE BUFFER bitem-bom FOR item-bom.

  FOR EACH item-bom EXCLUSIVE-LOCK WHERE item-bom.company EQ ipCompanyTo:
    DELETE item-bom.
  END.
  FOR EACH item-bom NO-LOCK WHERE item-bom.company EQ ipCompanyFrom:
    CREATE bitem-bom.
    BUFFER-COPY item-bom EXCEPT company TO bitem-bom
      ASSIGN bitem-bom.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE item-specCopy:
  DEFINE BUFFER bitem-spec FOR item-spec.

  FOR EACH item-spec EXCLUSIVE-LOCK WHERE item-spec.company EQ ipCompanyTo:
    DELETE item-spec.
  END.
  FOR EACH item-spec NO-LOCK WHERE item-spec.company EQ ipCompanyFrom:
    CREATE bitem-spec.
    BUFFER-COPY item-spec EXCEPT company TO bitem-spec
      ASSIGN bitem-spec.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE itemfgCopy:
  DEFINE BUFFER bitemfg FOR itemfg.

  FOR EACH itemfg EXCLUSIVE-LOCK WHERE itemfg.company EQ ipCompanyTo:
    DELETE itemfg.
  END.
  FOR EACH itemfg NO-LOCK WHERE itemfg.company EQ ipCompanyFrom:
    CREATE bitemfg.
    BUFFER-COPY itemfg EXCEPT company TO bitemfg
      ASSIGN bitemfg.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE itemfg-bomCopy:
  DEFINE BUFFER bitemfg-bom FOR itemfg-bom.

  FOR EACH itemfg-bom EXCLUSIVE-LOCK WHERE itemfg-bom.company EQ ipCompanyTo:
    DELETE itemfg-bom.
  END.
  FOR EACH itemfg-bom NO-LOCK WHERE itemfg-bom.company EQ ipCompanyFrom:
    CREATE bitemfg-bom.
    BUFFER-COPY itemfg-bom EXCEPT company TO bitemfg-bom
      ASSIGN bitemfg-bom.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE itemfg-inkCopy:
  DEFINE BUFFER bitemfg-ink FOR itemfg-ink.

  FOR EACH itemfg-ink EXCLUSIVE-LOCK WHERE itemfg-ink.company EQ ipCompanyTo:
    DELETE itemfg-ink.
  END.
  FOR EACH itemfg-ink NO-LOCK WHERE itemfg-ink.company EQ ipCompanyFrom:
    CREATE bitemfg-ink.
    BUFFER-COPY itemfg-ink EXCEPT company TO bitemfg-ink
      ASSIGN bitemfg-ink.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE itemfg-locCopy:
  DEFINE BUFFER bitemfg-loc FOR itemfg-loc.

  FOR EACH itemfg-loc EXCLUSIVE-LOCK WHERE itemfg-loc.company EQ ipCompanyTo:
    DELETE itemfg-loc.
  END.
  FOR EACH itemfg-loc NO-LOCK WHERE itemfg-loc.company EQ ipCompanyFrom:
    CREATE bitemfg-loc.
    BUFFER-COPY itemfg-loc EXCEPT company TO bitemfg-loc
      ASSIGN bitemfg-loc.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE itemfgdtlCopy:
  DEFINE BUFFER bitemfgdtl FOR itemfgdtl.

  FOR EACH itemfgdtl EXCLUSIVE-LOCK WHERE itemfgdtl.company EQ ipCompanyTo:
    DELETE itemfgdtl.
  END.
  FOR EACH itemfgdtl NO-LOCK WHERE itemfgdtl.company EQ ipCompanyFrom:
    CREATE bitemfgdtl.
    BUFFER-COPY itemfgdtl EXCEPT company TO bitemfgdtl
      ASSIGN bitemfgdtl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE jc-ctrlCopy:
  DEFINE BUFFER bjc-ctrl FOR jc-ctrl.

  FOR EACH jc-ctrl EXCLUSIVE-LOCK WHERE jc-ctrl.company EQ ipCompanyTo:
    DELETE jc-ctrl.
  END.
  FOR EACH jc-ctrl NO-LOCK WHERE jc-ctrl.company EQ ipCompanyFrom:
    CREATE bjc-ctrl.
    BUFFER-COPY jc-ctrl EXCEPT company TO bjc-ctrl
      ASSIGN bjc-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE loadtagCopy:
  DEFINE BUFFER bloadtag FOR loadtag.

  FOR EACH loadtag EXCLUSIVE-LOCK WHERE loadtag.company EQ ipCompanyTo:
    DELETE loadtag.
  END.
  FOR EACH loadtag NO-LOCK WHERE loadtag.company EQ ipCompanyFrom:
    CREATE bloadtag.
    BUFFER-COPY loadtag EXCEPT company TO bloadtag
      ASSIGN bloadtag.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE locCopy:
  DEFINE BUFFER bloc FOR loc.

  FOR EACH loc EXCLUSIVE-LOCK WHERE loc.company EQ ipCompanyTo:
    DELETE loc.
  END.
  FOR EACH loc NO-LOCK WHERE loc.company EQ ipCompanyFrom:
    CREATE bloc.
    BUFFER-COPY loc EXCEPT company TO bloc
      ASSIGN bloc.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE machCopy:
  DEFINE BUFFER bmach FOR mach.

  FOR EACH mach EXCLUSIVE-LOCK WHERE mach.company EQ ipCompanyTo:
    DELETE mach.
  END.
  FOR EACH mach NO-LOCK WHERE mach.company EQ ipCompanyFrom:
    CREATE bmach.
    BUFFER-COPY mach EXCEPT company TO bmach
      ASSIGN bmach.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE mach-adderCopy:
  DEFINE BUFFER bmach-adder FOR mach-adder.

  FOR EACH mach-adder EXCLUSIVE-LOCK WHERE mach-adder.company EQ ipCompanyTo:
    DELETE mach-adder.
  END.
  FOR EACH mach-adder NO-LOCK WHERE mach-adder.company EQ ipCompanyFrom:
    CREATE bmach-adder.
    BUFFER-COPY mach-adder EXCEPT company TO bmach-adder
      ASSIGN bmach-adder.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE mach-calendarCopy:
  DEFINE BUFFER bmach-calendar FOR mach-calendar.

  FOR EACH mach-calendar EXCLUSIVE-LOCK WHERE mach-calendar.company EQ ipCompanyTo:
    DELETE mach-calendar.
  END.
  FOR EACH mach-calendar NO-LOCK WHERE mach-calendar.company EQ ipCompanyFrom:
    CREATE bmach-calendar.
    BUFFER-COPY mach-calendar EXCEPT company TO bmach-calendar
      ASSIGN bmach-calendar.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE matCopy:
  DEFINE BUFFER bmat FOR mat.

  FOR EACH mat EXCLUSIVE-LOCK WHERE mat.company EQ ipCompanyTo:
    DELETE mat.
  END.
  FOR EACH mat NO-LOCK WHERE mat.company EQ ipCompanyFrom:
    CREATE bmat.
    BUFFER-COPY mat EXCEPT company TO bmat
      ASSIGN bmat.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE mat-actCopy:
  DEFINE BUFFER bmat-act FOR mat-act.

  FOR EACH mat-act EXCLUSIVE-LOCK WHERE mat-act.company EQ ipCompanyTo:
    DELETE mat-act.
  END.
  FOR EACH mat-act NO-LOCK WHERE mat-act.company EQ ipCompanyFrom:
    CREATE bmat-act.
    BUFFER-COPY mat-act EXCEPT company TO bmat-act
      ASSIGN bmat-act.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE matprepCopy:
  DEFINE BUFFER bmatprep FOR matprep.

  FOR EACH matprep EXCLUSIVE-LOCK WHERE matprep.company EQ ipCompanyTo:
    DELETE matprep.
  END.
  FOR EACH matprep NO-LOCK WHERE matprep.company EQ ipCompanyFrom:
    CREATE bmatprep.
    BUFFER-COPY matprep EXCEPT company TO bmatprep
      ASSIGN bmatprep.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE mmtxCopy:
  DEFINE BUFFER bmmtx FOR mmtx.

  FOR EACH mmtx EXCLUSIVE-LOCK WHERE mmtx.company EQ ipCompanyTo:
    DELETE mmtx.
  END.
  FOR EACH mmtx NO-LOCK WHERE mmtx.company EQ ipCompanyFrom:
    CREATE bmmtx.
    BUFFER-COPY mmtx EXCEPT company TO bmmtx
      ASSIGN bmmtx.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE mmtx2Copy:
  DEFINE BUFFER bmmtx2 FOR mmtx2.

  FOR EACH mmtx2 EXCLUSIVE-LOCK WHERE mmtx2.company EQ ipCompanyTo:
    DELETE mmtx2.
  END.
  FOR EACH mmtx2 NO-LOCK WHERE mmtx2.company EQ ipCompanyFrom:
    CREATE bmmtx2.
    BUFFER-COPY mmtx2 EXCEPT company TO bmmtx2
      ASSIGN bmmtx2.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE mmtyCopy:
  DEFINE BUFFER bmmty FOR mmty.

  FOR EACH mmty EXCLUSIVE-LOCK WHERE mmty.company EQ ipCompanyTo:
    DELETE mmty.
  END.
  FOR EACH mmty NO-LOCK WHERE mmty.company EQ ipCompanyFrom:
    CREATE bmmty.
    BUFFER-COPY mmty EXCEPT company TO bmmty
      ASSIGN bmmty.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE mstdCopy:
  DEFINE BUFFER bmstd FOR mstd.

  FOR EACH mstd EXCLUSIVE-LOCK WHERE mstd.company EQ ipCompanyTo:
    DELETE mstd.
  END.
  FOR EACH mstd NO-LOCK WHERE mstd.company EQ ipCompanyFrom:
    CREATE bmstd.
    BUFFER-COPY mstd EXCEPT company TO bmstd
      ASSIGN bmstd.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE oe-ctrlCopy:
  DEFINE BUFFER boe-ctrl FOR oe-ctrl.

  FOR EACH oe-ctrl EXCLUSIVE-LOCK WHERE oe-ctrl.company EQ ipCompanyTo:
    DELETE oe-ctrl.
  END.
  FOR EACH oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ ipCompanyFrom:
    CREATE boe-ctrl.
    BUFFER-COPY oe-ctrl EXCEPT company TO boe-ctrl
      ASSIGN boe-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE oe-prmtxCopy:
  DEFINE BUFFER boe-prmtx FOR oe-prmtx.

  FOR EACH oe-prmtx EXCLUSIVE-LOCK WHERE oe-prmtx.company EQ ipCompanyTo:
    DELETE oe-prmtx.
  END.
  FOR EACH oe-prmtx NO-LOCK WHERE oe-prmtx.company EQ ipCompanyFrom:
    CREATE boe-prmtx.
    BUFFER-COPY oe-prmtx EXCEPT company TO boe-prmtx
      ASSIGN boe-prmtx.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE periodCopy:
  DEFINE BUFFER bperiod FOR period.

  FOR EACH period EXCLUSIVE-LOCK WHERE period.company EQ ipCompanyTo:
    DELETE period.
  END.
  FOR EACH period NO-LOCK WHERE period.company EQ ipCompanyFrom:
    CREATE bperiod.
    BUFFER-COPY period EXCEPT company TO bperiod
      ASSIGN bperiod.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE po-ctrlCopy:
  DEFINE BUFFER bpo-ctrl FOR po-ctrl.

  FOR EACH po-ctrl EXCLUSIVE-LOCK WHERE po-ctrl.company EQ ipCompanyTo:
    DELETE po-ctrl.
  END.
  FOR EACH po-ctrl NO-LOCK WHERE po-ctrl.company EQ ipCompanyFrom:
    CREATE bpo-ctrl.
    BUFFER-COPY po-ctrl EXCEPT company TO bpo-ctrl
      ASSIGN bpo-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE prepCopy:
  DEFINE BUFFER bprep FOR prep.

  FOR EACH prep EXCLUSIVE-LOCK WHERE prep.company EQ ipCompanyTo:
    DELETE prep.
  END.
  FOR EACH prep NO-LOCK WHERE prep.company EQ ipCompanyFrom:
    CREATE bprep.
    BUFFER-COPY prep EXCEPT company TO bprep
      ASSIGN bprep.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE procatCopy:
  DEFINE BUFFER bprocat FOR procat.

  FOR EACH procat EXCLUSIVE-LOCK WHERE procat.company EQ ipCompanyTo:
    DELETE procat.
  END.
  FOR EACH procat NO-LOCK WHERE procat.company EQ ipCompanyFrom:
    CREATE bprocat.
    BUFFER-COPY procat EXCEPT company TO bprocat
      ASSIGN bprocat.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE prodCopy:
  DEFINE BUFFER bprod FOR prod.

  FOR EACH prod EXCLUSIVE-LOCK WHERE prod.company EQ ipCompanyTo:
    DELETE prod.
  END.
  FOR EACH prod NO-LOCK WHERE prod.company EQ ipCompanyFrom:
    CREATE bprod.
    BUFFER-COPY prod EXCEPT company TO bprod
      ASSIGN bprod.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE prodlCopy:
  DEFINE BUFFER bprodl FOR prodl.

  FOR EACH prodl EXCLUSIVE-LOCK WHERE prodl.company EQ ipCompanyTo:
    DELETE prodl.
  END.
  FOR EACH prodl NO-LOCK WHERE prodl.company EQ ipCompanyFrom:
    CREATE bprodl.
    BUFFER-COPY prodl EXCEPT company TO bprodl
      ASSIGN bprodl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE reftableCopy:
  DEFINE BUFFER breftable FOR reftable.

  FOR EACH reftable EXCLUSIVE-LOCK WHERE reftable.company EQ ipCompanyTo:
    DELETE reftable.
  END.
  FOR EACH reftable NO-LOCK WHERE reftable.company EQ ipCompanyFrom:
    CREATE breftable.
    BUFFER-COPY reftable EXCEPT company TO breftable
      ASSIGN breftable.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE rm-binCopy:
  DEFINE BUFFER brm-bin FOR rm-bin.

  FOR EACH rm-bin EXCLUSIVE-LOCK WHERE rm-bin.company EQ ipCompanyTo:
    DELETE rm-bin.
  END.
  FOR EACH rm-bin NO-LOCK WHERE rm-bin.company EQ ipCompanyFrom:
    CREATE brm-bin.
    BUFFER-COPY rm-bin EXCEPT company TO brm-bin
      ASSIGN brm-bin.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE rm-ctrlCopy:
  DEFINE BUFFER brm-ctrl FOR rm-ctrl.

  FOR EACH rm-ctrl EXCLUSIVE-LOCK WHERE rm-ctrl.company EQ ipCompanyTo:
    DELETE rm-ctrl.
  END.
  FOR EACH rm-ctrl NO-LOCK WHERE rm-ctrl.company EQ ipCompanyFrom:
    CREATE brm-ctrl.
    BUFFER-COPY rm-ctrl EXCEPT company TO brm-ctrl
      ASSIGN brm-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE routingCopy:
  DEFINE BUFFER brouting FOR routing.

  FOR EACH routing EXCLUSIVE-LOCK WHERE routing.company EQ ipCompanyTo:
    DELETE routing.
  END.
  FOR EACH routing NO-LOCK WHERE routing.company EQ ipCompanyFrom:
    CREATE brouting.
    BUFFER-COPY routing EXCEPT company TO brouting
      ASSIGN brouting.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE routing-mtxCopy:
  DEFINE BUFFER brouting-mtx FOR routing-mtx.

  FOR EACH routing-mtx EXCLUSIVE-LOCK WHERE routing-mtx.company EQ ipCompanyTo:
    DELETE routing-mtx.
  END.
  FOR EACH routing-mtx NO-LOCK WHERE routing-mtx.company EQ ipCompanyFrom:
    CREATE brouting-mtx.
    BUFFER-COPY routing-mtx EXCEPT company TO brouting-mtx
      ASSIGN brouting-mtx.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE shiftCopy:
  DEFINE BUFFER bshift FOR shift.

  FOR EACH shift EXCLUSIVE-LOCK WHERE shift.company EQ ipCompanyTo:
    DELETE shift.
  END.
  FOR EACH shift NO-LOCK WHERE shift.company EQ ipCompanyFrom:
    CREATE bshift.
    BUFFER-COPY shift EXCEPT company TO bshift
      ASSIGN bshift.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE shiptoCopy:
  DEFINE BUFFER bshipto FOR shipto.

  FOR EACH shipto EXCLUSIVE-LOCK WHERE shipto.company EQ ipCompanyTo:
    DELETE shipto.
  END.
  FOR EACH shipto NO-LOCK WHERE shipto.company EQ ipCompanyFrom:
    CREATE bshipto.
    BUFFER-COPY shipto EXCEPT company TO bshipto
      ASSIGN bshipto.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE smanCopy:
  DEFINE BUFFER bsman FOR sman.

  FOR EACH sman EXCLUSIVE-LOCK WHERE sman.company EQ ipCompanyTo:
    DELETE sman.
  END.
  FOR EACH sman NO-LOCK WHERE sman.company EQ ipCompanyFrom:
    CREATE bsman.
    BUFFER-COPY sman EXCEPT company TO bsman
      ASSIGN bsman.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE sman-mtxCopy:
  DEFINE BUFFER bsman-mtx FOR sman-mtx.

  FOR EACH sman-mtx EXCLUSIVE-LOCK WHERE sman-mtx.company EQ ipCompanyTo:
    DELETE sman-mtx.
  END.
  FOR EACH sman-mtx NO-LOCK WHERE sman-mtx.company EQ ipCompanyFrom:
    CREATE bsman-mtx.
    BUFFER-COPY sman-mtx EXCEPT company TO bsman-mtx
      ASSIGN bsman-mtx.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE soldtoCopy:
  DEFINE BUFFER bsoldto FOR soldto.

  FOR EACH soldto EXCLUSIVE-LOCK WHERE soldto.company EQ ipCompanyTo:
    DELETE soldto.
  END.
  FOR EACH soldto NO-LOCK WHERE soldto.company EQ ipCompanyFrom:
    CREATE bsoldto.
    BUFFER-COPY soldto EXCEPT company TO bsoldto
      ASSIGN bsoldto.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE stack-fluteCopy:
  DEFINE BUFFER bstack-flute FOR stack-flute.

  FOR EACH stack-flute EXCLUSIVE-LOCK WHERE stack-flute.company EQ ipCompanyTo:
    DELETE stack-flute.
  END.
  FOR EACH stack-flute NO-LOCK WHERE stack-flute.company EQ ipCompanyFrom:
    CREATE bstack-flute.
    BUFFER-COPY stack-flute EXCEPT company TO bstack-flute
      ASSIGN bstack-flute.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE stack-sizeCopy:
  DEFINE BUFFER bstack-size FOR stack-size.

  FOR EACH stack-size EXCLUSIVE-LOCK WHERE stack-size.company EQ ipCompanyTo:
    DELETE stack-size.
  END.
  FOR EACH stack-size NO-LOCK WHERE stack-size.company EQ ipCompanyFrom:
    CREATE bstack-size.
    BUFFER-COPY stack-size EXCEPT company TO bstack-size
      ASSIGN bstack-size.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE staxCopy:
  DEFINE BUFFER bstax FOR stax.

  FOR EACH stax EXCLUSIVE-LOCK WHERE stax.company EQ ipCompanyTo:
    DELETE stax.
  END.
  FOR EACH stax NO-LOCK WHERE stax.company EQ ipCompanyFrom:
    CREATE bstax.
    BUFFER-COPY stax EXCEPT company TO bstax
      ASSIGN bstax.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE stax-groupCopy:
  DEFINE BUFFER bstax-group FOR stax-group.

  FOR EACH stax-group EXCLUSIVE-LOCK WHERE stax-group.company EQ ipCompanyTo:
    DELETE stax-group.
  END.
  FOR EACH stax-group NO-LOCK WHERE stax-group.company EQ ipCompanyFrom:
    CREATE bstax-group.
    BUFFER-COPY stax-group EXCEPT company TO bstax-group
      ASSIGN bstax-group.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE std-codeCopy:
  DEFINE BUFFER bstd-code FOR std-code.

  FOR EACH std-code EXCLUSIVE-LOCK WHERE std-code.company EQ ipCompanyTo:
    DELETE std-code.
  END.
  FOR EACH std-code NO-LOCK WHERE std-code.company EQ ipCompanyFrom:
    CREATE bstd-code.
    BUFFER-COPY std-code EXCEPT company TO bstd-code
      ASSIGN bstd-code.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE styleCopy:
  DEFINE BUFFER bstyle FOR style.

  FOR EACH style EXCLUSIVE-LOCK WHERE style.company EQ ipCompanyTo:
    DELETE style.
  END.
  FOR EACH style NO-LOCK WHERE style.company EQ ipCompanyFrom:
    CREATE bstyle.
    BUFFER-COPY style EXCEPT company TO bstyle
      ASSIGN bstyle.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE sys-ctrlCopy:
  DEFINE BUFFER bsys-ctrl FOR sys-ctrl.

  FOR EACH sys-ctrl EXCLUSIVE-LOCK WHERE sys-ctrl.company EQ ipCompanyTo:
    DELETE sys-ctrl.
  END.
  FOR EACH sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ ipCompanyFrom:
    CREATE bsys-ctrl.
    BUFFER-COPY sys-ctrl EXCEPT company TO bsys-ctrl
      ASSIGN bsys-ctrl.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE termsCopy:
  DEFINE BUFFER bterms FOR terms.

  FOR EACH terms EXCLUSIVE-LOCK WHERE terms.company EQ ipCompanyTo:
    DELETE terms.
  END.
  FOR EACH terms NO-LOCK WHERE terms.company EQ ipCompanyFrom:
    CREATE bterms.
    BUFFER-COPY terms EXCEPT company TO bterms
      ASSIGN bterms.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE terrCopy:
  DEFINE BUFFER bterr FOR terr.

  FOR EACH terr EXCLUSIVE-LOCK WHERE terr.company EQ ipCompanyTo:
    DELETE terr.
  END.
  FOR EACH terr NO-LOCK WHERE terr.company EQ ipCompanyFrom:
    CREATE bterr.
    BUFFER-COPY terr EXCEPT company TO bterr
      ASSIGN bterr.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE test-redCopy:
  DEFINE BUFFER btest-red FOR test-red.

  FOR EACH test-red EXCLUSIVE-LOCK WHERE test-red.company EQ ipCompanyTo:
    DELETE test-red.
  END.
  FOR EACH test-red NO-LOCK WHERE test-red.company EQ ipCompanyFrom:
    CREATE btest-red.
    BUFFER-COPY test-red EXCEPT company TO btest-red
      ASSIGN btest-red.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE usercompCopy:
  DEFINE BUFFER busercomp FOR usercomp.

  FOR EACH usercomp EXCLUSIVE-LOCK WHERE usercomp.company EQ ipCompanyTo:
    DELETE usercomp.
  END.
  FOR EACH usercomp NO-LOCK WHERE usercomp.company EQ ipCompanyFrom:
    CREATE busercomp.
    BUFFER-COPY usercomp EXCEPT company TO busercomp
      ASSIGN busercomp.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE vendCopy:
  DEFINE BUFFER bvend FOR vend.

  FOR EACH vend EXCLUSIVE-LOCK WHERE vend.company EQ ipCompanyTo:
    DELETE vend.
  END.
  FOR EACH vend NO-LOCK WHERE vend.company EQ ipCompanyFrom:
    CREATE bvend.
    BUFFER-COPY vend EXCEPT company TO bvend
      ASSIGN bvend.company = ipCompanyTo.
  END.
END PROCEDURE.

PROCEDURE ventypeCopy:
  DEFINE BUFFER bventype FOR ventype.

  FOR EACH ventype EXCLUSIVE-LOCK WHERE ventype.company EQ ipCompanyTo:
    DELETE ventype.
  END.
  FOR EACH ventype NO-LOCK WHERE ventype.company EQ ipCompanyFrom:
    CREATE bventype.
    BUFFER-COPY ventype EXCEPT company TO bventype
      ASSIGN bventype.company = ipCompanyTo.
  END.
END PROCEDURE.
