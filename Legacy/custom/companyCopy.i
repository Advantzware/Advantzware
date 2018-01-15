/* custom/company.i - used in custom/companyCopy.w */
/* auto generated from genCompanyCopy.p on 04/20/05 @ 11:12:50 am */
/* gdm - 11070801 - Updated the rec_key creation to follow standard way      */
/*       of creating a rec_key. Also added an new procedure for mach-part    */
/*       table. As well as the notes transfer.                               */

DISABLE TRIGGERS FOR LOAD OF itemfg.
DISABLE TRIGGERS FOR LOAD OF itemfg-ink.
DISABLE TRIGGERS FOR LOAD OF eb.
PROCEDURE startCopy:
  IF tg_account THEN
      RUN accountCopy.
  IF tg_ap-buy THEN
      RUN ap-buyCopy.
  IF tg_ap-ctrl THEN
      RUN ap-ctrlCopy.
  IF tg_ap-ledger THEN
      RUN ap-ledgerCopy.
  IF tg_ar-ctrl THEN
      RUN ar-ctrlCopy.
  IF tg_bank THEN
      RUN bankCopy.
  IF tg_buyer THEN
      RUN buyerCopy.
  IF tg_carr-mtx THEN
      RUN carr-mtxCopy.
  IF tg_carrier THEN
      RUN carrierCopy.
  IF tg_ce-ctrl THEN
      RUN ce-ctrlCopy.
  IF tg_costtype THEN
      RUN costtypeCopy.
  IF tg_crew THEN
      RUN crewCopy.
  IF tg_currency THEN
      RUN currencyCopy.
  IF tg_cust THEN
      RUN custCopy.
  IF tg_cust-markup THEN
      RUN cust-markupCopy.
  IF tg_cust-part THEN
      RUN cust-partCopy.
  IF tg_cust-prod-sales THEN
      RUN cust-prod-salesCopy.
  IF tg_custype THEN
      RUN custypeCopy.
  IF tg_db-ctrl THEN
      RUN db-ctrlCopy.
  IF tg_e-item THEN
      RUN e-itemCopy.
  IF tg_e-item-cust THEN
      RUN e-item-custCopy.
  IF tg_e-item-vend THEN
      RUN e-item-vendCopy.
  IF tg_e-itemfg THEN
      RUN e-itemfgCopy.
  IF tg_e-itemfg-vend THEN
      RUN e-itemfg-vendCopy.
  IF tg_emp THEN
      RUN empCopy.
  IF tg_fg-act THEN
      RUN fg-actCopy.
  IF tg_fg-bin THEN
      RUN fg-binCopy.
  IF tg_fg-ctrl THEN
      RUN fg-ctrlCopy.
  IF tg_fg-set THEN
      RUN fg-setCopy.
  IF tg_fgcat THEN
      RUN fgcatCopy.
  IF tg_flute THEN
      RUN fluteCopy.
  IF tg_gl-ctrl THEN
      RUN gl-ctrlCopy.
  IF tg_gl-rpt THEN
      RUN gl-rptCopy.
  IF tg_gl-rptd THEN
      RUN gl-rptdCopy.
  IF tg_item THEN
      RUN itemCopy.
  IF tg_item-bom THEN
      RUN item-bomCopy.
  IF tg_item-spec THEN
      RUN item-specCopy.
  IF tg_itemfg THEN
      RUN itemfgCopy.
  IF tg_itemfg-bom THEN
      RUN itemfg-bomCopy.
  IF tg_itemfg-ink THEN
      RUN itemfg-inkCopy.
  IF tg_itemfg-loc THEN
      RUN itemfg-locCopy.
  IF tg_itemfgdtl THEN
      RUN itemfgdtlCopy.
  IF tg_jc-ctrl THEN
      RUN jc-ctrlCopy.
  IF tg_loadtag THEN
      RUN loadtagCopy.
  IF tg_loc THEN
      RUN locCopy.
  IF tg_mach THEN
      RUN machCopy.
  IF tg_mach-adder THEN
      RUN mach-adderCopy.
  IF tg_mach-calendar THEN
      RUN mach-calendarCopy.
  IF tg_mat-act THEN
      RUN mat-actCopy.
  IF tg_matprep THEN
      RUN matprepCopy.
  IF tg_mmtx THEN
      RUN mmtxCopy.
  IF tg_mmty THEN
      RUN mmtyCopy.
  IF tg_mstd THEN
      RUN mstdCopy.
  IF tg_oe-ctrl THEN
      RUN oe-ctrlCopy.
  IF tg_oe-prmtx THEN
      RUN oe-prmtxCopy.
  IF tg_period THEN
      RUN periodCopy.
  IF tg_po-ctrl THEN
      RUN po-ctrlCopy.
  IF tg_prep THEN
      RUN prepCopy.
  IF tg_procat THEN
      RUN procatCopy.
  IF tg_prod THEN
      RUN prodCopy.
  IF tg_prodl THEN
      RUN prodlCopy.
  IF tg_reftable THEN
      RUN reftableCopy.
  IF tg_rm-bin THEN
      RUN rm-binCopy.
  IF tg_rm-ctrl THEN
      RUN rm-ctrlCopy.
  IF tg_routing THEN
      RUN routingCopy.
  IF tg_routing-mtx THEN
      RUN routing-mtxCopy.
  IF tg_shift THEN
      RUN shiftCopy.
  IF tg_shipto THEN
      RUN shiptoCopy.
  IF tg_sman THEN
      RUN smanCopy.
  IF tg_sman-mtx THEN
      RUN sman-mtxCopy.
  /*RUN smanmtrxCopy.*/
  IF tg_soldto THEN
      RUN soldtoCopy.
  IF tg_stack-flute THEN
      RUN stack-fluteCopy.
  IF tg_stack-size THEN
      RUN stack-sizeCopy.
  IF tg_stax THEN
      RUN staxCopy.
  IF tg_stax-group THEN
      RUN stax-groupCopy.
  IF tg_std-code THEN
      RUN std-codeCopy.
  IF tg_style THEN
      RUN styleCopy.
  IF tg_sys-ctrl THEN
      RUN sys-ctrlCopy.
  IF tg_terms THEN
      RUN termsCopy.
  IF tg_terr THEN
      RUN terrCopy.
  IF tg_test-red THEN
      RUN test-redCopy.
  IF tg_usercomp THEN
      RUN usercompCopy.
  IF tg_vend THEN
      RUN vendCopy.
  IF tg_ventype THEN
      RUN ventypeCopy.
  
  IF tg_copy-transaction THEN DO:
    /* 1: Assign new rec_key for copy by excepting from BUFFER-COPY
       2: Make to check for tables with sequence index that does include company.
          If table has an index like this, sequnece must be reset -JLF 11/17/05
    */
     /*
     RUN ap-chk.
     /*RUN ap-dis.
     RUN ap-disl. 
     RUN ap-inv.
     RUN ap-invl.
     RUN ap-invlr.
     RUN ap-pay  .*/
     RUN ap-sel  .
     RUN aphist  .
     /*RUN ar-cash.
     RUN ar-cashl.
     RUN ar-inv  .
     RUN ar-invl .
     RUN ar-invm .*/
     RUN ar-ledger.
     /*RUN ar-mcash .*/
     RUN asinotes .
     RUN box-design-hdr.
     RUN box-design-line.
     RUN eb .
     RUN ef .
     RUN ef-nsh.
     RUN est.
     RUN est-flm.
     RUN est-inst.
     RUN est-op.
     RUN est-pf.
     RUN est-prep.
     RUN est-qty  .
     RUN est-summ .
     RUN fg-hist   .
     RUN fg-rcpth  .
    /* RUN fg-rcpts  .*/
     RUN fg-rctd   .
     RUN fg-rdtl   .
     RUN fg-rdtlh  .
     RUN gl-freq   .
     RUN gl-jrn    .
     RUN glhist    .
     RUN gltrans   .
     /*RUN inv-head  .
     RUN inv-line  .
     RUN inv-misc  .*/
     RUN job       .
     RUN job-all   .
     RUN job-brd   .
     RUN job-hdr   .
     RUN job-mat   .
     RUN job-prep  .
     RUN job-sch   .
     RUN mch-act   .
     RUN misc-act  .
     RUN mmtx2     .
     /*RUN oe-bolh   .
     RUN oe-boll   .
     RUN oe-boll-qty.*/
     RUN oe-ord     .
     RUN oe-ordl    .
     RUN oe-ordm    .
     /*RUN oe-rel     .
     RUN oe-relh    .
     RUN oe-rell    .*/
     RUN oe-reth    .
     RUN oe-retl    .
     RUN oe-ship    .
     RUN pc-misc    .
     RUN pc-prdd    .
     RUN pc-prdh    .
     RUN po-all     .
     RUN po-ord     .
     RUN po-ordl    .
     RUN po-rcpts   .
     RUN probe      .
     RUN probeit    .
     RUN probeit-price.
     RUN quotechg     .
     RUN quotehd      .
     RUN quoteitm     .
     RUN quoteqty     .
     /*RUN rm-rcpt      .*/
     RUN rm-rcpth     .
     RUN rm-rctd      .
     RUN rm-rcth      .
     RUN rm-rdtl      .
     RUN rm-rdtlh     .
     RUN rm-receipts  .
     RUN user-print   .
     */
  END.
  IF tg_mach-part THEN
      RUN mach-partCopy. 
  IF tg_box-design THEN
      RUN box-design .
END PROCEDURE.

PROCEDURE accountCopy:
  DEFINE BUFFER baccount FOR account.
  DEFINE BUFFER bnotes FOR notes.
  
  RUN showMsg ('account',NO).
  IF CAN-FIND(FIRST account WHERE account.company EQ ipCompanyTo) THEN
  FOR EACH account EXCLUSIVE-LOCK WHERE account.company EQ ipCompanyTo:
    DELETE account.
  END.
  FOR EACH account NO-LOCK WHERE account.company EQ ipCompanyFrom:
    CREATE baccount.
    BUFFER-COPY account EXCEPT company rec_key TO baccount
      ASSIGN baccount.company        = ipCompanyTo
             baccount.cyr            = 0
             baccount.cyr-open       = 0
             baccount.lyr            = 0
             baccount.lyr-open       = 0
             baccount.bud            = 0
             baccount.ly-bud         = 0
             baccount.ny-bud         = 0
             baccount.last-auto-date = 01/01/0001
             baccount.last-auto-pnum = 0
             baccount.last-rvrs-date = 01/01/0001
             baccount.last-rvrs-pnum = 0
             baccount.last-jrnl-date = 01/01/0001
             baccount.last-jrnl-pnum = 0
             baccount.last-rcur-date = 01/01/0001
             baccount.last-rcur-pnum = 0.

    {custom\rec_key.i baccount}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ account.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = baccount.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ap-buyCopy:
  DEFINE BUFFER bap-buy FOR ap-buy.

  RUN showMsg ('ap-buy',NO).
  IF CAN-FIND(FIRST ap-buy WHERE ap-buy.company EQ ipCompanyTo) THEN
  FOR EACH ap-buy EXCLUSIVE-LOCK WHERE ap-buy.company EQ ipCompanyTo:
    DELETE ap-buy.
  END.
  FOR EACH ap-buy NO-LOCK WHERE ap-buy.company EQ ipCompanyFrom:
    CREATE bap-buy.
    BUFFER-COPY ap-buy EXCEPT company rec_key TO bap-buy
      ASSIGN bap-buy.company = ipCompanyTo.

    {custom\rec_key.i bap-buy}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ap-ctrlCopy:
  DEFINE BUFFER bap-ctrl FOR ap-ctrl.

  RUN showMsg ('ap-ctrl',NO).
  IF CAN-FIND(FIRST ap-ctrl WHERE ap-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH ap-ctrl EXCLUSIVE-LOCK WHERE ap-ctrl.company EQ ipCompanyTo:
    DELETE ap-ctrl.
  END.
  FOR EACH ap-ctrl NO-LOCK WHERE ap-ctrl.company EQ ipCompanyFrom:
    CREATE bap-ctrl.
    BUFFER-COPY ap-ctrl EXCEPT company rec_key TO bap-ctrl
      ASSIGN bap-ctrl.company = ipCompanyTo.

    {custom\rec_key.i bap-ctrl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ap-ledgerCopy:
  DEFINE BUFFER bap-ledger FOR ap-ledger.

  RUN showMsg ('ap-ledger',NO).
  IF CAN-FIND(FIRST ap-ledger WHERE ap-ledger.company EQ ipCompanyTo) THEN
  FOR EACH ap-ledger EXCLUSIVE-LOCK WHERE ap-ledger.company EQ ipCompanyTo:
    DELETE ap-ledger.
  END.
  FOR EACH ap-ledger NO-LOCK WHERE ap-ledger.company EQ ipCompanyFrom:
    CREATE bap-ledger.
    BUFFER-COPY ap-ledger EXCEPT company rec_key TO bap-ledger
      ASSIGN bap-ledger.company = ipCompanyTo.

    {custom\rec_key.i bap-ledger}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ar-ctrlCopy:
  DEFINE BUFFER bar-ctrl FOR ar-ctrl.

  RUN showMsg ('ar-ctrl',NO).
  IF CAN-FIND(FIRST ar-ctrl WHERE ar-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH ar-ctrl EXCLUSIVE-LOCK WHERE ar-ctrl.company EQ ipCompanyTo:
    DELETE ar-ctrl.
  END.
  FOR EACH ar-ctrl NO-LOCK WHERE ar-ctrl.company EQ ipCompanyFrom:
    CREATE bar-ctrl.
    BUFFER-COPY ar-ctrl EXCEPT company rec_key TO bar-ctrl
      ASSIGN bar-ctrl.company = ipCompanyTo.

    {custom\rec_key.i bar-ctrl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE bankCopy:
  DEFINE BUFFER bbank FOR bank.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('bank',NO).
  IF CAN-FIND(FIRST bank WHERE bank.company EQ ipCompanyTo) THEN
  FOR EACH bank EXCLUSIVE-LOCK WHERE bank.company EQ ipCompanyTo:
    DELETE bank.
  END.
  FOR EACH bank NO-LOCK WHERE bank.company EQ ipCompanyFrom:
    CREATE bbank.
    BUFFER-COPY bank EXCEPT company rec_key TO bbank
      ASSIGN bbank.company = ipCompanyTo.

    {custom\rec_key.i bbank}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ bank.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bbank.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE buyerCopy:
  DEFINE BUFFER bbuyer FOR buyer.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('buyer',NO).
  IF CAN-FIND(FIRST buyer WHERE buyer.company EQ ipCompanyTo) THEN
  FOR EACH buyer EXCLUSIVE-LOCK WHERE buyer.company EQ ipCompanyTo:
    DELETE buyer.
  END.
  FOR EACH buyer NO-LOCK WHERE buyer.company EQ ipCompanyFrom:
    CREATE bbuyer.
    BUFFER-COPY buyer EXCEPT company rec_key TO bbuyer
      ASSIGN bbuyer.company = ipCompanyTo.

    {custom\rec_key.i bbuyer}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ buyer.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bbuyer.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE carr-mtxCopy:
  DEFINE BUFFER bcarr-mtx FOR carr-mtx.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('carr-mtx',NO).
  IF CAN-FIND(FIRST carr-mtx WHERE carr-mtx.company EQ ipCompanyTo) THEN
  FOR EACH carr-mtx EXCLUSIVE-LOCK WHERE carr-mtx.company EQ ipCompanyTo:
    DELETE carr-mtx.
  END.
  FOR EACH carr-mtx NO-LOCK WHERE carr-mtx.company EQ ipCompanyFrom:
    CREATE bcarr-mtx.
    BUFFER-COPY carr-mtx EXCEPT company rec_key TO bcarr-mtx
      ASSIGN bcarr-mtx.company = ipCompanyTo.

    {custom\rec_key.i bcarr-mtx}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ carr-mtx.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bcarr-mtx.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE carrierCopy:
  DEFINE BUFFER bcarrier FOR carrier.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('carrier',NO).
  IF CAN-FIND(FIRST carrier WHERE carrier.company EQ ipCompanyTo) THEN
  FOR EACH carrier EXCLUSIVE-LOCK WHERE carrier.company EQ ipCompanyTo:
    DELETE carrier.
  END.
  FOR EACH carrier NO-LOCK WHERE carrier.company EQ ipCompanyFrom:
    CREATE bcarrier.
    BUFFER-COPY carrier EXCEPT company rec_key TO bcarrier
      ASSIGN bcarrier.company = ipCompanyTo.

    {custom\rec_key.i bcarrier}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ carrier.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bcarrier.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ce-ctrlCopy:
  DEFINE BUFFER bce-ctrl FOR ce-ctrl.

  RUN showMsg ('ce-ctrl',NO).
  IF CAN-FIND(FIRST ce-ctrl WHERE ce-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH ce-ctrl EXCLUSIVE-LOCK WHERE ce-ctrl.company EQ ipCompanyTo:
    DELETE ce-ctrl.
  END.
  FOR EACH ce-ctrl NO-LOCK WHERE ce-ctrl.company EQ ipCompanyFrom:
    CREATE bce-ctrl.
    BUFFER-COPY ce-ctrl EXCEPT company rec_key TO bce-ctrl
      ASSIGN bce-ctrl.company = ipCompanyTo.

    {custom\rec_key.i bce-ctrl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE costtypeCopy:
  DEFINE BUFFER bcosttype FOR costtype.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('costtype',NO).
  IF CAN-FIND(FIRST costtype WHERE costtype.company EQ ipCompanyTo) THEN
  FOR EACH costtype EXCLUSIVE-LOCK WHERE costtype.company EQ ipCompanyTo:
    DELETE costtype.
  END.
  FOR EACH costtype NO-LOCK WHERE costtype.company EQ ipCompanyFrom:
    CREATE bcosttype.
    BUFFER-COPY costtype EXCEPT company rec_key TO bcosttype
      ASSIGN bcosttype.company = ipCompanyTo.

    {custom\rec_key.i bcosttype}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ costtype.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bcosttype.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE crewCopy:
  DEFINE BUFFER bcrew FOR crew.

  RUN showMsg ('crew',NO).
  IF CAN-FIND(FIRST crew WHERE crew.company EQ ipCompanyTo) THEN
  FOR EACH crew EXCLUSIVE-LOCK WHERE crew.company EQ ipCompanyTo:
    DELETE crew.
  END.
  FOR EACH crew NO-LOCK WHERE crew.company EQ ipCompanyFrom:
    CREATE bcrew.
    BUFFER-COPY crew EXCEPT company rec_key TO bcrew
      ASSIGN bcrew.company = ipCompanyTo.

    {custom\rec_key.i bcrew}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE currencyCopy:
  DEFINE BUFFER bcurrency FOR currency.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('currency',NO).
  IF CAN-FIND(FIRST currency WHERE currency.company EQ ipCompanyTo) THEN
  FOR EACH currency EXCLUSIVE-LOCK WHERE currency.company EQ ipCompanyTo:
    DELETE currency.
  END.
  FOR EACH currency NO-LOCK WHERE currency.company EQ ipCompanyFrom:
    CREATE bcurrency.
    BUFFER-COPY currency EXCEPT company rec_key TO bcurrency
      ASSIGN bcurrency.company = ipCompanyTo.

    {custom\rec_key.i bcurrency}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ currency.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bcurrency.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE custCopy:
  DEFINE BUFFER bcust FOR cust.
  DEFINE BUFFER bnotes FOR notes.
  DEFINE BUFFER bphone FOR phone.
  DEFINE BUFFER bemaildtl FOR emaildtl.
  
  RUN showMsg ('cust',NO).
  IF CAN-FIND(FIRST cust WHERE cust.company EQ ipCompanyTo) THEN
  FOR EACH cust EXCLUSIVE-LOCK WHERE cust.company EQ ipCompanyTo:
    DELETE cust.
  END.
  FOR EACH cust NO-LOCK WHERE cust.company EQ ipCompanyFrom:
    CREATE bcust.
    BUFFER-COPY cust EXCEPT company rec_key TO bcust
      ASSIGN bcust.company = ipCompanyTo.

    {custom\rec_key.i bcust}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ cust.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bcust.rec_key.

    END.

    /* emailcod and Phone */
    FOR EACH phone NO-LOCK
        WHERE phone.table_rec_key = cust.rec_key:

        CREATE bphone.
        BUFFER-COPY phone EXCEPT table_rec_key rec_key TO bphone
        ASSIGN
            bphone.table_rec_key = bcust.rec_key.

        {custom\rec_key.i bphone}
            
        FOR EACH emaildtl NO-LOCK
            WHERE emaildtl.table_rec_key EQ phone.rec_key:
       
            CREATE bemaildtl.
            BUFFER-COPY emaildtl EXCEPT table_rec_key TO bemaildtl 
            ASSIGN 
                bemaildtl.table_rec_key = bphone.rec_key.
        
        END.        
       
    END.
    
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE cust-markupCopy:
  DEFINE BUFFER bcust-markup FOR cust-markup.

  RUN showMsg ('cust-markup',NO).
  IF CAN-FIND(FIRST cust-markup WHERE cust-markup.company EQ ipCompanyTo) THEN
  FOR EACH cust-markup EXCLUSIVE-LOCK WHERE cust-markup.company EQ ipCompanyTo:
    DELETE cust-markup.
  END.
  FOR EACH cust-markup NO-LOCK WHERE cust-markup.company EQ ipCompanyFrom:
    CREATE bcust-markup.
    BUFFER-COPY cust-markup EXCEPT company rec_key TO bcust-markup
      ASSIGN bcust-markup.company = ipCompanyTo.

    {custom\rec_key.i bcust-markup}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE cust-partCopy:
  DEFINE BUFFER bcust-part FOR cust-part.

  RUN showMsg ('cust-part',NO).
  IF CAN-FIND(FIRST cust-part WHERE cust-part.company EQ ipCompanyTo) THEN
  FOR EACH cust-part EXCLUSIVE-LOCK WHERE cust-part.company EQ ipCompanyTo:
    DELETE cust-part.
  END.
  FOR EACH cust-part NO-LOCK WHERE cust-part.company EQ ipCompanyFrom:
    CREATE bcust-part.
    BUFFER-COPY cust-part EXCEPT company rec_key TO bcust-part
      ASSIGN bcust-part.company = ipCompanyTo.

    {custom\rec_key.i bcust-part}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE cust-prod-salesCopy:
  DEFINE BUFFER bcust-prod-sales FOR cust-prod-sales.

  RUN showMsg ('cust-prod-sales',NO).
  IF CAN-FIND(FIRST cust-prod-sales WHERE cust-prod-sales.company EQ ipCompanyTo) THEN
  FOR EACH cust-prod-sales EXCLUSIVE-LOCK WHERE cust-prod-sales.company EQ ipCompanyTo:
    DELETE cust-prod-sales.
  END.
  FOR EACH cust-prod-sales NO-LOCK WHERE cust-prod-sales.company EQ ipCompanyFrom:
    CREATE bcust-prod-sales.
    BUFFER-COPY cust-prod-sales EXCEPT company rec_key TO bcust-prod-sales
      ASSIGN bcust-prod-sales.company = ipCompanyTo.

    {custom\rec_key.i bcust-prod-sales}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE custypeCopy:
  DEFINE BUFFER bcustype FOR custype.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('custype',NO).
  IF CAN-FIND(FIRST custype WHERE custype.company EQ ipCompanyTo) THEN
  FOR EACH custype EXCLUSIVE-LOCK WHERE custype.company EQ ipCompanyTo:
    DELETE custype.
  END.
  FOR EACH custype NO-LOCK WHERE custype.company EQ ipCompanyFrom:
    CREATE bcustype.
    BUFFER-COPY custype EXCEPT company rec_key TO bcustype
      ASSIGN bcustype.company = ipCompanyTo.

    {custom\rec_key.i bcustype}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ custype.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bcustype.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE db-ctrlCopy:
  DEFINE BUFFER bdb-ctrl FOR db-ctrl.

  RUN showMsg ('db-ctrl',NO).
  IF CAN-FIND(FIRST db-ctrl WHERE db-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH db-ctrl EXCLUSIVE-LOCK WHERE db-ctrl.company EQ ipCompanyTo:
    DELETE db-ctrl.
  END.
  FOR EACH db-ctrl NO-LOCK WHERE db-ctrl.company EQ ipCompanyFrom:
    CREATE bdb-ctrl.
    BUFFER-COPY db-ctrl EXCEPT company rec_key TO bdb-ctrl
      ASSIGN bdb-ctrl.company = ipCompanyTo.

    {custom\rec_key.i bdb-ctrl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE e-itemCopy:
  DEFINE BUFFER be-item FOR e-item.

  RUN showMsg ('e-item',NO).
  IF CAN-FIND(FIRST e-item WHERE e-item.company EQ ipCompanyTo) THEN
  FOR EACH e-item EXCLUSIVE-LOCK WHERE e-item.company EQ ipCompanyTo:
    DELETE e-item.
  END.
  FOR EACH e-item NO-LOCK WHERE e-item.company EQ ipCompanyFrom:
    CREATE be-item.
    BUFFER-COPY e-item EXCEPT company rec_key TO be-item
      ASSIGN be-item.company = ipCompanyTo.

    {custom\rec_key.i be-item}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE e-item-custCopy:
  DEFINE BUFFER be-item-cust FOR e-item-cust.

  RUN showMsg ('e-item-cust',NO).
  IF CAN-FIND(FIRST e-item-cust WHERE e-item-cust.company EQ ipCompanyTo) THEN
  FOR EACH e-item-cust EXCLUSIVE-LOCK WHERE e-item-cust.company EQ ipCompanyTo:
    DELETE e-item-cust.
  END.
  FOR EACH e-item-cust NO-LOCK WHERE e-item-cust.company EQ ipCompanyFrom:
    CREATE be-item-cust.
    BUFFER-COPY e-item-cust EXCEPT company rec_key TO be-item-cust
      ASSIGN be-item-cust.company = ipCompanyTo.

    {custom\rec_key.i be-item-cust}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE e-item-vendCopy:
  DEFINE BUFFER be-item-vend FOR e-item-vend.

  RUN showMsg ('e-item-vend',NO).
  IF CAN-FIND(FIRST e-item-vend WHERE e-item-vend.company EQ ipCompanyTo) THEN
  FOR EACH e-item-vend EXCLUSIVE-LOCK WHERE e-item-vend.company EQ ipCompanyTo:
    DELETE e-item-vend.
  END.
  FOR EACH e-item-vend NO-LOCK WHERE e-item-vend.company EQ ipCompanyFrom:
    CREATE be-item-vend.
    BUFFER-COPY e-item-vend EXCEPT company rec_key TO be-item-vend
      ASSIGN be-item-vend.company = ipCompanyTo.

    {custom\rec_key.i be-item-vend}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE e-itemfgCopy:
  DEFINE BUFFER be-itemfg FOR e-itemfg.

  RUN showMsg ('e-itemfg',NO).
  IF CAN-FIND(FIRST e-itemfg WHERE e-itemfg.company EQ ipCompanyTo) THEN
  FOR EACH e-itemfg EXCLUSIVE-LOCK WHERE e-itemfg.company EQ ipCompanyTo:
    DELETE e-itemfg.
  END.
  FOR EACH e-itemfg NO-LOCK WHERE e-itemfg.company EQ ipCompanyFrom:
    CREATE be-itemfg.
    BUFFER-COPY e-itemfg EXCEPT company rec_key TO be-itemfg
      ASSIGN be-itemfg.company = ipCompanyTo.

    {custom\rec_key.i be-itemfg}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE e-itemfg-vendCopy:
  DEFINE BUFFER be-itemfg-vend FOR e-itemfg-vend.

  RUN showMsg ('e-itemfg-vend',NO).
  IF CAN-FIND(FIRST e-itemfg-vend WHERE e-itemfg-vend.company EQ ipCompanyTo) THEN
  FOR EACH e-itemfg-vend EXCLUSIVE-LOCK WHERE e-itemfg-vend.company EQ ipCompanyTo:
    DELETE e-itemfg-vend.
  END.
  FOR EACH e-itemfg-vend NO-LOCK WHERE e-itemfg-vend.company EQ ipCompanyFrom:
    CREATE be-itemfg-vend.
    BUFFER-COPY e-itemfg-vend EXCEPT company rec_key TO be-itemfg-vend
      ASSIGN be-itemfg-vend.company = ipCompanyTo.

    {custom\rec_key.i be-itemfg-vend}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE empCopy:
  DEFINE BUFFER bemp FOR emp.

  RUN showMsg ('emp',NO).
  IF CAN-FIND(FIRST emp WHERE emp.company EQ ipCompanyTo) THEN
  FOR EACH emp EXCLUSIVE-LOCK WHERE emp.company EQ ipCompanyTo:
    DELETE emp.
  END.
  FOR EACH emp NO-LOCK WHERE emp.company EQ ipCompanyFrom:
    CREATE bemp.
    BUFFER-COPY emp EXCEPT company rec_key TO bemp
      ASSIGN bemp.company = ipCompanyTo.

    {custom\rec_key.i bemp}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fg-actCopy:
  DEFINE BUFFER bfg-act FOR fg-act.

  RUN showMsg ('fg-act',NO).
  IF CAN-FIND(FIRST fg-act WHERE fg-act.company EQ ipCompanyTo) THEN
  FOR EACH fg-act EXCLUSIVE-LOCK WHERE fg-act.company EQ ipCompanyTo:
    DELETE fg-act.
  END.
  FOR EACH fg-act NO-LOCK WHERE fg-act.company EQ ipCompanyFrom:
    CREATE bfg-act.
    BUFFER-COPY fg-act EXCEPT company rec_key TO bfg-act
      ASSIGN bfg-act.company = ipCompanyTo.

    {custom\rec_key.i bfg-act}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fg-binCopy:
  DEFINE BUFFER bfg-bin FOR fg-bin.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('fg-bin',NO).
  IF CAN-FIND(FIRST fg-bin WHERE fg-bin.company EQ ipCompanyTo) THEN
  FOR EACH fg-bin EXCLUSIVE-LOCK WHERE fg-bin.company EQ ipCompanyTo:
    DELETE fg-bin.
  END.
  FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ ipCompanyFrom:
    CREATE bfg-bin.
    BUFFER-COPY fg-bin EXCEPT company rec_key TO bfg-bin
      ASSIGN bfg-bin.company = ipCompanyTo.

    {custom\rec_key.i bfg-bin}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ fg-bin.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bfg-bin.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fg-ctrlCopy:
  DEFINE BUFFER bfg-ctrl FOR fg-ctrl.

  RUN showMsg ('fg-ctrl',NO).
  IF CAN-FIND(FIRST fg-ctrl WHERE fg-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH fg-ctrl EXCLUSIVE-LOCK WHERE fg-ctrl.company EQ ipCompanyTo:
    DELETE fg-ctrl.
  END.
  FOR EACH fg-ctrl NO-LOCK WHERE fg-ctrl.company EQ ipCompanyFrom:
    CREATE bfg-ctrl.
    BUFFER-COPY fg-ctrl EXCEPT company rec_key TO bfg-ctrl
      ASSIGN bfg-ctrl.company = ipCompanyTo.

    {custom\rec_key.i bfg-ctrl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fg-setCopy:
  DEFINE BUFFER bfg-set FOR fg-set.

  RUN showMsg ('fg-set',NO).
  IF CAN-FIND(FIRST fg-set WHERE fg-set.company EQ ipCompanyTo) THEN
  FOR EACH fg-set EXCLUSIVE-LOCK WHERE fg-set.company EQ ipCompanyTo:
    DELETE fg-set.
  END.
  FOR EACH fg-set NO-LOCK WHERE fg-set.company EQ ipCompanyFrom:
    CREATE bfg-set.
    {custom/cc_fg-set.i}
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fgcatCopy:
  DEFINE BUFFER bfgcat FOR fgcat.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('fgcat',NO).
  IF CAN-FIND(FIRST fgcat WHERE fgcat.company EQ ipCompanyTo) THEN
  FOR EACH fgcat EXCLUSIVE-LOCK WHERE fgcat.company EQ ipCompanyTo:
    DELETE fgcat.
  END.
  FOR EACH fgcat NO-LOCK WHERE fgcat.company EQ ipCompanyFrom:
    CREATE bfgcat.
    BUFFER-COPY fgcat EXCEPT company rec_key TO bfgcat
      ASSIGN bfgcat.company = ipCompanyTo.

    {custom\rec_key.i bfgcat}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ fgcat.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bfgcat.rec_key.

    END.


  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fluteCopy:
  DEFINE BUFFER bflute FOR flute.

  RUN showMsg ('flute',NO).
  IF CAN-FIND(FIRST flute WHERE flute.company EQ ipCompanyTo) THEN
  FOR EACH flute EXCLUSIVE-LOCK WHERE flute.company EQ ipCompanyTo:
    DELETE flute.
  END.
  FOR EACH flute NO-LOCK WHERE flute.company EQ ipCompanyFrom:
    CREATE bflute.
    BUFFER-COPY flute EXCEPT company rec_key TO bflute
      ASSIGN bflute.company = ipCompanyTo.

    {custom\rec_key.i bflute}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE gl-ctrlCopy:
  DEFINE BUFFER bgl-ctrl FOR gl-ctrl.

  RUN showMsg ('gl-ctrl',NO).
  IF CAN-FIND(FIRST gl-ctrl WHERE gl-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH gl-ctrl EXCLUSIVE-LOCK WHERE gl-ctrl.company EQ ipCompanyTo:
    DELETE gl-ctrl.
  END.
  FOR EACH gl-ctrl NO-LOCK WHERE gl-ctrl.company EQ ipCompanyFrom:
    CREATE bgl-ctrl.
    BUFFER-COPY gl-ctrl EXCEPT company rec_key TO bgl-ctrl
      ASSIGN bgl-ctrl.company = ipCompanyTo.

    {custom\rec_key.i bgl-ctrl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE gl-rptCopy:
  DEFINE BUFFER bgl-rpt FOR gl-rpt.

  RUN showMsg ('gl-rpt',NO).
  IF CAN-FIND(FIRST gl-rpt WHERE gl-rpt.company EQ ipCompanyTo) THEN
  FOR EACH gl-rpt EXCLUSIVE-LOCK WHERE gl-rpt.company EQ ipCompanyTo:
    DELETE gl-rpt.
  END.
  FOR EACH gl-rpt NO-LOCK WHERE gl-rpt.company EQ ipCompanyFrom:
    CREATE bgl-rpt.
    BUFFER-COPY gl-rpt EXCEPT company rec_key TO bgl-rpt
      ASSIGN bgl-rpt.company = ipCompanyTo.

    {custom\rec_key.i bgl-rpt}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE gl-rptdCopy:
  DEFINE BUFFER bgl-rptd FOR gl-rptd.

  RUN showMsg ('gl-rptd',NO).
  IF CAN-FIND(FIRST gl-rptd WHERE gl-rptd.company EQ ipCompanyTo) THEN
  FOR EACH gl-rptd EXCLUSIVE-LOCK WHERE gl-rptd.company EQ ipCompanyTo:
    DELETE gl-rptd.
  END.
  FOR EACH gl-rptd NO-LOCK WHERE gl-rptd.company EQ ipCompanyFrom:
    CREATE bgl-rptd.
    BUFFER-COPY gl-rptd EXCEPT company rec_key TO bgl-rptd
      ASSIGN bgl-rptd.company = ipCompanyTo.

    {custom\rec_key.i bgl-rptd}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE itemCopy:
  DEFINE BUFFER bitem FOR item.

  RUN showMsg ('item',NO).
  IF CAN-FIND(FIRST item WHERE item.company EQ ipCompanyTo) THEN
  FOR EACH item EXCLUSIVE-LOCK WHERE item.company EQ ipCompanyTo:
    DELETE item.
  END.
  FOR EACH item NO-LOCK WHERE item.company EQ ipCompanyFrom:
    CREATE bitem.
    BUFFER-COPY item EXCEPT company rec_key TO bitem
      ASSIGN bitem.company = ipCompanyTo.

    {custom\rec_key.i bitem}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE item-bomCopy:
  DEFINE BUFFER bitem-bom FOR item-bom.

  RUN showMsg ('item-bom',NO).
  IF CAN-FIND(FIRST item-bom WHERE item-bom.company EQ ipCompanyTo) THEN
  FOR EACH item-bom EXCLUSIVE-LOCK WHERE item-bom.company EQ ipCompanyTo:
    DELETE item-bom.
  END.
  FOR EACH item-bom NO-LOCK WHERE item-bom.company EQ ipCompanyFrom:
    CREATE bitem-bom.
    BUFFER-COPY item-bom EXCEPT company rec_key TO bitem-bom
      ASSIGN bitem-bom.company = ipCompanyTo.

    {custom\rec_key.i bitem-bom}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE item-specCopy:
  DEFINE BUFFER bitem-spec FOR item-spec.

  RUN showMsg ('item-spec',NO).
  IF CAN-FIND(FIRST item-spec WHERE item-spec.company EQ ipCompanyTo) THEN
  FOR EACH item-spec EXCLUSIVE-LOCK WHERE item-spec.company EQ ipCompanyTo:
    DELETE item-spec.
  END.
  FOR EACH item-spec NO-LOCK WHERE item-spec.company EQ ipCompanyFrom:
    CREATE bitem-spec.
    BUFFER-COPY item-spec EXCEPT company rec_key TO bitem-spec
      ASSIGN bitem-spec.company = ipCompanyTo.

    {custom\rec_key.i bitem-spec}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE itemfgCopy:
  DEFINE BUFFER bitemfg FOR itemfg.

  RUN showMsg ('itemfg',NO).
  IF CAN-FIND(FIRST itemfg WHERE itemfg.company EQ ipCompanyTo) THEN
  FOR EACH itemfg EXCLUSIVE-LOCK WHERE itemfg.company EQ ipCompanyTo:
    DELETE itemfg.
  END.
  FOR EACH itemfg NO-LOCK WHERE itemfg.company EQ ipCompanyFrom:
    CREATE bitemfg.
    BUFFER-COPY itemfg EXCEPT company rec_key TO bitemfg
      ASSIGN bitemfg.company = ipCompanyTo.

    {custom\rec_key.i bitemfg}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE itemfg-bomCopy:
  DEFINE BUFFER bitemfg-bom FOR itemfg-bom.

  RUN showMsg ('itemfg-bom',NO).
  IF CAN-FIND(FIRST itemfg-bom WHERE itemfg-bom.company EQ ipCompanyTo) THEN
  FOR EACH itemfg-bom EXCLUSIVE-LOCK WHERE itemfg-bom.company EQ ipCompanyTo:
    DELETE itemfg-bom.
  END.
  FOR EACH itemfg-bom NO-LOCK WHERE itemfg-bom.company EQ ipCompanyFrom:
    CREATE bitemfg-bom.
    BUFFER-COPY itemfg-bom EXCEPT company rec_key TO bitemfg-bom
      ASSIGN bitemfg-bom.company = ipCompanyTo.

    {custom\rec_key.i bitemfg-bom}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE itemfg-inkCopy:
  DEFINE BUFFER bitemfg-ink FOR itemfg-ink.

  RUN showMsg ('itemfg-ink',NO).
  IF CAN-FIND(FIRST itemfg-ink WHERE itemfg-ink.company EQ ipCompanyTo) THEN
  FOR EACH itemfg-ink EXCLUSIVE-LOCK WHERE itemfg-ink.company EQ ipCompanyTo:
    DELETE itemfg-ink.
  END.
  FOR EACH itemfg-ink NO-LOCK WHERE itemfg-ink.company EQ ipCompanyFrom:
    CREATE bitemfg-ink.
    BUFFER-COPY itemfg-ink EXCEPT company rec_key TO bitemfg-ink
      ASSIGN bitemfg-ink.company = ipCompanyTo.

    {custom\rec_key.i bitemfg-ink}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE itemfg-locCopy:
  DEFINE BUFFER bitemfg-loc FOR itemfg-loc.

  RUN showMsg ('itemfg-loc',NO).
  IF CAN-FIND(FIRST itemfg-loc WHERE itemfg-loc.company EQ ipCompanyTo) THEN
  FOR EACH itemfg-loc EXCLUSIVE-LOCK WHERE itemfg-loc.company EQ ipCompanyTo:
    DELETE itemfg-loc.
  END.
  FOR EACH itemfg-loc NO-LOCK WHERE itemfg-loc.company EQ ipCompanyFrom:
    CREATE bitemfg-loc.
    BUFFER-COPY itemfg-loc EXCEPT company rec_key TO bitemfg-loc
      ASSIGN bitemfg-loc.company = ipCompanyTo.

    {custom\rec_key.i bitemfg-loc}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE itemfgdtlCopy:
  DEFINE BUFFER bitemfgdtl FOR itemfgdtl.

  RUN showMsg ('itemfgdtl',NO).
  IF CAN-FIND(FIRST itemfgdtl WHERE itemfgdtl.company EQ ipCompanyTo) THEN
  FOR EACH itemfgdtl EXCLUSIVE-LOCK WHERE itemfgdtl.company EQ ipCompanyTo:
    DELETE itemfgdtl.
  END.
  FOR EACH itemfgdtl NO-LOCK WHERE itemfgdtl.company EQ ipCompanyFrom:
    CREATE bitemfgdtl.
    BUFFER-COPY itemfgdtl EXCEPT company rec_key TO bitemfgdtl
      ASSIGN bitemfgdtl.company = ipCompanyTo.

    {custom\rec_key.i bitemfgdtl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE jc-ctrlCopy:
  DEFINE BUFFER bjc-ctrl FOR jc-ctrl.

  RUN showMsg ('jc-ctrl',NO).
  IF CAN-FIND(FIRST jc-ctrl WHERE jc-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH jc-ctrl EXCLUSIVE-LOCK WHERE jc-ctrl.company EQ ipCompanyTo:
    DELETE jc-ctrl.
  END.
  FOR EACH jc-ctrl NO-LOCK WHERE jc-ctrl.company EQ ipCompanyFrom:
    CREATE bjc-ctrl.
    BUFFER-COPY jc-ctrl EXCEPT company rec_key TO bjc-ctrl
      ASSIGN bjc-ctrl.company = ipCompanyTo.

    {custom\rec_key.i bjc-ctrl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE loadtagCopy:
  DEFINE BUFFER bloadtag FOR loadtag.

  RUN showMsg ('loadtag',NO).
  IF CAN-FIND(FIRST loadtag WHERE loadtag.company EQ ipCompanyTo) THEN
  FOR EACH loadtag EXCLUSIVE-LOCK WHERE loadtag.company EQ ipCompanyTo:
    DELETE loadtag.
  END.
  FOR EACH loadtag NO-LOCK WHERE loadtag.company EQ ipCompanyFrom:
    CREATE bloadtag.
    BUFFER-COPY loadtag EXCEPT company rec_key TO bloadtag
      ASSIGN bloadtag.company = ipCompanyTo.

    {custom\rec_key.i bloadtag}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE locCopy:
  DEFINE BUFFER bloc FOR loc.

  RUN showMsg ('loc',NO).
  IF CAN-FIND(FIRST loc WHERE loc.company EQ ipCompanyTo) THEN
  FOR EACH loc EXCLUSIVE-LOCK WHERE loc.company EQ ipCompanyTo:
    DELETE loc.
  END.
  FOR EACH loc NO-LOCK WHERE loc.company EQ ipCompanyFrom:
    CREATE bloc.
    BUFFER-COPY loc EXCEPT company rec_key TO bloc
      ASSIGN bloc.company = ipCompanyTo.

    {custom\rec_key.i bloc}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE machCopy:
  DEFINE BUFFER bmach FOR mach.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('mach',NO).
  IF CAN-FIND(FIRST mach WHERE mach.company EQ ipCompanyTo) THEN
  FOR EACH mach EXCLUSIVE-LOCK WHERE mach.company EQ ipCompanyTo:
    DELETE mach.
  END.
  FOR EACH mach NO-LOCK WHERE mach.company EQ ipCompanyFrom:
    CREATE bmach.
    BUFFER-COPY mach EXCEPT company rec_key TO bmach
      ASSIGN bmach.company = ipCompanyTo.

    {custom\rec_key.i bmach}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ mach.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bmach.rec_key.
    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE mach-adderCopy:
  DEFINE BUFFER bmach-adder FOR mach-adder.

  RUN showMsg ('mach-adder',NO).
  IF CAN-FIND(FIRST mach-adder WHERE mach-adder.company EQ ipCompanyTo) THEN
  FOR EACH mach-adder EXCLUSIVE-LOCK WHERE mach-adder.company EQ ipCompanyTo:
    DELETE mach-adder.
  END.
  FOR EACH mach-adder NO-LOCK WHERE mach-adder.company EQ ipCompanyFrom:
    CREATE bmach-adder.
    BUFFER-COPY mach-adder EXCEPT company rec_key TO bmach-adder
      ASSIGN bmach-adder.company = ipCompanyTo.

    {custom\rec_key.i bmach-adder}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE mach-calendarCopy:
  DEFINE BUFFER bmach-calendar FOR mach-calendar.

  RUN showMsg ('mach-calendar',NO).
  IF CAN-FIND(FIRST mach-calendar WHERE mach-calendar.company EQ ipCompanyTo) THEN
  FOR EACH mach-calendar EXCLUSIVE-LOCK WHERE mach-calendar.company EQ ipCompanyTo:
    DELETE mach-calendar.
  END.
  FOR EACH mach-calendar NO-LOCK WHERE mach-calendar.company EQ ipCompanyFrom:
    CREATE bmach-calendar.
    BUFFER-COPY mach-calendar EXCEPT company rec_key TO bmach-calendar
      ASSIGN bmach-calendar.company = ipCompanyTo.

    {custom\rec_key.i bmach-calendar}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE mat-actCopy:
  DEFINE BUFFER bmat-act FOR mat-act.

  RUN showMsg ('mat-act',NO).
  IF CAN-FIND(FIRST mat-act WHERE mat-act.company EQ ipCompanyTo) THEN
  FOR EACH mat-act EXCLUSIVE-LOCK WHERE mat-act.company EQ ipCompanyTo:
    DELETE mat-act.
  END.
  FOR EACH mat-act NO-LOCK WHERE mat-act.company EQ ipCompanyFrom:
    CREATE bmat-act.
    BUFFER-COPY mat-act EXCEPT company rec_key TO bmat-act
      ASSIGN bmat-act.company = ipCompanyTo.

    {custom\rec_key.i bmat-act}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE matprepCopy:
  DEFINE BUFFER bmatprep FOR matprep.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('matprep',NO).
  IF CAN-FIND(FIRST matprep WHERE matprep.company EQ ipCompanyTo) THEN
  FOR EACH matprep EXCLUSIVE-LOCK WHERE matprep.company EQ ipCompanyTo:
    DELETE matprep.
  END.
  FOR EACH matprep NO-LOCK WHERE matprep.company EQ ipCompanyFrom:
    CREATE bmatprep.
    BUFFER-COPY matprep EXCEPT company rec_key TO bmatprep
      ASSIGN bmatprep.company = ipCompanyTo.

    {custom\rec_key.i bmatprep}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ matprep.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bmatprep.rec_key.

    END.
    
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE mmtxCopy:
  DEFINE BUFFER bmmtx FOR mmtx.
  {custom/cc_mmtxDefs.i}

  RUN showMsg ('mmtx',NO).
  IF CAN-FIND(FIRST mmtx WHERE mmtx.company EQ ipCompanyTo) THEN
  FOR EACH mmtx EXCLUSIVE-LOCK WHERE mmtx.company EQ ipCompanyTo:
    DELETE mmtx.
  END.
  FOR EACH mmtx NO-LOCK WHERE mmtx.company EQ ipCompanyFrom:
    CREATE bmmtx.
    {custom/cc_mmtx.i}
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE mmtyCopy:
  DEFINE BUFFER bmmty FOR mmty.

  RUN showMsg ('mmty',NO).
  IF CAN-FIND(FIRST mmty WHERE mmty.company EQ ipCompanyTo) THEN
  FOR EACH mmty EXCLUSIVE-LOCK WHERE mmty.company EQ ipCompanyTo:
    DELETE mmty.
  END.
  FOR EACH mmty NO-LOCK WHERE mmty.company EQ ipCompanyFrom:
    CREATE bmmty.
    BUFFER-COPY mmty EXCEPT company rec_key TO bmmty
      ASSIGN bmmty.company = ipCompanyTo.

    {custom\rec_key.i bmmty}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE mstdCopy:
  DEFINE BUFFER bmstd FOR mstd.

  RUN showMsg ('mstd',NO).
  IF CAN-FIND(FIRST mstd WHERE mstd.company EQ ipCompanyTo) THEN
  FOR EACH mstd EXCLUSIVE-LOCK WHERE mstd.company EQ ipCompanyTo:
    DELETE mstd.
  END.
  FOR EACH mstd NO-LOCK WHERE mstd.company EQ ipCompanyFrom:
    CREATE bmstd.
    BUFFER-COPY mstd EXCEPT company rec_key TO bmstd
      ASSIGN bmstd.company = ipCompanyTo.
    
    {custom\rec_key.i bmstd}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-ctrlCopy:
  DEFINE BUFFER boe-ctrl FOR oe-ctrl.

  RUN showMsg ('oe-ctrl',NO).
  IF CAN-FIND(FIRST oe-ctrl WHERE oe-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH oe-ctrl EXCLUSIVE-LOCK WHERE oe-ctrl.company EQ ipCompanyTo:
    DELETE oe-ctrl.
  END.
  FOR EACH oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ ipCompanyFrom:
    CREATE boe-ctrl.
    BUFFER-COPY oe-ctrl EXCEPT company rec_key TO boe-ctrl
      ASSIGN boe-ctrl.company = ipCompanyTo.
    
    {custom\rec_key.i boe-ctrl}
    
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-prmtxCopy:
  DEFINE BUFFER boe-prmtx FOR oe-prmtx.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('oe-prmtx',NO).
  IF CAN-FIND(FIRST oe-prmtx WHERE oe-prmtx.company EQ ipCompanyTo) THEN
  FOR EACH oe-prmtx EXCLUSIVE-LOCK WHERE oe-prmtx.company EQ ipCompanyTo:
    DELETE oe-prmtx.
  END.
  FOR EACH oe-prmtx NO-LOCK WHERE oe-prmtx.company EQ ipCompanyFrom:
    CREATE boe-prmtx.
    BUFFER-COPY oe-prmtx EXCEPT company rec_key TO boe-prmtx
      ASSIGN boe-prmtx.company = ipCompanyTo.

    {custom\rec_key.i boe-prmtx}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ oe-prmtx.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = boe-prmtx.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE periodCopy:
  DEFINE BUFFER bperiod FOR period.

  RUN showMsg ('period',NO).
  IF CAN-FIND(FIRST period WHERE period.company EQ ipCompanyTo) THEN
  FOR EACH period EXCLUSIVE-LOCK WHERE period.company EQ ipCompanyTo:
    DELETE period.
  END.
  FOR EACH period NO-LOCK WHERE period.company EQ ipCompanyFrom:
    CREATE bperiod.
    BUFFER-COPY period EXCEPT company rec_key TO bperiod
      ASSIGN bperiod.company = ipCompanyTo.

    {custom\rec_key.i bperiod}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE po-ctrlCopy:
  DEFINE BUFFER bpo-ctrl FOR po-ctrl.

  RUN showMsg ('po-ctrl',NO).
  IF CAN-FIND(FIRST po-ctrl WHERE po-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH po-ctrl EXCLUSIVE-LOCK WHERE po-ctrl.company EQ ipCompanyTo:
    DELETE po-ctrl.
  END.
  FOR EACH po-ctrl NO-LOCK WHERE po-ctrl.company EQ ipCompanyFrom:
    CREATE bpo-ctrl.
    BUFFER-COPY po-ctrl EXCEPT company rec_key TO bpo-ctrl
      ASSIGN bpo-ctrl.company = ipCompanyTo.

    {custom\rec_key.i bpo-ctrl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE prepCopy:
  DEFINE BUFFER bprep FOR prep.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('prep',NO).
  IF CAN-FIND(FIRST prep WHERE prep.company EQ ipCompanyTo) THEN
  FOR EACH prep EXCLUSIVE-LOCK WHERE prep.company EQ ipCompanyTo:
    DELETE prep.
  END.
  FOR EACH prep NO-LOCK WHERE prep.company EQ ipCompanyFrom:
    CREATE bprep.
    BUFFER-COPY prep EXCEPT company rec_key TO bprep
      ASSIGN bprep.company = ipCompanyTo.

    {custom\rec_key.i bprep}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ prep.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bprep.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE procatCopy:
  DEFINE BUFFER bprocat FOR procat.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('procat',NO).
  IF CAN-FIND(FIRST procat WHERE procat.company EQ ipCompanyTo) THEN
  FOR EACH procat EXCLUSIVE-LOCK WHERE procat.company EQ ipCompanyTo:
    DELETE procat.
  END.
  FOR EACH procat NO-LOCK WHERE procat.company EQ ipCompanyFrom:
    CREATE bprocat.
    BUFFER-COPY procat EXCEPT company rec_key TO bprocat
      ASSIGN bprocat.company = ipCompanyTo.
    
    {custom\rec_key.i bprocat}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ procat.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bprocat.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE prodCopy:
  DEFINE BUFFER bprod FOR prod.

  RUN showMsg ('prod',NO).
  IF CAN-FIND(FIRST prod WHERE prod.company EQ ipCompanyTo) THEN
  FOR EACH prod EXCLUSIVE-LOCK WHERE prod.company EQ ipCompanyTo:
    DELETE prod.
  END.
  FOR EACH prod NO-LOCK WHERE prod.company EQ ipCompanyFrom:
    CREATE bprod.
    BUFFER-COPY prod EXCEPT company rec_key TO bprod
      ASSIGN bprod.company = ipCompanyTo.

    {custom\rec_key.i bprod} 

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE prodlCopy:
  DEFINE BUFFER bprodl FOR prodl.

  RUN showMsg ('prodl',NO).
  IF CAN-FIND(FIRST prodl WHERE prodl.company EQ ipCompanyTo) THEN
  FOR EACH prodl EXCLUSIVE-LOCK WHERE prodl.company EQ ipCompanyTo:
    DELETE prodl.
  END.
  FOR EACH prodl NO-LOCK WHERE prodl.company EQ ipCompanyFrom:
    CREATE bprodl.
    BUFFER-COPY prodl EXCEPT company rec_key TO bprodl
      ASSIGN bprodl.company = ipCompanyTo.
    
    {custom\rec_key.i bprodl}
        
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE reftableCopy:
  DEFINE BUFFER breftable FOR reftable.

  RUN showMsg ('reftable',NO).
  IF CAN-FIND(FIRST reftable WHERE reftable.company EQ ipCompanyTo) THEN
  FOR EACH reftable EXCLUSIVE-LOCK WHERE reftable.company EQ ipCompanyTo:
    DELETE reftable.
  END.
  FOR EACH reftable NO-LOCK WHERE reftable.company EQ ipCompanyFrom:
    CREATE breftable.
    BUFFER-COPY reftable EXCEPT company rec_key TO breftable
      ASSIGN breftable.company = ipCompanyTo.

    {custom\rec_key.i breftable}
    
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE rm-binCopy:
  DEFINE BUFFER brm-bin FOR rm-bin.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('rm-bin',NO).
  IF CAN-FIND(FIRST rm-bin WHERE rm-bin.company EQ ipCompanyTo) THEN
  FOR EACH rm-bin EXCLUSIVE-LOCK WHERE rm-bin.company EQ ipCompanyTo:
    DELETE rm-bin.
  END.
  FOR EACH rm-bin NO-LOCK WHERE rm-bin.company EQ ipCompanyFrom:
    CREATE brm-bin.
    BUFFER-COPY rm-bin EXCEPT company rec_key TO brm-bin
      ASSIGN brm-bin.company = ipCompanyTo.

    {custom\rec_key.i brm-bin}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ rm-bin.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = brm-bin.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE rm-ctrlCopy:
  DEFINE BUFFER brm-ctrl FOR rm-ctrl.

  RUN showMsg ('rm-ctrl',NO).
  IF CAN-FIND(FIRST rm-ctrl WHERE rm-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH rm-ctrl EXCLUSIVE-LOCK WHERE rm-ctrl.company EQ ipCompanyTo:
    DELETE rm-ctrl.
  END.
  FOR EACH rm-ctrl NO-LOCK WHERE rm-ctrl.company EQ ipCompanyFrom:
    CREATE brm-ctrl.
    BUFFER-COPY rm-ctrl EXCEPT company rec_key TO brm-ctrl
      ASSIGN brm-ctrl.company = ipCompanyTo.

    {custom\rec_key.i brm-ctrl}
    
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE routingCopy:
  DEFINE BUFFER brouting FOR routing.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('routing',NO).
  IF CAN-FIND(FIRST routing WHERE routing.company EQ ipCompanyTo) THEN
  FOR EACH routing EXCLUSIVE-LOCK WHERE routing.company EQ ipCompanyTo:
    DELETE routing.
  END.
  FOR EACH routing NO-LOCK WHERE routing.company EQ ipCompanyFrom:
    CREATE brouting.
    BUFFER-COPY routing EXCEPT company rec_key TO brouting
      ASSIGN brouting.company = ipCompanyTo.

    {custom\rec_key.i brouting} 

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ routing.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = brouting.rec_key.

    END.


  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE routing-mtxCopy:
  DEFINE BUFFER brouting-mtx FOR routing-mtx.

  RUN showMsg ('routing-mtx',NO).
  IF CAN-FIND(FIRST routing-mtx WHERE routing-mtx.company EQ ipCompanyTo) THEN
  FOR EACH routing-mtx EXCLUSIVE-LOCK WHERE routing-mtx.company EQ ipCompanyTo:
    DELETE routing-mtx.
  END.
  FOR EACH routing-mtx NO-LOCK WHERE routing-mtx.company EQ ipCompanyFrom:
    CREATE brouting-mtx.
    BUFFER-COPY routing-mtx EXCEPT company rec_key TO brouting-mtx
      ASSIGN brouting-mtx.company = ipCompanyTo.

    {custom\rec_key.i brouting-mtx}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE shiftCopy:
  DEFINE BUFFER bshift FOR shift.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('shift',NO).
  IF CAN-FIND(FIRST shift WHERE shift.company EQ ipCompanyTo) THEN
  FOR EACH shift EXCLUSIVE-LOCK WHERE shift.company EQ ipCompanyTo:
    DELETE shift.
  END.
  FOR EACH shift NO-LOCK WHERE shift.company EQ ipCompanyFrom:
    CREATE bshift.
    BUFFER-COPY shift EXCEPT company rec_key TO bshift
      ASSIGN bshift.company = ipCompanyTo.

    {custom\rec_key.i bshift}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ shift.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bshift.rec_key.

    END.


  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE shiptoCopy:
  DEFINE BUFFER bshipto FOR shipto.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('shipto',NO).
  IF CAN-FIND(FIRST shipto WHERE shipto.company EQ ipCompanyTo) THEN
  FOR EACH shipto EXCLUSIVE-LOCK WHERE shipto.company EQ ipCompanyTo:
    DELETE shipto.
  END.
  FOR EACH shipto NO-LOCK WHERE shipto.company EQ ipCompanyFrom:
    CREATE bshipto.
    BUFFER-COPY shipto EXCEPT company rec_key TO bshipto
      ASSIGN bshipto.company = ipCompanyTo.

    {custom\rec_key.i bshipto}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ shipto.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bshipto.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE smanCopy:
  DEFINE BUFFER bsman FOR sman.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('sman',NO).
  IF CAN-FIND(FIRST sman WHERE sman.company EQ ipCompanyTo) THEN
  FOR EACH sman EXCLUSIVE-LOCK WHERE sman.company EQ ipCompanyTo:
    DELETE sman.
  END.
  FOR EACH sman NO-LOCK WHERE sman.company EQ ipCompanyFrom:
    CREATE bsman.
    BUFFER-COPY sman EXCEPT company rec_key TO bsman
      ASSIGN bsman.company = ipCompanyTo.

    {custom\rec_key.i bsman}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ sman.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bsman.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE sman-mtxCopy:
  DEFINE BUFFER bsman-mtx FOR sman-mtx.

  RUN showMsg ('sman-mtx',NO).
  IF CAN-FIND(FIRST sman-mtx WHERE sman-mtx.company EQ ipCompanyTo) THEN
  FOR EACH sman-mtx EXCLUSIVE-LOCK WHERE sman-mtx.company EQ ipCompanyTo:
    DELETE sman-mtx.
  END.
  FOR EACH sman-mtx NO-LOCK WHERE sman-mtx.company EQ ipCompanyFrom:
    CREATE bsman-mtx.
    BUFFER-COPY sman-mtx EXCEPT company rec_key TO bsman-mtx
      ASSIGN bsman-mtx.company = ipCompanyTo.

    {custom\rec_key.i bsman-mtx}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

/*
PROCEDURE smanmtrxCopy:
  DEFINE BUFFER bsmanmtrx FOR smanmtrx.

  RUN showMsg ('smanmtrx',NO).
  IF CAN-FIND(FIRST smanmtrx WHERE smanmtrx.company EQ ipCompanyTo) THEN
  FOR EACH smanmtrx EXCLUSIVE-LOCK WHERE smanmtrx.company EQ ipCompanyTo:
    DELETE smanmtrx.
  END.
  FOR EACH smanmtrx NO-LOCK WHERE smanmtrx.company EQ ipCompanyFrom:
    CREATE bsmanmtrx.
    BUFFER-COPY smanmtrx EXCEPT company rec_key TO bsmanmtrx
      ASSIGN bsmanmtrx.company = ipCompanyTo.
  END.
  RUN showMsg ('',YES).
END PROCEDURE.
*/

PROCEDURE soldtoCopy:
  DEFINE BUFFER bsoldto FOR soldto.

  RUN showMsg ('soldto',NO).
  IF CAN-FIND(FIRST soldto WHERE soldto.company EQ ipCompanyTo) THEN
  FOR EACH soldto EXCLUSIVE-LOCK WHERE soldto.company EQ ipCompanyTo:
    DELETE soldto.
  END.
  FOR EACH soldto NO-LOCK WHERE soldto.company EQ ipCompanyFrom:
    CREATE bsoldto.
    BUFFER-COPY soldto EXCEPT company rec_key TO bsoldto
      ASSIGN bsoldto.company = ipCompanyTo.

    {custom\rec_key.i bsoldto}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE stack-fluteCopy:
  DEFINE BUFFER bstack-flute FOR stack-flute.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('stack-flute',NO).
  IF CAN-FIND(FIRST stack-flute WHERE stack-flute.company EQ ipCompanyTo) THEN
  FOR EACH stack-flute EXCLUSIVE-LOCK WHERE stack-flute.company EQ ipCompanyTo:
    DELETE stack-flute.
  END.
  FOR EACH stack-flute NO-LOCK WHERE stack-flute.company EQ ipCompanyFrom:
    CREATE bstack-flute.
    BUFFER-COPY stack-flute EXCEPT company rec_key TO bstack-flute
      ASSIGN bstack-flute.company = ipCompanyTo.

    {custom\rec_key.i bstack-flute}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ stack-flute.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bstack-flute.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE stack-sizeCopy:
  DEFINE BUFFER bstack-size FOR stack-size.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('stack-size',NO).
  IF CAN-FIND(FIRST stack-size WHERE stack-size.company EQ ipCompanyTo) THEN
  FOR EACH stack-size EXCLUSIVE-LOCK WHERE stack-size.company EQ ipCompanyTo:
    DELETE stack-size.
  END.
  FOR EACH stack-size NO-LOCK WHERE stack-size.company EQ ipCompanyFrom:
    CREATE bstack-size.
    BUFFER-COPY stack-size EXCEPT company rec_key TO bstack-size
      ASSIGN bstack-size.company = ipCompanyTo.

    {custom\rec_key.i bstack-size}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ stack-size.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bstack-size.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE staxCopy:
  DEFINE BUFFER bstax FOR stax.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('stax',NO).
  IF CAN-FIND(FIRST stax WHERE stax.company EQ ipCompanyTo) THEN
  FOR EACH stax EXCLUSIVE-LOCK WHERE stax.company EQ ipCompanyTo:
    DELETE stax.
  END.
  FOR EACH stax NO-LOCK WHERE stax.company EQ ipCompanyFrom:
    CREATE bstax.
    BUFFER-COPY stax EXCEPT company rec_key TO bstax
      ASSIGN bstax.company = ipCompanyTo.

    {custom\rec_key.i bstax}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ stax.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bstax.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE stax-groupCopy:
  DEFINE BUFFER bstax-group FOR stax-group.

  RUN showMsg ('stax-group',NO).
  IF CAN-FIND(FIRST stax-group WHERE stax-group.company EQ ipCompanyTo) THEN
  FOR EACH stax-group EXCLUSIVE-LOCK WHERE stax-group.company EQ ipCompanyTo:
    DELETE stax-group.
  END.
  FOR EACH stax-group NO-LOCK WHERE stax-group.company EQ ipCompanyFrom:
    CREATE bstax-group.
    BUFFER-COPY stax-group EXCEPT company rec_key TO bstax-group
      ASSIGN bstax-group.company = ipCompanyTo.

    {custom\rec_key.i bstax-group}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE std-codeCopy:
  DEFINE BUFFER bstd-code FOR std-code.

  RUN showMsg ('std-code',NO).
  IF CAN-FIND(FIRST std-code WHERE std-code.company EQ ipCompanyTo) THEN
  FOR EACH std-code EXCLUSIVE-LOCK WHERE std-code.company EQ ipCompanyTo:
    DELETE std-code.
  END.
  FOR EACH std-code NO-LOCK WHERE std-code.company EQ ipCompanyFrom:
    CREATE bstd-code.
    BUFFER-COPY std-code EXCEPT company rec_key TO bstd-code
      ASSIGN bstd-code.company = ipCompanyTo.

    {custom\rec_key.i bstd-code}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE styleCopy:
  DEFINE BUFFER bstyle FOR style.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('style',NO).
  IF CAN-FIND(FIRST style WHERE style.company EQ ipCompanyTo) THEN
  FOR EACH style EXCLUSIVE-LOCK WHERE style.company EQ ipCompanyTo:
    DELETE style.
  END.
  FOR EACH style NO-LOCK WHERE style.company EQ ipCompanyFrom:
    CREATE bstyle.
    BUFFER-COPY style EXCEPT company rec_key TO bstyle
      ASSIGN bstyle.company = ipCompanyTo.

    {custom\rec_key.i bstyle}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ style.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bstyle.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE sys-ctrlCopy:
  DEFINE BUFFER bsys-ctrl FOR sys-ctrl.

  RUN showMsg ('sys-ctrl',NO).
  IF CAN-FIND(FIRST sys-ctrl WHERE sys-ctrl.company EQ ipCompanyTo) THEN
  FOR EACH sys-ctrl EXCLUSIVE-LOCK WHERE sys-ctrl.company EQ ipCompanyTo:
    DELETE sys-ctrl.
  END.
  FOR EACH sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ ipCompanyFrom:
    CREATE bsys-ctrl.
    BUFFER-COPY sys-ctrl EXCEPT company rec_key TO bsys-ctrl
      ASSIGN bsys-ctrl.company = ipCompanyTo.

    {custom\rec_key.i bsys-ctrl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE termsCopy:
  DEFINE BUFFER bterms FOR terms.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('terms',NO).
  IF CAN-FIND(FIRST terms WHERE terms.company EQ ipCompanyTo) THEN
  FOR EACH terms EXCLUSIVE-LOCK WHERE terms.company EQ ipCompanyTo:
    DELETE terms.
  END.
  FOR EACH terms NO-LOCK WHERE terms.company EQ ipCompanyFrom:
    CREATE bterms.
    BUFFER-COPY terms EXCEPT company rec_key TO bterms
      ASSIGN bterms.company = ipCompanyTo.

    {custom\rec_key.i bterms}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ terms.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bterms.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE terrCopy:
  DEFINE BUFFER bterr FOR terr.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('terr',NO).
  IF CAN-FIND(FIRST terr WHERE terr.company EQ ipCompanyTo) THEN
  FOR EACH terr EXCLUSIVE-LOCK WHERE terr.company EQ ipCompanyTo:
    DELETE terr.
  END.
  FOR EACH terr NO-LOCK WHERE terr.company EQ ipCompanyFrom:
    CREATE bterr.
    BUFFER-COPY terr EXCEPT company rec_key TO bterr
      ASSIGN bterr.company = ipCompanyTo.

    {custom\rec_key.i bterr}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ terr.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bterr.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE test-redCopy:
  DEFINE BUFFER btest-red FOR test-red.

  RUN showMsg ('test-red',NO).
  IF CAN-FIND(FIRST test-red WHERE test-red.company EQ ipCompanyTo) THEN
  FOR EACH test-red EXCLUSIVE-LOCK WHERE test-red.company EQ ipCompanyTo:
    DELETE test-red.
  END.
  FOR EACH test-red NO-LOCK WHERE test-red.company EQ ipCompanyFrom:
    CREATE btest-red.
    BUFFER-COPY test-red EXCEPT company rec_key TO btest-red
      ASSIGN btest-red.company = ipCompanyTo.

    {custom\rec_key.i btest-red}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE usercompCopy:
  DEFINE BUFFER busercomp FOR usercomp.

  RUN showMsg ('usercomp',NO).
  IF CAN-FIND(FIRST usercomp WHERE usercomp.company EQ ipCompanyTo) THEN
  FOR EACH usercomp EXCLUSIVE-LOCK WHERE usercomp.company EQ ipCompanyTo:
    DELETE usercomp.
  END.
  FOR EACH usercomp NO-LOCK WHERE usercomp.company EQ ipCompanyFrom:
    CREATE busercomp.
    BUFFER-COPY usercomp EXCEPT company rec_key TO busercomp
      ASSIGN busercomp.company = ipCompanyTo.

    {custom\rec_key.i busercomp}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE vendCopy:
  DEFINE BUFFER bvend FOR vend.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('vend',NO).
  IF CAN-FIND(FIRST vend WHERE vend.company EQ ipCompanyTo) THEN
  FOR EACH vend EXCLUSIVE-LOCK WHERE vend.company EQ ipCompanyTo:
    DELETE vend.
  END.
  FOR EACH vend NO-LOCK WHERE vend.company EQ ipCompanyFrom:
    CREATE bvend.
    BUFFER-COPY vend EXCEPT company rec_key TO bvend
      ASSIGN bvend.company = ipCompanyTo.

    {custom\rec_key.i bvend}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ vend.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bvend.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ventypeCopy:
  DEFINE BUFFER bventype FOR ventype.
  DEFINE BUFFER bnotes FOR notes.

  RUN showMsg ('ventype',NO).
  IF CAN-FIND(FIRST ventype WHERE ventype.company EQ ipCompanyTo) THEN
  FOR EACH ventype EXCLUSIVE-LOCK WHERE ventype.company EQ ipCompanyTo:
    DELETE ventype.
  END.
  FOR EACH ventype NO-LOCK WHERE ventype.company EQ ipCompanyFrom:
    CREATE bventype.
    BUFFER-COPY ventype EXCEPT company rec_key TO bventype
      ASSIGN bventype.company = ipCompanyTo.

    {custom\rec_key.i bventype}

    FOR EACH notes NO-LOCK
        WHERE notes.rec_key EQ ventype.rec_key:
        
        CREATE bnotes.
        BUFFER-COPY notes EXCEPT rec_key TO bnotes
            ASSIGN 
               bnotes.rec_key = bventype.rec_key.

    END.

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

/* transaction files copy */
PROCEDURE ap-chk:
  DEFINE BUFFER bap-chk FOR ap-chk.

  RUN showMsg ('ap-chk',NO).
  IF CAN-FIND(FIRST ap-chk WHERE ap-chk.company EQ ipCompanyTo) THEN
  FOR EACH ap-chk EXCLUSIVE-LOCK WHERE ap-chk.company EQ ipCompanyTo:
    DELETE ap-chk.
  END.
  FOR EACH ap-chk NO-LOCK WHERE ap-chk.company EQ ipCompanyFrom:
    CREATE bap-chk.
    BUFFER-COPY ap-chk EXCEPT company rec_key TO bap-chk
      ASSIGN bap-chk.company = ipCompanyTo.

    {custom\rec_key.i bap-chk}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ap-dis:
  DEFINE BUFFER bap-dis FOR ap-dis.

  RUN showMsg ('ap-dis',NO).
  IF CAN-FIND(FIRST ap-dis WHERE ap-dis.company EQ ipCompanyTo) THEN
  FOR EACH ap-dis EXCLUSIVE-LOCK WHERE ap-dis.company EQ ipCompanyTo:
    DELETE ap-dis.
  END.
  FOR EACH ap-dis NO-LOCK WHERE ap-dis.company EQ ipCompanyFrom:
    CREATE bap-dis.
    BUFFER-COPY ap-dis EXCEPT company rec_key TO bap-dis
      ASSIGN bap-dis.company = ipCompanyTo.

    {custom\rec_key.i bap-dis}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.


PROCEDURE ap-disl:
  DEFINE BUFFER bap-disl FOR ap-disl.

  RUN showMsg ('ap-disl',NO).
  IF CAN-FIND(FIRST ap-disl WHERE ap-disl.company EQ ipCompanyTo) THEN
  FOR EACH ap-disl EXCLUSIVE-LOCK WHERE ap-disl.company EQ ipCompanyTo:
    DELETE ap-disl.
  END.
  FOR EACH ap-disl NO-LOCK WHERE ap-disl.company EQ ipCompanyFrom:
    CREATE bap-disl.
    BUFFER-COPY ap-disl EXCEPT company rec_key TO bap-disl
      ASSIGN bap-disl.company = ipCompanyTo.

    {custom\rec_key.i bap-disl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ap-inv:
  DEFINE BUFFER bap-inv FOR ap-inv.

  RUN showMsg ('ap-inv',NO).
  IF CAN-FIND(FIRST ap-inv WHERE ap-inv.company EQ ipCompanyTo) THEN
  FOR EACH ap-inv EXCLUSIVE-LOCK WHERE ap-inv.company EQ ipCompanyTo:
    DELETE ap-inv.
  END.
  FOR EACH ap-inv NO-LOCK WHERE ap-inv.company EQ ipCompanyFrom:
    CREATE bap-inv.
    BUFFER-COPY ap-inv EXCEPT company rec_key TO bap-inv
      ASSIGN bap-inv.company = ipCompanyTo.

    {custom\rec_key.i bap-inv}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ap-invl:
  DEFINE BUFFER bap-invl FOR ap-invl.

  RUN showMsg ('ap-invl',NO).
  IF CAN-FIND(FIRST ap-invl WHERE ap-invl.company EQ ipCompanyTo) THEN
  FOR EACH ap-invl EXCLUSIVE-LOCK WHERE ap-invl.company EQ ipCompanyTo:
    DELETE ap-invl.
  END.
  FOR EACH ap-invl NO-LOCK WHERE ap-invl.company EQ ipCompanyFrom:
    CREATE bap-invl.
    BUFFER-COPY ap-invl EXCEPT company rec_key TO bap-invl
      ASSIGN bap-invl.company = ipCompanyTo.

    {custom\rec_key.i bap-invl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ap-invlr:
  DEFINE BUFFER bap-invlr FOR ap-invlr.

  RUN showMsg ('ap-invlr',NO).
  IF CAN-FIND(FIRST ap-invlr WHERE ap-invlr.company EQ ipCompanyTo) THEN
  FOR EACH ap-invlr EXCLUSIVE-LOCK WHERE ap-invlr.company EQ ipCompanyTo:
    DELETE ap-invlr.
  END.
  FOR EACH ap-invlr NO-LOCK WHERE ap-invlr.company EQ ipCompanyFrom:
    CREATE bap-invlr.
    BUFFER-COPY ap-invlr EXCEPT company rec_key TO bap-invlr
      ASSIGN bap-invlr.company = ipCompanyTo.

    {custom\rec_key.i bap-invlr}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ap-pay:
  DEFINE BUFFER bap-pay FOR ap-pay.

  RUN showMsg ('ap-pay',NO).
  IF CAN-FIND(FIRST ap-pay WHERE ap-pay.company EQ ipCompanyTo) THEN
  FOR EACH ap-pay EXCLUSIVE-LOCK WHERE ap-pay.company EQ ipCompanyTo:
    DELETE ap-pay.
  END.
  FOR EACH ap-pay NO-LOCK WHERE ap-pay.company EQ ipCompanyFrom:
    CREATE bap-pay.
    BUFFER-COPY ap-pay EXCEPT company rec_key TO bap-pay
      ASSIGN bap-pay.company = ipCompanyTo.

    {custom\rec_key.i bap-pay}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ap-sel:
  DEFINE BUFFER bap-sel FOR ap-sel.

  RUN showMsg ('ap-sel',NO).
  IF CAN-FIND(FIRST ap-sel WHERE ap-sel.company EQ ipCompanyTo) THEN
  FOR EACH ap-sel EXCLUSIVE-LOCK WHERE ap-sel.company EQ ipCompanyTo:
    DELETE ap-sel.
  END.
  FOR EACH ap-sel NO-LOCK WHERE ap-sel.company EQ ipCompanyFrom:
    CREATE bap-sel.
    BUFFER-COPY ap-sel EXCEPT company rec_key TO bap-sel
      ASSIGN bap-sel.company = ipCompanyTo.

    {custom\rec_key.i bap-sel}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE aphist:
  DEFINE BUFFER baphist FOR aphist.

  RUN showMsg ('aphist',NO).
  IF CAN-FIND(FIRST aphist WHERE aphist.company EQ ipCompanyTo) THEN
  FOR EACH aphist EXCLUSIVE-LOCK WHERE aphist.company EQ ipCompanyTo:
    DELETE aphist.
  END.
  FOR EACH aphist NO-LOCK WHERE aphist.company EQ ipCompanyFrom:
    CREATE baphist.
    BUFFER-COPY aphist EXCEPT company rec_key TO baphist
      ASSIGN baphist.company = ipCompanyTo.

    {custom\rec_key.i baphist}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ar-cash:
  DEFINE BUFFER bar-cash FOR ar-cash.

  RUN showMsg ('ar-cash',NO).
  IF CAN-FIND(FIRST ar-cash WHERE ar-cash.company EQ ipCompanyTo) THEN
  FOR EACH ar-cash EXCLUSIVE-LOCK WHERE ar-cash.company EQ ipCompanyTo:
    DELETE ar-cash.
  END.
  FOR EACH ar-cash NO-LOCK WHERE ar-cash.company EQ ipCompanyFrom:
    CREATE bar-cash.
    BUFFER-COPY ar-cash EXCEPT company rec_key TO bar-cash
      ASSIGN bar-cash.company = ipCompanyTo.

    {custom\rec_key.i bar-cash}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ar-cashl:
  DEFINE BUFFER bar-cashl FOR ar-cashl.

  RUN showMsg ('ar-cashl',NO).
  IF CAN-FIND(FIRST ar-cashl WHERE ar-cashl.company EQ ipCompanyTo) THEN
  FOR EACH ar-cashl EXCLUSIVE-LOCK WHERE ar-cashl.company EQ ipCompanyTo:
    DELETE ar-cashl.
  END.
  FOR EACH ar-cashl NO-LOCK WHERE ar-cashl.company EQ ipCompanyFrom:
    CREATE bar-cashl.
    BUFFER-COPY ar-cashl EXCEPT company rec_key TO bar-cashl
      ASSIGN bar-cashl.company = ipCompanyTo.

    {custom\rec_key.i bar-cashl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ar-inv:
  DEFINE BUFFER bar-inv FOR ar-inv.

  RUN showMsg ('ar-inv',NO).
  IF CAN-FIND(FIRST ar-inv WHERE ar-inv.company EQ ipCompanyTo) THEN
  FOR EACH ar-inv EXCLUSIVE-LOCK WHERE ar-inv.company EQ ipCompanyTo:
    DELETE ar-inv.
  END.
  FOR EACH ar-inv NO-LOCK WHERE ar-inv.company EQ ipCompanyFrom:
    CREATE bar-inv.
    BUFFER-COPY ar-inv EXCEPT company rec_key TO bar-inv
      ASSIGN bar-inv.company = ipCompanyTo.

    {custom\rec_key.i bar-inv}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ar-invl:
  DEFINE BUFFER bar-invl FOR ar-invl.

  RUN showMsg ('ar-invl',NO).
  IF CAN-FIND(FIRST ar-invl WHERE ar-invl.company EQ ipCompanyTo) THEN
  FOR EACH ar-invl EXCLUSIVE-LOCK WHERE ar-invl.company EQ ipCompanyTo:
    DELETE ar-invl.
  END.
  FOR EACH ar-invl NO-LOCK WHERE ar-invl.company EQ ipCompanyFrom:
    CREATE bar-invl.
    BUFFER-COPY ar-invl EXCEPT company rec_key TO bar-invl
      ASSIGN bar-invl.company = ipCompanyTo.
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ar-invm:
  DEFINE BUFFER bar-invm FOR ar-invm.

  RUN showMsg ('ar-invm',NO).
  IF CAN-FIND(FIRST ar-invm WHERE ar-invm.company EQ ipCompanyTo) THEN
  FOR EACH ar-invm EXCLUSIVE-LOCK WHERE ar-invm.company EQ ipCompanyTo:
    DELETE ar-invm.
  END.
  FOR EACH ar-invm NO-LOCK WHERE ar-invm.company EQ ipCompanyFrom:
    CREATE bar-invm.
    BUFFER-COPY ar-invm EXCEPT company rec_key TO bar-invm
      ASSIGN bar-invm.company = ipCompanyTo.

    {custom\rec_key.i bar-invm}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ar-ledger:
  DEFINE BUFFER bar-ledger FOR ar-ledger.

  RUN showMsg ('ar-ledger',NO).
  IF CAN-FIND(FIRST ar-ledger WHERE ar-ledger.company EQ ipCompanyTo) THEN
  FOR EACH ar-ledger EXCLUSIVE-LOCK WHERE ar-ledger.company EQ ipCompanyTo:
    DELETE ar-ledger.
  END.
  FOR EACH ar-ledger NO-LOCK WHERE ar-ledger.company EQ ipCompanyFrom:
    CREATE bar-ledger.
    BUFFER-COPY ar-ledger EXCEPT company rec_key TO bar-ledger
      ASSIGN bar-ledger.company = ipCompanyTo.

    {custom\rec_key.i bar-ledger}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ar-mcash:
  DEFINE BUFFER bar-mcash FOR ar-mcash.

  RUN showMsg ('ar-mcash',NO).
  IF CAN-FIND(FIRST ar-mcash WHERE ar-mcash.company EQ ipCompanyTo) THEN
  FOR EACH ar-mcash EXCLUSIVE-LOCK WHERE ar-mcash.company EQ ipCompanyTo:
    DELETE ar-mcash.
  END.
  FOR EACH ar-mcash NO-LOCK WHERE ar-mcash.company EQ ipCompanyFrom:
    CREATE bar-mcash.
    BUFFER-COPY ar-mcash EXCEPT company rec_key TO bar-mcash
      ASSIGN bar-mcash.company = ipCompanyTo.

    {custom\rec_key.i bar-mcash}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE asinotes:
  DEFINE BUFFER basinotes FOR asinotes.

  RUN showMsg ('asinotes',NO).
  IF CAN-FIND(FIRST asinotes WHERE asinotes.company EQ ipCompanyTo) THEN
  FOR EACH asinotes EXCLUSIVE-LOCK WHERE asinotes.company EQ ipCompanyTo:
    DELETE asinotes.
  END.
  FOR EACH asinotes NO-LOCK WHERE asinotes.company EQ ipCompanyFrom:
    CREATE basinotes.
    BUFFER-COPY asinotes EXCEPT company rec_key TO basinotes
      ASSIGN basinotes.company = ipCompanyTo.

    {custom\rec_key.i basinotes}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE box-design-hdr:
  DEFINE BUFFER bbox-design-hdr FOR box-design-hdr.

  RUN showMsg ('box-design-hdr',NO).
  IF CAN-FIND(FIRST box-design-hdr WHERE box-design-hdr.company EQ ipCompanyTo) THEN
  FOR EACH box-design-hdr EXCLUSIVE-LOCK WHERE box-design-hdr.company EQ ipCompanyTo:
    DELETE box-design-hdr.
  END.
  FOR EACH box-design-hdr NO-LOCK WHERE box-design-hdr.company EQ ipCompanyFrom:
    CREATE bbox-design-hdr.
    BUFFER-COPY box-design-hdr EXCEPT company rec_key TO bbox-design-hdr
      ASSIGN bbox-design-hdr.company = ipCompanyTo.

    {custom\rec_key.i bbox-design-hdr}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE box-design-line:
  DEFINE BUFFER bbox-design-line FOR box-design-line.

  RUN showMsg ('box-design-line',NO).
  IF CAN-FIND(FIRST box-design-line WHERE box-design-line.company EQ ipCompanyTo) THEN
  FOR EACH box-design-line EXCLUSIVE-LOCK WHERE box-design-line.company EQ ipCompanyTo:
    DELETE box-design-line.
  END.
  FOR EACH box-design-line NO-LOCK WHERE box-design-line.company EQ ipCompanyFrom:
    CREATE bbox-design-line.
    BUFFER-COPY box-design-line EXCEPT company rec_key TO bbox-design-line
      ASSIGN bbox-design-line.company = ipCompanyTo.

    {custom\rec_key.i bbox-design-line}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE eb:
  DEFINE BUFFER beb FOR eb.

  RUN showMsg ('eb',NO).
  IF CAN-FIND(FIRST eb WHERE eb.company EQ ipCompanyTo) THEN
  FOR EACH eb EXCLUSIVE-LOCK WHERE eb.company EQ ipCompanyTo:
    DELETE eb.
  END.
  FOR EACH eb NO-LOCK WHERE eb.company EQ ipCompanyFrom:
    CREATE beb.
    BUFFER-COPY eb EXCEPT company rec_key TO beb
      ASSIGN beb.company = ipCompanyTo.

    {custom\rec_key.i beb}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ef:
  DEFINE BUFFER bef FOR ef.

  RUN showMsg ('ef',NO).
  IF CAN-FIND(FIRST ef WHERE ef.company EQ ipCompanyTo) THEN
  FOR EACH ef EXCLUSIVE-LOCK WHERE ef.company EQ ipCompanyTo:
    DELETE ef.
  END.
  FOR EACH ef NO-LOCK WHERE ef.company EQ ipCompanyFrom:
    CREATE bef.
    BUFFER-COPY ef EXCEPT company rec_key TO bef
      ASSIGN bef.company = ipCompanyTo.

    {custom\rec_key.i bef}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE ef-nsh:
  DEFINE BUFFER bef-nsh FOR ef-nsh.

  RUN showMsg ('ef-nsh',NO).
  IF CAN-FIND(FIRST ef-nsh WHERE ef-nsh.company EQ ipCompanyTo) THEN
  FOR EACH ef-nsh EXCLUSIVE-LOCK WHERE ef-nsh.company EQ ipCompanyTo:
    DELETE ef-nsh.
  END.
  FOR EACH ef-nsh NO-LOCK WHERE ef-nsh.company EQ ipCompanyFrom:
    CREATE bef-nsh.
    BUFFER-COPY ef-nsh EXCEPT company rec_key TO bef-nsh
      ASSIGN bef-nsh.company = ipCompanyTo.

    {custom\rec_key.i bef-nsh}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE est:
  DEFINE BUFFER best FOR est.

  RUN showMsg ('est',NO).
  IF CAN-FIND(FIRST est WHERE est.company EQ ipCompanyTo) THEN
  FOR EACH est EXCLUSIVE-LOCK WHERE est.company EQ ipCompanyTo:
    DELETE est.
  END.
  FOR EACH est NO-LOCK WHERE est.company EQ ipCompanyFrom:
    CREATE best.
    BUFFER-COPY est EXCEPT company rec_key TO best
      ASSIGN best.company = ipCompanyTo.

    {custom\rec_key.i best}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE est-flm:
  DEFINE BUFFER best-flm FOR est-flm.

  RUN showMsg ('est-flm',NO).
  IF CAN-FIND(FIRST est-flm WHERE est-flm.company EQ ipCompanyTo) THEN
  FOR EACH est-flm EXCLUSIVE-LOCK WHERE est-flm.company EQ ipCompanyTo:
    DELETE est-flm.
  END.
  FOR EACH est-flm NO-LOCK WHERE est-flm.company EQ ipCompanyFrom:
    CREATE best-flm.
    BUFFER-COPY est-flm EXCEPT company rec_key TO best-flm
      ASSIGN best-flm.company = ipCompanyTo.

    {custom\rec_key.i best-flm}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE est-inst:
  DEFINE BUFFER best-inst FOR est-inst.

  RUN showMsg ('est-inst',NO).
  IF CAN-FIND(FIRST est-inst WHERE est-inst.company EQ ipCompanyTo) THEN
  FOR EACH est-inst EXCLUSIVE-LOCK WHERE est-inst.company EQ ipCompanyTo:
    DELETE est-inst.
  END.
  FOR EACH est-inst NO-LOCK WHERE est-inst.company EQ ipCompanyFrom:
    CREATE best-inst.
    BUFFER-COPY est-inst EXCEPT company rec_key TO best-inst
      ASSIGN best-inst.company = ipCompanyTo.

    {custom\rec_key.i best-inst}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE est-op:
  DEFINE BUFFER best-op FOR est-op.

  RUN showMsg ('est-op',NO).
  IF CAN-FIND(FIRST est-op WHERE est-op.company EQ ipCompanyTo) THEN
  FOR EACH est-op EXCLUSIVE-LOCK WHERE est-op.company EQ ipCompanyTo:
    DELETE est-op.
  END.
  FOR EACH est-op NO-LOCK WHERE est-op.company EQ ipCompanyFrom:
    CREATE best-op.
    BUFFER-COPY est-op EXCEPT company rec_key TO best-op
      ASSIGN best-op.company = ipCompanyTo.

    {custom\rec_key.i best-op}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE est-pf:
  DEFINE BUFFER best-pf FOR est-pf.

  RUN showMsg ('est-pf',NO).
  IF CAN-FIND(FIRST est-pf WHERE est-pf.company EQ ipCompanyTo) THEN
  FOR EACH est-pf EXCLUSIVE-LOCK WHERE est-pf.company EQ ipCompanyTo:
    DELETE est-pf.
  END.
  FOR EACH est-pf NO-LOCK WHERE est-pf.company EQ ipCompanyFrom:
    CREATE best-pf.
    BUFFER-COPY est-pf EXCEPT company TO best-pf
      ASSIGN best-pf.company = ipCompanyTo.          
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE est-prep:
  DEFINE BUFFER best-prep FOR est-prep.

  RUN showMsg ('est-prep',NO).
  IF CAN-FIND(FIRST est-prep WHERE est-prep.company EQ ipCompanyTo) THEN
  FOR EACH est-prep EXCLUSIVE-LOCK WHERE est-prep.company EQ ipCompanyTo:
    DELETE est-prep.
  END.
  FOR EACH est-prep NO-LOCK WHERE est-prep.company EQ ipCompanyFrom:
    CREATE best-prep.
    BUFFER-COPY est-prep EXCEPT company rec_key TO best-prep
      ASSIGN best-prep.company = ipCompanyTo.

    {custom\rec_key.i best-prep}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE est-qty:
  DEFINE BUFFER best-qty FOR est-qty.

  RUN showMsg ('est-qty',NO).
  IF CAN-FIND(FIRST est-qty WHERE est-qty.company EQ ipCompanyTo) THEN
  FOR EACH est-qty EXCLUSIVE-LOCK WHERE est-qty.company EQ ipCompanyTo:
    DELETE est-qty.
  END.
  FOR EACH est-qty NO-LOCK WHERE est-qty.company EQ ipCompanyFrom:
    CREATE best-qty.
    BUFFER-COPY est-qty EXCEPT company rec_key TO best-qty
      ASSIGN best-qty.company = ipCompanyTo.

    {custom\rec_key.i best-qty}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE est-summ:
  DEFINE BUFFER best-summ FOR est-summ.

  RUN showMsg ('est-summ',NO).
  IF CAN-FIND(FIRST est-summ WHERE est-summ.company EQ ipCompanyTo) THEN
  FOR EACH est-summ EXCLUSIVE-LOCK WHERE est-summ.company EQ ipCompanyTo:
    DELETE est-summ.
  END.
  FOR EACH est-summ NO-LOCK WHERE est-summ.company EQ ipCompanyFrom:
    CREATE best-summ.
    BUFFER-COPY est-summ EXCEPT company rec_key TO best-summ
      ASSIGN best-summ.company = ipCompanyTo.

    {custom\rec_key.i best-summ}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fg-hist:
  DEFINE BUFFER bfg-hist FOR fg-hist.

  RUN showMsg ('fg-hist',NO).
  IF CAN-FIND(FIRST fg-hist WHERE fg-hist.company EQ ipCompanyTo) THEN
  FOR EACH fg-hist EXCLUSIVE-LOCK WHERE fg-hist.company EQ ipCompanyTo:
    DELETE fg-hist.
  END.
  FOR EACH fg-hist NO-LOCK WHERE fg-hist.company EQ ipCompanyFrom:
    CREATE bfg-hist.
    BUFFER-COPY fg-hist EXCEPT company rec_key TO bfg-hist
      ASSIGN bfg-hist.company = ipCompanyTo.

    {custom\rec_key.i bfg-hist}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fg-rcpth:
  DEFINE BUFFER bfg-rcpth FOR fg-rcpth.

  RUN showMsg ('fg-rcpth',NO).
  IF CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.company EQ ipCompanyTo) THEN
  FOR EACH fg-rcpth EXCLUSIVE-LOCK WHERE fg-rcpth.company EQ ipCompanyTo:
    DELETE fg-rcpth.
  END.
  FOR EACH fg-rcpth NO-LOCK WHERE fg-rcpth.company EQ ipCompanyFrom:
    CREATE bfg-rcpth.
    BUFFER-COPY fg-rcpth EXCEPT company rec_key TO bfg-rcpth
      ASSIGN bfg-rcpth.company = ipCompanyTo.

    {custom\rec_key.i bfg-rcpth}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fg-rcpts:
  DEFINE BUFFER bfg-rcpts FOR fg-rcpts.

  RUN showMsg ('fg-rcpts',NO).
  IF CAN-FIND(FIRST fg-rcpts WHERE fg-rcpts.company EQ ipCompanyTo) THEN
  FOR EACH fg-rcpts EXCLUSIVE-LOCK WHERE fg-rcpts.company EQ ipCompanyTo:
    DELETE fg-rcpts.
  END.
  FOR EACH fg-rcpts NO-LOCK WHERE fg-rcpts.company EQ ipCompanyFrom:
    CREATE bfg-rcpts.
    BUFFER-COPY fg-rcpts EXCEPT company rec_key TO bfg-rcpts
      ASSIGN bfg-rcpts.company = ipCompanyTo.

    {custom\rec_key.i bfg-rcpts}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fg-rctd:
  DEFINE BUFFER bfg-rctd FOR fg-rctd.

  RUN showMsg ('fg-rctd',NO).
  IF CAN-FIND(FIRST fg-rctd WHERE fg-rctd.company EQ ipCompanyTo) THEN
  FOR EACH fg-rctd EXCLUSIVE-LOCK WHERE fg-rctd.company EQ ipCompanyTo:
    DELETE fg-rctd.
  END.
  FOR EACH fg-rctd NO-LOCK WHERE fg-rctd.company EQ ipCompanyFrom:
    CREATE bfg-rctd.
    BUFFER-COPY fg-rctd EXCEPT company rec_key TO bfg-rctd
      ASSIGN bfg-rctd.company = ipCompanyTo.

    {custom\rec_key.i bfg-rctd}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fg-rdtl:
  DEFINE BUFFER bfg-rdtl FOR fg-rdtl.

  RUN showMsg ('fg-rdtl',NO).
  IF CAN-FIND(FIRST fg-rdtl WHERE fg-rdtl.company EQ ipCompanyTo) THEN
  FOR EACH fg-rdtl EXCLUSIVE-LOCK WHERE fg-rdtl.company EQ ipCompanyTo:
    DELETE fg-rdtl.
  END.
  FOR EACH fg-rdtl NO-LOCK WHERE fg-rdtl.company EQ ipCompanyFrom:
    CREATE bfg-rdtl.
    BUFFER-COPY fg-rdtl EXCEPT company rec_key TO bfg-rdtl
      ASSIGN bfg-rdtl.company = ipCompanyTo.

    {custom\rec_key.i bfg-rdtl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE fg-rdtlh:
  DEFINE BUFFER bfg-rdtlh FOR fg-rdtlh.

  RUN showMsg ('fg-rdtlh',NO).
  IF CAN-FIND(FIRST fg-rdtlh WHERE fg-rdtlh.company EQ ipCompanyTo) THEN
  FOR EACH fg-rdtlh EXCLUSIVE-LOCK WHERE fg-rdtlh.company EQ ipCompanyTo:
    DELETE fg-rdtlh.
  END.
  FOR EACH fg-rdtlh NO-LOCK WHERE fg-rdtlh.company EQ ipCompanyFrom:
    CREATE bfg-rdtlh.
    BUFFER-COPY fg-rdtlh EXCEPT company rec_key TO bfg-rdtlh
      ASSIGN bfg-rdtlh.company = ipCompanyTo.

    {custom\rec_key.i bfg-rdtlh}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE gl-freq:
  DEFINE BUFFER bgl-freq FOR gl-freq.

  RUN showMsg ('gl-freq',NO).
  IF CAN-FIND(FIRST gl-freq WHERE gl-freq.company EQ ipCompanyTo) THEN
  FOR EACH gl-freq EXCLUSIVE-LOCK WHERE gl-freq.company EQ ipCompanyTo:
    DELETE gl-freq.
  END.
  FOR EACH gl-freq NO-LOCK WHERE gl-freq.company EQ ipCompanyFrom:
    CREATE bgl-freq.
    BUFFER-COPY gl-freq EXCEPT company rec_key TO bgl-freq
      ASSIGN bgl-freq.company = ipCompanyTo.

    {custom\rec_key.i bgl-freq}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE gl-jrn:
  DEFINE BUFFER bgl-jrn FOR gl-jrn.

  RUN showMsg ('gl-jrn',NO).
  IF CAN-FIND(FIRST gl-jrn WHERE gl-jrn.company EQ ipCompanyTo) THEN
  FOR EACH gl-jrn EXCLUSIVE-LOCK WHERE gl-jrn.company EQ ipCompanyTo:
    DELETE gl-jrn.
  END.
  FOR EACH gl-jrn NO-LOCK WHERE gl-jrn.company EQ ipCompanyFrom:
    CREATE bgl-jrn.
    BUFFER-COPY gl-jrn EXCEPT company rec_key TO bgl-jrn
      ASSIGN bgl-jrn.company = ipCompanyTo.

    {custom\rec_key.i bgl-jrn}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE glhist:
  DEFINE BUFFER bglhist FOR glhist.

  RUN showMsg ('glhist',NO).
  IF CAN-FIND(FIRST glhist WHERE glhist.company EQ ipCompanyTo) THEN
  FOR EACH glhist EXCLUSIVE-LOCK WHERE glhist.company EQ ipCompanyTo:
    DELETE glhist.
  END.
  FOR EACH glhist NO-LOCK WHERE glhist.company EQ ipCompanyFrom:
    CREATE bglhist.
    BUFFER-COPY glhist EXCEPT company rec_key TO bglhist
      ASSIGN bglhist.company = ipCompanyTo.

    {custom\rec_key.i bglhist}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE gltrans:
  DEFINE BUFFER bgltrans FOR gltrans.

  RUN showMsg ('gltrans',NO).
  IF CAN-FIND(FIRST gltrans WHERE gltrans.company EQ ipCompanyTo) THEN
  FOR EACH gltrans EXCLUSIVE-LOCK WHERE gltrans.company EQ ipCompanyTo:
    DELETE gltrans.
  END.
  FOR EACH gltrans NO-LOCK WHERE gltrans.company EQ ipCompanyFrom:
    CREATE bgltrans.
    BUFFER-COPY gltrans EXCEPT company rec_key TO bgltrans
      ASSIGN bgltrans.company = ipCompanyTo.

    {custom\rec_key.i bgltrans}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE inv-head:
  DEFINE BUFFER binv-head FOR inv-head.

  RUN showMsg ('inv-head',NO).
  IF CAN-FIND(FIRST inv-head WHERE inv-head.company EQ ipCompanyTo) THEN
  FOR EACH inv-head EXCLUSIVE-LOCK WHERE inv-head.company EQ ipCompanyTo:
    DELETE inv-head.
  END.
  FOR EACH inv-head NO-LOCK WHERE inv-head.company EQ ipCompanyFrom:
    CREATE binv-head.
    BUFFER-COPY inv-head EXCEPT company rec_key TO binv-head
      ASSIGN binv-head.company = ipCompanyTo.

    {custom\rec_key.i binv-head}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE inv-line:
  DEFINE BUFFER binv-line FOR inv-line.

  RUN showMsg ('inv-line',NO).
  IF CAN-FIND(FIRST inv-line WHERE inv-line.company EQ ipCompanyTo) THEN
  FOR EACH inv-line EXCLUSIVE-LOCK WHERE inv-line.company EQ ipCompanyTo:
    DELETE inv-line.
  END.
  FOR EACH inv-line NO-LOCK WHERE inv-line.company EQ ipCompanyFrom:
    CREATE binv-line.
    BUFFER-COPY inv-line EXCEPT company rec_key TO binv-line
      ASSIGN binv-line.company = ipCompanyTo.

    {custom\rec_key.i binv-line}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE inv-misc:
  DEFINE BUFFER binv-misc FOR inv-misc.

  RUN showMsg ('inv-misc',NO).
  IF CAN-FIND(FIRST inv-misc WHERE inv-misc.company EQ ipCompanyTo) THEN
  FOR EACH inv-misc EXCLUSIVE-LOCK WHERE inv-misc.company EQ ipCompanyTo:
    DELETE inv-misc.
  END.
  FOR EACH inv-misc NO-LOCK WHERE inv-misc.company EQ ipCompanyFrom:
    CREATE binv-misc.
    BUFFER-COPY inv-misc EXCEPT company rec_key TO binv-misc
      ASSIGN binv-misc.company = ipCompanyTo.

    {custom\rec_key.i binv-misc}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE job:
  DEFINE BUFFER bjob FOR job.

  RUN showMsg ('job',NO).
  IF CAN-FIND(FIRST job WHERE job.company EQ ipCompanyTo) THEN
  FOR EACH job EXCLUSIVE-LOCK WHERE job.company EQ ipCompanyTo:
    DELETE job.
  END.
  FOR EACH job NO-LOCK WHERE job.company EQ ipCompanyFrom:
    CREATE bjob.
    BUFFER-COPY job EXCEPT company rec_key TO bjob
      ASSIGN bjob.company = ipCompanyTo.

    {custom\rec_key.i bjob}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE job-all:
  DEFINE BUFFER bjob-all FOR job-all.

  RUN showMsg ('job-all',NO).
  IF CAN-FIND(FIRST job-all WHERE job-all.company EQ ipCompanyTo) THEN
  FOR EACH job-all EXCLUSIVE-LOCK WHERE job-all.company EQ ipCompanyTo:
    DELETE job-all.
  END.
  FOR EACH job-all NO-LOCK WHERE job-all.company EQ ipCompanyFrom:
    CREATE bjob-all.
    BUFFER-COPY job-all EXCEPT company rec_key TO bjob-all
      ASSIGN bjob-all.company = ipCompanyTo.

    {custom\rec_key.i bjob-all}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE job-brd:
  DEFINE BUFFER bjob-brd FOR job-brd.

  RUN showMsg ('job-brd',NO).
  IF CAN-FIND(FIRST job-brd WHERE job-brd.company EQ ipCompanyTo) THEN
  FOR EACH job-brd EXCLUSIVE-LOCK WHERE job-brd.company EQ ipCompanyTo:
    DELETE job-brd.
  END.
  FOR EACH job-brd NO-LOCK WHERE job-brd.company EQ ipCompanyFrom:
    CREATE bjob-brd.
    BUFFER-COPY job-brd EXCEPT company rec_key TO bjob-brd
      ASSIGN bjob-brd.company = ipCompanyTo.

    {custom\rec_key.i bjob-brd}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE job-hdr:
  DEFINE BUFFER bjob-hdr FOR job-hdr.

  RUN showMsg ('job-hdr',NO).
  IF CAN-FIND(FIRST job-hdr WHERE job-hdr.company EQ ipCompanyTo) THEN
  FOR EACH job-hdr EXCLUSIVE-LOCK WHERE job-hdr.company EQ ipCompanyTo:
    DELETE job-hdr.
  END.
  FOR EACH job-hdr NO-LOCK WHERE job-hdr.company EQ ipCompanyFrom:
    CREATE bjob-hdr.
    BUFFER-COPY job-hdr EXCEPT company rec_key TO bjob-hdr
      ASSIGN bjob-hdr.company = ipCompanyTo.

    {custom\rec_key.i bjob-hdr}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE job-mat:
  DEFINE BUFFER bjob-mat FOR job-mat.

  RUN showMsg ('job-mat',NO).
  IF CAN-FIND(FIRST job-mat WHERE job-mat.company EQ ipCompanyTo) THEN
  FOR EACH job-mat EXCLUSIVE-LOCK WHERE job-mat.company EQ ipCompanyTo:
    DELETE job-mat.
  END.
  FOR EACH job-mat NO-LOCK WHERE job-mat.company EQ ipCompanyFrom:
    CREATE bjob-mat.
    BUFFER-COPY job-mat EXCEPT company rec_key TO bjob-mat
      ASSIGN bjob-mat.company = ipCompanyTo.

    {custom\rec_key.i bjob-mat}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE job-prep:
  DEFINE BUFFER bjob-prep FOR job-prep.

  RUN showMsg ('job-prep',NO).
  IF CAN-FIND(FIRST job-prep WHERE job-prep.company EQ ipCompanyTo) THEN
  FOR EACH job-prep EXCLUSIVE-LOCK WHERE job-prep.company EQ ipCompanyTo:
    DELETE job-prep.
  END.
  FOR EACH job-prep NO-LOCK WHERE job-prep.company EQ ipCompanyFrom:
    CREATE bjob-prep.
    BUFFER-COPY job-prep EXCEPT company rec_key TO bjob-prep
      ASSIGN bjob-prep.company = ipCompanyTo.

    {custom\rec_key.i bjob-prep}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE job-sch:
  DEFINE BUFFER bjob-sch FOR job-sch.

  RUN showMsg ('job-sch',NO).
  IF CAN-FIND(FIRST job-sch WHERE job-sch.company EQ ipCompanyTo) THEN
  FOR EACH job-sch EXCLUSIVE-LOCK WHERE job-sch.company EQ ipCompanyTo:
    DELETE job-sch.
  END.
  FOR EACH job-sch NO-LOCK WHERE job-sch.company EQ ipCompanyFrom:
    CREATE bjob-sch.
    BUFFER-COPY job-sch EXCEPT company rec_key TO bjob-sch
      ASSIGN bjob-sch.company = ipCompanyTo.

    {custom\rec_key.i bjob-sch}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE mch-act:
  DEFINE BUFFER bmch-act FOR mch-act.

  RUN showMsg ('mch-act',NO).
  IF CAN-FIND(FIRST mch-act WHERE mch-act.company EQ ipCompanyTo) THEN
  FOR EACH mch-act EXCLUSIVE-LOCK WHERE mch-act.company EQ ipCompanyTo:
    DELETE mch-act.
  END.
  FOR EACH mch-act NO-LOCK WHERE mch-act.company EQ ipCompanyFrom:
    CREATE bmch-act.
    BUFFER-COPY mch-act EXCEPT company rec_key TO bmch-act
      ASSIGN bmch-act.company = ipCompanyTo.

    {custom\rec_key.i bmch-act}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE misc-act:
  DEFINE BUFFER bmisc-act FOR misc-act.

  RUN showMsg ('misc-act',NO).
  IF CAN-FIND(FIRST misc-act WHERE misc-act.company EQ ipCompanyTo) THEN
  FOR EACH misc-act EXCLUSIVE-LOCK WHERE misc-act.company EQ ipCompanyTo:
    DELETE misc-act.
  END.
  FOR EACH misc-act NO-LOCK WHERE misc-act.company EQ ipCompanyFrom:
    CREATE bmisc-act.
    BUFFER-COPY misc-act EXCEPT company rec_key TO bmisc-act
      ASSIGN bmisc-act.company = ipCompanyTo.

    {custom\rec_key.i bmisc-act}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE mmtx2:
  DEFINE BUFFER bmmtx2 FOR mmtx2.

  RUN showMsg ('mmtx2',NO).
  IF CAN-FIND(FIRST mmtx2 WHERE mmtx2.company EQ ipCompanyTo) THEN
  FOR EACH mmtx2 EXCLUSIVE-LOCK WHERE mmtx2.company EQ ipCompanyTo:
    DELETE mmtx2.
  END.
  FOR EACH mmtx2 NO-LOCK WHERE mmtx2.company EQ ipCompanyFrom:
    CREATE bmmtx2.
    BUFFER-COPY mmtx2 EXCEPT company rec_key TO bmmtx2
      ASSIGN bmmtx2.company = ipCompanyTo.

    {custom\rec_key.i bmmtx2}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-bolh:
  DEFINE BUFFER boe-bolh FOR oe-bolh.

  RUN showMsg ('oe-bolh',NO).
  IF CAN-FIND(FIRST oe-bolh WHERE oe-bolh.company EQ ipCompanyTo) THEN
  FOR EACH oe-bolh EXCLUSIVE-LOCK WHERE oe-bolh.company EQ ipCompanyTo:
    DELETE oe-bolh.
  END.
  FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.company EQ ipCompanyFrom:
    CREATE boe-bolh.
    BUFFER-COPY oe-bolh EXCEPT company rec_key TO boe-bolh
      ASSIGN boe-bolh.company = ipCompanyTo.

    {custom\rec_key.i boe-bolh}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-boll:
  DEFINE BUFFER boe-boll FOR oe-boll.

  RUN showMsg ('oe-boll',NO).
  IF CAN-FIND(FIRST oe-boll WHERE oe-boll.company EQ ipCompanyTo) THEN
  FOR EACH oe-boll EXCLUSIVE-LOCK WHERE oe-boll.company EQ ipCompanyTo:
    DELETE oe-boll.
  END.
  FOR EACH oe-boll NO-LOCK WHERE oe-boll.company EQ ipCompanyFrom:
    CREATE boe-boll.
    BUFFER-COPY oe-boll EXCEPT company rec_key TO boe-boll
      ASSIGN boe-boll.company = ipCompanyTo.

    {custom\rec_key.i boe-boll}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-boll-qty:
  DEFINE BUFFER boe-boll-qty FOR oe-boll-qty.

  RUN showMsg ('oe-boll-qty',NO).
  IF CAN-FIND(FIRST oe-boll-qty WHERE oe-boll-qty.company EQ ipCompanyTo) THEN
  FOR EACH oe-boll-qty EXCLUSIVE-LOCK WHERE oe-boll-qty.company EQ ipCompanyTo:
    DELETE oe-boll-qty.
  END.
  FOR EACH oe-boll-qty NO-LOCK WHERE oe-boll-qty.company EQ ipCompanyFrom:
    CREATE boe-boll-qty.
    BUFFER-COPY oe-boll-qty EXCEPT company rec_key TO boe-boll-qty
      ASSIGN boe-boll-qty.company = ipCompanyTo.

    {custom\rec_key.i boe-boll-qty}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-ord:
  DEFINE BUFFER boe-ord FOR oe-ord.

  RUN showMsg ('oe-ord',NO).
  IF CAN-FIND(FIRST oe-ord WHERE oe-ord.company EQ ipCompanyTo) THEN
  FOR EACH oe-ord EXCLUSIVE-LOCK WHERE oe-ord.company EQ ipCompanyTo:
    DELETE oe-ord.
  END.
  FOR EACH oe-ord NO-LOCK WHERE oe-ord.company EQ ipCompanyFrom:
    CREATE boe-ord.
    BUFFER-COPY oe-ord EXCEPT company rec_key TO boe-ord
      ASSIGN boe-ord.company = ipCompanyTo.

    {custom\rec_key.i boe-ord}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-ordl:
  DEFINE BUFFER boe-ordl FOR oe-ordl.

  RUN showMsg ('oe-ordl',NO).
  IF CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company EQ ipCompanyTo) THEN
  FOR EACH oe-ordl EXCLUSIVE-LOCK WHERE oe-ordl.company EQ ipCompanyTo:
    DELETE oe-ordl.
  END.
  FOR EACH oe-ordl NO-LOCK WHERE oe-ordl.company EQ ipCompanyFrom:
    CREATE boe-ordl.
    BUFFER-COPY oe-ordl EXCEPT company rec_key TO boe-ordl
      ASSIGN boe-ordl.company = ipCompanyTo.

    {custom\rec_key.i boe-ordl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-ordm:
  DEFINE BUFFER boe-ordm FOR oe-ordm.

  RUN showMsg ('oe-ordm',NO).
  IF CAN-FIND(FIRST oe-ordm WHERE oe-ordm.company EQ ipCompanyTo) THEN
  FOR EACH oe-ordm EXCLUSIVE-LOCK WHERE oe-ordm.company EQ ipCompanyTo:
    DELETE oe-ordm.
  END.
  FOR EACH oe-ordm NO-LOCK WHERE oe-ordm.company EQ ipCompanyFrom:
    CREATE boe-ordm.
    BUFFER-COPY oe-ordm EXCEPT company rec_key TO boe-ordm
      ASSIGN boe-ordm.company = ipCompanyTo.

    {custom\rec_key.i boe-ordm}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-rel:
  DEFINE BUFFER boe-rel FOR oe-rel.

  RUN showMsg ('oe-rel',NO).
  IF CAN-FIND(FIRST oe-rel WHERE oe-rel.company EQ ipCompanyTo) THEN
  FOR EACH oe-rel EXCLUSIVE-LOCK WHERE oe-rel.company EQ ipCompanyTo:
    DELETE oe-rel.
  END.
  FOR EACH oe-rel NO-LOCK WHERE oe-rel.company EQ ipCompanyFrom:
    CREATE boe-rel.
    BUFFER-COPY oe-rel EXCEPT company rec_key TO boe-rel
      ASSIGN boe-rel.company = ipCompanyTo.

    {custom\rec_key.i boe-rel}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-relh:
  DEFINE BUFFER boe-relh FOR oe-relh.

  RUN showMsg ('oe-relh',NO).
  IF CAN-FIND(FIRST oe-relh WHERE oe-relh.company EQ ipCompanyTo) THEN
  FOR EACH oe-relh EXCLUSIVE-LOCK WHERE oe-relh.company EQ ipCompanyTo:
    DELETE oe-relh.
  END.
  FOR EACH oe-relh NO-LOCK WHERE oe-relh.company EQ ipCompanyFrom:
    CREATE boe-relh.
    BUFFER-COPY oe-relh EXCEPT company rec_key TO boe-relh
      ASSIGN boe-relh.company = ipCompanyTo.

    {custom\rec_key.i boe-relh}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-rell:
  DEFINE BUFFER boe-rell FOR oe-rell.

  RUN showMsg ('oe-rell',NO).
  IF CAN-FIND(FIRST oe-rell WHERE oe-rell.company EQ ipCompanyTo) THEN
  FOR EACH oe-rell EXCLUSIVE-LOCK WHERE oe-rell.company EQ ipCompanyTo:
    DELETE oe-rell.
  END.
  FOR EACH oe-rell NO-LOCK WHERE oe-rell.company EQ ipCompanyFrom:
    CREATE boe-rell.
    BUFFER-COPY oe-rell EXCEPT company rec_key TO boe-rell
      ASSIGN boe-rell.company = ipCompanyTo.

    {custom\rec_key.i boe-rell}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-reth:
  DEFINE BUFFER boe-reth FOR oe-reth.

  RUN showMsg ('oe-reth',NO).
  IF CAN-FIND(FIRST oe-reth WHERE oe-reth.company EQ ipCompanyTo) THEN
  FOR EACH oe-reth EXCLUSIVE-LOCK WHERE oe-reth.company EQ ipCompanyTo:
    DELETE oe-reth.
  END.
  FOR EACH oe-reth NO-LOCK WHERE oe-reth.company EQ ipCompanyFrom:
    CREATE boe-reth.
    BUFFER-COPY oe-reth EXCEPT company rec_key TO boe-reth
      ASSIGN boe-reth.company = ipCompanyTo.

    {custom\rec_key.i boe-reth}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-retl:
  DEFINE BUFFER boe-retl FOR oe-retl.

  RUN showMsg ('oe-retl',NO).
  IF CAN-FIND(FIRST oe-retl WHERE oe-retl.company EQ ipCompanyTo) THEN
  FOR EACH oe-retl EXCLUSIVE-LOCK WHERE oe-retl.company EQ ipCompanyTo:
    DELETE oe-retl.
  END.
  FOR EACH oe-retl NO-LOCK WHERE oe-retl.company EQ ipCompanyFrom:
    CREATE boe-retl.
    BUFFER-COPY oe-retl EXCEPT company rec_key TO boe-retl
      ASSIGN boe-retl.company = ipCompanyTo.

    {custom\rec_key.i boe-retl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE oe-ship:
  DEFINE BUFFER boe-ship FOR oe-ship.

  RUN showMsg ('oe-ship',NO).
  IF CAN-FIND(FIRST oe-ship WHERE oe-ship.company EQ ipCompanyTo) THEN
  FOR EACH oe-ship EXCLUSIVE-LOCK WHERE oe-ship.company EQ ipCompanyTo:
    DELETE oe-ship.
  END.
  FOR EACH oe-ship NO-LOCK WHERE oe-ship.company EQ ipCompanyFrom:
    CREATE boe-ship.
    BUFFER-COPY oe-ship EXCEPT company rec_key TO boe-ship
      ASSIGN boe-ship.company = ipCompanyTo.

    {custom\rec_key.i boe-ship}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE pc-misc:
  DEFINE BUFFER bpc-misc FOR pc-misc.

  RUN showMsg ('pc-misc',NO).
  IF CAN-FIND(FIRST pc-misc WHERE pc-misc.company EQ ipCompanyTo) THEN
  FOR EACH pc-misc EXCLUSIVE-LOCK WHERE pc-misc.company EQ ipCompanyTo:
    DELETE pc-misc.
  END.
  FOR EACH pc-misc NO-LOCK WHERE pc-misc.company EQ ipCompanyFrom:
    CREATE bpc-misc.
    BUFFER-COPY pc-misc EXCEPT company rec_key TO bpc-misc
      ASSIGN bpc-misc.company = ipCompanyTo.

    {custom\rec_key.i bpc-misc}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE pc-prdd:
  DEFINE BUFFER bpc-prdd FOR pc-prdd.

  RUN showMsg ('pc-prdd',NO).
  IF CAN-FIND(FIRST pc-prdd WHERE pc-prdd.company EQ ipCompanyTo) THEN
  FOR EACH pc-prdd EXCLUSIVE-LOCK WHERE pc-prdd.company EQ ipCompanyTo:
    DELETE pc-prdd.
  END.
  FOR EACH pc-prdd NO-LOCK WHERE pc-prdd.company EQ ipCompanyFrom:
    CREATE bpc-prdd.
    BUFFER-COPY pc-prdd EXCEPT company rec_key TO bpc-prdd
      ASSIGN bpc-prdd.company = ipCompanyTo.

    {custom\rec_key.i bpc-prdd}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE pc-prdh:
  DEFINE BUFFER bpc-prdh FOR pc-prdh.

  RUN showMsg ('pc-prdh',NO).
  IF CAN-FIND(FIRST pc-prdh WHERE pc-prdh.company EQ ipCompanyTo) THEN
  FOR EACH pc-prdh EXCLUSIVE-LOCK WHERE pc-prdh.company EQ ipCompanyTo:
    DELETE pc-prdh.
  END.
  FOR EACH pc-prdh NO-LOCK WHERE pc-prdh.company EQ ipCompanyFrom:
    CREATE bpc-prdh.
    BUFFER-COPY pc-prdh EXCEPT company rec_key TO bpc-prdh
      ASSIGN bpc-prdh.company = ipCompanyTo.

    {custom\rec_key.i bpc-prdh}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE po-all:
  DEFINE BUFFER bpo-all FOR po-all.

  RUN showMsg ('po-all',NO).
  IF CAN-FIND(FIRST po-all WHERE po-all.company EQ ipCompanyTo) THEN
  FOR EACH po-all EXCLUSIVE-LOCK WHERE po-all.company EQ ipCompanyTo:
    DELETE po-all.
  END.
  FOR EACH po-all NO-LOCK WHERE po-all.company EQ ipCompanyFrom:
    CREATE bpo-all.
    BUFFER-COPY po-all EXCEPT company rec_key TO bpo-all
      ASSIGN bpo-all.company = ipCompanyTo.

    {custom\rec_key.i bpo-all}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE po-ord:
  DEFINE BUFFER bpo-ord FOR po-ord.

  RUN showMsg ('po-ord',NO).
  IF CAN-FIND(FIRST po-ord WHERE po-ord.company EQ ipCompanyTo) THEN
  FOR EACH po-ord EXCLUSIVE-LOCK WHERE po-ord.company EQ ipCompanyTo:
    DELETE po-ord.
  END.
  FOR EACH po-ord NO-LOCK WHERE po-ord.company EQ ipCompanyFrom:
    CREATE bpo-ord.
    BUFFER-COPY po-ord EXCEPT company rec_key TO bpo-ord
      ASSIGN bpo-ord.company = ipCompanyTo.

    {custom\rec_key.i bpo-ord}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE po-ordl:
  DEFINE BUFFER bpo-ordl FOR po-ordl.

  RUN showMsg ('po-ordl',NO).
  IF CAN-FIND(FIRST po-ordl WHERE po-ordl.company EQ ipCompanyTo) THEN
  FOR EACH po-ordl EXCLUSIVE-LOCK WHERE po-ordl.company EQ ipCompanyTo:
    DELETE po-ordl.
  END.
  FOR EACH po-ordl NO-LOCK WHERE po-ordl.company EQ ipCompanyFrom:
    CREATE bpo-ordl.
    BUFFER-COPY po-ordl EXCEPT company rec_key TO bpo-ordl
      ASSIGN bpo-ordl.company = ipCompanyTo.

    {custom\rec_key.i bpo-ordl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE po-rcpts:
  DEFINE BUFFER bpo-rcpts FOR po-rcpts.

  RUN showMsg ('po-rcpts',NO).
  IF CAN-FIND(FIRST po-rcpts WHERE po-rcpts.company EQ ipCompanyTo) THEN
  FOR EACH po-rcpts EXCLUSIVE-LOCK WHERE po-rcpts.company EQ ipCompanyTo:
    DELETE po-rcpts.
  END.
  FOR EACH po-rcpts NO-LOCK WHERE po-rcpts.company EQ ipCompanyFrom:
    CREATE bpo-rcpts.
    BUFFER-COPY po-rcpts EXCEPT company rec_key TO bpo-rcpts
      ASSIGN bpo-rcpts.company = ipCompanyTo.

    {custom\rec_key.i bpo-rcpts}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE probe:
  DEFINE BUFFER bprobe FOR probe.

  RUN showMsg ('probe',NO).
  IF CAN-FIND(FIRST probe WHERE probe.company EQ ipCompanyTo) THEN
  FOR EACH probe EXCLUSIVE-LOCK WHERE probe.company EQ ipCompanyTo:
    DELETE probe.
  END.
  FOR EACH probe NO-LOCK WHERE probe.company EQ ipCompanyFrom:
    CREATE bprobe.
    BUFFER-COPY probe EXCEPT company rec_key TO bprobe
      ASSIGN bprobe.company = ipCompanyTo.

    {custom\rec_key.i bprobe}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE probeit:
  DEFINE BUFFER bprobeit FOR probeit.

  RUN showMsg ('probeit',NO).
  IF CAN-FIND(FIRST probeit WHERE probeit.company EQ ipCompanyTo) THEN
  FOR EACH probeit EXCLUSIVE-LOCK WHERE probeit.company EQ ipCompanyTo:
    DELETE probeit.
  END.
  FOR EACH probeit NO-LOCK WHERE probeit.company EQ ipCompanyFrom:
    CREATE bprobeit.
    BUFFER-COPY probeit EXCEPT company rec_key TO bprobeit
      ASSIGN bprobeit.company = ipCompanyTo.

    {custom\rec_key.i bprobeit}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE probeit-price:
  DEFINE BUFFER bprobeit-price FOR probeit-price.

  RUN showMsg ('probeit-price',NO).
  IF CAN-FIND(FIRST probeit-price WHERE probeit-price.company EQ ipCompanyTo) THEN
  FOR EACH probeit-price EXCLUSIVE-LOCK WHERE probeit-price.company EQ ipCompanyTo:
    DELETE probeit-price.
  END.
  FOR EACH probeit-price NO-LOCK WHERE probeit-price.company EQ ipCompanyFrom:
    CREATE bprobeit-price.
    BUFFER-COPY probeit-price EXCEPT company rec_key TO bprobeit-price
      ASSIGN bprobeit-price.company = ipCompanyTo.

    {custom\rec_key.i bprobeit-price}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE quotechg:
  DEFINE BUFFER bquotechg FOR quotechg.

  RUN showMsg ('quotechg',NO).
  IF CAN-FIND(FIRST quotechg WHERE quotechg.company EQ ipCompanyTo) THEN
  FOR EACH quotechg EXCLUSIVE-LOCK WHERE quotechg.company EQ ipCompanyTo:
    DELETE quotechg.
  END.
  FOR EACH quotechg NO-LOCK WHERE quotechg.company EQ ipCompanyFrom:
    CREATE bquotechg.
    BUFFER-COPY quotechg EXCEPT company rec_key TO bquotechg
      ASSIGN bquotechg.company = ipCompanyTo.

    {custom\rec_key.i bquotechg}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE quotehd:
  DEFINE BUFFER bquotehd FOR quotehd.

  RUN showMsg ('quotehd',NO).
  IF CAN-FIND(FIRST quotehd WHERE quotehd.company EQ ipCompanyTo) THEN
  FOR EACH quotehd EXCLUSIVE-LOCK WHERE quotehd.company EQ ipCompanyTo:
    DELETE quotehd.
  END.
  FOR EACH quotehd NO-LOCK WHERE quotehd.company EQ ipCompanyFrom:
    CREATE bquotehd.
    BUFFER-COPY quotehd EXCEPT company rec_key TO bquotehd
      ASSIGN bquotehd.company = ipCompanyTo.

    {custom\rec_key.i bquotehd}
  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE quoteitm:
  DEFINE BUFFER bquoteitm FOR quoteitm.

  RUN showMsg ('quoteitm',NO).
  IF CAN-FIND(FIRST quoteitm WHERE quoteitm.company EQ ipCompanyTo) THEN
  FOR EACH quoteitm EXCLUSIVE-LOCK WHERE quoteitm.company EQ ipCompanyTo:
    DELETE quoteitm.
  END.
  FOR EACH quoteitm NO-LOCK WHERE quoteitm.company EQ ipCompanyFrom:
    CREATE bquoteitm.
    BUFFER-COPY quoteitm EXCEPT company rec_key TO bquoteitm
      ASSIGN bquoteitm.company = ipCompanyTo.

    {custom\rec_key.i bquoteitm}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE quoteqty:
  DEFINE BUFFER bquoteqty FOR quoteqty.

  RUN showMsg ('quoteqty',NO).
  IF CAN-FIND(FIRST quoteqty WHERE quoteqty.company EQ ipCompanyTo) THEN
  FOR EACH quoteqty EXCLUSIVE-LOCK WHERE quoteqty.company EQ ipCompanyTo:
    DELETE quoteqty.
  END.
  FOR EACH quoteqty NO-LOCK WHERE quoteqty.company EQ ipCompanyFrom:
    CREATE bquoteqty.
    BUFFER-COPY quoteqty EXCEPT company rec_key TO bquoteqty
      ASSIGN bquoteqty.company = ipCompanyTo.

    {custom\rec_key.i bquoteqty}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE rm-rcpt:
  DEFINE BUFFER brm-rcpt FOR rm-rcpt.

  RUN showMsg ('rm-rcpt',NO).
  IF CAN-FIND(FIRST rm-rcpt WHERE rm-rcpt.company EQ ipCompanyTo) THEN
  FOR EACH rm-rcpt EXCLUSIVE-LOCK WHERE rm-rcpt.company EQ ipCompanyTo:
    DELETE rm-rcpt.
  END.
  FOR EACH rm-rcpt NO-LOCK WHERE rm-rcpt.company EQ ipCompanyFrom:
    CREATE brm-rcpt.
    BUFFER-COPY rm-rcpt EXCEPT company rec_key TO brm-rcpt
      ASSIGN brm-rcpt.company = ipCompanyTo.

    {custom\rec_key.i brm-rcpt}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE rm-rcpth:
  DEFINE BUFFER brm-rcpth FOR rm-rcpth.

  RUN showMsg ('rm-rcpth',NO).
  IF CAN-FIND(FIRST rm-rcpth WHERE rm-rcpth.company EQ ipCompanyTo) THEN
  FOR EACH rm-rcpth EXCLUSIVE-LOCK WHERE rm-rcpth.company EQ ipCompanyTo:
    DELETE rm-rcpth.
  END.
  FOR EACH rm-rcpth NO-LOCK WHERE rm-rcpth.company EQ ipCompanyFrom:
    CREATE brm-rcpth.
    BUFFER-COPY rm-rcpth EXCEPT company rec_key TO brm-rcpth
      ASSIGN brm-rcpth.company = ipCompanyTo.

    {custom\rec_key.i brm-rcpth}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE rm-rctd:
  DEFINE BUFFER brm-rctd FOR rm-rctd.

  RUN showMsg ('rm-rctd',NO).
  IF CAN-FIND(FIRST rm-rctd WHERE rm-rctd.company EQ ipCompanyTo) THEN
  FOR EACH rm-rctd EXCLUSIVE-LOCK WHERE rm-rctd.company EQ ipCompanyTo:
    DELETE rm-rctd.
  END.
  FOR EACH rm-rctd NO-LOCK WHERE rm-rctd.company EQ ipCompanyFrom:
    CREATE brm-rctd.
    BUFFER-COPY rm-rctd EXCEPT company rec_key TO brm-rctd
      ASSIGN brm-rctd.company = ipCompanyTo.

    {custom\rec_key.i brm-rctd}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE rm-rcth:
  DEFINE BUFFER brm-rcth FOR rm-rcth.

  RUN showMsg ('rm-rcth',NO).
  IF CAN-FIND(FIRST rm-rcth WHERE rm-rcth.company EQ ipCompanyTo) THEN
  FOR EACH rm-rcth EXCLUSIVE-LOCK WHERE rm-rcth.company EQ ipCompanyTo:
    DELETE rm-rcth.
  END.
  FOR EACH rm-rcth NO-LOCK WHERE rm-rcth.company EQ ipCompanyFrom:
    CREATE brm-rcth.
    BUFFER-COPY rm-rcth EXCEPT company rec_key TO brm-rcth
      ASSIGN brm-rcth.company = ipCompanyTo.

    {custom\rec_key.i brm-rcth}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE rm-rdtl:
  DEFINE BUFFER brm-rdtl FOR rm-rdtl.

  RUN showMsg ('rm-rdtl',NO).
  IF CAN-FIND(FIRST rm-rdtl WHERE rm-rdtl.company EQ ipCompanyTo) THEN
  FOR EACH rm-rdtl EXCLUSIVE-LOCK WHERE rm-rdtl.company EQ ipCompanyTo:
    DELETE rm-rdtl.
  END.
  FOR EACH rm-rdtl NO-LOCK WHERE rm-rdtl.company EQ ipCompanyFrom:
    CREATE brm-rdtl.
    BUFFER-COPY rm-rdtl EXCEPT company rec_key TO brm-rdtl
      ASSIGN brm-rdtl.company = ipCompanyTo.

    {custom\rec_key.i brm-rdtl}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE rm-rdtlh:
  DEFINE BUFFER brm-rdtlh FOR rm-rdtlh.

  RUN showMsg ('rm-rdtlh',NO).
  IF CAN-FIND(FIRST rm-rdtlh WHERE rm-rdtlh.company EQ ipCompanyTo) THEN
  FOR EACH rm-rdtlh EXCLUSIVE-LOCK WHERE rm-rdtlh.company EQ ipCompanyTo:
    DELETE rm-rdtlh.
  END.
  FOR EACH rm-rdtlh NO-LOCK WHERE rm-rdtlh.company EQ ipCompanyFrom:
    CREATE brm-rdtlh.
    BUFFER-COPY rm-rdtlh EXCEPT company rec_key TO brm-rdtlh
      ASSIGN brm-rdtlh.company = ipCompanyTo.

    {custom\rec_key.i brm-rdtlh}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE rm-receipts:
  DEFINE BUFFER brm-receipts FOR rm-receipts.

  RUN showMsg ('rm-receipts',NO).
  IF CAN-FIND(FIRST rm-receipts WHERE rm-receipts.company EQ ipCompanyTo) THEN
  FOR EACH rm-receipts EXCLUSIVE-LOCK WHERE rm-receipts.company EQ ipCompanyTo:
    DELETE rm-receipts.
  END.
  FOR EACH rm-receipts NO-LOCK WHERE rm-receipts.company EQ ipCompanyFrom:
    CREATE brm-receipts.
    BUFFER-COPY rm-receipts EXCEPT company rec_key TO brm-receipts
      ASSIGN brm-receipts.company = ipCompanyTo.

    {custom\rec_key.i brm-receipts}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

PROCEDURE user-print:
  DEFINE BUFFER buser-print FOR user-print.

  RUN showMsg ('user-print',NO).
  IF CAN-FIND(FIRST user-print WHERE user-print.company EQ ipCompanyTo) THEN
  FOR EACH user-print EXCLUSIVE-LOCK WHERE user-print.company EQ ipCompanyTo:
    DELETE user-print.
  END.
  FOR EACH user-print NO-LOCK WHERE user-print.company EQ ipCompanyFrom:
    CREATE buser-print.
    BUFFER-COPY user-print EXCEPT company rec_key TO buser-print
      ASSIGN buser-print.company = ipCompanyTo.

    {custom\rec_key.i buser-print}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.

/* gdm - 11070801 */
PROCEDURE mach-partCopy:
    DEFINE BUFFER bmach-part FOR mach-part.

    RUN showMsg ('user-print',NO).

    IF CAN-FIND(FIRST mach-part 
                  WHERE mach-part.company EQ ipCompanyTo) 
      THEN
        FOR EACH mach-part EXCLUSIVE-LOCK 
            WHERE mach-part.company EQ ipCompanyTo:
             DELETE mach-part.
    END.

    FOR EACH mach-part NO-LOCK 
        WHERE mach-part.company EQ ipCompanyFrom:

        CREATE bmach-part.
        BUFFER-COPY mach-part EXCEPT company rec_key TO bmach-part
            ASSIGN bmach-part.company = ipCompanyTo.

    {custom\rec_key.i bmach-part}

  END.
  RUN showMsg ('',YES).

END PROCEDURE.

PROCEDURE box-design:
  DEFINE BUFFER bbox-design-hdr FOR box-design-hdr.

  RUN showMsg ('box-design-hdr',NO).
  IF CAN-FIND(FIRST box-design-hdr WHERE box-design-hdr.company EQ ipCompanyTo) THEN
  FOR EACH box-design-hdr EXCLUSIVE-LOCK WHERE box-design-hdr.company EQ ipCompanyTo:
    DELETE box-design-hdr.
  END.
  FOR EACH box-design-hdr NO-LOCK WHERE box-design-hdr.company EQ ipCompanyFrom
                                    AND box-design-hdr.design-no <> 0 :
    CREATE bbox-design-hdr.
    BUFFER-COPY box-design-hdr EXCEPT company rec_key TO bbox-design-hdr
      ASSIGN bbox-design-hdr.company = ipCompanyTo.

    {custom\rec_key.i bbox-design-hdr}

  END.
  RUN showMsg ('',YES).
END PROCEDURE.
