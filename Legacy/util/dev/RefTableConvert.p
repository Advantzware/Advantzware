/*------------------------------------------------------------------------
    File        : RefTableConvert.p
    Purpose     : 
    Syntax      :
    Description : Used to perform reftable conversions to native data files
    Author(s)   : MYT
    Created     : Mon Mar 11 14:06:04 EDT 2019
    Notes       : This is the "batch wrapper" for Reftable Conversions
                    The work is performed by the structured procedure spReftable.p which
                    is called as a persistent procedure here and is referenced as hConvert.
                    The scope-define "converter" contains logic that tests for reftable 
                    existence before running the conversion procedure.  This should result
                    in an efficiency gain over the previous versions.
                    In general, the only "maintenance" for this program is to add a 
                    reftable-type to the list "cRefTableList".  All other processing and
                    error catch are either built-in, or handled by hConvert.
                    Logs are configured in hConvert, but are stored in c:\tmp by default.
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR cRefTableEntry AS CHAR NO-UNDO.
DEF VAR cRefTableList AS CHAR NO-UNDO.
DEF VAR hConvert AS HANDLE NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN util/dev/spRefTable.p PERSISTENT SET hConvert.

ASSIGN 
    cRefTableList = "aoaReport,ar-cashl.inv-line,ar-cashl.return,ARCASHHOLD,ARCASHLVDDATE,Batchrpt," +
                    "blank-vend-cost,blank-vend-qty,ce-ctrl.broker-pct,ce-ctrl.fg-rate-farm," +
                    "ce-ctrl.fold-pct,ce-ctrl.hand-pct-farm,ce-ctrl.rm-rate-farm,ce/com/probemk.p," +
                    "ce/com/selwhif1.w,ce/v-est3.w-Unit#,cecrep/d-artios.w,chargecode," +
                    "COLOR AUDIT,cp-lab-p,cust.flat-comm,cust.po-mand,cust.show-set," +
                    "dropslit,e-item-vend.adders,e-itemfg-vend.markup,e-itemfg-vend.std-uom,est/getqty.w2," +
                    "est/globquot.w,FACTORED,fg-bin.cost,fg-rctd.use-job,fg-rctd.user-id,FGSTATUS," +
                    "FGTAXABLE,Flute,FREEZENOTE,gl-rpt.pct-subtotal,GLACCTDISC,gsa-fm,HM1,HM1Acct," +
                    "HM1SF,HM5,inv-line.lot-no,itemfg-ink.occurs,itemfg.exempt-disc,JDEDWARDCUST#," +
                    "job.close-checked,job.create-time,job.qty-changed,MACH-CREW,Mach.obsolete," +
                    "mach.plain-jobs,MachinePosition,msf-limit,NextRelease#,NextRelh,oe-boll.lot-no," +
                    "oe-boll.selected,oe-boll.sell-price,oe-ord.close-checked,oe-ord.whs-order," +
                    "oe-ordl.q-no,oe-ordl.whs-item,oe-rel.job,oe-rel.lot-no,oe-rel.s-code,oe-rel.sell-price," +
                    "oe-relh.can-print,oe-rell.lot-no,oe-rell.selected,oe-rell.sell-price,oe/ordlmisc.p," +
                    "POORDLDEPTH,POUserid,PREPCADFILE,PREPLASTJOB,print42,probe.board,probe.per-msf," +
                    "relcredconv,rm-bin.age-date,rm-rctd.user-id,rm/rmglobpr.w,SALETOOL,score-type," +
                    "ShiftDays,ship-to.mandatory-tax,SPLITSHIP,SPLITSHP,STACK,style.per-msf,terms.cod," +
                    "trp-car,users.fax-cnty,users.fax-no,users.phone-cnty,users.phone-no,users.user-docs," +
                    "v10-TaxCode-Upgrade,vend-cost,vend-qty,vend-setup".
    
RUN _initializeLog IN hConvert.

DO iCtr = 1 TO NUM-ENTRIES(cRefTableList):
    ASSIGN 
        cRefTableEntry = ENTRY(iCtr,cRefTableList). 
    IF CAN-DO(hConvert:INTERNAL-ENTRIES,cRefTableEntry) THEN DO:
        IF CAN-FIND(FIRST reftable WHERE reftable.reftable EQ cRefTableEntry) THEN 
            RUN _epConvert IN hConvert (INPUT cRefTableEntry). ~
        ELSE 
            RUN _writeLog IN hConvert (cRefTableEntry, 0, "Zero").    
    END.
    ELSE 
        RUN _writeLog IN hConvert (cRefTableEntry,1,"Conversion routine not defined in spRefTable.p.").
END. 

RUN _closeLog IN hConvert.
