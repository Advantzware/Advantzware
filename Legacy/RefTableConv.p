
/*------------------------------------------------------------------------
    File        : RefTableConversion.p
    Purpose     : Ref Table Data Conversion

    Syntax      :

    Description : This will initiate RefTableMigration Class and Run Methods    

    Author(s)   : Jitender Gill
    Created     : Fri Feb 09 09:35:08 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

BLOCK-LEVEL ON ERROR UNDO, THROW.
DEFINE VARIABLE oRefTableMigration AS RefTableMigration.
DEFINE VARIABLE iCount             AS INTEGER           NO-UNDO.
DEFINE VARIABLE iRecordLimit       AS INTEGER           NO-UNDO INITIAL 10000000.
DEFINE VARIABLE iProcessCount      AS INTEGER           NO-UNDO.
DEFINE VARIABLE cOutputFile        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cOutFile2          AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cError             AS CHARACTER         NO-UNDO.
DEFINE VARIABLE startTime          AS INTEGER           NO-UNDO.

DEF STREAM sText.
DEF STREAM sReport.

DEFINE TEMP-TABLE ttResults NO-UNDO
    FIELD cReftable      AS CHARACTER
    FIELD iTotalCount    AS INTEGER
    FIELD iChangeCount   AS INTEGER
    FIELD cConvError     AS CHARACTER
    FIELD timetaken      AS INTEGER
    .
disable triggers for load of reftable.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
    cOutputFile = "c:\tmp\reftableconversion" 
                   + STRING(YEAR(TODAY)) 
                   + STRING(MONTH(TODAY)) 
                   + STRING(DAY(TODAY))
                   + "_"
                   + STRING(TIME)
                   + ".csv".
ASSIGN 
    cOutFile2 = "c:\tmp\reftableGroup" 
                   + STRING(YEAR(TODAY)) 
                   + STRING(MONTH(TODAY)) 
                   + STRING(DAY(TODAY))
                   + "_"
                   + STRING(TIME)
                   + ".txt".

FIND FIRST company NO-LOCK NO-ERROR.

OUTPUT STREAM sText TO VALUE(cOutFile2).
PUT company.company SPACE(5)
    company.name SPACE(5)
    pdbname(1) SKIP(2).
OUTPUT STREAM sText CLOSE.

ASSIGN 
    oRefTableMigration = NEW RefTableMigration().

    {refTableConv.i "CeCtrlBrokerPct" 'ce-ctrl.broker-pct'}
    {refTableConv.i "CeCtrlFgRateFarm" 'ce-ctrl.fg-rate-farm'}
    {refTableConv.i "CeCtrlFoldPct" 'ce-ctrl.fold-pct'}
    {refTableConv.i "CeCtrlHandPctFarm" 'ce-ctrl.hand-pct-farm'}
    {refTableConv.i "CeCtrlRmRateFarm" 'ce-ctrl.rm-rate-farm'}
    {refTableConv.i "CustFlatComm" 'cust.flat-comm'}
    {refTableConv.i "CustPoMand" 'cust.po-mand'}
    {refTableConv.i "CustshowSet" 'cust.show-set'}
    {refTableConv.i "DropSlit" 'dropslit'}
    {refTableConv.i "ExportCustId" 'JDEDWARDCUST#'}
    {refTableConv.i "Factored" 'FACTORED'}
    {refTableConv.i "FgBinCost" 'fg-bin.cost'}
    {refTableConv.i "FgRctdUseJob" 'fg-rctd.use-job'}
    {refTableConv.i "FgRctdUserId" 'fg-rctd.user-id'}
    {refTableConv.i "FgStatus" 'FGSTATUS'}
    {refTableConv.i "FgTaxable" 'FGTAXABLE'}
    {refTableConv.i "FreezeNote" 'FREEZENOTE'}
    {refTableConv.i "GlAcctDisc" 'GLACCTDISC'}
    {refTableConv.i "GlRptPctSubtotal" 'gl-rpt.pct-subtotal'}
    {refTableConv.i "InvLineLotNo" 'inv-line.lot-no'}
    {refTableConv.i "ItemfgExemptDisc" 'itemfg.exempt-disc'}
    {refTableConv.i "ItemfgInkOccurs" 'itemfg-ink.occurs'}
    {refTableConv.i "JobCloseChecked" 'job.close-checked'}
    {refTableConv.i "JobCreateTime" 'job.create-time'}
    {refTableConv.i "JobQtyChanged" 'job.qty-changed'}
    {refTableConv.i "MachinePosition" 'MachinePosition'}
    {refTableConv.i "MachObsolete" 'Mach.obsolete'}
    {refTableConv.i "MachPlainJobs" 'mach.plain-jobs'}
    {refTableConv.i "NextRelease" 'NextRelease#'}
    {refTableConv.i "NextRelh" 'NextRelh'}
    {refTableConv.i "OeBollLotNo" 'oe-boll.lot-no'}
    {refTableConv.i "OeBollSellPrice" 'oe-boll.sell-price'}
    {refTableConv.i "OeOrdClosechecked" 'oe-ord.close-checked'}
    {refTableConv.i "OeOrdlMisc" 'oe/ordlmisc.p'}
    {refTableConv.i "OeOrdlQNo" 'oe-ordl.q-no'}
    {refTableConv.i "OeOrdlWhsItem" 'oe-ordl.whs-item'}
    {refTableConv.i "OeOrdWhsOrder" 'oe-ord.whs-order'}
    {refTableConv.i "OeRelhCanPrint" 'oe-relh.can-print'}
    {refTableConv.i "OeRelJob" 'oe-rel.job'}
    {refTableConv.i "OeRellLotNo" 'oe-rell.lot-no'}
    {refTableConv.i "OeRelLotNo" 'oe-rel.lot-no'}
    {refTableConv.i "OeRellSelected" 'oe-rell.selected'}
    {refTableConv.i "OeRellSellPrice" 'oe-rell.sell-price'}
    {refTableConv.i "OeRelScode" 'oe-rel.s-code'}
    {refTableConv.i "OeRelSellPrice" 'oe-rel.sell-price'}
    {refTableConv.i "PoUserId" 'POUserid'}
    {refTableConv.i "PrePlastJob" 'PREPLASTJOB'}
    {refTableConv.i "RefToUserPrintHM1" 'HM1'}
    {refTableConv.i "RefToUserPrintHM1Acct" 'HM1Acct'}
    {refTableConv.i "RefToUserPrintHM1SF" 'HM1SF'}
    {refTableConv.i "RefToUserPrintHM5" 'HM5'}
    {refTableConv.i "RmBinAgeDate" 'rm-bin.age-date'}
    {refTableConv.i "RmGlobPr" 'rm/rmglobpr.w'}
    {refTableConv.i "RmRctdUserId" 'rm-rctd.user-id'}
    {refTableConv.i "ScoreType" 'score-type'}
    {refTableConv.i "ShiptoMandatoryTax" 'ship-to.mandatory-tax'}
    {refTableConv.i "SplitShip" 'SPLITSHIP'}
    {refTableConv.i "SplitShp" 'SPLITSHP'}
    {refTableConv.i "Stack" 'STACK'}
    {refTableConv.i "StylePerMSF" 'style.per-msf'}
    {refTableConv.i "Termscod" 'terms.cod'}
    {refTableConv.i "UserDocs" 'users.user-docs'}
    {refTableConv.i "UserFaxCnty" 'users.fax-cnty'}
    {refTableConv.i "UserFaxNo" 'users.fax-no'}
    {refTableConv.i "UserPhoneNo" 'users.phone-no'}
    {refTableConv.i "UsersPhoneCnty" 'users.phone-cnty'}
    {refTableConv.i "VendCost" 'vend-cost'}
    {refTableConv.i "VendQty" 'vend-qty'}
    {refTableConv.i "VendSetup" 'vend-setup'}
    {refTableConv.i "DArtios" 'cecrep/d-artios.w'}
    {refTableConv.i "Flute" 'Flute'}
    {refTableConv.i "aoaReport" 'aoaReport'}
    {refTableConv.i "ItemfgMarkup" 'e-itemfg-vend.markup'}
    {refTableConv.i "StdUom" 'e-itemfg-vend.std-uom'}
    {refTableConv.i "relcredconv" 'relcredconv'}
    {refTableConv.i "Arcashhold" 'ARCASHHOLD'}
    {refTableConv.i "Saletool" 'SALETOOL'}
    {refTableConv.i "EitemvendAdders" 'e-item-vend.adders'}
    {refTableConv.i "Batchrpt" 'Batchrpt'}
    {refTableConv.i "CeComProbemk" 'ce/com/probemk.p'}
    {refTableConv.i "CeComSelwhif1" 'ce/com/selwhif1.w'}
    {refTableConv.i "Arcashlvdate" 'ARCASHLVDDATE'}
    {refTableConv.i "print42" 'print42'}
    {refTableConv.i "ColorAudit" 'COLOR' + chr(32) + 'AUDIT'}
    {refTableConv.i "Machcrew" 'MACH-CREW'}
    {refTableConv.i "Proboard" 'probe.board'}
    {refTableConv.i "CpLabP" 'cp-lab-p'}
    {refTableConv.i "PoordlDepth" 'POORDLDEPTH'}
    {refTableConv.i "EstGlobquot" 'est/globquot.w'}
    {refTableConv.i "ArcashlInvline" 'ar-cashl.inv-line'}
    {refTableConv.i "BlankRefTable" ''}
    {refTableConv.i "OeBollSelected" 'oe-boll.selected'}
    {refTableConv.i "GsaFm" 'gsa-fm'}
    {refTableConv.i "TruckRunPrint" 'trp-car'}
    {refTableConv.i "ShiftDays" 'ShiftDays'}
    {refTableConv.i "EstGetqtyw2" 'est/getqty.w2'}
    {refTableConv.i "CevEst3" 'ce/v-est3.w Unit#'}
    {refTableConv.i "ProbePerMsf" 'probe.per-msf'}
    {refTableConv.i "v10TaxCode" 'v10-TaxCode-Upgrade'}

/* Deferred for 16.7.0                                              */
/*    {refTableConv.i "STYFLU" 'STYFLU'}                            */
/*    {refTableConv.i "STYSCORE" 'STYSCORE'}                        */
/*    {refTableConv.i "EstOpLock" 'est.op-lock'}                    */
/*                                                                  */

OUTPUT STREAM sReport TO cOutputFile.
PUT STREAM sReport "RefTable,Total Records,Records Converted,Error,Time Taken (Secs)" SKIP.
FOR EACH ttResults: 
    EXPORT STREAM sReport DELIMITER "," ttResults.
END.
OUTPUT STREAM sReport CLOSE.        
     
