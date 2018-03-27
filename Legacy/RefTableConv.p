
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

    {util/refTableConv.i "CeCtrlBrokerPct" 'ce-ctrl.broker-pct'}
    {util/refTableConv.i "CeCtrlFgRateFarm" 'ce-ctrl.fg-rate-farm'}
    {util/refTableConv.i "CeCtrlFoldPct" 'ce-ctrl.fold-pct'}
    {util/refTableConv.i "CeCtrlHandPctFarm" 'ce-ctrl.hand-pct-farm'}
    {util/refTableConv.i "CeCtrlRmRateFarm" 'ce-ctrl.rm-rate-farm'}
    {util/refTableConv.i "CustFlatComm" 'cust.flat-comm'}
    {util/refTableConv.i "CustPoMand" 'cust.po-mand'}
    {util/refTableConv.i "CustshowSet" 'cust.show-set'}
    {util/refTableConv.i "DropSlit" 'dropslit'}
    {util/refTableConv.i "ExportCustId" 'JDEDWARDCUST#'}
    {util/refTableConv.i "Factored" 'FACTORED'}
    {util/refTableConv.i "FgBinCost" 'fg-bin.cost'}
    {util/refTableConv.i "FgRctdUseJob" 'fg-rctd.use-job'}
    {util/refTableConv.i "FgRctdUserId" 'fg-rctd.user-id'}
    {util/refTableConv.i "FgStatus" 'FGSTATUS'}
    {util/refTableConv.i "FgTaxable" 'FGTAXABLE'}
    {util/refTableConv.i "FreezeNote" 'FREEZENOTE'}
    {util/refTableConv.i "GlAcctDisc" 'GLACCTDISC'}
    {util/refTableConv.i "GlRptPctSubtotal" 'gl-rpt.pct-subtotal'}
    {util/refTableConv.i "InvLineLotNo" 'inv-line.lot-no'}
    {util/refTableConv.i "ItemfgExemptDisc" 'itemfg.exempt-disc'}
    {util/refTableConv.i "ItemfgInkOccurs" 'itemfg-ink.occurs'}
    {util/refTableConv.i "JobCloseChecked" 'job.close-checked'}
    {util/refTableConv.i "JobCreateTime" 'job.create-time'}
    {util/refTableConv.i "JobQtyChanged" 'job.qty-changed'}
    {util/refTableConv.i "MachinePosition" 'MachinePosition'}
    {util/refTableConv.i "MachObsolete" 'Mach.obsolete'}
    {util/refTableConv.i "MachPlainJobs" 'mach.plain-jobs'}
    {util/refTableConv.i "NextRelease" 'NextRelease#'}
    {util/refTableConv.i "NextRelh" 'NextRelh'}
    {util/refTableConv.i "OeBollLotNo" 'oe-boll.lot-no'}
    {util/refTableConv.i "OeBollSellPrice" 'oe-boll.sell-price'}
    {util/refTableConv.i "OeOrdClosechecked" 'oe-ord.close-checked'}
    {util/refTableConv.i "OeOrdlMisc" 'oe/ordlmisc.p'}
    {util/refTableConv.i "OeOrdlQNo" 'oe-ordl.q-no'}
    {util/refTableConv.i "OeOrdlWhsItem" 'oe-ordl.whs-item'}
    {util/refTableConv.i "OeOrdWhsOrder" 'oe-ord.whs-order'}
    {util/refTableConv.i "OeRelhCanPrint" 'oe-relh.can-print'}
    {util/refTableConv.i "OeRelJob" 'oe-rel.job'}
    {util/refTableConv.i "OeRellLotNo" 'oe-rell.lot-no'}
    {util/refTableConv.i "OeRelLotNo" 'oe-rel.lot-no'}
    {util/refTableConv.i "OeRellSelected" 'oe-rell.selected'}
    {util/refTableConv.i "OeRellSellPrice" 'oe-rell.sell-price'}
    {util/refTableConv.i "OeRelScode" 'oe-rel.s-code'}
    {util/refTableConv.i "OeRelSellPrice" 'oe-rel.sell-price'}
    {util/refTableConv.i "PoUserId" 'POUserid'}
    {util/refTableConv.i "PrePlastJob" 'PREPLASTJOB'}
    {util/refTableConv.i "RefToUserPrintHM1" 'HM1'}
    {util/refTableConv.i "RefToUserPrintHM1Acct" 'HM1Acct'}
    {util/refTableConv.i "RefToUserPrintHM1SF" 'HM1SF'}
    {util/refTableConv.i "RefToUserPrintHM5" 'HM5'}
    {util/refTableConv.i "RmBinAgeDate" 'rm-bin.age-date'}
    {util/refTableConv.i "RmGlobPr" 'rm/rmglobpr.w'}
    {util/refTableConv.i "RmRctdUserId" 'rm-rctd.user-id'}
    {util/refTableConv.i "ScoreType" 'score-type'}
    {util/refTableConv.i "ShiptoMandatoryTax" 'ship-to.mandatory-tax'}
    {util/refTableConv.i "SplitShip" 'SPLITSHIP'}
    {util/refTableConv.i "SplitShp" 'SPLITSHP'}
    {util/refTableConv.i "Stack" 'STACK'}
    {util/refTableConv.i "StylePerMSF" 'style.per-msf'}
    {util/refTableConv.i "Termscod" 'terms.cod'}
    {util/refTableConv.i "UserDocs" 'users.user-docs'}
    {util/refTableConv.i "UserFaxCnty" 'users.fax-cnty'}
    {util/refTableConv.i "UserFaxNo" 'users.fax-no'}
    {util/refTableConv.i "UserPhoneNo" 'users.phone-no'}
    {util/refTableConv.i "UsersPhoneCnty" 'users.phone-cnty'}
    {util/refTableConv.i "VendCost" 'vend-cost'}
    {util/refTableConv.i "VendQty" 'vend-qty'}
    {util/refTableConv.i "VendSetup" 'vend-setup'}

/* Deferred for 16.7.0                                              */
/*    {util/refTableConv.i "STYFLU" 'STYFLU'}                            */
/*    {util/refTableConv.i "STYSCORE" 'STYSCORE'}                        */
/*    {util/refTableConv.i "EstOpLock" 'est.op-lock'}                    */
/*                                                                  */

OUTPUT STREAM sReport TO cOutputFile.
PUT STREAM sReport "RefTable,Total Records,Records Converted,Error,Time Taken (Secs)" SKIP.
FOR EACH ttResults: 
    EXPORT STREAM sReport DELIMITER "," ttResults.
END.
OUTPUT STREAM sReport CLOSE.        
     
MESSAGE 
    "Reftable data migration complete."
    VIEW-AS ALERT-BOX INFO. 
