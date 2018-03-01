
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
DEFINE VARIABLE iProcessCount      AS INTEGER           NO-UNDO INITIAL 90000.
DEFINE VARIABLE cOutputFile        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cError             AS CHARACTER         NO-UNDO.
DEFINE VARIABLE startTime          AS INTEGER           NO-UNDO.

DEFINE TEMP-TABLE ttResults NO-UNDO
    FIELD cReftable    AS CHARACTER
    FIELD iRecordCount AS INTEGER
    FIELD cConvError   AS CHARACTER
    FIELD timetaken    AS INTEGER
    .
disable triggers for load of reftable.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
    oRefTableMigration = NEW RefTableMigration().



ASSIGN cError = ""
       startTime = MTIME
       .
ASSIGN 
    iCount = oRefTableMigration:STYFLU(iProcessCount) NO-ERROR.
IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
CREATE ttResults.
ASSIGN 
    ttResults.cReftable    = "STYFLU"
    ttResults.iRecordCount = iCount
    ttResults.cConvError   = cError
    ttResults.timetaken    = (MTIME - startTime) / 1000
    .            
MESSAGE "STYFLU reftable data migration complete. Record Count:" + STRING(iCount)
        + " Time Taken: " STRING(ttResults.timetaken).




    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:STYSCORE(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "STYSCORE"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "STYSCORE reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeBollLotNo(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-boll.lot-no"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-boll.lot-no reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeRellLotNo(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rell.lot-no"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-rell.lot-no reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeRelLotNo(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rel.lot-no"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-rel.lot-no reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
            

    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeRellSellPrice(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rell.sell-price"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-rell.sell-price reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
            


    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:FgRctdUseJob(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "fg-rctd.use-job"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "fg-rctd.use-job reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).


    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeOrdlWhsItem(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-ordl.whs-item"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-ordl.whs-item reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
            


    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeOrdWhsOrder(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-ord.whs-order"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-ord.whs-order reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeOrdlQNo(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-ordl.q-no"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-ordl.q-no reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
            


    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeRelJob(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rel.job"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-rel.job reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).


    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:FgRctdUserId(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "fg-rctd.user-id"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "fg-rctd.user-id reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
            


    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:FgBinCost(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "fg-bin.cost"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "fg-bin.cost reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
            


    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeRelSellPrice(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rel.sell-price"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-rel.sell-price reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:ReftoUserPrintHM5(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "HM5"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "HM5 reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:ReftoUserPrintHM1SF(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "HM1SF"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "HM1SF reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:ReftoUserPrintHM1(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "HM1"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "HM1 reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:ReftoUserPrintHM1Acct(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "HM1Acct"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "HM1Acct reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeBollSellPrice(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-boll.sell-price"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-boll.sell-price reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:JobCreateTime(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "job.create-time"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "job.create-time reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeOrdlMisc(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe/ordlmisc.p"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe/ordlmisc.p reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:Factored(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "FACTORED"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "FACTORED reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:Termscod(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "terms.cod"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "terms.cod reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:Stack(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "STACK"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "STACK reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).




    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:ItemfgInkOccurs(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "itemfg-ink.occurs"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "itemfg-ink.occurs reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).




    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:MachinePosition(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "MachinePosition"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "MachinePosition reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:MachPlainJob(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "mach.plain-job"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "mach.plain-job reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:PrePlastJob(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "PREPLASTJOB"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "PREPLASTJOB reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:JobQtyChanged(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "job.qty-changed"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "job.qty-changed reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:GlRptPctSubtotal(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "gl-rpt.pct-subtotal"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "gl-rpt.pct-subtotal reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:CeCtrlBrokerPct(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "ce-ctrl.broker-pct"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "ce-ctrl.broker-pct reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:RmBinAgingDate(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "rm-bin.aging-date"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "rm-bin.aging-date reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:CeCtrlFoldPct(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "ce-ctrl.fold-pct"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "ce-ctrl.fold-pct reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:CeCtrlFgRateFarm(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "ce-ctrl.fg-rate-farm"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "ce-ctrl.fg-rate-farm reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:CeCtrlRmRateFarm(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "ce-ctrl.rm-rate-farm"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "ce-ctrl.rm-rate-farm reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:CeCtrlHandPctFarm(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "ce-ctrl.hand-pct-farm"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "ce-ctrl.hand-pct-farm reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:EstOpLock(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "est.op-lock"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "est.op-lock reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:OeRelScode(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rel.s-code"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "oe-rel.s-code reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:Splitship(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "SPLITSHIP"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "SPLITSHIP reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:Splitshp(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "splitshp"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "splitshp reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:MachObsolete(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "mach.obsolete"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "mach.obsolete reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:ExportCustId(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "JDEDWARDCUST#"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "JDEDWARDCUST# reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).



    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:UserDocs(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "users.user-docs"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "users.user-docs reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
            
            
    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:UserPhoneNo(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "users.phone-no"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "users.phone-no reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
            
    
    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:UserFaxNo(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "users.fax-no"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "users.fax-no reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
    
    
    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:UsersPhoneCnty(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "users.phone-cnty"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "users.phone-cnty reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
    
    
    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:UserFaxCnty(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "users.fax-cnty"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "users.fax-cnty reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).
    
    ASSIGN cError = ""
           startTime = MTIME
           .
    ASSIGN
        iCount = oRefTableMigration:scoreType(iProcessCount) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "scoreType"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    MESSAGE "scoreType reftable data migration complete. Record Count:" + STRING(iCount)
            + " Time Taken: " STRING(ttResults.timetaken).        
                                 


ASSIGN 
    cOutputFile = "c:\temp\reftableconversion" 
                   + STRING(YEAR(TODAY)) 
                   + STRING(MONTH(TODAY)) 
                   + STRING(DAY(TODAY))
                   + "_"
                   + STRING(TIME)
                   + ".csv".
OUTPUT TO VALUE(cOutputFile).
PUT "RefTable,Records Converted,Error,Time Taken (Secs)" SKIP.
FOR EACH ttResults: 
    EXPORT DELIMITER "," ttResults.
END.
OUTPUT close.        
     
