
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
DEFINE VARIABLE cOutputFile        AS CHARACTER         NO-UNDO.
DEFINE VARIABLE cError             AS CHARACTER         NO-UNDO.

DEFINE TEMP-TABLE ttResults NO-UNDO
    FIELD cReftable    AS CHARACTER
    FIELD iRecordCount AS INTEGER
    FIELD cConvError   AS CHARACTER
    .

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
    oRefTableMigration = NEW RefTableMigration().


DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:STYFLU() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "STYFLU"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:STYSCORE() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "STYSCORE"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeBollLotNo() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-boll.lot-no"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeRellLotNo() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-rell.lot-no"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeRelLotNo() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-rel.lot-no"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeRellSellPrice() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-rell.sell-price"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:FgRctdUseJob() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "fg-rctd.use-job"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeOrdlWhsItem() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-ordl.whs-item"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeOrdWhsOrder() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-ord.whs-order"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeOrdlQNo() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-ordl.q-no"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeRelJob() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-rel.job"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:FgRctdUserId() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "fg-rctd.user-id"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:FgBinCost() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "fg-bin.cost"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeRelSellPrice() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-rel.sell-price"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:ReftoUserPrintHM5() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "HM5"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:ReftoUserPrintHM1SF() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "HM1SF"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:ReftoUserPrintHM1() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "HM1"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:ReftoUserPrintHM1Acct() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "HM1Acct"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeBollSellPrice() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-boll.sell-price"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:JobCreateTime() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "job.create-time"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeOrdlMisc() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe/ordlmisc.p"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:Factored() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "FACTORED"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:Termscod() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "terms.cod"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:Stack() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "STACK"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:Stack() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "STACKSTRAP"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:Stack() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "STACKPAT"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:ItemfgInkOccurs() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "itemfg-ink.occurs"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:MachinePosition() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "MachinePosition"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:MachPlainJob() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "mach.plain-job"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:PrePlastJob() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "PREPLASTJOB"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:JobQtyChanged() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "job.qty-changed"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:GlRptPctSubtotal() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "gl-rpt.pct-subtotal"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:CeCtrlBrokerPct() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "ce-ctrl.broker-pct"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:RmBinAgingDate() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "rm-bin.aging-date"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:CeCtrlFoldPct() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "ce-ctrl.fold-pct"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:CeCtrlFgRateFarm() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "ce-ctrl.fg-rate-farm"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:CeCtrlRmRateFarm() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "ce-ctrl.rm-rate-farm"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:CeCtrlHandPctFarm() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "ce-ctrl.hand-pct-farm"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:EstOpLock() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "est.op-lock"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:OeRelScode() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "oe-rel.s-code"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:Splitship() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "SPLITSHIP"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:Splitshp() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "splitshp"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

DO TRANSACTION:
    ASSIGN cError = "".
    ASSIGN 
        iCount = oRefTableMigration:MachObsolete() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN 
        ttResults.cReftable    = "mach.obsolete"
        ttResults.iRecordCount = iCount
        ttResults.cConvError   = cError.
        .    
END. 

ASSIGN 
    cOutputFile = "c:\temp\reftableconversion" 
                   + STRING(YEAR(TODAY)) 
                   + STRING(MONTH(TODAY)) 
                   + STRING(DAY(TODAY))
                   + "_"
                   + STRING(TIME)
                   + ".csv".
OUTPUT TO VALUE(cOutputFile).
PUT "RefTable,Records Converted,Error" SKIP.
FOR EACH ttResults: 
    EXPORT DELIMITER "," ttResults.
END.
OUTPUT close.        
     
