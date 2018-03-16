
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
DEFINE VARIABLE cError             AS CHARACTER         NO-UNDO.
DEFINE VARIABLE startTime          AS INTEGER           NO-UNDO.

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
OUTPUT TO VALUE(cOutputFile).
PUT company.company SPACE(5)
    company.name SPACE(5)
    pdbname(1) SKIP(2).

ASSIGN 
    oRefTableMigration = NEW RefTableMigration().



/*ASSIGN cError = ""                                                                    */
/*       startTime = MTIME                                                              */
/*       .                                                                              */
/*ASSIGN                                                                                */
/*    iCount = oRefTableMigration:STYFLU(iRecordLimit) NO-ERROR.                       */
/*IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".*/
/*CREATE ttResults.                                                                     */
/*ASSIGN                                                                                */
/*    ttResults.cReftable    = "STYFLU"                                                 */
/*    ttResults.iTotalCount = iCount                                                   */
/*    ttResults.cConvError   = cError                                                   */
/*    ttResults.timetaken    = (MTIME - startTime) / 1000                               */
/*    .                                                                                 */
/*MESSAGE "STYFLU reftable data migration complete. Record Count:" + STRING(iCount)     */
/*        + " Time Taken: " STRING(ttResults.timetaken).                                */




/*    ASSIGN cError = ""                                                                    */
/*           startTime = MTIME                                                              */
/*           .                                                                              */
/*    ASSIGN                                                                                */
/*        iCount = oRefTableMigration:STYSCORE(iRecordLimit) NO-ERROR.                     */
/*    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".*/
/*    CREATE ttResults.                                                                     */
/*    ASSIGN                                                                                */
/*        ttResults.cReftable    = "STYSCORE"                                               */
/*        ttResults.iTotalCount = iCount                                                   */
/*        ttResults.cConvError   = cError                                                   */
/*        ttResults.timetaken    = (MTIME - startTime) / 1000                               */
/*        .                                                                                 */
/*    MESSAGE "STYSCORE reftable data migration complete. Record Count:" + STRING(iCount)   */
/*            + " Time Taken: " STRING(ttResults.timetaken).                                */



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeBollLotNo(iRecordLimit) 
        iProcessCount = oRefTableMigration:iProcessCount
        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-boll.lot-no"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeRellLotNo(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rell.lot-no"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeRelLotNo(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rel.lot-no"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
            

    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeRellSellPrice(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rell.sell-price"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
            


    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:FgRctdUseJob(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "fg-rctd.use-job"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    


    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeOrdlWhsItem(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-ordl.whs-item"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
            


    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeOrdWhsOrder(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-ord.whs-order"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeOrdlQNo(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-ordl.q-no"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
            


    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeRelJob(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rel.job"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    


    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:FgRctdUserId(iRecordLimit) NO-ERROR.
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "fg-rctd.user-id"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .


    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:FgBinCost(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "fg-bin.cost"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
            


    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeRelSellPrice(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rel.sell-price"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:ReftoUserPrintHM5(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "HM5"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:ReftoUserPrintHM1SF(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "HM1SF"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:ReftoUserPrintHM1(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "HM1"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:ReftoUserPrintHM1Acct(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "HM1Acct"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeBollSellPrice(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-boll.sell-price"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:JobCreateTime(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "job.create-time"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeOrdlMisc(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe/ordlmisc.p"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:Factored(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "FACTORED"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:Termscod(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "terms.cod"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:Stack(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "STACK"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    




    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:ItemfgInkOccurs(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "itemfg-ink.occurs"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    

    
    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:ShiptoMandatoryTax(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "shipto.mandatory-tax"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .

    
    
    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:CustPoMand(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "cust.po-mand"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    


    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:CustShowSet(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "cust.show-set"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .




    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:MachinePosition(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "MachinePosition"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    


    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:GlAcctDisc(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "GLACCTDISC"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:CustFlatComm(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "cust.flat-comm"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:StylePerMsf(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "style.per-msf"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:FreezeNote(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "FREEZENOTE"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .




    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:MachPlainJobs(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "mach.plain-jobs"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
        


    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:PrePlastJob(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "PREPLASTJOB"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:JobQtyChanged(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "job.qty-changed"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:GlRptPctSubtotal(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "gl-rpt.pct-subtotal"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:CeCtrlBrokerPct(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "ce-ctrl.broker-pct"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:RmBinAgeDate(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "rm-bin.age-date"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:CeCtrlFoldPct(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "ce-ctrl.fold-pct"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:CeCtrlFgRateFarm(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "ce-ctrl.fg-rate-farm"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:CeCtrlRmRateFarm(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "ce-ctrl.rm-rate-farm"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:CeCtrlHandPctFarm(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "ce-ctrl.hand-pct-farm"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



/*    ASSIGN cError = ""                                                                    */
/*           startTime = MTIME                                                              */
/*           .                                                                              */
/*    ASSIGN                                                                                */
/*        iCount = oRefTableMigration:EstOpLock(iRecordLimit) NO-ERROR.                    */
/*    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".*/
/*    CREATE ttResults.                                                                     */
/*    ASSIGN                                                                                */
/*        ttResults.cReftable    = "est.op-lock"                                            */
/*        ttResults.iTotalCount = iCount                                                   */
/*        ttResults.cConvError   = cError                                                   */
/*        ttResults.timetaken    = (MTIME - startTime) / 1000                               */
/*        .                                                                                 */
/*    MESSAGE "est.op-lock reftable data migration complete. Record Count:" + STRING(iCount)*/
/*            + " Time Taken: " STRING(ttResults.timetaken).                                */



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeRelScode(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rel.s-code"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:Splitship(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "SPLITSHIP"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:Splitshp(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "splitshp"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:MachObsolete(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "mach.obsolete"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:ExportCustId(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "JDEDWARDCUST#"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:UserDocs(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "users.user-docs"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
            
            
    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:UserPhoneNo(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "users.phone-no"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
            
    
    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:UserFaxNo(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "users.fax-no"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
    
    
    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:UsersPhoneCnty(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "users.phone-cnty"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
    
    
    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:UserFaxCnty(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "users.fax-cnty"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:InvLineLotNo(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "inv-line.lot-no"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeRelhCanPrint(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-relh.can-print"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:Dropslit(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "dropslit"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:NextRelh(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "NextRelh"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:PoUserId(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "POUserid"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeOrdCloseChecked(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-ord.close-checked"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:RmRctdUserId(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "rm-rctd.user-id"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:ItemfgExemptDisc(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "itemfg.exempt-disc"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:FGStatus(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "FGSTATUS"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:JobCloseChecked(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "job.close-checked"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   




    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:NextRelease(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "NextRelease#"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   




    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:Rmglobpr(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "rm/rmglobpr.w"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .   




    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:OeRellSelected(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "oe-rell.selected"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        . 



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:Fgtaxable(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "FGTAXABLE"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        . 
         



     ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:ScoreType(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "score-type"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        . 



    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:VendQty(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "vend-qty"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
        
    
    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:VendCost(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "vend-cost"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
    
    ASSIGN cError = ""
           startTime = MTIME
           iCount = 0
           iProcessCount = 0
           .
    ASSIGN
        iCount = oRefTableMigration:VendSetup(iRecordLimit)
        iProcessCount = oRefTableMigration:iProcessCount NO-ERROR.
    IF ERROR-STATUS:ERROR THEN ASSIGN cError = "Error occured while reftable conversion.".
    CREATE ttResults.
    ASSIGN
        ttResults.cReftable    = "vend-setup"
        ttResults.iTotalCount  = iCount
        ttResults.iChangeCount = iProcessCount
        ttResults.cConvError   = cError
        ttResults.timetaken    = (MTIME - startTime) / 1000
        .
    
            






    
    
    MESSAGE "Reftable data migration complete". 
                                 


PUT "RefTable,Total Records,Records Converted,Error,Time Taken (Secs)" SKIP.
FOR EACH ttResults: 
    EXPORT DELIMITER "," ttResults.
END.
OUTPUT close.        
     
