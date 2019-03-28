
/*------------------------------------------------------------------------
    File        : spReftable.p
    Purpose     : 
    Syntax      : RUN _epConvert in hConvert (reftable, rowid).
    Description : Structured Procedure to handle individual reftable conversions
    Author(s)   : MYT
    Created     : Mon Mar 11 14:18:06 EDT 2019
    Notes       : This is the "worker" procedure for reftable conversion, in the form of 
                    a structured procedure instantiated by a PERSISTENT RUN.  Individual
                    reftable types are converted by procedures which are (convenently?) named
                    with the reftable.reftable value.
                    The "RUN _epConvert" allows for handling of common tasks (record counting,
                    buffer definitions, error logging, etc. so that the indivdual procedures
                    are ONLY responsible for updating the underlying tables.
                    There is a template procedure (zProcTemplate) which can be used to quickly
                    add a new reftable type to the conversion process. 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    DEFINE VARIABLE cLogFile AS CHAR NO-UNDO.
    DEFINE VARIABLE iLimit AS INT NO-UNDO INITIAL 1000000.
    DEFINE VARIABLE iRecordCount AS INT NO-UNDO.
    DEFINE VARIABLE iProcessCount AS INT NO-UNDO.
    DEFINE VARIABLE iTotRecordCount AS INT NO-UNDO.
    DEFINE VARIABLE iTotProcessCount AS INT NO-UNDO.
    DEFINE VARIABLE iCtr AS INT NO-UNDO.
    DEFINE VARIABLE jCtr AS INT NO-UNDO.
    DEFINE VARIABLE iStartTime AS INT NO-UNDO.
    DEFINE VARIABLE iEndTime AS INT NO-UNDO.
    DEFINE VARIABLE iHaveRecs AS INT NO-UNDO.
    DEFINE VARIABLE iElapsedTime AS INT NO-UNDO.
    
    DEFINE TEMP-TABLE ttErrorList
        FIELD cRefTable AS CHAR FORMAT "x(30)"
        FIELD cErrText AS CHAR FORMAT "x(50)".
        
/* ********************  Preprocessor Definitions  ******************** */
&SCOPED-DEFINE CommonCode ~
    DEFINE INPUT PARAMETER ipcRefTable AS CHAR. ~
    DEFINE INPUT PARAMETER iprRefTable AS ROWID. ~
    DEFINE BUFFER bRefTable FOR refTable. ~
    ~
    DISABLE TRIGGERS FOR LOAD OF ~{&cTable}. ~
    ~
    FIND bRefTable NO-LOCK WHERE ~
        ROWID(bRefTable) EQ iprRefTable ~
        NO-ERROR.
    

/* ***************************  Main Block  *************************** */
ASSIGN 
    cLogFile = "c:\tmp\RefTableConversion-" + 
                STRING(YEAR(TODAY),"9999") +
                STRING(MONTH(TODAY),"99") +
                STRING(DAY(TODAY),"99") + "-" +
                SUBSTRING(STRING(time,"HH:MM:SS"),1,2) +
                SUBSTRING(STRING(time,"HH:MM:SS"),4,2) +
                SUBSTRING(STRING(time,"HH:MM:SS"),7,2) +
                ".txt".


/* **********************  Internal Procedures  *********************** */
PROCEDURE _closeLog:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iElapsed AS INT NO-UNDO.
    OUTPUT TO VALUE(cLogFile) APPEND.
    ASSIGN 
        iCtr = 0
        iElapsedTime = ETIME
        iElapsedTime = iElapsedTime MOD 1000
        iEndTime = TIME
        iElapsed = IF iElapsed LT 0 THEN iElapsed + 86400 ELSE iElapsed.  /* Ran over a midnight */
    PUT UNFORMATTED "----------------------------------------------------------------------".
    PUT "Total:" AT 1
        TRIM(STRING(jCtr,">>9")) + " reftables; " + TRIM(STRING(iHaveRecs,">>9")) + " had records." AT 9 FORMAT "x(33)"
        iTotRecordCount TO 54 
        iTotProcessCount TO 70.
    PUT UNFORMATTED CHR(10).        
    PUT UNFORMATTED "----------------------------------------------------------------------" + CHR(10).
    PUT UNFORMATTED CHR(10).
    PUT UNFORMATTED "Errors:".
    FOR EACH ttErrorList:
        ASSIGN 
            iCtr = iCtr + 1.
        PUT  
            ttErrorList.cRefTable AT 1
            ttErrorList.cErrText  AT 35.
    END. 
    PUT UNFORMATTED CHR(10).        
    PUT UNFORMATTED "----------------------------------------------------------------------" + CHR(10).
    PUT UNFORMATTED STRING(iCtr) + " total errors." + CHR(10).
    PUT UNFORMATTED "----------------------------------------------------------------------" + CHR(10).
    PUT UNFORMATTED "Elapsed time: " + STRING(iElapsed,"HH:MM:SS") + "." + STRING(iElapsedTime,"999") + CHR(10).
    
    OUTPUT CLOSE.                     

END PROCEDURE.

PROCEDURE _epConvert:
/*------------------------------------------------------------------------------
 Purpose:   Contains code common to all reftable conversions
 Notes:     Record counts, reftable1 creation, logging, etc.
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRefTable AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER ipcCount AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCount AS INT NO-UNDO.
    DEFINE OUTPUT PARAMETER opiProcessed AS INT NO-UNDO.
    
    DISABLE TRIGGERS FOR LOAD OF reftable.        
    DISABLE TRIGGERS FOR LOAD OF reftable1.        
    
    ASSIGN 
        iProcessCount = 0
        iRecordCount = 0.
    
    blkProcess:
    FOR EACH reftable NO-LOCK WHERE reftable.reftable EQ ipcRefTable:
        ASSIGN 
            iRecordCount = iRecordCount + 1.
        
        IF ipcCount NE "Count" THEN DO:
            RUN VALUE(ipcRefTable) (INPUT ipcRefTable,
                                    INPUT ROWID(reftable)). 
    
            FIND CURRENT reftable EXCLUSIVE NO-ERROR.
            CREATE reftable1.
            BUFFER-COPY reftable TO reftable1.
            RELEASE reftable1.
            DELETE reftable. 
    
            IF iProcessCount GE iLimit THEN LEAVE blkProcess.
        END. 
    END.  /*FOR EACH reftable*/  

    IF ipcCount NE "Count" THEN DO:
        RUN _writeLog (ipcReftable, iRecordCount, "").
        
        ASSIGN 
            iTotProcessCount = iTotProcessCount + iProcessCount
            iTotRecordCount = iTotRecordCount + iRecordCount.
    END.
    
    ASSIGN 
        opiCount = iRecordCount
        opiProcessed = iProcessCount.
            
END PROCEDURE.

PROCEDURE _initializeLog:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    OUTPUT TO VALUE(cLogFile).
    PUT UNFORMATTED "Reftable conversion program." + CHR(10).
    PUT UNFORMATTED "Run Date: " + STRING(today,"99/99/9999") + " Time: " + STRING(TIME,"HH:MM:SS") + CHR(10).
    PUT UNFORMATTED "----------------------------------------------------------------------" + CHR(10).
    PUT UNFORMATTED "Reftable                                      Reviewed       Processed" + CHR(10).
    PUT UNFORMATTED "----------------------------------------------------------------------" + CHR(10).
    OUTPUT CLOSE.    
    ASSIGN 
        jCtr = 0
        iStartTime = TIME
        iElapsedTime = ETIME(TRUE).                 

END PROCEDURE.

PROCEDURE _writeLog:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcReftable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipiRecs AS INT NO-UNDO.
    DEF INPUT PARAMETER ipcError AS CHAR NO-UNDO.
    
    OUTPUT TO VALUE(cLogFile) APPEND.
    
    IF ipcError EQ "" THEN DO:
        PUT  
            ipcReftable AT 1 FORMAT "x(30)"
            iRecordCount TO 54 
            iProcessCount TO 70.
        PUT UNFORMATTED CHR(10).
        ASSIGN 
            jCtr = jCtr + 1
            iHaveRecs = IF iRecordCount GT 0 THEN (iHaveRecs + 1) ELSE iHaveRecs.
    END.
    ELSE IF ipcError EQ "Zero" THEN DO:
        PUT  
            ipcReftable AT 1 FORMAT "x(30)"
            ipiRecs TO 54 
            ipiRecs TO 70.
        PUT UNFORMATTED CHR(10).
        ASSIGN 
            jCtr = jCtr + 1.
    END.
    ELSE DO:
        CREATE ttErrorList.
        ASSIGN 
            ttErrorList.cRefTable = ipcReftable
            ttErrorList.cErrText = ipcError.
    END. 
    
    OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE _ProcTemplate:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    /*   REMOVE THIS LINE AFTER COPYING THE TEMPLATE
&SCOPED-DEFINE cTable <tablename>    
{&CommonCode}
        
    FOR EACH <tablename> EXCLUSIVE WHERE 
        <condition>:
        
        IF <condition> THEN ASSIGN 
            truck.<value> = reftable.val[1] 
            iProcessCount = iProcessCount + 1.

    END.
    REMOVE THIS LINE AFTER COPYING THE TEMPLATE  */

END PROCEDURE.


PROCEDURE AOAreport:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE ar-cashl.inv-line:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable ar-cashl    
{&CommonCode}
        
    FOR EACH ar-invl NO-LOCK WHERE 
        ar-invl.company EQ reftable.company AND 
        ar-invl.x-no    EQ INT(SUBSTRING(reftable.code2,1,10)) AND 
        ar-invl.line    EQ INT(SUBSTRING(reftable.code2,11,20)):
        
        FIND FIRST ar-cashl EXCLUSIVE WHERE 
            ar-cashl.company EQ reftable.company AND 
            ar-cashl.c-no    EQ INT(SUBSTRING(reftable.code,1,10)) AND 
            ar-cashl.line    EQ INT(SUBSTRING(reftable.code,11,20)) 
            NO-ERROR.
        
        IF AVAIL ar-cashl THEN DO: 
            ASSIGN 
                ar-cashl.invoiceXNo  = ar-invl.x-no
                ar-cashl.invoiceLine = ar-invl.line
                iProcessCount = iProcessCount + 1.
        END.
    END.

END PROCEDURE.


PROCEDURE ar-cashl.return:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable ar-cashl    
{&CommonCode}
        
    FOR EACH ar-cashl EXCLUSIVE WHERE 
        ar-cashl.c-no    EQ INT(reftable.code) AND 
        ar-cashl.company EQ reftable.company AND 
        ar-cashl.line    EQ INT(reftable.code2):
        
        IF ar-cashl.r-no EQ 0 THEN ASSIGN 
            ar-cashl.r-no = reftable.val[1].
        IF ar-cashl.dscr EQ "" THEN ASSIGN 
            ar-cashl.dscr = reftable.dscr. 
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE Batchrpt:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable user-print    
{&CommonCode}
        
    FOR EACH user-print EXCLUSIVE WHERE 
        user-print.company eq reftable.company AND 
        user-print.program-id = reftable.code:
        
        IF user-print.prgmName EQ "" THEN ASSIGN 
            user-print.prgmName = reftable.code2
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE ARCASHHOLD:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable ar-cash    
{&CommonCode}
        
    FOR EACH ar-cash EXCLUSIVE WHERE 
        ar-cash.rec_key EQ reftable.rec_key:
        
        IF ar-cash.stat EQ "" THEN ASSIGN 
            ar-cash.stat = reftable.code
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE ARCASHLVDDATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable ar-cashl    
{&CommonCode}
        
    FOR EACH ar-cashl EXCLUSIVE WHERE 
        ar-cashl.rec_key EQ reftable.rec_key:
        
        IF ar-cashl.voidDate EQ ? THEN ASSIGN 
            ar-cashl.voidDate = DATE(reftable.code).
        ASSIGN 
            ar-cashl.voided = TRUE
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE blank-vend-cost:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable e-item   
{&CommonCode}
        
    FOR EACH e-item EXCLUSIVE WHERE 
        e-item.i-no    EQ reftable.code AND 
        e-item.company EQ reftable.company:
        
        DO iCtr = 1 to 10: 
            IF e-item.runCost[iCtr] EQ 0 THEN ASSIGN 
                e-item.runCost[iCtr] = reftable.val[iCtr]. 
        END.
        ASSIGN  
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.

PROCEDURE blank-vend-qty:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable e-item    
{&CommonCode}
        
    FOR EACH e-item EXCLUSIVE WHERE 
        e-item.i-no    EQ reftable.code AND 
        e-item.company EQ reftable.company:
        
        DO iCtr = 1 to 10: 
            IF e-item.runQty[iCtr] EQ 0 THEN ASSIGN 
                e-item.runQty[iCtr] = reftable.val[iCtr]. 
        END.
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.

PROCEDURE ce-ctrl.broker-pct:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable ce-ctrl    
{&CommonCode}
    
    FOR EACH ce-ctrl EXCLUSIVE WHERE 
        ce-ctrl.company EQ bRefTable.company AND 
        ce-ctrl.loc     EQ bRefTable.loc:

        IF ce-ctrl.broker-pct EQ 0 THEN ASSIGN 
            ce-ctrl.broker-pct = reftable.val[1]
            iProcessCount = iProcessCount + 1.

    END. 

END PROCEDURE.


PROCEDURE ce-ctrl.fg-rate-farm:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable ce-ctrl    
{&CommonCode}
        
    FOR EACH ce-ctrl EXCLUSIVE WHERE 
        ce-ctrl.company EQ reftable.company AND 
        ce-ctrl.loc     EQ reftable.loc:
        
        IF ce-ctrl.fg-rate-farm EQ 0 THEN ASSIGN 
            ce-ctrl.fg-rate-farm = reftable.val[1]
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE ce-ctrl.fold-pct:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable ce-ctrl   
{&CommonCode}
        
    FOR EACH ce-ctrl EXCLUSIVE WHERE 
        ce-ctrl.company EQ reftable.company AND 
        ce-ctrl.loc     EQ reftable.loc:
        
        IF ce-ctrl.fold-pct EQ 0 THEN ASSIGN 
            ce-ctrl.fold-pct = reftable.val[1] 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.

PROCEDURE ce-ctrl.hand-pct-farm:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE ce-ctrl ce-ctrl    
{&CommonCode}
        
    FOR EACH ce-ctrl EXCLUSIVE WHERE 
        ce-ctrl.company EQ reftable.company AND 
        ce-ctrl.loc     EQ reftable.loc:
        
        IF ce-ctrl.hand-pct-farm EQ 0 THEN ASSIGN 
            ce-ctrl.hand-pct-farm = reftable.val[1] 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE ce-ctrl.rm-rate-farm:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE ce-ctrl ce-ctrl    
{&CommonCode}
        
    FOR EACH ce-ctrl EXCLUSIVE WHERE 
        ce-ctrl.company EQ reftable.company AND 
        ce-ctrl.loc     EQ reftable.loc:
        
        IF ce-ctrl.rm-rate-farm EQ 0 THEN ASSIGN 
            ce-ctrl.rm-rate-farm = reftable.val[1]
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE ce/com/probemk.p:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable probeit    
{&CommonCode}
        
    FOR EACH probeit EXCLUSIVE WHERE 
        probeit.company EQ reftable.company AND 
        probeit.est-no  EQ reftable.loc AND 
        probeit.line    EQ INT(reftable.code) AND 
        probeit.part-no EQ reftable.code2:
        
        IF probeit.releaseCount      EQ 0 THEN ASSIGN probeit.releaseCount = reftable.val[1].
        IF probeit.pctCommission     EQ 0 THEN ASSIGN probeit.pctCommission = reftable.val[2].
        IF probeit.pctRoyalty        EQ 0 THEN ASSIGN probeit.pctRoyalty = reftable.val[3].
        IF probeit.pctWarehouse      EQ 0 THEN ASSIGN probeit.pctWarehouse = reftable.val[4].
        IF probeit.pctCustMargin     EQ 0 THEN ASSIGN probeit.pctCustMargin = reftable.val[5].
        IF probeit.totCostCommission EQ 0 THEN ASSIGN probeit.totCostCommission = reftable.val[6].
        IF probeit.totCostRoyalty    EQ 0 THEN ASSIGN probeit.totCostRoyalty = reftable.val[7].
        IF probeit.totCostWarehousr  EQ 0 THEN ASSIGN probeit.totCostWarehousr = reftable.val[8]. 
        IF probeit.totCostCustMargin EQ 0 THEN ASSIGN probeit.totCostCustMargin = reftable.val[9]. 
        ASSIGN     
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE ce/com/selwhif1.w:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable eb    
{&CommonCode}
        
    FOR EACH eb EXCLUSIVE WHERE 
        eb.company  EQ reftable.company AND 
        eb.est-no   EQ reftable.loc AND 
        eb.form-no  EQ INT(reftable.code) AND 
        eb.blank-no EQ INT(reftable.code2):
        
        IF eb.releaseCount EQ 0 THEN ASSIGN 
            eb.releaseCount = reftable.val[1]
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE ce/v-est3.w-Unit#:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable eb    
{&CommonCode}
    DEFINE BUFFER b-rt FOR reftable.
        
    FOR EACH eb EXCLUSIVE WHERE 
        eb.company     EQ reftable.company AND 
        eb.est-no      EQ reftable.loc AND 
        eb.form-no     EQ int(reftable.code) AND 
        eb.blank-no    EQ int(reftable.code2):
        
        DO iCtr = 1 to 12: 
            /* Load 'side' value if empty or user hasn't updated manually */
            IF eb.side[iCtr] EQ "" THEN ASSIGN 
                eb.side[iCtr] = SUBSTRING(reftable.dscr,iCtr,1).
            /* Load 'unit' value if 0 and user hasn't updated manually */
            IF eb.unitNo[iCtr] EQ 0 THEN ASSIGN  
                eb.unitNo[iCtr] = reftable.val[iCtr].                                      
        END.

        /* Find the second reftable, if it exists */
        FIND FIRST b-rt EXCLUSIVE WHERE 
            b-rt.reftable EQ "ce/v-est3.w Unit#1" AND   /* Unit#1, NOT Unit# */ 
            b-rt.company  EQ eb.company AND 
            b-rt.loc      EQ eb.est-no  AND 
            b-rt.code     EQ STRING(eb.form-no,"9999999999") AND 
            b-rt.code2    EQ STRING(eb.blank-no,"9999999999")
            NO-ERROR.            
        IF AVAIL b-rt THEN DO iCtr = 13 to 20: 
            /* Load 'side' value if empty or user hasn't updated manually */
            IF eb.side[iCtr] EQ "" THEN ASSIGN 
                eb.side[iCtr] = SUBSTRING(b-rt.dscr,iCtr,1).
            /* Load 'unit' value if 0 and user hasn't updated manually */
            IF eb.unitNo[iCtr] EQ 0 THEN ASSIGN  
                eb.unitNo[iCtr] = b-rt.val[iCtr - 12].
            CREATE reftable1.
            BUFFER-COPY b-rt TO reftable1.
            RELEASE reftable1.
            DELETE b-rt.
        END.
        
        ASSIGN 
            iProcessCount = iProcessCount + 1.  
    END.

END PROCEDURE.


PROCEDURE cecrep/d-artios.w:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE chargecode:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable fgcat    
{&CommonCode}
        
    FOR EACH fgcat EXCLUSIVE WHERE 
        fgcat.company EQ reftable.company AND 
        fgcat.procat  EQ reftable.loc:
        
        IF fgcat.miscCharge EQ "" THEN ASSIGN 
            fgcat.miscCharge = reftable.code.
        IF fgcat.brdExpAcct EQ "" THEN ASSIGN 
            fgcat.brdExpAcct = reftable.code2.
        IF fgcat.cogsExpAcct EQ "" THEN ASSIGN 
            fgcat.cogsExpAcct = reftable.dscr.
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE "COLOR AUDIT":
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE cp-lab-p:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable cust-part    
{&CommonCode}
        
    FOR EACH cust-part EXCLUSIVE WHERE 
        cust-part.company EQ reftable.company AND 
        cust-part.i-no    EQ reftable.loc AND 
        cust-part.cust-no EQ reftable.code:
        
        IF cust-part.labelCase EQ "" THEN ASSIGN 
            cust-part.labelCase = reftable.code2.
        IF cust-part.labelPallet EQ "" THEN ASSIGN 
            cust-part.labelPallet = reftable.dscr.
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE cust.flat-comm:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable cust    
{&CommonCode}
        
    FOR EACH cust EXCLUSIVE WHERE 
        cust.company EQ reftable.company AND 
        cust.cust-no EQ reftable.code:
        
        IF cust.flatCommPct EQ 0 THEN ASSIGN 
            cust.flatCommPct = DEC(reftable.val[1])    
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE cust.po-mand:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable cust    
{&CommonCode}
        
    FOR EACH cust EXCLUSIVE WHERE 
        cust.company EQ reftable.company AND 
        cust.cust-no EQ reftable.code:
        
        IF cust.po-mandatory EQ NO THEN ASSIGN
            cust.po-mandatory = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE    
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE cust.show-set:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable cust    
{&CommonCode}
        
    FOR EACH cust EXCLUSIVE WHERE 
        cust.company EQ reftable.company AND 
        cust.cust-no EQ reftable.code:
        
        IF cust.show-set EQ NO THEN ASSIGN 
            cust.show-set = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE  
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE d-shp2nt:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE dropslit:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable est    
{&CommonCode}
        
    FOR EACH est EXCLUSIVE WHERE 
        est.company EQ reftable.company AND 
        est.est-no  EQ reftable.code:
        
        IF est.dropslit EQ NO THEN ASSIGN 
            est.dropslit = IF reftable.loc EQ "yes" THEN TRUE ELSE FALSE      
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.

PROCEDURE e-item-vend.adders:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable e-item-vend    
{&CommonCode}
        
    FOR EACH e-item-vend EXCLUSIVE WHERE 
        e-item-vend.rec_key EQ reftable.rec_key:
        
        IF e-item-vend.underWidth EQ 0 THEN ASSIGN 
            e-item-vend.underWidth = reftable.val[1].
        IF e-item-vend.underLength EQ 0 THEN ASSIGN 
            e-item-vend.underWidth = reftable.val[2].
        IF e-item-vend.underWidthCost EQ 0 THEN ASSIGN 
            e-item-vend.underWidth = reftable.val[3].
        IF e-item-vend.underLengthCost EQ 0 THEN ASSIGN 
            e-item-vend.underWidth = reftable.val[4].
        ASSIGN  
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.

PROCEDURE e-itemfg-vend.markup:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable e-itemfg-vend    
{&CommonCode}
        
    FOR EACH e-itemfg-vend EXCLUSIVE WHERE 
        e-itemfg-vend.est-no  EQ "" AND 
        e-itemfg-vend.company EQ reftable.company AND 
        e-itemfg-vend.i-no    EQ reftable.loc AND 
        e-itemfg-vend.vend-no EQ reftable.code:
        
        IF e-itemfg-vend.markup EQ 0 THEN ASSIGN 
            e-itemfg-vend.markup = reftable.val[1]        
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE e-itemfg-vend.std-uom:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable e-itemfg-vend    
{&CommonCode}
        
    FOR EACH e-itemfg-vend EXCLUSIVE WHERE 
        e-itemfg-vend.est-no   EQ reftable.code AND 
        e-itemfg-vend.company  EQ reftable.company AND 
        e-itemfg-vend.form-no  EQ reftable.val[1] AND 
        e-itemfg-vend.blank-no EQ reftable.val[2] AND 
        e-itemfg-vend.est-no   NE "":
        
        IF e-itemfg-vend.std-uom EQ "" THEN ASSIGN 
            e-itemfg-vend.std-uom = reftable.code2
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE est/getqty.w2:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable est    
{&CommonCode}
        
    FOR EACH est EXCLUSIVE WHERE 
        est.est-no  EQ reftable.code AND 
        est.company EQ reftable.company:
        
        IF est.markupPct EQ 0 THEN ASSIGN 
            est.markupPct = reftable.val[1] 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE est/globquot.w:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE FACTORED:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable itemfg    
{&CommonCode}
        
    FOR EACH itemfg EXCLUSIVE WHERE 
        itemfg.company EQ reftable.company AND 
        itemfg.i-no    EQ reftable.code:
        
        IF itemfg.factored EQ NO THEN ASSIGN 
            itemfg.factored = IF reftable.code2 = "YES" THEN YES ELSE NO
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE fg-bin.cost:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable fg-rdtlh    
{&CommonCode}
        
    FOR EACH fg-rdtlh EXCLUSIVE WHERE 
        fg-rdtlh.rec_key EQ reftable.rec_key:
        
        IF fg-rdtlh.avg-cost     EQ 0 THEN ASSIGN 
            fg-rdtlh.avg-cost = reftable.val[1].
        IF fg-rdtlh.last-cost    EQ 0 THEN ASSIGN
            fg-rdtlh.last-cost = reftable.val[2].
        IF fg-rdtlh.std-fix-cost EQ 0 THEN ASSIGN
            fg-rdtlh.std-fix-cost = reftable.val[3].
        IF fg-rdtlh.std-lab-cost EQ 0 THEN ASSIGN
            fg-rdtlh.std-lab-cost = reftable.val[4].
        IF fg-rdtlh.std-mat-cost EQ 0 THEN ASSIGN
            fg-rdtlh.std-mat-cost = reftable.val[5].
        IF fg-rdtlh.std-tot-cost EQ 0 THEN ASSIGN
            fg-rdtlh.std-tot-cost = reftable.val[6].
        IF fg-rdtlh.std-var-cost EQ 0 THEN ASSIGN
            fg-rdtlh.std-var-cost = reftable.val[7].
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE fg-rctd.use-job:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable fg-rctd    
{&CommonCode}
        
    FOR EACH fg-rctd EXCLUSIVE WHERE 
        fg-rctd.r-no EQ INT(reftable.company):
        
        IF fg-rctd.use-job EQ NO THEN ASSIGN 
            fg-rctd.use-job = IF reftable.val[1] EQ 1 THEN TRUE ELSE FALSE 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE fg-rctd.user-id:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable fg-rctd    
{&CommonCode}
        
    FOR EACH fg-rctd EXCLUSIVE WHERE 
        fg-rctd.r-no EQ INT(reftable.loc):
        
        IF reftable.dscr BEGINS "fg-rctd:" THEN DO:
            ASSIGN 
                fg-rctd.SetHeaderRno = INTEGER(SUBSTRING(reftable.dscr, 10, 10)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN ASSIGN 
                fg-rctd.SetHeaderRno = 0.
        END.
        ELSE ASSIGN
            fg-rctd.CreateInvoice      = reftable.val[1] NE 0
            fg-rctd.BillableFreightAmt = reftable.val[2]
            fg-rctd.EmailBOL           = reftable.val[3] EQ 1
            fg-rctd.InvoiceFreight     = reftable.val[2] NE 0
            fg-rctd.spare-char-1       = reftable.dscr /* tag # */
            .                        
            
        IF fg-rctd.created-by EQ "" THEN ASSIGN 
            fg-rctd.created-by = reftable.code.
        IF fg-rctd.updated-by EQ "" THEN ASSIGN 
            fg-rctd.updated-by = reftable.code2.
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE FGSTATUS:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE FGTAXABLE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE Flute:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable flute    
{&CommonCode}
        
    FOR EACH flute EXCLUSIVE WHERE 
        flute.code = reftable.code:
        
        ASSIGN
            flute.thickness = IF flute.thickness = 0 AND reftable.val[1] NE 0 THEN reftable.val[1] ELSE flute.thickness
            flute.class = IF flute.class = "" AND reftable.code2 NE "" THEN reftable.code2 ELSE flute.class
            flute.dscr = IF flute.dscr = "" AND reftable.dscr NE "" THEN reftable.dscr ELSE flute.dscr
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE FREEZENOTE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable job-hdr    
{&CommonCode}
        
    FOR EACH job-hdr EXCLUSIVE WHERE 
        job-hdr.company EQ reftable.company AND 
        job-hdr.job-no  EQ reftable.loc AND 
        job-hdr.job-no2 EQ INT(reftable.code):
        
        ASSIGN 
            job-hdr.freezeNote = YES
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE gl-rpt.pct-subtotal:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable gl-rpt    
{&CommonCode}
        
    FOR EACH gl-rpt EXCLUSIVE WHERE 
        gl-rpt.company EQ reftable.company AND 
        gl-rpt.rp      EQ reftable.code AND 
        gl-rpt.line    EQ INT(reftable.code2):
        
        IF gl-rpt.pct-subtotal EQ NO THEN ASSIGN 
            gl-rpt.pct-subtotal = reftable.val[1] EQ 1
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE GLACCTDISC:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable account    
{&CommonCode}
        
    FOR EACH account EXCLUSIVE WHERE 
        account.company EQ reftable.company AND 
        account.actnum  EQ reftable.code:
        
        IF account.terms-discount EQ NO THEN ASSIGN 
            account.terms-discount = IF reftable.val[1] = 1 THEN YES ELSE NO                
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE gsa-fm:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable probe    
{&CommonCode}
        
    FOR EACH probe EXCLUSIVE WHERE 
        probe.company EQ reftable.company AND 
        probe.est-no  EQ reftable.code:
        
        IF probe.gsa-fm EQ "" THEN ASSIGN 
            probe.gsa-fm = string(reftable.val[1]) 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE HM1:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable user-print    
{&CommonCode}
        
    DEFINE VARIABLE iHm1Count AS INT NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK WHERE 
        user-print.company EQ reftable.loc AND 
        user-print.program-id EQ "HM1" AND 
        user-print.user-id EQ reftable.company 
        NO-ERROR.

    IF NOT AVAIL user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company        = reftable.loc 
            user-print.program-id     = "HM1" 
            user-print.user-id        = reftable.company 
            user-print.field-value[1] = reftable.CODE
            iHm1Count = 1 
            iProcessCount = iProcessCount + 1.
    END. 
    ELSE 
    DO: 
        ASSIGN 
            iHm1Count = iHm1Count + 1 . 
        IF iHm1Count LT 100 THEN ASSIGN 
            user-print.field-value[iHm1Count] = reftable.CODE
            iProcessCount = iProcessCount + 1.    
    END.

END PROCEDURE.


PROCEDURE HM1Acct:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable user-print    
{&CommonCode}
        
    DEFINE VARIABLE iHmAcctCount AS INT NO-UNDO.
    
    FIND FIRST user-print EXCLUSIVE-LOCK WHERE 
        user-print.company EQ reftable.loc AND 
        user-print.program-id EQ "HM1Acct" AND 
        user-print.user-id EQ reftable.company 
        NO-ERROR.

    IF NOT AVAIL user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company        = reftable.loc 
            user-print.program-id     = "HM1Acct" 
            user-print.user-id        = reftable.company 
            user-print.field-value[1] = reftable.CODE 
            iHmAcctCount = 1
            iProcessCount = iProcessCount + 1. 
    END. 
    ELSE DO:
        ASSIGN 
            iHmAcctCount = iHmAcctCount + 1 . 
        IF iHmAcctCount LT 100 THEN ASSIGN 
            user-print.field-value[iHmAcctCount] = reftable.CODE
            iProcessCount = iProcessCount + 1.    
    END.

END PROCEDURE.


PROCEDURE HM1SF:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable user-print    
{&CommonCode}
        
    DEFINE VARIABLE iHm1SFCount AS INT NO-UNDO.

    FIND FIRST user-print EXCLUSIVE-LOCK WHERE 
        user-print.program-id EQ "HM1SF" AND 
        user-print.user-id EQ reftable.company 
        NO-ERROR.

    IF NOT AVAIL user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.program-id     = "HM1SF" 
            user-print.user-id        = reftable.company 
            user-print.field-value[1] = reftable.loc
            iHm1SFCount = 1
            iProcessCount = iProcessCount + 1. 
    END. 
    ELSE DO: 
        ASSIGN 
            iHm1SFCount = iHm1SFCount + 1 .
        IF iHm1SFCount LT 100 THEN ASSIGN 
            user-print.field-value[iHm1SFCount] = reftable.loc
            iProcessCount = iProcessCount + 1.
    END.

END PROCEDURE.


PROCEDURE HM5:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable user-print    
{&CommonCode}
        
    DEFINE VARIABLE iHmCount AS INT NO-UNDO.

    FIND FIRST user-print EXCLUSIVE-LOCK WHERE 
        user-print.company EQ reftable.loc AND 
        user-print.program-id EQ "HM5" AND 
        user-print.user-id EQ reftable.company NO-ERROR.

    IF NOT AVAIL user-print THEN DO:
        CREATE user-print.
        ASSIGN
            user-print.company        = reftable.loc 
            user-print.program-id     = "HM5" 
            user-print.user-id        = reftable.company 
            user-print.field-value[1] = reftable.CODE
            iHmCount = 1 
            iProcessCount = iProcessCount + 1.
    END. 
    ELSE DO: 
        ASSIGN 
            iHmCount = iHmCount + 1 
            user-print.field-value[iHmCount] = reftable.CODE
            iProcessCount = iProcessCount + 1.
    END.

END PROCEDURE.


PROCEDURE inv-line.lot-no:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE itemfg-ink.occurs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable itemfg-ink
{&CommonCode}
        
    FOR EACH itemfg-ink EXCLUSIVE WHERE 
        itemfg-ink.rec_key EQ reftable.rec_key:
        
        IF itemfg-ink.occurs EQ 0 THEN ASSIGN 
            itemfg-ink.occurs = reftable.val[1]
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE itemfg.exempt-disc:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE JDEDWARDCUST#:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable shipto    
{&CommonCode}
        
    FOR EACH shipto EXCLUSIVE WHERE 
        shipto.company EQ reftable.company AND 
        shipto.cust-no EQ reftable.code AND 
        shipto.ship-id EQ reftable.code2:
        
        IF shipto.exportCustID EQ "" THEN ASSIGN 
            shipto.exportCustID = reftable.dscr
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE job.close-checked:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE job.create-time:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable job    
{&CommonCode}
        
    FOR EACH job EXCLUSIVE WHERE 
        job.company EQ reftable.company AND 
        job.loc     EQ "" AND 
        job.job     EQ INT(reftable.code):
        
        IF job.create-time EQ 0 THEN ASSIGN 
            job.create-time = reftable.val[1]
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE job.qty-changed:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable job    
{&CommonCode}
        
    FOR EACH job EXCLUSIVE WHERE 
        job.company EQ reftable.company AND 
        job.job     EQ INT(reftable.code):
        
        ASSIGN 
            job.qty-changed = YES
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE MACH-CREW:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable mach    
{&CommonCode}
        
    FOR EACH mach EXCLUSIVE WHERE 
        mach.company EQ reftable.company AND 
        mach.loc     EQ reftable.loc AND 
        mach.m-code  EQ reftable.code:
        
        IF reftable.code2 = "M R-QTY" THEN DO iCtr = 1 TO 13:
            IF mach.mr-crusiz-qty[iCtr] EQ 0 THEN ASSIGN 
                mach.mr-crusiz-qty[iCtr] = reftable.val[iCtr].
        END. 
        
        IF reftable.code2 = "M R-CST" THEN DO iCtr = 1 TO 13:
            IF mach.mr-crusiz-qty[iCtr] EQ 0 THEN ASSIGN 
                mach.mr-crusiz-cst[iCtr] = reftable.val[iCtr].
        END. 
        
        IF reftable.code2 = "RUN-QTY" THEN DO iCtr = 1 TO 13:
            IF mach.mr-crusiz-qty[iCtr] EQ 0 THEN ASSIGN 
                mach.run-crusiz-qty[iCtr] = reftable.val[iCtr].
        END. 
        
        IF reftable.code2 = "RUN-CST" THEN DO iCtr = 1 TO 13:
            IF mach.mr-crusiz-qty[iCtr] EQ 0 THEN ASSIGN 
                mach.run-crusiz-cst[iCtr] = reftable.val[iCtr].
        END. 
        
        ASSIGN     
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE Mach.obsolete:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable mach    
{&CommonCode}
        
    FOR EACH mach EXCLUSIVE WHERE 
        mach.company EQ reftable.company AND 
        mach.loc     EQ reftable.loc AND
        mach.m-code  EQ reftable.code:
        
        IF mach.obsolete EQ NO THEN ASSIGN 
            mach.obsolete = reftable.val[1] EQ 1
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE mach.plain-jobs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable mach    
{&CommonCode}
        
    FOR EACH mach EXCLUSIVE WHERE 
        mach.company EQ reftable.company AND 
        mach.loc     EQ reftable.loc AND 
        mach.m-code  EQ reftable.code:
        
        IF mach.plain-job EQ NO THEN ASSIGN 
            mach.plain-job = reftable.val[1] EQ 1
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE MachinePosition:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable est-op    
{&CommonCode}
        
    FOR EACH est-op EXCLUSIVE WHERE 
        est-op.company EQ SUBSTRING(reftable.company,1,10) AND 
        est-op.est-no  EQ SUBSTRING(reftable.company,11,10) AND 
        est-op.LINE    EQ INT(reftable.loc):
        
        IF est-op.len-pos EQ 0 THEN ASSIGN 
            est-op.len-pos = int(reftable.val[1]).
        IF est-op.len EQ 0 THEN ASSIGN 
            est-op.len = DECIMAL(reftable.val[4]).
        IF est-op.wid-pos EQ 0 THEN ASSIGN 
            est-op.wid-pos = INTEGER(reftable.val[2]).
        IF est-op.wid EQ 0 THEN ASSIGN 
            est-op.wid = DECIMAL(reftable.val[5]).
        IF est-op.dep-pos EQ 0 THEN ASSIGN 
            est-op.dep-pos = INTEGER(reftable.val[3]).
        IF est-op.dep EQ 0 THEN ASSIGN 
            est-op.dep = DECIMAL(reftable.val[6]).
        ASSIGN  
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE msf-limit:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable truck    
{&CommonCode}
    
    FOR EACH truck EXCLUSIVE WHERE 
        truck.carrier    EQ reftable.code AND 
        truck.company    EQ reftable.company AND 
        truck.loc        EQ reftable.loc AND 
        truck.truck-code EQ reftable.code2:
        
        IF truck.msfLimit EQ 0 THEN ASSIGN 
            truck.msfLimit = reftable.val[1] 
            iProcessCount  = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE NextRelease#:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE NextRelh:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE oe-boll.lot-no:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-boll   
{&CommonCode}
        
    FOR EACH oe-boll EXCLUSIVE WHERE 
        oe-boll.rec_key EQ reftable.rec_key:
        
        IF oe-boll.lot-no   EQ "" THEN ASSIGN 
            oe-boll.lot-no   = reftable.code.
        IF oe-boll.frt-pay  EQ "" THEN ASSIGN 
            oe-boll.frt-pay  = reftable.code2.
        IF oe-boll.fob-code EQ "" THEN ASSIGN 
            oe-boll.fob-code = reftable.dscr.
        ASSIGN 
            iProcessCount    = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe-boll.selected:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE oe-boll.sell-price:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-boll    
{&CommonCode}
        
    FOR EACH oe-boll EXCLUSIVE WHERE 
        oe-boll.rec_key EQ reftable.rec_key:
        
        IF oe-boll.sell-price EQ 0 THEN ASSIGN 
            oe-boll.sell-price = reftable.val[1].
        IF oe-boll.zeroPrice EQ 0 THEN ASSIGN 
            oe-boll.zeroPrice = reftable.val[2].
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe-ord.close-checked:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE oe-ord.whs-order:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-ord    
{&CommonCode}
        
    FOR EACH oe-ord EXCLUSIVE WHERE 
        oe-ord.company EQ reftable.company AND 
        oe-ord.ord-no  EQ INT(reftable.loc):
        
        IF oe-ord.managed EQ NO THEN ASSIGN 
            oe-ord.managed = (reftable.val[1] EQ 1)
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe-ordl.q-no:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-ordl    
{&CommonCode}
        
    FOR EACH oe-ordl EXCLUSIVE WHERE 
        oe-ordl.company EQ reftable.company AND 
        oe-ordl.ord-no  EQ INT(reftable.loc) AND 
        oe-ordl.i-no    EQ reftable.code AND 
        oe-ordl.line    EQ INT(reftable.code2):
        
        IF oe-ordl.q-no EQ 0 THEN ASSIGN 
            oe-ordl.q-no = INT(reftable.val[1] EQ 1) 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe-ordl.whs-item:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-ordl    
{&CommonCode}
        
    FOR EACH oe-ordl EXCLUSIVE WHERE 
        oe-ordl.company EQ reftable.company AND 
        oe-ordl.ord-no  EQ INT(reftable.loc) AND 
        oe-ordl.i-no    EQ reftable.code AND 
        oe-ordl.line    EQ INT(reftable.code2):
        
        IF oe-ordl.managed EQ NO THEN ASSIGN 
            oe-ordl.managed = (reftable.val[1] EQ 1)
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe-rel.job:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-rel    
{&CommonCode}
        
    FOR EACH oe-rel EXCLUSIVE WHERE 
        oe-rel.company EQ reftable.company AND 
        oe-rel.r-no    EQ INT(reftable.code):
        
        IF oe-rel.job EQ 0 THEN ASSIGN 
            oe-rel.job = INT(reftable.code2)
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe-rel.lot-no:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-rel    
{&CommonCode}
        
    FOR EACH oe-rel EXCLUSIVE WHERE 
        oe-rel.r-no EQ INT(reftable.company):
        
        IF oe-rel.lot-no    EQ "" THEN ASSIGN 
            oe-rel.lot-no = reftable.code.
        IF oe-rel.frt-pay   EQ "" THEN ASSIGN 
            oe-rel.frt-pay = reftable.code2.
        IF oe-rel.fob-code  EQ "" THEN ASSIGN 
            oe-rel.fob-code = reftable.dscr.
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe-rel.s-code:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-rel    
{&CommonCode}
        
    FOR EACH oe-rel EXCLUSIVE WHERE 
        oe-rel.r-no EQ INT(reftable.company):
        
        ASSIGN 
            oe-rel.s-code = reftable.code 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe-rel.sell-price:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-rel    
{&CommonCode}
        
    FOR EACH oe-rel EXCLUSIVE WHERE 
        oe-rel.r-no EQ INT(reftable.company):
        
        IF oe-rel.sell-price EQ 0 THEN ASSIGN 
            oe-rel.sell-price = reftable.val[1].
        IF oe-rel.zeroPrice  EQ 0 THEN ASSIGN 
            oe-rel.zeroPrice = reftable.val[2].
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe-relh.can-print:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE oe-rell.lot-no:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-rell    
{&CommonCode}
        
    FOR EACH oe-rell EXCLUSIVE WHERE 
        oe-rell.rec_key EQ reftable.rec_key:
        
        IF oe-rell.lot-no EQ "" THEN ASSIGN 
            oe-rell.lot-no = reftable.code.
        IF oe-rell.frt-pay EQ "" THEN ASSIGN 
            oe-rell.frt-pay = reftable.code2.
        IF oe-rell.fob-code EQ "" THEN ASSIGN 
            oe-rell.fob-code = reftable.dscr.
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe-rell.selected:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE oe-rell.sell-price:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-rell    
{&CommonCode}
        
    FOR EACH oe-rell EXCLUSIVE WHERE 
        oe-rell.rec_key EQ reftable.rec_key:
        
        IF oe-rell.newSellPrice EQ 0 THEN ASSIGN 
            oe-rell.newSellPrice = reftable.val[1].
        IF oe-rell.newZeroPrice EQ 0 THEN ASSIGN 
            oe-rell.newZeroPrice = reftable.val[2].
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE oe/ordlmisc.p:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable oe-ordm    
{&CommonCode}
        
    FOR EACH oe-ordm EXCLUSIVE WHERE 
        oe-ordm.company EQ reftable.company AND 
        oe-ordm.ord-no  EQ INT(reftable.loc) AND 
        oe-ordm.line    EQ INT(reftable.code) AND 
        oe-ordm.charge  EQ reftable.code2:
        
        IF oe-ordm.miscType EQ 0 THEN ASSIGN 
            oe-ordm.miscType = INT(reftable.val[1]).
        IF oe-ordm.estPrepEqty EQ 0 THEN ASSIGN 
            oe-ordm.estPrepEqty = reftable.val[2].
        IF oe-ordm.estPrepLine EQ 0 THEN ASSIGN 
            oe-ordm.estPrepLine = INT(reftable.val[3]).
        IF oe-ordm.miscInd EQ "" THEN ASSIGN 
            oe-ordm.miscInd = STRING(reftable.val[4]).
        IF oe-ordm.est-no EQ "" THEN ASSIGN 
            oe-ordm.est-no = reftable.dscr.
        ASSIGN  
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE ORDERPO:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable job-mat    
{&CommonCode}
        
    FOR EACH job NO-LOCK WHERE 
        job.company EQ reftable.company AND 
        job.job     EQ INT(SUBSTRING(reftable.CODE,1,10)):
        
        FOR EACH job-mat EXCLUSIVE WHERE 
            job-mat.company  EQ job.company AND
            job-mat.job      EQ job.job AND  
            job-mat.job-no   EQ job.job-no AND 
            job-mat.job-no2  EQ job.job-no2 AND 
            job-mat.rm-i-no  EQ reftable.CODE2 AND 
            job-mat.frm      EQ INT(SUBSTRING(reftable.CODE,11,10)):
        
            IF job-mat.po-no EQ 0 THEN ASSIGN 
                job-mat.po-no = INT(reftable.val[1]) 
                iProcessCount = iProcessCount + 1.
        
        END.
    END.

END PROCEDURE.


PROCEDURE POORDLDEPTH:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable po-ordl    
{&CommonCode}
        
    FOR EACH po-ordl EXCLUSIVE WHERE 
        po-ordl.company EQ reftable.company AND 
        po-ordl.i-no    EQ reftable.loc AND 
        po-ordl.vend-no EQ reftable.code:
        
        IF po-ordl.s-dep EQ 0 THEN ASSIGN 
            po-ordl.s-dep = INT(reftable.code2)
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE POUserid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable po-ord    
{&CommonCode}
        
    FOR EACH po-ord EXCLUSIVE WHERE 
        po-ord.company EQ reftable.company AND 
        po-ord.po-no   EQ INT(reftable.loc) :
        
        IF po-ord.user-id EQ "" THEN ASSIGN 
            po-ord.user-id = reftable.code
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE PREPCADFILE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable prep    
{&CommonCode}
        
    FOR EACH prep EXCLUSIVE WHERE 
        prep.rec_key = reftable.rec_key:
        
        IF prep.cadNo EQ "" THEN ASSIGN 
            prep.cadNo = reftable.code.
        IF prep.fileNo EQ "" THEN ASSIGN 
            prep.fileNo = reftable.code2.
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE PREPLASTJOB:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable prep    
{&CommonCode}
        
    FOR EACH prep EXCLUSIVE WHERE 
        prep.company EQ reftable.company AND 
        prep.loc     EQ reftable.loc AND 
        prep.Code    EQ reftable.code:
        
        IF prep.last-job-no EQ "" THEN ASSIGN 
            prep.last-job-no = reftable.code2.
        IF prep.last-job-no2 EQ 0 THEN ASSIGN 
            prep.last-job-no2 = reftable.val[1].
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE print42:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE probe.board:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable probe    
{&CommonCode}
        
    FOR EACH probe EXCLUSIVE WHERE 
        probe.company EQ reftable.company AND 
        probe.est-no  EQ reftable.code AND 
        probe.line    EQ INT(reftable.code2):
        
        IF probe.boardCostTotal EQ 0 THEN ASSIGN 
            probe.boardCostTotal = reftable.val[1].
        IF probe.boardCostPerM EQ 0 THEN ASSIGN 
            probe.boardCostPerM = reftable.val[2].  
        IF probe.boardCostPct EQ 0 THEN ASSIGN 
            probe.boardCostPct = reftable.val[3].
        IF probe.boardContributionPerM EQ 0 THEN ASSIGN 
            probe.boardContributionPerM = reftable.val[4].
        IF probe.boardContributionTotal EQ 0 THEN ASSIGN 
            probe.boardContributionTotal = reftable.val[5].
        IF probe.manHoursTotal EQ 0 THEN ASSIGN 
            probe.manHoursTotal = reftable.val[6].
        IF probe.directMaterialCost EQ 0 THEN ASSIGN 
            probe.directMaterialCost = reftable.val[7].
        IF probe.grossProfitPerM EQ 0 THEN ASSIGN 
            probe.grossProfitPerM = reftable.val[8].
        IF probe.grossProfitPerManhourAssemb EQ 0 THEN ASSIGN 
            probe.grossProfitPerManhourAssemb = reftable.val[9].
        IF probe.grossProfitPerManHourOther EQ 0 THEN ASSIGN 
            probe.grossProfitPerManHourOther = reftable.val[10].
        IF probe.grossProfitPctTemp EQ 0 THEN ASSIGN 
            probe.grossProfitPctTemp = reftable.val[11].
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE probe.per-msf:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable probe    
{&CommonCode}
        
    FOR EACH probe EXCLUSIVE WHERE 
        probe.company EQ reftable.company AND 
        probe.est-no  EQ reftable.code AND 
        probe.line    EQ INT(reftable.code2):
        
        IF probe.per-msf EQ 0 THEN ASSIGN
            probe.per-msf = reftable.val[1].
        IF probe.setup EQ 0 THEN ASSIGN
            probe.setup = reftable.val[2].
        ASSIGN                     
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE relcredconv:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE rm-bin.age-date:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable rm-bin    
{&CommonCode}
        
    FOR EACH rm-bin EXCLUSIVE WHERE 
        rm-bin.company EQ reftable.company AND 
        rm-bin.loc     EQ SUBSTRING(reftable.code, 1, 50) AND 
        rm-bin.i-no    EQ reftable.loc AND 
        rm-bin.loc-bin EQ SUBSTRING(reftable.code, 51, 50) AND 
        rm-bin.tag     EQ reftable.code2:
        
        IF rm-bin.aging-date EQ ? THEN ASSIGN 
            rm-bin.aging-date = DATE(INT(reftable.val[1])) 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE rm-rctd.user-id:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE rm/rmglobpr.w:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE SALETOOL:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE score-type:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable scoreType    
{&CommonCode}
        
    FOR EACH scoreType EXCLUSIVE WHERE 
        scoreType.company   EQ reftable.company AND 
        scoreType.scoreType EQ reftable.code:
        
        ASSIGN 
            scoreType.company     = reftable.company
            scoreType.scoreType   = reftable.code
            scoreType.description = reftable.dscr
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE ShiftDays:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable shifts    
{&CommonCode}
        
    FOR EACH shifts EXCLUSIVE WHERE 
        shifts.rec_key EQ reftable.code:
        
        IF shifts.useSpecificDays EQ NO THEN ASSIGN 
            shifts.useSpecificDays = IF reftable.loc = "1" THEN YES ELSE NO.
        IF shifts.dayList EQ "" THEN ASSIGN 
            shifts.dayList = reftable.code2.
        ASSIGN                 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE ship-to.mandatory-tax:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable shipto    
{&CommonCode}
        
    FOR EACH shipto EXCLUSIVE WHERE 
        shipto.company EQ reftable.company AND 
        shipto.cust-no EQ reftable.code AND 
        shipto.ship-id EQ reftable.code2:
        
        IF shipto.tax-mandatory EQ NO THEN ASSIGN 
            shipto.tax-mandatory = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE SPECSAMP:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable est    
{&CommonCode}
        
    FOR EACH est EXCLUSIVE WHERE 
        est.company EQ reftable.company AND 
        est.est-no  EQ reftable.code:
        
        IF est.sampleNum EQ 0 THEN ASSIGN 
            est.sampleNum = INT(reftable.code2)
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE SPLITSHIP:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable job-hdr    
{&CommonCode}
        
    FOR EACH job-hdr EXCLUSIVE WHERE 
        job-hdr.company EQ reftable.company AND 
        job-hdr.job-no  EQ reftable.loc AND 
        job-hdr.job-no2 EQ INT(reftable.code):
        
        ASSIGN
            job-hdr.splitShip = YES    
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE SPLITSHP:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable job-hdr    
{&CommonCode}
        
    FOR EACH job-hdr EXCLUSIVE WHERE 
        job-hdr.company EQ reftable.company AND 
        job-hdr.job-no  EQ reftable.loc AND 
        job-hdr.job-no2 EQ INT(reftable.code):
        
        ASSIGN 
            job-hdr.splitShip = YES
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE STACK:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable stackpattern    
{&CommonCode}
        
    FOR EACH stackpattern EXCLUSIVE WHERE 
        stackPattern.stackCode  EQ reftable.code:
        
        ASSIGN 
            stackPattern.stackCode        = reftable.code
            stackPattern.rec_key          = reftable.rec_key
            stackPattern.stackDescription = reftable.dscr
            stackPattern.stackCount       = reftable.val[1]     
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE style.per-msf:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable style    
{&CommonCode}
        
    FOR EACH style EXCLUSIVE WHERE 
        style.company EQ reftable.company AND 
        style.style   EQ reftable.code:
        
        IF style.sqft-len-trim = 0 THEN ASSIGN 
            style.sqft-len-trim = reftable.val[1].
        IF style.sqft-wid-trim = 0 THEN ASSIGN 
            style.sqft-wid-trim = reftable.val[2].
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE terms.cod:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable terms    
{&CommonCode}
        
    FOR EACH terms EXCLUSIVE WHERE 
        terms.company EQ reftable.company AND 
        terms.t-code  EQ reftable.code  :
        
        IF terms.cod EQ NO THEN ASSIGN 
            terms.cod = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE trp-car:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable truck-run-print    
{&CommonCode}
        
    FOR EACH truck-run-print EXCLUSIVE WHERE 
        truck-run-print.rec_key EQ reftable.rec_key:
        
        IF truck-run-print.carrier EQ "" THEN ASSIGN 
            truck-run-print.carrier = reftable.code
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE users.fax-cnty:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable users    
{&CommonCode}
        
    FOR EACH users EXCLUSIVE WHERE 
        users.user_id EQ reftable.company:
        
        ASSIGN
            users.fax-cnty = IF users.fax-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax-cnty
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE users.fax-no:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable users    
{&CommonCode}
        
    FOR EACH users EXCLUSIVE WHERE 
        users.user_id EQ reftable.company :
        
        ASSIGN
            users.fax = IF users.fax = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE users.phone-cnty:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable users    
{&CommonCode}
        
    FOR EACH users EXCLUSIVE WHERE 
        users.user_id EQ reftable.company:
        
        ASSIGN
            users.phone-cnty = IF users.phone-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.phone-cnty 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE users.phone-no:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable users    
{&CommonCode}
        
    FOR EACH users EXCLUSIVE WHERE 
        users.user_id EQ reftable.company:
        
        ASSIGN
            users.phone = IF users.phone = "" AND reftable.CODE = "" THEN reftable.CODE ELSE users.phone
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE users.user-docs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable users    
{&CommonCode}
        
    FOR EACH users EXCLUSIVE WHERE 
        users.user_id EQ reftable.company:
        
        ASSIGN
            users.showOnPO    = IF users.showOnPO = TRUE OR reftable.val[1] = 1 THEN TRUE ELSE FALSE
            users.showOnBOL   = IF users.showOnBOL = TRUE OR reftable.val[2] = 1 THEN TRUE ELSE FALSE
            users.showOnInv   = IF users.showOnInv = TRUE OR reftable.val[3] = 1 THEN TRUE ELSE FALSE
            users.showOnAck   = IF users.showOnAck = TRUE OR reftable.val[4] = 1 THEN TRUE ELSE FALSE
            users.showOnQuote = IF users.showOnQuote = TRUE OR reftable.val[5] = 1 THEN TRUE ELSE FALSE
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE v10-TaxCode-Upgrade:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable reftable    
{&CommonCode}
    /* This reftable has no effect on the "normal" files */
    ASSIGN 
        iProcessCount = iProcessCount + 1.    

END PROCEDURE.


PROCEDURE vend-cost:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable e-item-vend
{&CommonCode}
        
    FOR EACH e-item-vend EXCLUSIVE WHERE 
        e-item-vend.i-no    EQ reftable.code AND 
        e-item-vend.company EQ reftable.company AND 
        e-item-vend.vend-no EQ reftable.code2:
        
        DO iCtr = 1 to 10: 
            IF e-item-vend.runCostXtra[iCtr] EQ 0 THEN ASSIGN 
                e-item-vend.runCostXtra[iCtr] = reftable.val[iCtr]. 
        END.
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE vend-qty:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable e-item    
{&CommonCode}
        
    FOR EACH e-item EXCLUSIVE WHERE 
        e-item.i-no    EQ reftable.code AND 
        e-item.company EQ reftable.company:
        
        DO iCtr = 1 to 13: 
            IF e-item.runQty[iCtr] EQ 0 THEN ASSIGN 
                e-item.runQty[iCtr] = reftable.val[iCtr]. 
        END.
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.


PROCEDURE vend-setup:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
&SCOPED-DEFINE cTable e-item-vend    
{&CommonCode}
        
    FOR EACH e-item-vend EXCLUSIVE WHERE 
        e-item-vend.i-no    EQ reftable.code AND 
        e-item-vend.company EQ reftable.company AND 
        e-item-vend.vend-no EQ reftable.code2:
        
        DO iCtr = 1 to 10: 
            IF e-item-vend.setupsXtra[iCtr] EQ 0 THEN ASSIGN 
                e-item-vend.setupsXtra[iCtr] = reftable.val[iCtr]. 
        END.
        ASSIGN 
            iProcessCount = iProcessCount + 1.

    END.

END PROCEDURE.

