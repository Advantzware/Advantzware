
/*------------------------------------------------------------------------
    File        : FGBinBuild.p
    Purpose     : 

    Syntax      :

    Description : Builds Bins from history based on a given as of date.
                  Also has argument to purge history records processed

    Author(s)   : BV
    Created     : Thu Feb 14 13:53:01 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{fg/ttFGBins.i "SHARED"}
DEFINE STREAM sExport1.
DEFINE STREAM sExport2.
DEFINE VARIABLE gcOutputFile1 AS CHARACTER.
DEFINE VARIABLE gcOutputFile2 AS CHARACTER.
DEFINE VARIABLE gcDefaultExportPath AS CHARACTER INITIAL "C:\Temp".
 
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
PROCEDURE BuildBinsForItemAndPurge:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper Proc that doesn't purge
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriFGItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcExportFilePath AS CHARACTER NO-UNDO.
    
    RUN pBuildBinsForItem(ipriFGItem, ipdtAsOf, INPUT-OUTPUT iopiCountOfProcessed, YES, ipcExportFilePath).
    
END PROCEDURE.

PROCEDURE BuildBinsForItem:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper Proc that doesn't purge
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriFGItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER NO-UNDO.
    
    RUN pBuildBinsForItem(ipriFGItem, ipdtAsOf, INPUT-OUTPUT iopiCountOfProcessed, NO, "").
    
END PROCEDURE.

PROCEDURE CreateCycleCountTransactions:
/*------------------------------------------------------------------------------
 Purpose: Given an AsOf Date, create Cycle Count Transactions for ttFGBins      
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.

DEFINE VARIABLE iNextRNo AS INTEGER NO-UNDO.

FOR EACH ttFGBins 
    WHERE ttFGBins.qty NE 0 NO-LOCK:
    DO TRANSACTION:  /*To complete Create Trigger processing*/
        
        FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAIL fg-rctd AND fg-rctd.r-no GT iNextRNo THEN iNextRNo = fg-rctd.r-no.
        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAIL fg-rcpth AND fg-rcpth.r-no GT iNextRNo THEN iNextRNo = fg-rcpth.r-no.
        iNextRNo = iNextRNo + 1.
        
        CREATE fg-rcpth.
        ASSIGN
            fg-rcpth.company = ttFGBins.company
            fg-rcpth.r-no = iNextRNo
            fg-rcpth.adjustmentCode = "PRG"
            fg-rcpth.spare-char-5 = "Added Through Purge"
            fg-rcpth.create-by = USERID("asi")
            fg-rcpth.i-name = ttFGBins.itemfgIName
            fg-rcpth.i-no = ttFGBins.i-no
            fg-rcpth.job-no = ttFGBins.job-no
            fg-rcpth.job-no2 = ttFGBins.job-no2
            fg-rcpth.loc = ttFGBins.loc
            fg-rcpth.po-no = ttFGBins.po-no
            fg-rcpth.post-date = ipdtAsOf
            fg-rcpth.pur-uom = ttFGBins.pur-uom
            fg-rcpth.rita-code = "C"
            fg-rcpth.trans-date = ipdtAsOf
            .
    
    END.
    CREATE fg-rdtlh.
    ASSIGN
        fg-rdtlh.company = fg-rcpth.company
        fg-rdtlh.rita-code = fg-rcpth.rita-code
        fg-rdtlh.r-no = fg-rcpth.r-no
        fg-rdtlh.i-no = fg-rcpth.i-no
      
        fg-rdtlh.cust-no = ttFGBins.cust-no
        fg-rdtlh.loc = ttFGBins.loc
        fg-rdtlh.loc-bin = ttFGBins.loc-bin
        fg-rdtlh.job-no = ttFGBins.job-no
        fg-rdtlh.job-no2 = ttFGBins.job-no2
        fg-rdtlh.last-cost = ttFGBins.std-tot-cost
        fg-rdtlh.avg-cost = ttFGBins.std-tot-cost
        fg-rdtlh.cost = ttFGBins.std-tot-cost
        fg-rdtlh.qty = ttFGBins.qty
        fg-rdtlh.units-pallet = ttFGBins.units-pallet
/*        fg-rdtlh.partial =*/
        fg-rdtlh.cases = ttFGBins.cases
        fg-rdtlh.qty-case = ttFGBins.qty-case
        fg-rdtlh.std-fix-cost = ttFGBins.std-fix-cost
        fg-rdtlh.std-lab-cost = ttFGBins.std-lab-cost
        fg-rdtlh.std-mat-cost = ttFGBins.std-mat-cost
        fg-rdtlh.std-tot-cost = ttFGBins.std-tot-cost
        fg-rdtlh.std-var-cost = ttFGBins.std-var-cost
        fg-rdtlh.tag = ttFGBins.tag
        fg-rdtlh.trans-date = ipdtAsOf
        fg-rdtlh.trans-time = 86399
        .
        
END.

END PROCEDURE.

PROCEDURE pBuildBinsForItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a FG Item, builds bins - will purge and export too 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriFGItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplPurge AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcExportPath AS CHARACTER NO-UNDO.

    
    IF iplPurge THEN DO:
        RUN pInitializeOutput(ipcExportPath, ipdtAsOf).
    END.
    FIND FIRST itemfg NO-LOCK
        WHERE ROWID(itemfg) EQ ipriFGItem
        NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company    EQ itemfg.company
            AND fg-rcpth.i-no       EQ itemfg.i-no
            AND fg-rcpth.trans-date LE ipdtAsOf
            /*
            AND STRING(FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                TRIM(fg-rcpth.job-no) + STRING(fg-rcpth.job-no2,"99"))
             GE ""
            AND STRING(FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                TRIM(fg-rcpth.job-no) + STRING(fg-rcpth.job-no2,"99"))
             LE "ZZZZZZZZZZZZ"
            */
            USE-INDEX tran,
            EACH fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
              /*
              AND fg-rdtlh.loc       GE ""
              AND fg-rdtlh.loc       LE "ZZZZZZZZZZZ"
              AND fg-rdtlh.loc-bin   GE ""
              AND fg-rdtlh.loc-bin   LE "ZZZZZZZZZZZ"
              AND fg-rdtlh.cust-no   EQ ""
              */
            BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no
            :
            FIND FIRST ttFGBins
                WHERE ttFGBins.company EQ fg-rcpth.company
                AND ttFGBins.i-no    EQ fg-rcpth.i-no
                AND ttFGBins.job-no  EQ fg-rcpth.job-no
                AND ttFGBins.job-no2 EQ fg-rcpth.job-no2
                AND ttFGBins.loc     EQ fg-rdtlh.loc
                AND ttFGBins.loc-bin EQ fg-rdtlh.loc-bin
                AND ttFGBins.tag     EQ fg-rdtlh.tag
                AND ttFGBins.cust-no EQ fg-rdtlh.cust-no
                NO-ERROR.
            IF NOT AVAILABLE ttFGBins THEN 
            DO:
                CREATE ttFGBins.
                ASSIGN
                    ttFGBins.company      = fg-rcpth.company
                    ttFGBins.job-no       = fg-rcpth.job-no
                    ttFGBins.job-no2      = fg-rcpth.job-no2
                    ttFGBins.loc          = fg-rdtlh.loc
                    ttFGBins.loc-bin      = fg-rdtlh.loc-bin
                    ttFGBins.tag          = fg-rdtlh.tag
                    ttFGBins.cust-no      = fg-rdtlh.cust-no
                    ttFGBins.i-no         = fg-rcpth.i-no
                    ttFGBins.po-no        = fg-rcpth.po-no
                    ttFGBins.aging-date   = fg-rcpth.trans-date
                    ttFGBins.pur-uom      = (IF fg-rcpth.pur-uom GT "" THEN fg-rcpth.pur-uom ELSE itemfg.prod-uom)
                    ttFGBins.std-tot-cost = fg-rdtlh.std-tot-cost
                    ttFGBins.std-mat-cost = fg-rdtlh.std-mat-cost
                    ttFGBins.std-lab-cost = fg-rdtlh.std-lab-cost
                    ttFGBins.std-var-cost = fg-rdtlh.std-var-cost
                    ttFGBins.std-fix-cost = fg-rdtlh.std-fix-cost
                    ttFGBins.rita-code    = fg-rcpth.rita-code
                    ttFGBins.itemFGCustNo = itemfg.cust-no
                    ttFGBins.itemFGINo    = itemfg.i-no
                    ttFGBins.itemFGPartNo = itemfg.part-no
                    ttFGBins.itemFGIName  = itemfg.i-name
                    .
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom  EQ itemfg.prod-uom
                    AND uom.mult NE 0
                    NO-ERROR.
                ttFGBins.uomMult = IF AVAILABLE uom THEN uom.mult ELSE ?.
                FIND FIRST fg-bin NO-LOCK
                    WHERE fg-bin.company EQ ttFGBins.company
                    AND fg-bin.i-no    EQ ttFGBins.i-no
                    AND fg-bin.job-no  EQ ttFGBins.job-no
                    AND fg-bin.job-no2 EQ ttFGBins.job-no2
                    AND fg-bin.loc     EQ ttFGBins.loc
                    AND fg-bin.loc-bin EQ ttFGBins.loc-bin
                    AND fg-bin.tag     EQ ttFGBins.tag
                    AND fg-bin.cust-no EQ ttFGBins.cust-no
                    NO-ERROR.
                IF AVAILABLE fg-bin THEN
                    ASSIGN
                        ttFGBins.std-tot-cost = fg-bin.std-tot-cost
                        ttFGBins.std-mat-cost = fg-bin.std-mat-cost
                        ttFGBins.std-lab-cost = fg-bin.std-lab-cost
                        ttFGBins.std-var-cost = fg-bin.std-var-cost
                        ttFGBins.std-fix-cost = fg-bin.std-fix-cost
                        .
            END.  /*create new ttFGBins*/
            IF ttFGBins.case-count LE 0 AND fg-rdtlh.qty-case GT 0 THEN
                ttFGBins.case-count = fg-rdtlh.qty-case.
            IF ttFGBins.units-pallet LE 0 AND fg-rdtlh.units-pallet GT 0 THEN
                ttFGBins.units-pallet = fg-rdtlh.units-pallet.
            IF ttFGBins.cases-unit LE 0 AND fg-rdtlh.stacks-unit GT 0 THEN
                ttFGBins.cases-unit = fg-rdtlh.stacks-unit.
            CASE fg-rcpth.rita-code:
                WHEN "S" OR 
                WHEN "s" THEN
                    ttFGBins.qty = ttFGBins.qty - fg-rdtlh.qty.
                WHEN "C" OR 
                WHEN "c" THEN 
                    ttFGBins.qty = fg-rdtlh.qty.
                WHEN "R" OR 
                WHEN "r" THEN 
                    DO:
                        ASSIGN 
                            ttFGBins.qty          = ttFGBins.qty + fg-rdtlh.qty
                            ttFGBins.aging-date   = fg-rcpth.trans-date
                            ttFGBins.units-pallet = fg-rdtlh.units-pallet
                            ttFGBins.cases        = fg-rdtlh.cases
                            ttFGBins.qty-case     = fg-rdtlh.qty-case
                            .
                        IF ttFGBins.aging-date EQ ? OR
                            ttFGBins.aging-date GT fg-rcpth.trans-date THEN
                            ttFGBins.aging-date = fg-rcpth.trans-date.
                    END.
                OTHERWISE 
                ttFGBins.qty = ttFGBins.qty + fg-rdtlh.qty.
            END CASE.
            IF ttFGBins.qty EQ 0 THEN 
            DO:
                DELETE ttFGBins.
            END.
            iopiCountOfProcessed = iopiCountOfProcessed + 1.
            IF iplPurge THEN DO:
                RUN pPurgeAndExport(BUFFER fg-rcpth, BUFFER fg-rdtlh).
            END.
        END.  /*each history record */
    END.
    IF iplPurge THEN DO:
         OUTPUT STREAM sExport1 CLOSE.
         OUTPUT STREAM sExport2 CLOSE.
    END.
        
END PROCEDURE.

PROCEDURE pInitializeOutput PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcExportPath AS CHARACTER.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE.
    
    IF ipcExportPath EQ "" THEN DO:
        OS-CREATE-DIR  VALUE(gcDefaultExportPath).
        ipcExportPath = gcDefaultExportPath.
    END.
    ASSIGN 
        gcOutputFile1 = ipcExportPath + "/fg-rcpth" + STRING(ipdtAsOf,"99-99-9999") + ".d"
        gcOutputFile2 = ipcExportPath + "/fg-rdtlh" + STRING(ipdtAsOf,"99-99-9999") + ".d"
        .
     OUTPUT STREAM sExport1 to VALUE(gcOutputFile1) APPEND.
     OUTPUT STREAM sExport2 to VALUE(gcOutputFile2) APPEND.
     
END PROCEDURE.

PROCEDURE pPurgeAndExport PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-fg-rcpth FOR fg-rcpth.
    DEFINE PARAMETER BUFFER ipbf-fg-rdtlh FOR fg-rdtlh.

    EXPORT STREAM sExport1 ipbf-fg-rcpth.
    EXPORT STREAM sExport2 ipbf-fg-rdtlh.
       
    FIND CURRENT ipbf-fg-rcpth EXCLUSIVE-LOCK.
    DELETE ipbf-fg-rcpth.

/*Delete of fg-rdtlh handled in delete trigger of fg-rctph*/
/*    FIND CURRENT ipbf-fg-rdtlh EXCLUSIVE-LOCK.*/
/*    DELETE ipbf-fg-rdtlh.                     */
    
END PROCEDURE.

