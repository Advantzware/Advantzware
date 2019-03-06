
/*------------------------------------------------------------------------
    File        : InventoryProcs.p
    Purpose     : 

    Syntax      :

    Description : All procedures for creating and printing Loadtags for FG, RM, and WIP

    Author(s)   : BV
    Created     : Sun Mar 03 18:31:30 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{Inventory/ttInventory.i SHARED}
DEFINE VARIABLE gcPreLoadtagStatus AS CHARACTER INITIAL "PreLoadtag".

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE CreatePreLoadtagsFromInputsFG:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE CreatePreLoadtagsFromInputsRM:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE CreatePreLoadtagsFromInputsWIP:
    /*------------------------------------------------------------------------------
     Purpose:  Given critical inputs for WIP process, generate the Pre-Loadtags
     for processing.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJobMch AS ROWID NO-UNDO.  /*Last Operation*/
    DEFINE INPUT PARAMETER ipriJobMat AS ROWID NO-UNDO.  /*Board Material*/
    DEFINE INPUT PARAMETER ipdQuantityTotal AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityPerSubUnit AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job-mch FOR job-mch.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-item FOR item.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
    FIND FIRST bf-job-mch NO-LOCK 
        WHERE ROWID(bf-job-mch) EQ ipriJobMch
        NO-ERROR.
    FIND FIRST bf-job-mat NO-LOCK 
        WHERE ROWID(bf-job-mat) EQ ipriJobMat
        NO-ERROR.
    IF AVAILABLE bf-job-mch AND AVAILABLE bf-job-mat THEN 
    DO:
        CREATE ttInventoryStockPreLoadtag.
        ASSIGN 
            ttInventoryStockPreLoadtag.Company = bf-job-mch.company
            ttInventoryStockPreLoadtag.MachineID = bf-job-mch.m-code
            ttInventoryStockPreLoadtag.JobNo = bf-job-mch.job-no
            ttInventoryStockPreLoadtag.JobNo2 = bf-job-mch.job-no2
            ttInventoryStockPreLoadtag.FormNo = bf-job-mch.frm
            ttInventoryStockPreLoadtag.BlankNo = bf-job-mch.blank-no
            ttInventoryStockPreLoadtag.PassNo = bf-job-mch.pass
            ttInventoryStockPreLoadtag.InventoryStatus = gcPreLoadtagStatus
            ttInventoryStockPreLoadtag.ItemType = "WIP"
            ttInventoryStockPreLoadtag.RMItemID = bf-job-mat.rm-i-no
            ttInventoryStockPreLoadtag.EachDimLen = bf-job-mat.len
            ttInventoryStockPreLoadtag.EachDimWid = bf-job-mat.wid
            ttInventoryStockPreLoadtag.EachDimDep = bf-job-mat.dep
            ttInventoryStockPreLoadtag.EachDimUOM = "IN"
            ttInventoryStockPreLoadtag.QuantityTotal = ipdQuantityTotal
            ttInventoryStockPreLoadtag.QuantityUOM = ipcQuantityUOM
            ttInventoryStockPreLoadtag.QuantitySubUnitsPerUnit = ipiQuantitySubUnitsPerUnit
            ttInventoryStockPreLoadtag.QuantityPerSubUnit = ipdQuantityPerSubUnit
            .
        RUN pRecalcQuantityUnits(ipdQuantityTotal, INPUT-OUTPUT ttInventoryStockPreLoadtag.QuantityPerSubUnit, INPUT-OUTPUT ttInventoryStockPreLoadtag.QuantitySubUnitsPerUnit, 
            OUTPUT ttInventoryStockPreLoadtag.QuantityOfSubUnits, OUTPUT ttInventoryStockPreLoadtag.QuantityOfUnits, OUTPUT ttInventoryStockPreLoadtag.QuantityPartial).
            
        FIND FIRST bf-item NO-LOCK 
            WHERE bf-item.company EQ bf-job-mat.company
            AND bf-item.i-no EQ bf-job-mat.rm-i-no
            NO-ERROR.
        IF AVAILABLE bf-item THEN 
            ASSIGN 
                ttInventoryStockPreLoadtag.BasisWeight = bf-item.basis-w
                ttInventoryStockPreLoadtag.BasisWeightUOM = "LBS/MSF"
                .
        FIND FIRST bf-job-hdr NO-LOCK 
            WHERE bf-job-hdr.company EQ bf-job-mch.company
            AND bf-job-hdr.job EQ bf-job-mch.job
            AND bf-job-hdr.job-no EQ bf-job-mch.job-no
            AND bf-job-hdr.job-no2 EQ bf-job-mch.job-no2
            AND (bf-job-hdr.frm EQ bf-job-mch.frm OR bf-job-hdr.frm EQ 0)
            AND (bf-job-hdr.blank-no EQ bf-job-mch.blank-no OR bf-job-hdr.blank-no EQ 0)
            NO-ERROR.
        IF AVAILABLE bf-job-hdr THEN 
            ASSIGN 
                ttInventoryStockPreLoadtag.FGItemID = bf-job-hdr.i-no
                ttInventoryStockPreLoadtag.WarehouseID = bf-job-hdr.loc
                ttInventoryStockPreLoadtag.OrderNo = bf-job-hdr.ord-no
                ttInventoryStockPreLoadtag.CustomerID = bf-job-hdr.cust-no
                .
    END.
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid Machine or Material Inputs" 
            .
    

END PROCEDURE.

PROCEDURE pCreateLoadtagFromPreLoadtag PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInventoryStockPreLoadtag FOR ttInventoryStockPreLoadtag.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
 
    CREATE ttInventoryStockLoadtag.
    BUFFER-COPY ipbf-ttInventoryStockPreLoadtag TO ttInventoryStockLoadtag.
    ASSIGN 
        ttInventoryStockLoadtag.QuantityOriginal = ipdQuantity
        .
    RUN pRecalcQuantityUnits(ttInventoryStockLoadtag.QuantityOriginal, 
        INPUT-OUTPUT ttInventoryStockLoadtag.QuantityPerSubUnit, INPUT-OUTPUT ttInventoryStockLoadtag.QuantitySubUnitsPerUnit,
        OUTPUT ttInventoryStockLoadtag.QuantityOfSubUnits, OUTPUT ttInventoryStockLoadtag.QuantityOfUnits, OUTPUT ttInventoryStockLoadtag.QuantityPartial).

END PROCEDURE.

PROCEDURE pGenerateLoadtagDataFile PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Generates the data file for a given loadtag
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE pGetFullUnitQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given Quantity Per SubUnit and Count of SubUnits pre Unit, return the
     quantity of a full unit
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopdQuantityPerSubUnit AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiQuantitySubUnitsPerUnit AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityPerUnit AS DECIMAL NO-UNDO.

    ASSIGN 
        iopdQuantityPerSubUnit      = MAX(iopdQuantityPerSubUnit,1)
        iopiQuantitySubUnitsPerUnit = MAX(iopiQuantitySubUnitsPerUnit,1)
        opdQuantityPerUnit          = iopdQuantityPerSubUnit * iopiQuantitySubUnitsPerUnit
        .

END PROCEDURE.

PROCEDURE pRecalcQuantityUnits PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a quantity and unit count, return units and partial
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdQuantityTotal AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQuantityPerSubUnit AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiQuantitySubUnitsPerUnit AS INTEGER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opiQuantityOfSubUnits AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQuantityOfUnits AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityPartialSubUnit AS DECIMAL NO-UNDO.

    ASSIGN 
        iopdQuantityPerSubUnit      = MAX(1,iopdQuantityPerSubUnit) 
        iopiQuantitySubUnitsPerUnit = MAX(1,iopiQuantitySubUnitsPerUnit)
        opiQuantityOfSubUnits       = TRUNC(ipdQuantityTotal / iopdQuantityPerSubUnit, 0)
        opdQuantityPartialSubUnit   = ipdQuantityTotal - iopdQuantityPerSubUnit * opiQuantityOfSubUnits
        opiQuantityOfUnits          = INTEGER(TRUNC(opiQuantityOfSubUnits / iopiQuantitySubUnitsPerUnit, 0)) 
        + INTEGER((opiQuantityOfSubUnits MODULO iopiQuantitySubUnitsPerUnit) NE 0) + INTEGER(opdQuantityPartialSubUnit GT 0)
        .  
    
END PROCEDURE.

PROCEDURE CreateInventoryLoadtagsFromPreLoadtags:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCountOfLoadtags          AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQuantityPerFullLoadtag   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCountOfFullLoadtags      AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQuantityOfPartialLoadtag AS DECIMAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttInventoryStockLoadtag.
    /*Process Inputs to "explode" the loadtag records required based on inputs*/
    FOR EACH ttInventoryStockPreLoadtag:
        ttInventoryStockPreLoadtag.CountOfLoadtags = MAX(ttInventoryStockPreLoadtag.CountOfLoadtags,1).    
        RUN pGetFullUnitQuantity(INPUT-OUTPUT ttInventoryStockPreLoadtag.QuantityPerSubUnit, INPUT-OUTPUT ttInventoryStockPreLoadtag.QuantitySubUnitsPerUnit, OUTPUT dQuantityPerFullLoadtag).
        ASSIGN 
            iCountOfFullLoadtags      = INTEGER(TRUNC(ttInventoryStockPreLoadtag.QuantityTotal / dQuantityPerFullLoadtag, 0))
            dQuantityOfPartialLoadtag = ttInventoryStockPreLoadtag.QuantityTotal - dQuantityPerFullLoadtag * iCountOfFullLoadtags
            .
        IF dQuantityOfPartialLoadtag NE 0 AND iCountOfFullLoadtags EQ ttInventoryStockPreLoadtag.CountOfLoadtags THEN 
            ASSIGN 
                dQuantityOfPartialLoadtag = dQuantityOfPartialLoadtag + dQuantityPerFullLoadtag
                iCountOfFullLoadtags      = iCountOfFullLoadtags - 1
                .    
        DO iCountOfLoadtags = 1 TO iCountOfFullLoadtags:
            RUN pCreateLoadtagFromPreLoadtag(BUFFER ttInventoryStockPreLoadtag, dQuantityPerFullLoadtag).
        END. 
        IF dQuantityOfPartialLoadtag NE 0 THEN 
            RUN pCreateLoadtagFromPreLoadtag(BUFFER ttInventoryStockPreLoadtag, dQuantityOfPartialLoadtag).    
    END.

END PROCEDURE.

