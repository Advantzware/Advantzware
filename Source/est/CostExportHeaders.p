
/*------------------------------------------------------------------------
    File        : CostExportHeaders.p
    Purpose     : 

    Syntax      :

    Description : Exports the contents of Headers to a file

    Author(s)   : BV
    Created     : Wed Feb 07 23:38:21 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFilePrefix AS CHARACTER NO-UNDO.

{est/CostTempTables.i "shared"}
DEFINE STREAM csvOutput.
DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cExportType AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSaveToDB   AS LOGICAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetFullFilePath RETURNS CHARACTER 
    (ipcFilePrefix AS CHARACTER, ipcFileFolder AS CHARACTER, ipcType AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */
RUN pCompleteTemptable.
RUN pGetExportConfig(ipcCompany, OUTPUT cFile, OUTPUT cExportType, OUTPUT lSaveToDB).

cFile = fGetFullFilePath(ipcFilePrefix, cFile, cExportType).

CASE cExportType:
    WHEN "XML" THEN RUN pExportXML(cFile).
    WHEN "JSON" THEN RUN pExportJSON(cFile).
    WHEN "CSV" THEN RUN pExportCSV(cFile).
END.

IF lSaveToDB THEN RUN pSaveToDB.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pCompleteTempTable:
    /*------------------------------------------------------------------------------
     Purpose: Fills in the blanks of temp-table based on data already in the temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-ttCostHeader FOR ttCostHeader.
    DEFINE VARIABLE dFactor AS DECIMAL NO-UNDO.

    /*Process Combo Items based on their portion/factor of form costs*/
    FOR EACH ttCostHeader EXCLUSIVE-LOCK
        WHERE ttCostHeader.isItem
        AND ttCostHeader.rec_keyParentForm NE ttCostHeader.rec_key
        AND ttCostHeader.stdCostTotalFactory EQ 0:
        
        dFactor = ttCostHeader.factorForm.
        FIND bf-ttCostHeader NO-LOCK 
            WHERE bf-ttCostHeader.rec_key EQ ttCostHeader.rec_keyParentForm
            NO-ERROR.
        IF AVAILABLE bf-ttCostHeader THEN 
            ASSIGN 
                ttCostHeader.stdCostBoard            = bf-ttCostHeader.stdCostBoard * dFactor
                ttCostHeader.stdCostBrokerComm       = bf-ttCostHeader.stdCostBrokerComm * dFactor
                ttCostHeader.stdCostCommission       = bf-ttCostHeader.stdCostCommission * dFactor
                ttCostHeader.stdCostDirectMaterial   = bf-ttCostHeader.stdCostDirectMaterial * dFactor 
                ttCostHeader.stdCostDirectFactory    = bf-ttCostHeader.stdCostDirectFactory * dFactor
                ttCostHeader.stdCostDirectLabor      = bf-ttCostHeader.stdCostDirectLabor * dFactor
                ttCostHeader.stdCostFixedOverhead    = bf-ttCostHeader.stdCostFixedOverhead * dFactor
                ttCostHeader.stdCostFreight          = bf-ttCostHeader.stdCostFreight * dFactor
                ttCostHeader.stdCostFull             = bf-ttCostHeader.stdCostFull * dFactor
                ttCostHeader.stdCostGSABoard         = bf-ttCostHeader.stdCostGSABoard * dFactor
                ttCostHeader.stdCostGSALabor         = bf-ttCostHeader.stdCostGSALabor * dFactor
                ttCostHeader.stdCostGSAMaterial      = bf-ttCostHeader.stdCostGSAMaterial * dFactor
                ttCostHeader.stdCostMiscLabor        = bf-ttCostHeader.stdCostMiscLabor * dFactor
                ttCostHeader.stdCostMiscMaterial     = bf-ttCostHeader.stdCostMiscMaterial * dFactor
                ttCostHeader.stdCostPrepLabor        = bf-ttCostHeader.stdCostPrepLabor * dFactor
                ttCostHeader.stdCostPrepMaterial     = bf-ttCostHeader.stdCostPrepMaterial * dFactor
                ttCostHeader.stdCostSpecial1         = bf-ttCostHeader.stdCostSpecial1 * dFactor
                ttCostHeader.stdCostSpecial2         = bf-ttCostHeader.stdCostSpecial2 * dFactor
                ttCostHeader.stdCostSpecial3         = bf-ttCostHeader.stdCostSpecial3 * dFactor
                ttCostHeader.stdCostTotalFactory     = bf-ttCostHeader.stdCostTotalFactory * dFactor
                ttCostHeader.stdCostTotalGSA         = bf-ttCostHeader.stdCostTotalGSA * dFactor
                ttCostHeader.stdCostTotalOther       = bf-ttCostHeader.stdCostTotalOther * dFactor
                ttCostHeader.stdCostVariableOverhead = bf-ttCostHeader.stdCostVariableOverhead * dFactor
                ttCostHeader.stdCostWarehousing      = bf-ttCostHeader.stdCostWarehousing * dFactor
                ttCostHeader.stdProfitGross          = bf-ttCostHeader.stdProfitGross * dFactor 
                ttCostHeader.stdProfitNet            = bf-ttCostHeader.stdProfitNet * dFactor
                ttCostHeader.stdSellPrice            = bf-ttCostHeader.stdSellPrice * dFactor
                .
            
    END.
    
    /*Process Set Headers based on sum of component costs*/
    FOR EACH ttCostHeader EXCLUSIVE-LOCK
        WHERE ttCostHeader.isItem
        AND ttCostHeader.formNo EQ 0
        AND ttCostHeader.rec_key EQ ttCostHeader.rec_keyParentSet
        :
        FOR EACH bf-ttCostHeader NO-LOCK 
            WHERE bf-ttCostHeader.rec_keyParentSet EQ ttCostHeader.rec_key
            AND bf-ttCostHeader.isItem
            AND bf-ttCostHeader.rec_key NE ttCostHeader.rec_key
            :
            ASSIGN 
                ttCostHeader.stdCostBoard            = ttCostHeader.stdCostBoard + bf-ttCostHeader.stdCostBoard
                ttCostHeader.stdCostBrokerComm       = ttCostHeader.stdCostBrokerComm + bf-ttCostHeader.stdCostBrokerComm
                ttCostHeader.stdCostCommission       = ttCostHeader.stdCostCommission + bf-ttCostHeader.stdCostCommission
                ttCostHeader.stdCostDirectMaterial   = ttCostHeader.stdCostDirectMaterial + bf-ttCostHeader.stdCostDirectMaterial
                ttCostHeader.stdCostDirectFactory    = ttCostHeader.stdCostDirectFactory + bf-ttCostHeader.stdCostDirectFactory
                ttCostHeader.stdCostDirectLabor      = ttCostHeader.stdCostDirectLabor + bf-ttCostHeader.stdCostDirectLabor
                ttCostHeader.stdCostFixedOverhead    = ttCostHeader.stdCostFixedOverhead + bf-ttCostHeader.stdCostFixedOverhead
                ttCostHeader.stdCostFreight          = ttCostHeader.stdCostFreight + bf-ttCostHeader.stdCostFreight
                ttCostHeader.stdCostFull             = ttCostHeader.stdCostFull + bf-ttCostHeader.stdCostFull
                ttCostHeader.stdCostGSABoard         = ttCostHeader.stdCostGSABoard + bf-ttCostHeader.stdCostGSABoard
                ttCostHeader.stdCostGSALabor         = ttCostHeader.stdCostGSALabor + bf-ttCostHeader.stdCostGSALabor
                ttCostHeader.stdCostGSAMaterial      = ttCostHeader.stdCostGSAMaterial + bf-ttCostHeader.stdCostGSAMaterial
                ttCostHeader.stdCostMiscLabor        = ttCostHeader.stdCostMiscLabor + bf-ttCostHeader.stdCostMiscLabor
                ttCostHeader.stdCostMiscMaterial     = ttCostHeader.stdCostMiscMaterial + bf-ttCostHeader.stdCostMiscMaterial
                ttCostHeader.stdCostPrepLabor        = ttCostHeader.stdCostPrepLabor + bf-ttCostHeader.stdCostPrepLabor
                ttCostHeader.stdCostPrepMaterial     = ttCostHeader.stdCostPrepMaterial + bf-ttCostHeader.stdCostPrepMaterial
                ttCostHeader.stdCostSpecial1         = ttCostHeader.stdCostSpecial1 + bf-ttCostHeader.stdCostSpecial1
                ttCostHeader.stdCostSpecial2         = ttCostHeader.stdCostSpecial2 + bf-ttCostHeader.stdCostSpecial2
                ttCostHeader.stdCostSpecial3         = ttCostHeader.stdCostSpecial3 + bf-ttCostHeader.stdCostSpecial3
                ttCostHeader.stdCostTotalFactory     = ttCostHeader.stdCostTotalFactory + bf-ttCostHeader.stdCostTotalFactory
                ttCostHeader.stdCostTotalGSA         = ttCostHeader.stdCostTotalGSA + bf-ttCostHeader.stdCostTotalGSA
                ttCostHeader.stdCostTotalOther       = ttCostHeader.stdCostTotalOther + bf-ttCostHeader.stdCostTotalOther
                ttCostHeader.stdCostVariableOverhead = ttCostHeader.stdCostVariableOverhead + bf-ttCostHeader.stdCostVariableOverhead
                ttCostHeader.stdCostWarehousing      = ttCostHeader.stdCostWarehousing + bf-ttCostHeader.stdCostWarehousing
                ttCostHeader.stdProfitGross          = ttCostHeader.stdProfitGross + bf-ttCostHeader.stdProfitGross 
                ttCostHeader.stdProfitNet            = ttCostHeader.stdProfitNet + bf-ttCostHeader.stdProfitNet
                ttCostHeader.stdSellPrice            = ttCostHeader.stdSellPrice + bf-ttCostHeader.stdSellPrice
                .
        
        END.
    END.
END PROCEDURE.

PROCEDURE pGetExportConfig:
    /*------------------------------------------------------------------------------
     Purpose: Tests value of NK1 for output/save options 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilePath AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcExportType AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER oplSaveToDB AS LOGICAL NO-UNDO.

    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO. 

    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CECostSave",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cResult,
        OUTPUT lFound).

    IF NOT lFound OR cResult EQ "" 
        THEN opcFilePath = "C:\tmp".
    ELSE opcFilePath = cResult.

    cResult = "".
    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CECostSave",
        INPUT "L",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cResult,
        OUTPUT lFound).
    oplSaveToDB = lFound AND cResult EQ "YES". 

    cResult = "".
    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CECostSave",
        INPUT "I",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cResult,
        OUTPUT lFound).
    CASE cResult:
        WHEN "1" THEN 
            opcExportType = "CSV".
        WHEN "2" THEN 
            opcExportType = "XML".
        WHEN "3" THEN 
            opcExportType = "JSON". 
    END CASE.

END PROCEDURE.

PROCEDURE pExportCSV:
    /*------------------------------------------------------------------------------
     Purpose: Dumps the contents of the table to CSV
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    OUTPUT STREAM csvOutput TO VALUE(ipcFile).
    DO iIndex = 1 TO TEMP-TABLE ttCostHeader:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
        PUT STREAM csvOutput UNFORMATTED TEMP-TABLE ttCostHeader:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD(iIndex):COLUMN-LABEL ",".
    END.
    PUT STREAM csvOutput SKIP.

    FOR EACH ttCostHeader NO-LOCK: 

        EXPORT STREAM csvOutput DELIMITER "," 
            ttCostHeader 
            . 
    END. 

    OUTPUT STREAM csvOutput CLOSE. 


END PROCEDURE.

PROCEDURE pExportJSON:
    /*------------------------------------------------------------------------------
     Purpose: Exports the contents of the temp-table to XML
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFormatted  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRetOK      AS LOGICAL   NO-UNDO.


    MESSAGE "JSON Output Not currently Supported - Set NK1 CECostSave integer value to 1 for CSV or 2 for XML" VIEW-AS ALERT-BOX.
    ASSIGN
        cTargetType = "file"
        lFormatted  = TRUE.
/*    lRetOK = TEMP-TABLE ttCostHeader:WRITE-JSON(cTargetType, ipcFile, lFormatted).  <-Not supported in FWD*/

END PROCEDURE.

PROCEDURE pExportXML:
    /*------------------------------------------------------------------------------
     Purpose: Exports the contents of the temp-table to XML
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lRetOK          AS LOGICAL   NO-UNDO.

    /* Code to populate the temp-table  */
    ASSIGN
        cTargetType     = "file"
        lFormatted      = TRUE
        cEncoding       = ?
        cSchemaLocation = ?
        lWriteSchema    = FALSE
        lMinSchema      = FALSE.

    lRetOK = TEMP-TABLE ttCostHeader:WRITE-XML(cTargetType, ipcFile,lFormatted, cEncoding,
        cSchemaLocation, lWriteSchema, lMinSchema).

END PROCEDURE.

PROCEDURE pSaveToDB:
    /*------------------------------------------------------------------------------
     Purpose: Write the contents of the temp-table to the DB
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH ttCostHeader:
        CREATE costHeader.
        BUFFER-COPY ttCostHeader TO costHeader.
    END.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fGetFullFilePath RETURNS CHARACTER 
    (ipcFilePrefix AS CHARACTER, ipcFileFolder AS CHARACTER, ipcType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the full file path with unique file name
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cFullPath  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExtension AS CHARACTER NO-UNDO.

    ASSIGN 
        ipcFileFolder = TRIM(ipcFileFolder,"\")
        ipcFileFolder = TRIM(ipcFileFolder,"/")
        .

    CASE ipcType:
        WHEN "XML" THEN 
            cExtension = ".xml". 
        WHEN "JSON" THEN 
            cExtension = ".json".
        WHEN "CSV" THEN 
            cExtension = ".csv".
    END CASE.

    cFullPath = ipcFileFolder + "\" + ipcFilePrefix + STRING(TODAY,"99999999") + STRING(TIME) + cExtension.

    RETURN cFullPath.		

END FUNCTION.

