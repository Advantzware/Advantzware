
/*------------------------------------------------------------------------
    File        : FGBinBuildTester.p
    Purpose     : 

    Syntax      :

    Description : Tester application for FGBinBuild

    Author(s)   : BV
    Created     : Thu Feb 14 14:07:04 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{fg/ttFGBins.i "NEW SHARED"}
DEFINE STREAM sOutput. 

DEFINE VARIABLE glAtOnce        AS LOGICAL INITIAL YES.
DEFINE VARIABLE glPurge         AS LOGICAL INITIAL YES.
DEFINE VARIABLE glMakeCounts    AS LOGICAL.
DEFINE VARIABLE gcOutputFile    AS CHARACTER.
DEFINE VARIABLE hdFGBinBuild    AS HANDLE.
DEFINE VARIABLE giCounter       AS INTEGER.
DEFINE VARIABLE giTimer         AS INTEGER.
DEFINE VARIABLE gcAsOf          AS DATE      INITIAL 12/31/2010.
DEFINE VARIABLE gcCompany       AS CHARACTER INITIAL "001".
DEFINE VARIABLE gcFGItemIDStart AS CHARACTER INITIAL '6X6X6'.
DEFINE VARIABLE gcFGItemIDEnd   AS CHARACTER INITIAL '6X6X6'.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN fg/FgBinBuild.p PERSISTENT SET hdFGBinBuild.
giTimer = TIME.
glMakeCounts = glPurge.

IF glAtOnce THEN DO:
    gcOutputFile = "c:\temp\BinBuilder-AtOnce" + STRING(gcAsOf,"99-99-9999") + ".csv".
    RUN pProcessAtOnce.
END.
ELSE DO:
    gcOutputFile = "c:\temp\BinBuilder-Reset" + STRING(gcAsOf,"99-99-9999") + ".csv".
    RUN pProcessWithReset.
END.

IF glMakeCounts THEN RUN CreateCycleCountTransactions IN hdFGBinBuild (gcAsOf).
MESSAGE "Records processed: " giCounter SKIP 
    "Time: " TIME - giTimer
    VIEW-AS ALERT-BOX.
/* **********************  Internal Procedures  *********************** */



PROCEDURE pExportTempTable PRIVATE: 
    /*------------------------------------------------------------------------------ 
     Purpose: Exports the contents of any temp-table into CSV    
     Notes: 
    ------------------------------------------------------------------------------*/ 
    DEFINE INPUT PARAMETER iphTT AS HANDLE NO-UNDO. 
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER iplHeader AS LOGICAL NO-UNDO.
  
    DEFINE VARIABLE hQuery  AS HANDLE    NO-UNDO. 
    DEFINE VARIABLE iIndex  AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE cTTName AS CHARACTER NO-UNDO. 
    
    
    cTTName = iphTT:NAME. 
    IF iplHeader THEN 
    DO:
        OUTPUT STREAM sOutput to VALUE(ipcFileName). 
        DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS: 
            PUT STREAM sOutput UNFORMATTED iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):COLUMN-LABEL + ",". 
        END. 
        PUT STREAM sOutput UNFORMATTED SKIP. 
    END.
    ELSE 
        OUTPUT STREAM sOutput to VALUE(ipcFileName) APPEND. 
    CREATE QUERY hQuery. 
    hQuery:SET-BUFFERS (iphTT:DEFAULT-BUFFER-HANDLE). 
    hQuery:QUERY-PREPARE("FOR EACH " + cTTName). 
    hQuery:QUERY-OPEN().
    REPEAT:   
        hQuery:GET-NEXT().   
        IF hQuery:QUERY-OFF-END THEN LEAVE.   
        DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS: 
            PUT STREAM sOutput UNFORMATTED  
                '"' iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):buffer-value '",'. 
        END. 
        PUT STREAM sOutput UNFORMATTED SKIP. 
    END. 
    OUTPUT STREAM sOutput CLOSE.


END PROCEDURE.

PROCEDURE pProcessAtOnce PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    EMPTY TEMP-TABLE ttFGBins.
    FOR EACH itemfg NO-LOCK 
        WHERE itemfg.company EQ gcCompany
        AND itemfg.i-no GE gcFGItemIDStart
        AND itemfg.i-no LE gcFGItemIDEnd
        :
        IF glPurge THEN 
            RUN BuildBinsForItemAndPurge IN hdFGBinBuild (ROWID(itemfg), gcAsOf, INPUT-OUTPUT giCounter, "C:\Temp\").
        ELSE 
            RUN BuildBinsForItem IN hdFGBinBuild (ROWID(itemfg), gcAsOf, INPUT-OUTPUT giCounter).
    END.
    RUN pExportTempTable(TEMP-TABLE ttFGBins:HANDLE, gcOutputFile, YES).

END PROCEDURE.

PROCEDURE pProcessWithReset PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN pExportTempTable(TEMP-TABLE ttFGBins:HANDLE, gcOutputFile, YES).
    FOR EACH itemfg NO-LOCK 
        WHERE itemfg.company EQ gcCompany
        AND itemfg.i-no GE gcFGItemIDStart
        AND itemfg.i-no LE gcFGItemIDEnd
        :
        EMPTY TEMP-TABLE ttFGBins.
        IF glPurge THEN 
            RUN BuildBinsForItemAndPurge IN hdFGBinBuild (ROWID(itemfg), gcAsOf, INPUT-OUTPUT giCounter, "C:\Temp\").
        ELSE 
            RUN BuildBinsForItem IN hdFGBinBuild (ROWID(itemfg), gcAsOf, INPUT-OUTPUT giCounter).
        RUN pExportTempTable(TEMP-TABLE ttFGBins:HANDLE, gcOutputFile, NO).
    END.

END PROCEDURE.
