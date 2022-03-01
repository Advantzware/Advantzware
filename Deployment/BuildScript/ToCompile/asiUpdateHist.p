
/*------------------------------------------------------------------------
    File        : asiUpdateHist.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri Dec 06 14:49:22 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcEnvironment AS CHAR NO-UNDO.

DEF VAR cOverrideDir AS CHAR NO-UNDO.
DEF VAR cHotfixListTxtFile AS CHAR NO-UNDO.
DEF VAR cHotfixLog AS CHAR NO-UNDO.
DEF VAR cRawLine AS CHAR NO-UNDO.
DEF VAR cLastHotfix AS CHAR NO-UNDO.
DEF VAR cLineHotfix AS CHAR NO-UNDO.
DEF VAR cFromVersion AS CHAR NO-UNDO.

DEF SHARED TEMP-TABLE ttUpdateHist
    FIELD fromVersion AS CHAR 
    FIELD toVersion AS CHAR 
    FIELD applyDate AS DATE 
    FIELD startTimeInt AS INT
    FIELD startTime AS CHAR 
    FIELD endTimeInt AS INT 
    FIELD endTime AS CHAR 
    FIELD user_id AS CHAR 
    FIELD success AS LOG INITIAL NO 
    FIELD updLog AS CHAR.     


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND FIRST ttUpdateHist NO-ERROR.
IF AVAIL ttUpdateHist THEN DO:
    CREATE updateHist.
    ASSIGN 
        updateHist.fromVersion = ttUpdateHist.fromVersion
        updateHist.toVersion = ttUpdateHist.toVersion
        updateHist.applyDate = ttUpdateHist.applyDate
        updateHist.startTimeInt = ttUpdateHist.startTimeInt
        updateHist.startTime = ttUpdateHist.startTime
        updateHist.endTimeInt = ttUpdateHist.endTimeInt
        updateHist.endTime = ttUpdateHist.endTime
        updateHist.user_id = ttUpdateHist.user_id
        updateHist.success = ttUpdateHist.success
        updateHist.updLog = ttUpdateHist.updLog
        cFromVersion = ttUpdateHist.toVersion
        .        
END.
ELSE DO:
    MESSAGE 
        "Failure TO acquire TEMP-TABLE IN asiUpdateHist.p"
        VIEW-AS ALERT-BOX ERROR.
    RETURN.
END.

/* This section reads the override folder and indicates if any hotfixes have been applied */    
ASSIGN 
    cHotFixListTxtFile = ipcEnvironment + "\Override\Hotfixlist.txt"
    cLastHotfix = "00.00.00".
    
/* IF there's no HotfixList.txt file just return */
IF SEARCH(cHotFixListTxtFile) EQ ? THEN RETURN.

/* If there IS a file, read it and get the greatest listed Hotfix ID */
INPUT FROM VALUE(cHotFixListTxtFile).
REPEAT:
    IMPORT cRawLine.
    IF INDEX("0123456789",SUBSTRING(cRawLine,1,1)) EQ 0 THEN NEXT.
    ASSIGN 
        cLineHotfix = SUBSTRING(cRawLine,1,8)
        cLastHotfix = IF cLineHotfix GT cLastHotfix THEN cLineHotFix ELSE cLastHotfix. 
END.
INPUT CLOSE.

/* If there's a listed hotfixID, create an updateHist record to reflect it */ 
IF cLastHotFix NE "00.00.00" THEN DO:
    INPUT FROM VALUE (cHotFixListTxtFile).
    REPEAT:
        IMPORT UNFORMATTED cRawLine.
        ASSIGN 
            cHotFixLog = cHotFixLog + cRawLine + CHR(10).
    END. 
    INPUT CLOSE.
    CREATE updateHist.
    ASSIGN 
        updateHist.fromVersion = cFromVersion
        updateHist.toVersion = cLastHotFix
        updateHist.applyDate = TODAY
        updateHist.startTimeInt = TIME
        updateHist.startTime = STRING(TIME,"HH:MM:SS AM")
        updateHist.endTimeInt = TIME
        updateHist.endTime = STRING(TIME,"HH:MM:SS AM")
        updateHist.user_id = "Upgrader"
        updateHist.success = TRUE 
        updateHist.updLog = cHotFixLog.
END.            
        
    
    
    