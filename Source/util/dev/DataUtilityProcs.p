
/*------------------------------------------------------------------------
    File        : DataUtilityProcs.p
    Purpose     : 

    Syntax      :

    Description : Houses all procedures related to processing data, en masse, for Multi-company utilities 
                (add, copy, merge) and mass updates and changes.

    Author(s)   : BV
    Created     : Wed Oct 17 16:39:39 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE /*INPUT PARAMETER*/ VARIABLE ipcCompanyMain           AS CHARACTER NO-UNDO INIT '001'.
DEFINE /*INPUT PARAMETER*/ VARIABLE ipcCompanyToMerge        AS CHARACTER NO-UNDO INIT '002'.
DEFINE /*INPUT PARAMETER*/ VARIABLE ipcCompanyMerged         AS CHARACTER NO-UNDO INIT 'MRG'.
DEFINE /*INPUT PARAMETER*/ VARIABLE ipcOutputFileTables      AS CHARACTER NO-UNDO INIT 'c:\temp\CompanyTables.csv'.
DEFINE /*INPUT PARAMETER*/ VARIABLE ipcOutputFileMergeTables AS CHARACTER NO-UNDO INIT 'c:\temp\CompanyMergeTables.csv'.
DEFINE /*INPUT PARAMETER*/ VARIABLE ipcOutputFileCollisions  AS CHARACTER NO-UNDO INIT 'c:\temp\CompanyRecordCollisions.csv'.
DEFINE /*INPUT PARAMETER*/ VARIABLE ipcOutputFileCopyResults AS CHARACTER NO-UNDO INIT 'c:\temp\CompanyCopyResults.csv'.
DEFINE /*INPUT PARAMETER*/ VARIABLE ipcOutputFileFindResults AS CHARACTER NO-UNDO INIT 'c:\temp\FindReplaceResults.csv'.
DEFINE /*INPUT PARAMETER*/ VARIABLE iplCountAllRecords       AS LOGICAL   NO-UNDO INIT NO.
DEFINE /*INPUT PARAMETER*/ VARIABLE iplCountCompanyRecords   AS LOGICAL   NO-UNDO INIT YES.
DEFINE /*INPUT PARAMETER*/ VARIABLE iplReplace               AS LOGICAL   NO-UNDO INIT NO.

DEFINE TEMP-TABLE ttTables
    FIELD cTable          AS CHARACTER 
    FIELD cTableDesc      AS CHARACTER
    FIELD cKeyField       AS CHARACTER
    FIELD iRecordCount    AS INTEGER 
    FIELD lHasCompany     AS LOGICAL 
    FIELD lHasRecKey      AS LOGICAL 
    FIELD lHasINo         AS LOGICAL 
    FIELD lHasCustID      AS LOGICAL
    FIELD lHasCustIDIndex AS LOGICAL  
    FIELD cCustIDField    AS CHARACTER 
    FIELD lHasVendID      AS LOGICAL 
    FIELD cVendIDField    AS CHARACTER
    FIELD lHasVendIDIndex AS LOGICAL  
    FIELD lHasOrderNo     AS LOGICAL 
    FIELD lHasJobNo       AS LOGICAL 
    FIELD lHasAcctNo      AS LOGICAL
    FIELD lHasAcctNoIndex AS LOGICAL 
    FIELD cAcctNoField    AS CHARACTER
    FIELD lHasSequence    AS LOGICAL  
    FIELD lToBeCopied     AS LOGICAL 
    .
    
DEFINE TEMP-TABLE ttFullTableList
    FIELD cTable          AS CHARACTER 
    FIELD cTableDesc      AS CHARACTER
    FIELD cAllIndexFields AS CHARACTER
    FIELD cUIndexFields   AS CHARACTER
    FIELD iRecordCount    AS INT 
    FIELD lConvert        AS LOG 
    .

DEFINE TEMP-TABLE ttTablesToMerge
    FIELD cTable             AS CHARACTER
    FIELD cKeyField          AS CHARACTER
    FIELD cStatusNote        AS CHARACTER 
    FIELD iRecordsCompany1   AS INTEGER 
    FIELD iRecordsCompany2   AS INTEGER 
    FIELD iRecordsCollisions AS INTEGER 
    FIELD lProcess           AS LOGICAL
    .
DEFINE TEMP-TABLE ttMergeCollisions
    FIELD cTable       AS CHARACTER 
    FIELD cKeyElements AS CHARACTER 
    FIELD cCompany1    AS CHARACTER 
    FIELD cCompany2    AS CHARACTER 
    .
DEFINE TEMP-TABLE ttFindReplace
    FIELD cCompany AS CHARACTER 
    FIELD cFind    AS CHARACTER
    FIELD cReplace AS CHARACTER
    .

DEFINE TEMP-TABLE ttFindReplaceResults
    FIELD cCompany AS CHARACTER 
    FIELD cFind    AS CHARACTER
    FIELD cReplace AS CHARACTER
    FIELD cTable   AS CHARACTER 
    FIELD cField   AS CHARACTER 
    FIELD cRecKey  AS CHARACTER
    FIELD cStatus  AS CHARACTER
    .    
    
DEFINE TEMP-TABLE ttCopyResults
    FIELD cFromCompany AS CHARACTER 
    FIELD cToCompany   AS CHARACTER 
    FIELD cTable       AS CHARACTER 
    FIELD cRecKeyOld   AS CHARACTER 
    FIELD cRecKeyNew   AS CHARACTER 
    .
    
DEFINE TEMP-TABLE ttTablesWithMergeFields
    FIELD cFieldType AS CHAR FORMAT "x(12)"
    FIELD cTableName AS CHAR FORMAT "x(24)"
    FIELD cFieldName AS CHAR FORMAT "x(24)". 
    
DEFINE TEMP-TABLE ttNewCoA
    FIELD fromCompany AS CHAR 
    FIELD fromAcct    AS CHAR 
    FIELD toCompany   AS CHAR 
    FIELD toAcct      AS CHAR 
    FIELD AcctDesc    AS CHAR 
    .
    
/*{util\CompanyProcs.i}*/
DEFINE STREAM sInput.
DEFINE STREAM sOutput.
DEFINE STREAM logStream.

DEFINE VARIABLE gcAccountTables          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcAccountFields          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcAccountTableExceptions AS CHARACTER NO-UNDO INIT "gl-rpt".

DEFINE VARIABLE gcCompanyTables          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCompanyFields          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcCompanyTableExceptions AS CHARACTER NO-UNDO.

DEFINE VARIABLE gcLocTables          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcLocFields          AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcLocTableExceptions AS CHARACTER NO-UNDO.

DEFINE VARIABLE gcTableMergeExceptions   AS CHARACTER NO-UNDO 
    INIT "ar-ctrl,ap-ctrl,bank,ce-ctrl,attach,box-design-hdr,box-design-line,~
company,gl-ctrl,inv-head,inv-line,po-ctrl,fg-ctrl,,oe-ctrl,rm-ctrl,~
usrx,jc-ctrl,fg-rdtlh,fg-rcpth". 
    
DEFINE VARIABLE gcCustIDFields           AS CHARACTER NO-UNDO 
    INIT "cust-no,cust"
    .

DEFINE VARIABLE gcTableCopyExceptions    AS CHARACTER NO-UNDO
    INIT "company,attach"
    .

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetNextRecKey RETURNS CHARACTER PRIVATE
    (  ) FORWARD.


/* ***************************  Main Block  *************************** */
    
RUN pStatus("Initialize").

RUN pBuildFullTableList.
RUN pBuildFieldLists.
RUN pOutputPreMerge.
RUN pLoadCoAFromCSV ("c:\tmp\LYAccountConversion.csv").
RUN pBuildNewCoA.
RUN pConvertRecsByType ("CustNo").
RUN pConvertRecsByType ("Account").
RUN pConvertRecsByType ("Loc").
RUN pConvertRecsByType ("Carrier").
RUN pConvertRecsByType ("Est-no").
RUN pConvertRecsByType ("M-code").
RUN pConvertRecsByType ("ItemFG").
RUN pConvertRecsByType ("Item").
RUN pConvertRecsByType ("Company").

/*
RUN GenerateTableListing(ipcOutputFileTables, iplCountAllRecords).
RUN pBuildMergeTable(ipcCompanyMain, ipcCompanyToMerge, gcTableMergeExceptions, iplCountCompanyRecords).
RUN pIdentifyCollisions(ipcCompanyMain, ipcCompanyToMerge).
RUN pGenerateTableOutput(ipcOutputFileMergeTables).
RUN pGenerateCollisionOutput(ipcOutputFileCollisions).
/*RUN FindReplaceAccount(ipcCompanyMerged, "old","new", ipcOutputFileFindResults, iplReplace).*/
/*RUN CopyCompany(ipcCompanyMain,ipcCompanyToMerge, NO, NO, ipcOutputFileCopyResults).*/
*/
MESSAGE "Process Complete" SKIP
    "Output File for Database Tables: " ipcOutputFileTables SKIP
    "Output File for Merge Table List: " ipcOutputFileMergeTables SKIP
    "Output File for Collision List: " ipcOutputFileCollisions SKIP 
    "Output File for Copy Results: " ipcOutputFileCopyResults
    
    VIEW-AS ALERT-BOX.

/* **********************  Internal Procedures  *********************** */


PROCEDURE CopyCompany:
    /*------------------------------------------------------------------------------
     Purpose: Copies a given company into another existing company
     Notes: Clears out all tables flagged for copy prior to copy inside destination 
     company if iplMerge is NO
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompanySource AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanyDest AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplMerge AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplSelectedTables AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.

    RUN pStatus("pCopyCompany...").
    EMPTY TEMP-TABLE ttCopyResults.
    RUN pBuildTableListing(NO).
    FOR EACH ttTables EXCLUSIVE-LOCK
        WHERE LOOKUP(ttTables.cTable,"vend,cust,itemfg,item") GT 0
        :
        ttTables.lToBeCopied = YES.
    END.
    FOR EACH ttTables NO-LOCK 
        WHERE ttTables.lHasCompany
        AND (ttTables.lToBeCopied OR NOT iplSelectedTables)
        AND LOOKUP(ttTables.cTable, gcTableCopyExceptions) EQ 0:
            
        RUN pCopyTable(ttTables.cTable, ipcCompanySource, ipcCompanyDest, iplMerge, ttTables.lHasRecKey). 
    END.
    RUN pExportTempTable("ttCopyResults", TEMP-TABLE ttCopyResults:HANDLE, ipcOutputFile).
    
END PROCEDURE.

PROCEDURE FindReplaceAccount:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccountOld AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAccountNew AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplReportOnly AS LOGICAL NO-UNDO.

    RUN pStatus("pFindReplaceAccount...").
    RUN pInitializeFindReplace.
    CREATE ttFindReplace.
    ASSIGN 
        ttFindReplace.cCompany = ipcCompany
        ttFindReplace.cFind    = ipcAccountOld
        ttFindReplace.cReplace = ipcAccountNew
        .
    RUN pFindReplaceAccounts(iplReportOnly).
    RUN pGenerateFindReplaceOutput(ipcOutputFile).

END PROCEDURE.

PROCEDURE FindReplaceAccountsFromCSV:
    /*------------------------------------------------------------------------------
     Purpose: Given a CSV and company, run an Account find-replace based on contents in CSV.
     First column find, second column replace
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplReportOnly AS LOGICAL NO-UNDO.
    
    RUN pStatus("pFindReplaceAccountFromCSV...").
    
    RUN pInitializeFindReplace.
    RUN pLoadFindReplaceFromCSV(ipcFile, ipcCompany).
    RUN pFindReplaceAccounts(iplReportOnly).
    RUN pGenerateFindReplaceOutput(ipcOutputFile).

END PROCEDURE.

PROCEDURE GenerateTableListing:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplIncludeCounts AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE iIndex  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFields AS CHARACTER NO-UNDO.
    
    RUN pStatus("pGenerateTableListing...").
    RUN pBuildTableListing(iplIncludeCounts).
    RUN pExportTempTable("ttTables",TEMP-TABLE ttTables:HANDLE, ipcFileName).


END PROCEDURE.

PROCEDURE pAssessCollisions PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given 2 companies, a table, and a key field, count collisions
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcField AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany1 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany2 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiCollisionCount AS INTEGER NO-UNDO.

    DEFINE VARIABLE hQuery1       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hQuery2       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hBuffer       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cQueryString  AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE cBufferString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBufferValues AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKeyFieldArray AS CHARACTER EXTENT 10 NO-UNDO.
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.
    
    RUN pStatus("pAssessCollisions...").
    RUN pParseIntoArray(ipcField, OUTPUT cKeyFieldArray).
    RUN pCreateQuery(ipcTable, "WHERE " + ipcTable + ".company EQ '" + ipcCompany1 + "'", YES, OUTPUT hQuery1).
    IF VALID-HANDLE(hQuery1) THEN 
    DO:
        CREATE BUFFER hBuffer FOR TABLE ipcTable.
        REPEAT:
            hQuery1:GET-NEXT().
            IF hQuery1:QUERY-OFF-END THEN LEAVE.
            hBuffer = hQuery1:GET-BUFFER-HANDLE(ipcTable).
            cQueryString = "WHERE " + ipcTable + ".company EQ '" + ipcCompany2 + "'".
            cBufferValues = "".
            DO iIndex = 1 TO 10:
                IF cKeyFieldArray[iIndex] NE "" THEN 
                DO:
                   
                    cQueryString = cQueryString + " AND " + ipcTable + "." + cKeyFieldArray[iIndex] + " EQ ".
                    IF hBuffer:BUFFER-FIELD(cKeyFieldArray[iIndex]):DATA-TYPE EQ "CHARACTER" THEN 
                        ASSIGN
                            cBufferString = hBuffer:BUFFER-FIELD(cKeyFieldArray[iIndex]):BUFFER-VALUE
                            cBufferString = REPLACE(cBufferString,"'","")
                            cBufferString = "'" + cBufferString + "'"
                            .
                    ELSE 
                        ASSIGN 
                            cBufferString = STRING(hBuffer:BUFFER-FIELD(cKeyFieldArray[iIndex]):BUFFER-VALUE)
                            .
                    ASSIGN 
                        cBufferValues = cBufferValues + cBufferString + ","
                        cQueryString = cQueryString + cBufferString
                        .
                END.
            END.
            RUN pCreateQuery(ipcTable, cQueryString, YES, OUTPUT hQuery2).
            IF hQuery2:NUM-RESULTS GT 0 THEN 
            DO:
                opiCollisionCount = opiCollisionCount + 1.
                RUN pAddCollision(ipcCompany1, ipcCompany2, ipcTable, ipcField + "=" + cBufferValues).
            END.
            hQuery2:QUERY-CLOSE().
        END.    
        hQuery1:QUERY-CLOSE().

    END.
    DELETE OBJECT hQuery1.
    DELETE OBJECT hQuery2.
    DELETE OBJECT hBuffer.
      
    

END PROCEDURE.

PROCEDURE pBuildFieldLists:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RUN pStatus("pBuildFieldLists...").
    
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "actnum" OR
        _field._field-name MATCHES "acct*" OR 
        _field._field-name MATCHES "*acct*" OR 
        _field._label MATCHES "*Account*" OR
        _field._label MATCHES "*Acct*" OR
        _field._label MATCHES "Account*" OR
        _field._label MATCHES "Acct*" OR
        _field._label MATCHES "*Account" OR
        _field._label MATCHES "*Acct*" OR
        _field._help MATCHES "*account*" OR
        _field._help MATCHES "*acct*" OR
        _field._help MATCHES "account*" OR
        _field._help MATCHES "acct*" OR  
        _field._help MATCHES "*account*" OR
        _field._help MATCHES "*acct*" 
        :
        IF _field._label MATCHES "*desc*" 
            OR _field._label MATCHES "*period*" 
            OR _field._label MATCHES "*bank*" 
            OR _field._label MATCHES "desc*" 
            OR _field._label MATCHES "period*" 
            OR _field._label MATCHES "bank*" 
            OR _field._label MATCHES "*desc"
            OR _field._label MATCHES "*period" 
            OR _field._label MATCHES "*bank" 
            OR _field._label MATCHES "*Desc*" 
            OR _field._label MATCHES "*Period*" 
            OR _field._label MATCHES "*Bank*" 
            OR _field._label MATCHES "Desc*" 
            OR _field._label MATCHES "Period*" 
            OR _field._label MATCHES "Bank*" 
            OR _field._label MATCHES "*Desc"
            OR _field._label MATCHES "*Period" 
            OR _field._label MATCHES "*Bank"
            OR _field._help MATCHES "*desc*" 
            OR _field._help MATCHES "*credit card*" 
            OR _field._help MATCHES "*period*" 
            OR _field._help MATCHES "desc*" 
            OR _field._help MATCHES "credit card*" 
            OR _field._help MATCHES "period*"
            OR _field._help MATCHES "*desc" 
            OR _field._help MATCHES "*credit card" 
            OR _field._help MATCHES "*period"
            THEN NEXT.
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN DO:
            RUN pBuildSingleFieldList ("Account",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END.
    END.


    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "company" OR
        _field._field-name = "cocode":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN DO:
            RUN pBuildSingleFieldList ("Company",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.

    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "loc" OR
        _field._field-name MATCHES "*loc*" OR 
        _field._label MATCHES "*loc*" OR 
        _field._field-name MATCHES "loc*" OR 
        _field._label MATCHES "loc*" OR 
        _field._field-name MATCHES "*loc" OR 
        _field._label MATCHES "*loc":
        IF _field._field-name MATCHES "*bin*"
            OR _field._field-name MATCHES "*lock*"
            OR _field._field-name MATCHES "*a-no*"
            OR _field._field-name MATCHES "*all*"
            OR _field._field-name MATCHES "*dock*"
            OR _field._field-name MATCHES "*location*"
            OR _field._field-name MATCHES "bin*"
            OR _field._field-name MATCHES "lock*"
            OR _field._field-name MATCHES "a-no*"
            OR _field._field-name MATCHES "all*"
            OR _field._field-name MATCHES "dock*"
            OR _field._field-name MATCHES "location*" 
            OR _field._field-name MATCHES "*bin"
            OR _field._field-name MATCHES "*lock"
            OR _field._field-name MATCHES "*a-no"
            OR _field._field-name MATCHES "*all"
            OR _field._field-name MATCHES "*dock"
            OR _field._field-name MATCHES "*location" 
            THEN NEXT.
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN DO:
            RUN pBuildSingleFieldList ("Loc",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.

    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "i-no":
        IF _field._help MATCHES "*finished*"
        OR _field._help MATCHES "*F/G*" 
        OR _field._help MATCHES "finished*"
        OR _field._help MATCHES "F/G*"
        OR _field._help MATCHES "*finished"
        OR _field._help MATCHES "*F/G"THEN DO:
            FIND _file OF _field NO-LOCK NO-ERROR.
            IF _file._hidden THEN NEXT.
            IF AVAIL _file THEN 
            DO:
                RUN pBuildSingleFieldList ("ItemFG",_file._file-name,_field._field-name).
                RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
            END. 
        END.
        ELSE IF _field._help MATCHES "*Raw*"
            OR _field._label MATCHES "*RM*" 
            OR _field._help MATCHES "Raw*"
            OR _field._label MATCHES "RM*"
            OR _field._help MATCHES "*Raw"
            OR _field._label MATCHES "*RM" THEN DO:
            FIND _file OF _field NO-LOCK NO-ERROR.
            IF _file._hidden THEN NEXT.
            IF AVAIL _file THEN 
            DO:
               RUN pBuildSingleFieldList ("Item",_file._file-name,_field._field-name).
               RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
            END. 
        END.
        ELSE DO:
            FIND _file OF _field NO-LOCK NO-ERROR.
            IF _file._hidden THEN NEXT.
            IF AVAIL _file 
            AND CAN-DO (",",_file._file-name) THEN DO:
                RUN pBuildSingleFieldList ("ItemUnk",_file._file-name,_field._field-name).
                RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
            END. 
            ELSE IF AVAIL _file 
            AND CAN-DO (",",_file._file-name) THEN DO:
                RUN pBuildSingleFieldList ("Item",_file._file-name,_field._field-name).
                RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
            END. 
        END.
    END.

    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "carrier":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN DO:
            RUN pBuildSingleFieldList ("Carrier",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.
        
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "est-no":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("Est-no",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.
    
    FOR EACH _field NO-LOCK WHERE 
        _field._field-name = "m-code":
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("M-code",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.

    FOR EACH _field NO-LOCK WHERE 
        CAN-DO ("cust,cust_default,cust-no,customer,customerID,cust-vend-no,we-cust",_field._field-name):
        FIND _file OF _field NO-LOCK NO-ERROR.
        IF _file._hidden THEN NEXT.
        IF AVAIL _file THEN 
        DO:
            RUN pBuildSingleFieldList ("CustNo",_file._file-name,_field._field-name).
            RUN pUpdateTableConvert (ttTablesWithMergeFields.cTableName).                
        END. 
    END.

END PROCEDURE.

PROCEDURE pBuildFullTableList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR hQuery AS HANDLE NO-UNDO.
    
    RUN pStatus("pBuildFullTableList...").
    
    EMPTY TEMP-TABLE ttFullTableList.
    FOR EACH _file NO-LOCK
        WHERE NOT _file._hidden:
        CREATE ttFullTableList.
        ASSIGN 
            ttFullTableList.cTable     = _file._file-name
            ttFullTableList.cTableDesc = REPLACE(REPLACE(_file._desc,","," "),CHR(10)," ")
            .
        FOR EACH _index NO-LOCK OF _file:
            FOR EACH _index-field OF _index NO-LOCK,
            EACH _field OF _index-field:
                IF NOT CAN-DO(ttFullTableList.cAllIndexFields,_field._field-name) THEN ASSIGN 
                        ttFullTableList.cAllIndexFields = ttFullTableList.cAllIndexFields + _field._field-name + ",".
            END.
            IF _index._unique THEN DO:
                FOR EACH _index-field OF _index NO-LOCK,
                EACH _field OF _index-field:
                    IF NOT CAN-DO(ttFullTableList.cUIndexFields,_field._field-name) THEN ASSIGN 
                        ttFullTableList.cUIndexFields = ttFullTableList.cUIndexFields + _field._field-name + ",".
                END.
            END.
        END.
        ASSIGN 
            ttFullTableList.cAllIndexFields = TRIM(ttFullTableList.cAllIndexFields,",") 
            ttFullTableList.cUIndexFields = TRIM(ttFullTableList.cUIndexFields,","). 
    
        RUN pCreateQuery(_file._file-name, "", YES, OUTPUT hQuery).
        IF VALID-HANDLE(hQuery) THEN 
            ttFullTableList.iRecordCount = hQuery:NUM-RESULTS.
    
    END. /*each _file*/
    
END PROCEDURE.

PROCEDURE pBuildNewCoA:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bNewCoA FOR account.
    DEFINE BUFFER bOldCoA FOR account. 
    
    RUN pStatus("pBuildNewCoA...").

    DISABLE TRIGGERS FOR LOAD OF account.
    
    FOR EACH ttNewCoA:
        FIND FIRST bOldCoA WHERE 
            bOldCoA.company EQ ttNewCoA.toCompany AND 
            bOldCoA.actnum EQ ttNewCoA.toAcct
            NO-LOCK NO-ERROR.
        IF AVAIL bOldCoA THEN NEXT.        
        FIND FIRST bOldCoA WHERE 
            bOldCoA.company EQ ttNewCoA.fromCompany AND 
            bOldCoA.actnum EQ ttNewCoA.fromAcct
            EXCLUSIVE NO-ERROR.
        IF AVAIL bOldCoA THEN DO:
            CREATE bNewCoA.
            BUFFER-COPY bOldCoA EXCEPT company actnum dscr TO bNewCoA.
            ASSIGN 
                bNewCoA.company = ttNewCoA.toCompany
                bNewCoA.actnum  = ttNewCoA.toAcct
                bNewCoA.dscr    = ttNewCoA.AcctDesc.
            DELETE bOldCoA.
        END.
    END.
            
END PROCEDURE.

PROCEDURE pBuildSingleFieldList:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcType AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcField AS CHAR NO-UNDO.
    
    RUN pStatus("   Reviewing " + ipcType + " conflict fields in " + ipcTable + "," + ipcField).

    FIND FIRST ttTablesWithMergeFields WHERE 
        ttTablesWithMergeFields.cFieldType = ipcType AND 
        ttTablesWithMergeFields.cTableName EQ ipcTable
        NO-ERROR.
    IF NOT AVAIL ttTablesWithMergeFields THEN DO:
        CREATE ttTablesWithMergeFields.
        ASSIGN 
            ttTablesWithMergeFields.cFieldType = ipcType
            ttTablesWithMergeFields.cTableName = ipcTable
            ttTablesWithMergeFields.cFieldName = ipcField.
    END.
    ELSE ASSIGN 
        ttTablesWithMergeFields.cFieldName = ttTablesWithMergeFields.cFieldName + "," + ipcField.

END PROCEDURE.

PROCEDURE pBuildTableListing PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Builds ttTables for the DB Analysis
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplWithRecordCounts AS LOGICAL NO-UNDO.
    
    RUN pStatus("pBuildTableListing...").
    
    DEFINE VARIABLE lCalculateRecordCounts AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iCount                 AS INTEGER NO-UNDO.
    DEFINE VARIABLE hQuery                 AS HANDLE  NO-UNDO. 
    DEFINE VARIABLE hBuffer                AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lHasAcctNo             AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lHasCustID             AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iIndex                 AS INTEGER NO-UNDO.
    
    EMPTY TEMP-TABLE ttTables.
    FOR EACH asi._file NO-LOCK
        WHERE NOT asi._file._hidden:
        CREATE ttTables.
        ASSIGN 
            ttTables.cTable     = asi._file._file-name
            ttTables.cTableDesc = asi._file._desc
            .
        IF iplWithRecordCounts THEN 
        DO:
            RUN pCreateQuery(asi._file._file-name, "", YES, OUTPUT hQuery).
            IF VALID-HANDLE(hQuery) THEN 
                ttTables.iRecordCount = hQuery:NUM-RESULTS.
        END.    
        
        RUN pDoesFileHaveField(_file._file-name, "company", OUTPUT ttTables.lHasCompany).
        RUN pDoesFileHaveField(_file._file-name, "i-no", OUTPUT ttTables.lHasINo).
        RUN pDoesFileHaveField(_file._file-name, "rec_key", OUTPUT ttTables.lHasRecKey).
        RUN pDoesFileHaveField(_file._file-name, "job-no", OUTPUT ttTables.lHasJobNo).
        RUN pDoesFileHaveField(_file._file-name, "ord-no", OUTPUT ttTables.lHasOrderNo).
        RUN pDoesFileHaveField(_file._file-name, "vend-no", OUTPUT ttTables.lHasVendID).
     
        IF LOOKUP(ttTables.cTable, gcAccountTableExceptions) EQ 0 THEN 
        DO iIndex = 1 TO NUM-ENTRIES(gcAccountFields): 
            RUN pDoesFileHaveField(_file._file-name, ENTRY(iIndex,gcAccountFields), OUTPUT lHasAcctNo).
            IF lHasAcctNo THEN 
            DO: 
                ASSIGN 
                    ttTables.cAcctNoField = ENTRY(iIndex,gcAccountFields)
                    ttTables.lHasAcctNo   = lHasAcctNo
                    .
                RUN pDoesFileHaveFieldInIndex(_file._file-name, ttTables.cAcctNoField, OUTPUT ttTables.lHasAcctNoIndex).
                LEAVE.
            END.
        END.
       
        DO iIndex = 1 TO NUM-ENTRIES(gcCustIDFields): 
            RUN pDoesFileHaveField(_file._file-name, ENTRY(iIndex,gcCustIDFields), OUTPUT lHasCustID).
            IF lHasCustID THEN 
            DO: 
                
                ASSIGN 
                    ttTables.cCustIDField = ENTRY(iIndex,gcCustIDFields)
                    ttTables.lHasCustID   = lHasCustID
                    .
                RUN pDoesFileHaveFieldInIndex(_file._file-name, ttTables.cCustIDField, OUTPUT ttTables.lHasCustIDIndex).
                LEAVE.
            END.
        END. /*each custIDfield*/
        
        RUN pSetKeyFields(ttTables.cTable, OUTPUT cKeyField).
        ttTables.cKeyField = cKeyField.
    END. /*each _file*/
    
END PROCEDURE.

PROCEDURE pConvertRecsByType:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcType AS CHAR NO-UNDO.
    
    RUN pStatus("pConvertRecsByType - " + ipcType).
    FOR EACH ttTablesWithMergeFields WHERE 
        ttTablesWithMergeFields.cFieldType = ipcType:
        CASE ipcType:
            WHEN "Account"  THEN RUN pConvertAccountTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Carrier"  THEN RUN pConvertCarrierTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Company"  THEN RUN pConvertCompanyTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Loc"      THEN RUN pConvertLocTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "ItemFg"   THEN RUN pConvertItemFGTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Item"     THEN RUN pConvertItemTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "Est-no"   THEN RUN pConvertEstNoTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "M-code"   THEN RUN pConvertMcodeTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
            WHEN "CustNo"   THEN RUN pConvertCustTable (ttTablesWithMergeFields.cTableName, ttTablesWithMergeFields.cFieldName).
        END.            
    END.        

END PROCEDURE.

PROCEDURE pConvertAccountTable:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR cCompVal AS CHAR NO-UNDO.
    DEF VAR cFields AS CHAR NO-UNDO.
    DEF VAR cThisTable AS CHAR NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName).
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN DO:
                    FIND FIRST ttNewCoA WHERE 
                        ttNewCoA.fromCompany EQ hCoField:BUFFER-VALUE AND 
                        ttNewCoA.fromAcct EQ hField:BUFFER-VALUE
                        NO-LOCK NO-ERROR.
                    IF AVAIL ttNewCoA THEN ASSIGN 
                            hField:BUFFER-VALUE = ttNewCoA.toAcct NO-ERROR.
                END.
                ELSE DO jCtr = 1 TO hField:EXTENT:
                    ASSIGN 
                        hExtent = hField:BUFFER-VALUE[jCtr].
                    FIND FIRST ttNewCoA WHERE 
                        ttNewCoA.fromCompany EQ hCoField:BUFFER-VALUE AND 
                        ttNewCoA.fromAcct EQ hExtent:BUFFER-VALUE
                        NO-LOCK NO-ERROR.
                    IF AVAIL ttNewCoA THEN ASSIGN 
                            hExtent:BUFFER-VALUE = ttNewCoA.toAcct NO-ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertCarrierTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).
    
    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName).
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "001" THEN ASSIGN  
                        hField:BUFFER-VALUE = "P" + SUBSTRING(hField:BUFFER-VALUE,1,4) NO-ERROR.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002"  THEN ASSIGN  
                        hField:BUFFER-VALUE = "C" + SUBSTRING(hField:BUFFER-VALUE,1,4) NO-ERROR.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    ASSIGN 
                        hExtent = hField:BUFFER-VALUE[jCtr].
                    IF hCoField:BUFFER-VALUE EQ "001" THEN ASSIGN  
                        hExtent:BUFFER-VALUE = "P" + SUBSTRING(hExtent:BUFFER-VALUE,1,4) NO-ERROR.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002" THEN ASSIGN  
                        hExtent:BUFFER-VALUE = "C" + SUBSTRING(hExtent:BUFFER-VALUE,1,4) NO-ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertCompanyTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName).
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        ASSIGN 
                            hField:BUFFER-VALUE = "001" NO-ERROR.
                        IF ERROR-STATUS:ERROR THEN RUN pStatus("      Error merging " + ipcTableName + "record. Duplicate company.").
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    ASSIGN 
                        hExtent = hField:BUFFER-VALUE[jCtr].
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        ASSIGN 
                            hExtent:BUFFER-VALUE = "001" NO-ERROR.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertCustTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName).
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN DO:
                        IF CAN-FIND(FIRST cust WHERE 
                                    cust.company EQ "001" AND 
                                    cust.cust-no EQ hField:BUFFER-VALUE) THEN ASSIGN 
                            hField:BUFFER-VALUE = "z" + hField:BUFFER-VALUE NO-ERROR.
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    ASSIGN 
                        hExtent = hField:BUFFER-VALUE[jCtr].
                    IF hCoField:BUFFER-VALUE EQ "002" THEN DO:
                        IF CAN-FIND(FIRST cust WHERE 
                            cust.company EQ "001" AND 
                            cust.cust-no EQ hExtent:BUFFER-VALUE) THEN ASSIGN 
                                hExtent:BUFFER-VALUE = "z" + hExtent:BUFFER-VALUE NO-ERROR.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertEstNoTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.
    DEF VAR iEstNo AS INT NO-UNDO.
    DEF VAR cEstNo AS CHAR NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName).
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN ASSIGN
                        iEstNo = INT(hField:BUFFER-VALUE)
                        iEstNo = iEstNo + 10000
                        cEstNo = STRING(iEstNo,">>>>9")  
                        hField:BUFFER-VALUE = SUBSTRING("     " + cEstNo, LENGTH(cEstNo) + 1,5) NO-ERROR.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    ASSIGN 
                        hExtent = hField:BUFFER-VALUE[jCtr].
                    IF hCoField:BUFFER-VALUE EQ "002" THEN ASSIGN  
                        iEstNo = INT(hExtent:BUFFER-VALUE)
                        iEstNo = iEstNo + 10000
                        cEstNo = STRING(iEstNo,">>>>9")  
                        hExtent:BUFFER-VALUE = SUBSTRING("     " + cEstNo, LENGTH(cEstNo) + 1,5) NO-ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertItemTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName).
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        IF CAN-FIND(FIRST item WHERE 
                            item.company EQ "001" AND 
                            ITEM.i-no EQ hField:BUFFER-VALUE) THEN ASSIGN 
                                hField:BUFFER-VALUE = SUBSTRING(hField:BUFFER-VALUE,1,2) + "1" + SUBSTRING(hField:BUFFER-VALUE,3) NO-ERROR.
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    ASSIGN 
                        hExtent = hField:BUFFER-VALUE[jCtr].
                    IF hCoField:BUFFER-VALUE EQ "002" THEN 
                    DO:
                        IF CAN-FIND(FIRST item WHERE 
                            item.company EQ "001" AND 
                            ITEM.i-no EQ hExtent:BUFFER-VALUE) THEN ASSIGN 
                                hExtent:BUFFER-VALUE = SUBSTRING(hExtent:BUFFER-VALUE,1,2) + "1" + SUBSTRING(hExtent:BUFFER-VALUE,3) NO-ERROR.
                    END.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertItemFGTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName).
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "002" THEN ASSIGN  
                        hField:BUFFER-VALUE = SUBSTRING("z" + hField:BUFFER-VALUE,1,15) NO-ERROR.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    ASSIGN 
                        hExtent = hField:BUFFER-VALUE[jCtr].
                    IF hCoField:BUFFER-VALUE EQ "002" THEN ASSIGN  
                        hExtent:BUFFER-VALUE = SUBSTRING("z" + hExtent:BUFFER-VALUE,1,15) NO-ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertLocTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName).
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "001" 
                    AND hField:BUFFER-VALUE EQ "Main" THEN ASSIGN  
                        hField:BUFFER-VALUE = "PATT" NO-ERROR.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002" 
                    AND hField:BUFFER-VALUE EQ "Main" THEN ASSIGN  
                        hField:BUFFER-VALUE = "COMM" NO-ERROR.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    ASSIGN 
                        hExtent = hField:BUFFER-VALUE[jCtr].
                    IF hCoField:BUFFER-VALUE EQ "001" 
                    AND hExtent:BUFFER-VALUE EQ "Main" THEN ASSIGN  
                        hExtent:BUFFER-VALUE = "PATT" NO-ERROR.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002" 
                    AND hExtent:BUFFER-VALUE EQ "Main" THEN ASSIGN  
                        hExtent:BUFFER-VALUE = "COMM" NO-ERROR.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pConvertMcodeTable:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFieldList AS CHAR NO-UNDO.
    
    DEF VAR hBuffer AS HANDLE.
    DEF VAR hQuery AS HANDLE.
    DEF VAR hCoField AS HANDLE.
    DEF VAR hField AS HANDLE.
    DEF VAR hExtent AS HANDLE.
    DEF VAR iCtr AS INT NO-UNDO.
    DEF VAR jCtr AS INT NO-UNDO.

    RUN pStatus("   Converting Fields in Table - " + ipcTableName + " - " + ipcFieldList).

    CREATE BUFFER hBuffer FOR TABLE ipcTableName.
    CREATE QUERY hQuery.
    
    hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
        
    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE("FOR EACH " + ipcTableName).
    hQuery:QUERY-OPEN ().
        
    checkCompany:
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hCoField = hBuffer:BUFFER-FIELD (iCtr).
        IF hCoField:NAME = "Company" 
            OR hCoField:NAME = "cocode" THEN 
            LEAVE checkCompany.
    END.
        
    DO WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
        hQuery:GET-NEXT(EXCLUSIVE-LOCK).
        IF NOT hQuery:QUERY-OFF-END THEN 
        DO iCtr = 1 TO hBuffer:NUM-FIELDS:
            ASSIGN 
                hField = hBuffer:BUFFER-FIELD (iCtr).
            IF CAN-DO(ipcFieldList,hField:NAME) THEN 
            DO:
                IF hField:EXTENT EQ ? OR hField:EXTENT LE 1 THEN 
                DO:
                    IF hCoField:BUFFER-VALUE EQ "001" THEN DO:
                        CASE STRING(hField:BUFFER-VALUE):
                            WHEN "HS" THEN ASSIGN hField:BUFFER-VALUE = "HS-P".
                            WHEN "HOTMLT" THEN ASSIGN hField:BUFFER-VALUE = "HM-P" NO-ERROR.
                            WHEN "FLATPK" THEN ASSIGN hField:BUFFER-VALUE = "FP-P" NO-ERROR.
                            WHEN "FOLD" THEN ASSIGN hField:BUFFER-VALUE = "FOLD-P" NO-ERROR.
                            WHEN "ASSMB" THEN ASSIGN hField:BUFFER-VALUE = "ASM-P" NO-ERROR.
                            WHEN "HL" THEN ASSIGN hField:BUFFER-VALUE = "HL-P" NO-ERROR.
                            WHEN "SHIP" THEN ASSIGN hField:BUFFER-VALUE = "SHIP-P" NO-ERROR.
                            WHEN "FORK" THEN ASSIGN hField:BUFFER-VALUE = "FORK-P" NO-ERROR.
                        END CASE.
                    END.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002" THEN DO:
                        CASE STRING(hField:BUFFER-VALUE):
                            WHEN "HS" THEN ASSIGN hField:BUFFER-VALUE = "HS-C" NO-ERROR.
                            WHEN "HOTMLT" THEN ASSIGN hField:BUFFER-VALUE = "HM-C" NO-ERROR.
                            WHEN "FLATPK" THEN ASSIGN hField:BUFFER-VALUE = "FP-C" NO-ERROR.
                            WHEN "FOLD" THEN ASSIGN hField:BUFFER-VALUE = "FOLD-C" NO-ERROR.
                            WHEN "ASSMB" THEN ASSIGN hField:BUFFER-VALUE = "ASM-C" NO-ERROR.
                            WHEN "HL" THEN ASSIGN hField:BUFFER-VALUE = "HL-C" NO-ERROR.
                            WHEN "SHIP" THEN ASSIGN hField:BUFFER-VALUE = "SHIP-C" NO-ERROR.
                            WHEN "FORK" THEN ASSIGN hField:BUFFER-VALUE = "FORK-C" NO-ERROR.
                        END CASE.
                    END.
                END.
                ELSE 
                DO jCtr = 1 TO hField:EXTENT:
                    ASSIGN 
                        hExtent = hField:BUFFER-VALUE[jCtr].
                    IF hCoField:BUFFER-VALUE EQ "001" THEN 
                    DO:
                        CASE STRING(hExtent:BUFFER-VALUE):
                            WHEN "HS" THEN ASSIGN hExtent:BUFFER-VALUE = "HS-P".
                            WHEN "HOTMLT" THEN ASSIGN hExtent:BUFFER-VALUE = "HM-P" NO-ERROR.
                            WHEN "FLATPK" THEN ASSIGN hExtent:BUFFER-VALUE = "FP-P" NO-ERROR.
                            WHEN "FOLD" THEN ASSIGN hExtent:BUFFER-VALUE = "FOLD-P" NO-ERROR.
                            WHEN "ASSMB" THEN ASSIGN hExtent:BUFFER-VALUE = "ASM-P" NO-ERROR.
                            WHEN "HL" THEN ASSIGN hExtent:BUFFER-VALUE = "HL-P" NO-ERROR.
                            WHEN "SHIP" THEN ASSIGN hExtent:BUFFER-VALUE = "SHIP-P" NO-ERROR.
                            WHEN "FORK" THEN ASSIGN hExtent:BUFFER-VALUE = "FORK-P" NO-ERROR.
                        END CASE.
                    END.
                    ELSE IF hCoField:BUFFER-VALUE EQ "002" THEN 
                        DO:
                            CASE STRING(hExtent:BUFFER-VALUE):
                                WHEN "HS" THEN ASSIGN hExtent:BUFFER-VALUE = "HS-C" NO-ERROR.
                                WHEN "HOTMLT" THEN ASSIGN hExtent:BUFFER-VALUE = "HM-C" NO-ERROR.
                                WHEN "FLATPK" THEN ASSIGN hExtent:BUFFER-VALUE = "FP-C" NO-ERROR.
                                WHEN "FOLD" THEN ASSIGN hExtent:BUFFER-VALUE = "FOLD-C" NO-ERROR.
                                WHEN "ASSMB" THEN ASSIGN hExtent:BUFFER-VALUE = "ASM-C" NO-ERROR.
                                WHEN "HL" THEN ASSIGN hExtent:BUFFER-VALUE = "HL-C" NO-ERROR.
                                WHEN "SHIP" THEN ASSIGN hExtent:BUFFER-VALUE = "SHIP-C" NO-ERROR.
                                WHEN "FORK" THEN ASSIGN hExtent:BUFFER-VALUE = "FORK-C" NO-ERROR.
                            END CASE.
                        END.
                END.
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pCopyTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a table, copies all contents from one table to another
     Notes: If not iplMerge, the table will be replaced.  Otherwise, logic to handle 
     collisions will apply.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanySource AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanyDest AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplMerge AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplHasRecKey AS LOGICAL NO-UNDO.

    RUN pStatus("pCopyTable...").

    DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hBufferNew AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cOldRecKey AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNewRecKey AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cException AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-notes  FOR notes.
    DEFINE BUFFER bf-attach FOR attach.
    
    IF NOT iplMerge THEN 
    DO:
        RUN pCreateQuery(ipcTable, "WHERE " + ipcTable + ".company EQ '" + ipcCompanyDest + "'", NO, OUTPUT hQuery).
        IF VALID-HANDLE(hQuery) THEN 
        DO TRANSACTION:
            REPEAT:
                hQuery:GET-NEXT(EXCLUSIVE-LOCK).
                IF hQuery:QUERY-OFF-END THEN LEAVE.
                hBuffer = hQuery:GET-BUFFER-HANDLE(ipcTable).
                hBuffer:DISABLE-LOAD-TRIGGERS (YES).
                hBuffer:BUFFER-DELETE ().
            END.
            hQuery:QUERY-CLOSE().
        END.
    END.
    cException = "company" + IF iplHasRecKey THEN ",rec_key" ELSE "".
    RUN pCreateQuery(ipcTable, "WHERE " + ipcTable + ".company EQ '" + ipcCompanySource + "'", YES, OUTPUT hQuery).
    IF VALID-HANDLE(hQuery) THEN 
    DO TRANSACTION:
        CREATE BUFFER hBufferNew FOR TABLE ipcTable.
        REPEAT:
            hQuery:GET-NEXT().
            IF hQuery:QUERY-OFF-END THEN LEAVE.
            hBuffer = hQuery:GET-BUFFER-HANDLE(ipcTable).
            hBufferNew:DISABLE-LOAD-TRIGGERS (YES).
            hBufferNew:BUFFER-CREATE ().
            IF iplHasRecKey THEN
                cOldRecKey = hBuffer:BUFFER-FIELD("rec_key"):BUFFER-VALUE.
            hBufferNew:BUFFER-COPY(hBuffer,cException).
            hBufferNew:BUFFER-FIELD("company"):BUFFER-VALUE = ipcCompanyDest.
            IF iplHasRecKEy THEN 
            DO: 
                ASSIGN 
                    cNewRecKey                                      = fGetNextRecKey()
                    hBufferNew:BUFFER-FIELD("rec_key"):BUFFER-VALUE = cNewRecKey.
                .   
                FOR EACH notes NO-LOCK
                    WHERE notes.rec_key EQ cOldRecKey:    
                    CREATE bf-notes.
                    BUFFER-COPY notes EXCEPT rec_key TO bf-notes
                        ASSIGN 
                        bf-notes.rec_key = cNewRecKey.
    
                END.
                FOR EACH attach NO-LOCK
                    WHERE attach.rec_key EQ cOldRecKey:    
                    CREATE bf-notes.
                    BUFFER-COPY attach EXCEPT rec_key TO bf-attach
                        ASSIGN 
                        bf-attach.rec_key = cNewRecKey.
    
                END.
                CREATE ttCopyResults.
                ASSIGN 
                    ttCopyResults.cTable       = ipcTable
                    ttCopyResults.cFromCompany = ipcCompanySource
                    ttCopyResults.cToCompany   = ipcCompanyDest
                    ttCopyResults.cRecKeyOld   = cOldRecKey
                    ttCopyResults.cRecKeyNew   = cNewRecKey
                    .
            END. /*iplHasRecKey*/
        END.
        hQuery:QUERY-CLOSE().

    END.
    IF VALID-HANDLE(hQuery) THEN DELETE OBJECT hQuery.
    IF VALID-HANDLE(hBuffer) THEN DELETE OBJECT hBuffer.
    IF VALID-HANDLE(hBufferNew) THEN DELETE OBJECT hBufferNew.
END PROCEDURE.

PROCEDURE pCreateQuery PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns handle to opened query
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWhere AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplNoLock AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER ophQuery AS HANDLE NO-UNDO.

    RUN pStatus("   Analyzing table " + ipcTable).

    DEFINE VARIABLE cQuery  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hBuffer AS HANDLE    NO-UNDO.

    cQuery = "PRESELECT EACH " + ipcTable.
    IF iplNoLock THEN 
        cQuery = cQuery + " NO-LOCK ".
    ELSE  
        cQuery = cQuery + " EXCLUSIVE-LOCK ".
    IF ipcWhere NE "" THEN 
        cQuery = cQuery + " " + ipcWhere.
    IF VALID-HANDLE(hBuffer) THEN DELETE WIDGET hBuffer.                                                          
    IF VALID-HANDLE(ophQuery) THEN DELETE WIDGET ophQuery.                                                
    CREATE BUFFER hBuffer FOR TABLE ipcTable.
    CREATE QUERY ophQuery.
    ophQuery:ADD-BUFFER(hBuffer).
    ophQuery:QUERY-PREPARE(cQuery).
    ophQuery:QUERY-OPEN().

END PROCEDURE.

PROCEDURE pDoesFileHaveField PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a _file buffer, return yes/no based on field name passed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcField AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplHasField AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-file FOR _file.
    FIND FIRST bf-file NO-LOCK  
        WHERE bf-file._file-name EQ ipcTable
        NO-ERROR.
    IF AVAILABLE bf-file THEN   
        FIND FIRST _field OF bf-file NO-LOCK 
            WHERE _field._field-name EQ ipcField
            NO-ERROR.
    oplHasField = AVAILABLE _field.

END PROCEDURE.

PROCEDURE pAddCollision PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Adds collision record to temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany1 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany2 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcKey AS CHARACTER NO-UNDO.

    CREATE ttMergeCollisions.
    ASSIGN 
        ttMergeCollisions.cCompany1    = ipcCompany1
        ttMergeCollisions.cCompany2    = ipcCompany2
        ttMergeCollisions.cTable       = ipcTable
        ttMergeCollisions.cKeyElements = ipcKey
        .
END PROCEDURE.


PROCEDURE pDoesFileHaveFieldInIndex PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Determines if provided field is part of an index of input table buffer
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcField AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplFieldInIndex AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-file FOR _file.
    
    oplFieldInIndex = NO.
    FOR EACH bf-file NO-LOCK 
        WHERE bf-file._file-name EQ ipcTable,
        EACH _index OF bf-file NO-LOCK ,
        EACH _index-field OF _index NO-LOCK ,
        FIRST _field OF _index-field NO-LOCK  
        WHERE _field._field-name = ipcField:
        oplFieldInIndex = YES.
        LEAVE.
    END.

END PROCEDURE.

PROCEDURE pExportTempTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Exports the contents of any temp-table into CSV   
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTTname AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER iphTT AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER ipcFileName AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hQuery AS HANDLE  NO-UNDO.
    DEFINE VARIABLE iIndex AS INTEGER NO-UNDO.

    OUTPUT STREAM sOutput to VALUE(ipcFileName).

    CREATE QUERY hQuery.
    hQuery:SET-BUFFERS (iphTT:DEFAULT-BUFFER-HANDLE).

    hQuery:QUERY-PREPARE("FOR EACH " + ipcTTName).
    hQuery:QUERY-OPEN().

    DO iIndex = 1 TO iphTT:DEFAULT-BUFFER-HANDLE:NUM-FIELDS:
        PUT STREAM sOutput UNFORMATTED iphTT:DEFAULT-BUFFER-HANDLE:buffer-field(iIndex):name + ",".
    END.
    PUT STREAM sOutput UNFORMATTED SKIP.

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

PROCEDURE pFindReplaceAccounts PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Runs a Find Replace on the tables that have account and company
        Assumes a ttFindReplace table has records with company, old, new values.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplReportOnly AS LOGICAL NO-UNDO.
    
    RUN pBuildTableListing(NO).
    FOR EACH ttTables NO-LOCK 
        WHERE ttTables.lHasCompany 
        AND ttTables.lHasAcctNo:
        FOR EACH ttFindReplace:
            RUN pFindReplaceIndexedStringWithinCompany(ttTables.cTable, 
                ttTables.cAcctNoField, 
                ttFindReplace.cCompany, 
                ttFindReplace.cFind, 
                ttFindReplace.cReplace,
                iplReportOnly).
        END.
    END.

END PROCEDURE.

PROCEDURE pFindReplaceIndexedStringWithinCompany PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: For a given table and field, this proc will replace the value of the
        field with ipcStringNew if the current value matches ipcStringOld within a company
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcField AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStringOld AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcStringNew AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER iplReportOnly AS LOGICAL NO-UNDO.

    DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hBuffer    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cStatus    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lHasRecKey AS LOGICAL   NO-UNDO.
    
    RUN pCreateQuery(ipcTable, "WHERE " + ipcTable + "." + ipcField + " EQ '" + ipcStringOld + "' AND " + ipcTable + ".company EQ '" + ipcCompany + "'", NO, OUTPUT hQuery).
    RUN pDoesFileHaveField(ipcTable, "rec_key", OUTPUT lHasRecKey).
    IF VALID-HANDLE(hQuery) THEN 
    DO TRANSACTION:
        REPEAT:
            hQuery:GET-NEXT(EXCLUSIVE-LOCK).
            IF hQuery:QUERY-OFF-END THEN LEAVE.
            hBuffer = hQuery:GET-BUFFER-HANDLE(ipcTable).
            IF NOT iplReportOnly THEN 
            DO:
                hBuffer:DISABLE-LOAD-TRIGGERS (YES).
                hBuffer:BUFFER-FIELD(ipcField):BUFFER-VALUE = ipcStringNew.
                cStatus = "Found and Replaced".
            END.
            ELSE 
                cStatus = "Found but not replaced".
            CREATE ttFindReplaceResults.
            ASSIGN 
                ttFindReplaceResults.cCompany = ipcCompany
                ttFindReplaceResults.cFind    = ipcStringOld
                ttFindReplaceResults.cReplace = ipcStringOld
                ttFindReplaceResults.cTable   = ipcTable
                ttFindReplaceResults.cField   = ipcField
                ttFindReplaceResults.cStatus  = cStatus
                .
            IF lHasRecKey THEN 
                ttFindReplaceResults.cRecKey = hBuffer:BUFFER-FIELD("rec_key"):BUFFER-VALUE.
                
        END.
        hQuery:QUERY-CLOSE().
        DELETE OBJECT hQuery.
    END.

END PROCEDURE.

PROCEDURE pGenerateCollisionOutput PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Outputs the contents of the temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    
    RUN pExportTempTable("ttMergeCollisions",TEMP-TABLE ttMergeCollisions:HANDLE, ipcOutputFile).


END PROCEDURE.

PROCEDURE pGenerateFindReplaceOutput PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Outputs the contents of the temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.
    
    RUN pExportTempTable("ttFindReplaceResults",TEMP-TABLE ttFindReplaceResults:HANDLE, ipcOutputFile).


END PROCEDURE.

PROCEDURE pGenerateTableOutput PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Outputs the contents of the temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcOutputFile AS CHARACTER NO-UNDO.

    RUN pExportTempTable("ttTablesToMerge",TEMP-TABLE ttTablesToMerge:HANDLE, ipcOutputFile).

END PROCEDURE.

PROCEDURE pBuildMergeTable PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompanyMain AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanyToMerge AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTableExceptions AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplCountRecords AS LOGICAL NO-UNDO.

    DEFINE VARIABLE iCount  AS INTEGER NO-UNDO.
    DEFINE VARIABLE hQuery  AS HANDLE  NO-UNDO. 
    DEFINE VARIABLE hBuffer AS HANDLE  NO-UNDO.
    
    
    
    EMPTY TEMP-TABLE ttTablesToMerge.
    FOR EACH ttTables
        WHERE lHasCompany:
        CREATE ttTablesToMerge.
        ASSIGN 
            ttTablesToMerge.cTable      = ttTables.cTable 
            ttTablesToMerge.cKeyField   = ttTables.cKeyField
            ttTablesToMerge.cStatusNote = "Skipped - No Records in company to merge"
            ttTablesToMerge.lProcess    = YES
            .
        IF LOOKUP(ttTablesToMerge.cTable,ipcTableExceptions) GT 0 THEN 
            ASSIGN 
                ttTablesToMerge.cStatusNote = "Skipped - On Exception List"
                ttTablesToMerge.lProcess    = NO
                .
        IF iplCountRecords THEN 
        DO:
            RUN pCreateQuery(ttTables.cTable, "WHERE " + ttTables.cTable + ".company EQ '" + ipcCompanyMain + "'", YES, OUTPUT hQuery).
            IF VALID-HANDLE(hQuery) THEN 
                ttTablesToMerge.iRecordsCompany1 = hQuery:NUM-RESULTS.
            RUN pCreateQuery(ttTables.cTable, "WHERE " + ttTables.cTable + ".company EQ '" + ipcCompanyToMerge + "'", YES, OUTPUT hQuery).
            IF VALID-HANDLE(hQuery) THEN 
                ttTablesToMerge.iRecordsCompany2 = hQuery:NUM-RESULTS.
            IF ttTablesToMerge.iRecordsCompany2 GT 0 AND ttTablesToMerge.lProcess THEN 
                ASSIGN 
                    ttTablesToMerge.cStatusNote = "Records to Merge"
                    .
            ELSE 
                ttTablesToMerge.lProcess = NO.
        END.
    END.
     

END PROCEDURE.

PROCEDURE pIdentifyCollisions PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Idenifies Collisions in the tables to process
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompanyMain AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompanyToMerge AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttMergeCollisions.
    FOR EACH ttTablesToMerge
        WHERE ttTablesToMerge.lProcess:
        IF ttTablesToMerge.cKeyField NE "" THEN 
            RUN pAssessCollisions(ttTablesToMerge.cTable,
                cKeyField,
                ipcCompanyMain,
                ipcCompanyToMerge,
                OUTPUT ttTablesToMerge.iRecordsCollisions).
        ELSE 
        DO: 
            CASE ttTablesToMerge.cTable:
                OTHERWISE 
                DO:
                    ASSIGN 
                        ttTablesToMerge.cStatusNote = ttTablesToMerge.cStatusNote + " - NOT ASSESSED".
                END. /*otherwise*/
            END CASE.
        END. /*No single key table*/
        IF ttTablesToMerge.iRecordsCollisions EQ ttTablesToMerge.iRecordsCompany2 AND ttTablesToMerge.iRecordsCompany2 GT 0 THEN 
            ASSIGN 
                ttTablesToMerge.lProcess    = NO 
                ttTablesToMerge.cStatusNote = "Appears to Be Identical - Copy from Main Company but add to Merge Exception List"
                .
        ELSE IF ttTablesToMerge.iRecordsCollisions NE 0 THEN  
                ASSIGN 
                    ttTablesToMerge.lProcess    = YES
                    ttTablesToMerge.cStatusNote = "Records to Merge - Collisions Exist"
                    .
    END.  /*Each ttTablesToMerge*/

END PROCEDURE.




PROCEDURE pInitializeFindReplace PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttFindReplace.
    EMPTY TEMP-TABLE ttFindReplaceResults.

END PROCEDURE.

PROCEDURE pLoadCoAFromCSV:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcCoAConvFile AS CHAR. 
    DEF VAR cLine AS CHAR NO-UNDO.
    
    RUN pStatus("pLoadCoAFromCSV...").

    INPUT FROM VALUE(ipcCoAConvFile).
    REPEAT:
        IMPORT UNFORMATTED cLine.
        CREATE ttNewCoA.
        ASSIGN 
            ttNewCoA.fromCompany = ENTRY(1,cLine,",")
            ttNewCoA.fromAcct    = ENTRY(2,cLine,",")
            ttNewCoA.toCompany   = ENTRY(3,cLine,",")
            ttNewCoA.toAcct      = ENTRY(4,cLine,",")
            ttNewCoA.AcctDesc    = ENTRY(5,cLine,",").
    END.
    


END PROCEDURE.

PROCEDURE pLoadFindReplaceFromCSV PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given path to a CSV file, load
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
   
    IF SEARCH(ipcFile) NE ? THEN 
    DO:
        RUN pInitializeFindReplace.
        INPUT STREAM sInput FROM VALUE(ipcFile).
        REPEAT:
            CREATE ttFindReplace.
            ttFindReplace.cCompany = ipcCompany.
            IMPORT STREAM sInput DELIMITER ','
                    
                ttFindReplace.cFind
                ttFindReplace.cReplace
                . 
        END.
        OUTPUT STREAM sInput CLOSE.
    END.

END PROCEDURE.

PROCEDURE pOutputPreMerge:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN pStatus("pOutputPreMerge...").

    OUTPUT TO c:\tmp\TablesToConvert.csv.
    FOR EACH ttFullTableList WHERE 
        ttFullTableList.iRecordCount GT 0:
        PUT UNFORMATTED 
            cTable + "," + 
            cTableDesc + "," +
            REPLACE(cAllIndexFields,",","|") + "," +
            REPLACE(cUIndexFields,",","|") + "," +
            STRING(iRecordCount)  + "," +
            STRING(lConvert) + CHR(10).
    END.
    OUTPUT CLOSE.

    OUTPUT TO c:\tmp\TablesWithMergeFields.csv.
    FOR EACH ttTablesWithMergeFields:
        PUT UNFORMATTED 
            cFieldType + "," +
            cTableName + "," +
            cFieldName + CHR(10).
    END.
    OUTPUT CLOSE.

END PROCEDURE.

PROCEDURE pSetKeyFields PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given a table name, returns the key field
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTable AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcKeyField AS CHARACTER NO-UNDO.

    opcKeyField = "".
    CASE ipcTable:
        WHEN "vend" THEN
            opcKeyField = "vend-no".
        WHEN "cust" OR 
        WHEN "soldto" OR 
        WHEN "shipto" THEN
            opcKeyField = "cust-no".
        WHEN "item" THEN
            opcKeyField = "i-no".
        WHEN "itemfg" THEN
            opcKeyField = "i-no".
        WHEN "fg-bin" THEN 
            opcKeyField = "i-no,loc,loc-bin,job-no,job-no2,tag".
        WHEN "buyer" THEN
            opcKeyField = "buyer".
        WHEN "currency" THEN
            opcKeyField = "c-code".
        WHEN "flute" THEN
            opcKeyField = "code".
        WHEN "loc" THEN
            opcKeyField = "loc".
        WHEN "mach"  OR 
        WHEN "mach-calendar" OR 
        WHEN "mmtx" OR 
        WHEN "mmtx2" OR 
        WHEN "mmty" OR 
        WHEN "mstd" THEN
            opcKeyField = "m-code".
        WHEN "matprep" THEN
            opcKeyField = "mat".
        WHEN "payment-type" THEN
            opcKeyField = "type".
        WHEN "period" THEN
            opcKeyField = "pst".
        WHEN "procat" THEN
            opcKeyField = "procat".
        WHEN "scoreType" THEN
            opcKeyField = "scoreType".
        WHEN "stax" THEN
            opcKeyField = "tax-group".
        WHEN "stax-group" THEN
            opcKeyField = "tax-group".
        WHEN "account" THEN
            opcKeyField = "actnum".
        WHEN "carrier" OR 
        WHEN "carr-mtx" THEN
            opcKeyField = "carrier,loc".
        WHEN "costtype" THEN
            opcKeyField = "cost-type".
        WHEN "eb" THEN
            opcKeyField = "est-no".
        WHEN "ef" OR 
        WHEN "ef-nsh" THEN
            opcKeyField = "est-no".
        WHEN "est" THEN
            opcKeyField = "est-no".
        WHEN "est-op" THEN
            opcKeyField = "est-no".
        WHEN "est-prep" THEN
            opcKeyField = "est-no".
        /*        WHEN "est-summ" THEN       */
        /*            opcKeyField = "est-no".*/
        WHEN "est-qty" THEN
            opcKeyField = "est-no".
        WHEN "custype" THEN
            opcKeyField = "custype".
        WHEN "oe-bolh" THEN
            opcKeyField = "bol-no".
        WHEN "po-ord" THEN
            opcKeyField = "po-no".
        WHEN "po-ordl" THEN
            opcKeyField = "po-no".
        WHEN "routing" THEN
            opcKeyField = "r-code".
        WHEN "terr" THEN
            opcKeyField = "terr".
        WHEN "terms" THEN
            opcKeyField = "t-code".
        WHEN "sys-ctrl" OR 
        WHEN "sys-ctrl-shipto" THEN
            opcKeyField = "name".
        WHEN "style" THEN
            opcKeyField = "style".
        WHEN "sman" OR 
        WHEN "smanbugt" OR 
        WHEN "smanmtrx" OR 
        WHEN "sman-mtx" OR 
        WHEN "smanbcat" OR 
        WHEN "smanbcst" THEN
            opcKeyField = "sman".
        WHEN "cust-markup" THEN 
            opcKeyField = "cust-no,style,procat".
        WHEN "cust-part" THEN 
            opcKeyField = "i-no,cust-no".
        WHEN "e-item" OR 
        WHEN "e-itemfg" THEN 
            opcKeyField = "i-no".
        WHEN "e-item-vend"  THEN 
            opcKeyField = "i-no,vend-no".
        WHEN "e-itemfg-vend"  THEN 
            opcKeyField = "i-no,vend-no,est-no".   
        WHEN "job" OR 
        WHEN "job-hdr" OR 
        WHEN "job-mat" OR 
        WHEN "job-mch" OR 
        WHEN "mch-act" OR 
        WHEN "mat-act" OR 
        WHEN "fg-act" THEN 
            opcKeyField = "job-no,job-no2". 
        WHEN "ar-inv" OR 
        WHEN "ar-invl" THEN 
            opcKeyField = "cust-no,inv-no".
    END CASE.
        

END PROCEDURE.

PROCEDURE pParseIntoArray PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given comma separated input, outputs an array
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCommaList AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcArray AS CHARACTER EXTENT 10 NO-UNDO.
    
    DEFINE VARIABLE iEntry AS INTEGER NO-UNDO.
    
    IF NUM-ENTRIES(ipcCommaList) GT 1 THEN 
    DO:
        DO iEntry = 1 TO NUM-ENTRIES(ipcCommaList):
            IF iEntry LE 10 THEN 
                opcArray[iEntry] = ENTRY(iEntry,ipcCommaList).
        END.
    END.
    ELSE 
        opcArray[1] = ipcCommaList.
    

END PROCEDURE.


PROCEDURE pStatus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcStatus AS CHAR NO-UNDO.
    DEF VAR cLogFile AS CHAR NO-UNDO.
    DEF VAR cMsgStr AS CHAR NO-UNDO.
        ASSIGN
            cLogFile = "c:\tmp\convertLog.txt"
            cMsgStr = "  " + ipcStatus.
        
    IF ipcStatus = "Initialize" THEN 
    DO:
        OUTPUT STREAM logStream TO VALUE(cLogFile).

        PUT STREAM logStream UNFORMATTED "---------------------------------------------------------------" + CHR(10).
        PUT STREAM logStream UNFORMATTED "Action Date: " + STRING(TODAY,"99/99/99") + CHR(10).
        PUT STREAM logStream UNFORMATTED "Action Time: " + STRING(TIME,"HH:MM:SS") + CHR(10).
        PUT STREAM logStream UNFORMATTED "---------------------------------------------------------------" + CHR(10).
        PUT STREAM logStream UNFORMATTED CHR(10).
        PUT STREAM logStream
            STRING(TODAY,"99/99/99") AT 1
            STRING(TIME,"HH:MM:SS") AT 12
            "Initializing log" FORMAT "x(160)" AT 25
            SKIP.
        OUTPUT STREAM logStream CLOSE.
        RETURN.
    END.
    ELSE 
    DO:
        IF INDEX(ipcStatus,"duplicate") EQ 0 THEN 
        DO:
            ASSIGN
                cMsgStr = ipcStatus.
            OUTPUT STREAM logStream TO VALUE(cLogFile) APPEND.
            PUT STREAM logStream
                STRING(TODAY,"99/99/99") AT 1
                STRING(TIME,"HH:MM:SS") AT 12
                cMsgStr FORMAT "x(160)" AT 25
                SKIP.
            OUTPUT STREAM logStream CLOSE.
        END.
    END.
        
    PROCESS EVENTS.

END PROCEDURE.


PROCEDURE pUpdateTableConvert:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
    
    IF NOT CAN-FIND(FIRST ttFullTableList WHERE 
        ttFullTableList.cTable EQ ipcTableName AND 
        ttFullTableList.lConvert EQ TRUE) THEN 
    DO:
        FIND ttFullTableList WHERE 
            ttFullTableList.cTable EQ ipcTableName
            NO-ERROR.
        IF AVAIL ttFullTableList THEN ASSIGN 
                ttFullTableList.lConvert = TRUE.
    END. 


END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetNextRecKey RETURNS CHARACTER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: REplace with call to persistent, but can't within util run directly
    ------------------------------------------------------------------------------*/    
    RETURN STRING(YEAR(TODAY),"9999")
        + STRING(MONTH(TODAY),"99")
        + STRING(DAY(TODAY),"99")
        + STRING(TIME,"99999")
        + STRING(NEXT-VALUE(rec_key_seq,ASI),"99999999")
        .       
END FUNCTION.



