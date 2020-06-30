
&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
/* Procedure Description
"Structured Procedure File Template.

Use this template to create a new Structured Procedure file to compile and run PROGRESS 4GL code. You edit structured procedure files using the AB's Section Editor."
*/
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : purgeProcs.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : MYT
    Created     : Wed Jun 24 09:43:52 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF STREAM sDump.
DEF STREAM sReport.

DEFINE TEMP-TABLE ttFileList
    FIELD cFileName AS CHAR 
    FIELD rRowID AS ROWID 
    FIELD cRule AS CHAR 
    FIELD cMessage AS CHAR
    FIELD lPurged AS LOG
    FIELD cRec_key AS CHAR.

DEF VAR iCtr AS INT NO-UNDO.
DEF VAR iProcessedCount AS INT NO-UNDO.
DEF VAR cOutputDir AS CHAR NO-UNDO.
DEF VAR lPurge AS LOG NO-UNDO.
DEF VAR cRecKeyPrefix AS CHAR NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.
DEF VAR hRecKey AS HANDLE NO-UNDO.
DEF VAR hRnoField AS HANDLE NO-UNDO.
DEF VAR hCompanyField AS HANDLE NO-UNDO.
DEF VAR hBufferField AS HANDLE NO-UNDO.
DEF VAR hTestField AS HANDLE NO-UNDO.
DEF VAR cFieldName AS CHAR NO-UNDO.
DEF VAR cCompanyList AS CHAR NO-UNDO.
DEF VAR hInvNoField AS HANDLE NO-UNDO.
DEF VAR cLocList AS CHAR NO-UNDO.
DEF VAR hOrdNoField AS HANDLE NO-UNDO.
    

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */


&IF DEFINED(EXCLUDE-dynExport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD dynExport Procedure
FUNCTION dynExport RETURNS CHARACTER 
  (INPUT hRecord AS HANDLE,
   INPUT cDelim AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF




/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure Template
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME
 



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pGetFieldList) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetFieldList Procedure
PROCEDURE pGetFieldList:
/*------------------------------------------------------------------------------
 Purpose: Returns complete list of indexed fields for this table
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR.
    DEF OUTPUT PARAMETER opcFieldList AS CHAR.

    FIND FIRST asi._file NO-LOCK WHERE 
    asi._file._file-name = ipcFileName 
        NO-ERROR.

    IF AVAIL asi._file THEN FOR EACH asi._index OF asi._file, 
                                EACH asi._index-field OF asi._index, 
                                EACH asi._field OF asi._index-field NO-LOCK 
        BY asi._field._order:
        IF LOOKUP(_field._field-name,opcFieldList) EQ 0 THEN ASSIGN 
            opcFieldList = opcFieldList + asi._field._field-name + ",".
    END.

    ASSIGN 
        opcFieldList = TRIM(opcFieldList,",").

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pRuleBlankCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRuleBlankCompany Procedure
PROCEDURE pRuleBlankCompany:
/*------------------------------------------------------------------------------
 Purpose:   locates records with blank company code and adds to temp-table
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKeyPrefix AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    CREATE BUFFER hBuffer FOR TABLE ipcFileName. 
    CREATE QUERY hQuery.
     
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr). 
        IF hRecKey:NAME EQ "rec_key" THEN LEAVE.
    END.

    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
                          ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" + " AND " +
                          ipcFileName + ".company EQ ''" +
                          " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        FIND FIRST ttFileList WHERE 
            ttFileList.cFileName EQ ipcFileName AND 
            ttFileList.rRowID EQ hBuffer:ROWID
            NO-ERROR.
        IF NOT AVAIL ttFileList THEN DO:  
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName = ipcFileName
                ttFileList.rRowID = hBuffer:ROWID
                ttFileList.cRec_key = hRecKey:BUFFER-VALUE. 
        END.        
        ASSIGN 
            iProcessedCount = iProcessedCount + 1
            ttFileList.cRule = ttFileList.cRule + "Blank Company" + ","
            ttFileList.cMessage = ttFileList.cMessage + "-" + ",".
        hQuery:GET-NEXT().
    END.

        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pRuleBlankI-No) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRuleBlankI-No Procedure
PROCEDURE pRuleBlankI-No:
/*------------------------------------------------------------------------------
 Purpose:   Test for blank i-no fields
 Notes:      
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKeyPrefix AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    CREATE BUFFER hBuffer FOR TABLE ipcFileName. 
    CREATE QUERY hQuery.
     
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr). 
        IF hRecKey:NAME EQ "rec_key" THEN LEAVE.
    END.

    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
        ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" + " AND " +
        ipcFileName + ".i-no EQ ''" +
        " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        FIND FIRST ttFileList WHERE 
            ttFileList.cFileName EQ ipcFileName AND 
            ttFileList.rRowID EQ hBuffer:ROWID
            NO-ERROR.
        IF NOT AVAIL ttFileList THEN 
        DO:  
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName = ipcFileName
                ttFileList.rRowID = hBuffer:ROWID
                ttFileList.cRec_key = hRecKey:BUFFER-VALUE. 
        END.        
        ASSIGN 
            iProcessedCount = iProcessedCount + 1
            ttFileList.cRule = ttFileList.cRule + "Blank i-no" + ","
            ttFileList.cMessage = ttFileList.cMessage + "-" + ",".
        hQuery:GET-NEXT().
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pRuleBlankI-NoMOD1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRuleBlankI-NoMOD1 Procedure
PROCEDURE pRuleBlankI-NoMOD1:
/*------------------------------------------------------------------------------
 Purpose:   Test for blank i-no fields modified for related records
 Notes:      
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKeyPrefix AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    CREATE BUFFER hBuffer FOR TABLE ipcFileName. 
    CREATE QUERY hQuery.
     
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr). 
        IF hRecKey:NAME EQ "rec_key" THEN LEAVE.
    END.

    hQuery:ADD-BUFFER(hBuffer).
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hCompanyField = hBuffer:BUFFER-FIELD(iCtr). 
        IF hCompanyField:NAME EQ "company" THEN LEAVE.
    END.
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hRnoField = hBuffer:BUFFER-FIELD(iCtr). 
        IF hRnoField:NAME EQ "r-no" THEN LEAVE.
    END.

    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
        ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" + " AND " +
        ipcFileName + ".i-no EQ ''" + 
        " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        IF ipcFileName EQ "fg-rdtlh" THEN DO:
            IF CAN-FIND(FIRST fg-rcpth WHERE 
                        fg-rcpth.company EQ hCompanyField:BUFFER-VALUE AND 
                        fg-rcpth.r-no EQ INTEGER(hRnoField:BUFFER-VALUE)
                        USE-INDEX r-no) THEN DO:
                hQuery:GET-NEXT().
                NEXT.
            END.
        END.
        ELSE IF ipcFileName EQ "fg-rcpth" THEN DO:
            IF CAN-FIND(FIRST fg-rdtlh WHERE 
                        fg-rdtlh.company EQ hCompanyField:BUFFER-VALUE AND 
                        fg-rdtlh.r-no EQ INTEGER(hRnoField:BUFFER-VALUE)
                        USE-INDEX rm-rdtl) THEN DO: 
                hQuery:GET-NEXT().
                NEXT.
            END.
        END.
        FIND FIRST ttFileList WHERE 
            ttFileList.cFileName EQ ipcFileName AND 
            ttFileList.rRowID EQ hBuffer:ROWID
            NO-ERROR.
        IF NOT AVAIL ttFileList THEN 
        DO:  
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName = ipcFileName
                ttFileList.rRowID = hBuffer:ROWID 
                ttFileList.cRec_key = hRecKey:BUFFER-VALUE. 
        END.        
        ASSIGN 
            iProcessedCount = iProcessedCount + 1
            ttFileList.cRule = ttFileList.cRule + "Blank i-no" + ","
            ttFileList.cMessage = ttFileList.cMessage + "-" + ",".
        hQuery:GET-NEXT().
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pRuleBlankLoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRuleBlankLoc Procedure
PROCEDURE pRuleBlankLoc:
/*------------------------------------------------------------------------------
 Purpose:   Test for blank loc fields
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKeyPrefix AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    CREATE BUFFER hBuffer FOR TABLE ipcFileName. 
    CREATE QUERY hQuery.
     
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr). 
        IF hRecKey:NAME EQ "rec_key" THEN LEAVE.
    END.

    hQuery:ADD-BUFFER(hBuffer).
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
        ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" + " AND " +
        ipcFileName + ".loc EQ ''" +
        " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        FIND FIRST ttFileList WHERE 
            ttFileList.cFileName EQ ipcFileName AND 
            ttFileList.rRowID EQ hBuffer:ROWID
            NO-ERROR.
        IF NOT AVAIL ttFileList THEN 
        DO:  
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName = ipcFileName
                ttFileList.rRowID = hBuffer:ROWID 
                ttFileList.cRec_key = hRecKey:BUFFER-VALUE. 
        END.        
        ASSIGN 
            iProcessedCount = iProcessedCount + 1
            ttFileList.cRule = ttFileList.cRule + "Blank Loc" + ","
            ttFileList.cMessage = ttFileList.cMessage + "-" + ",".
        hQuery:GET-NEXT().
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pRuleInvalidCompany) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRuleInvalidCompany Procedure
PROCEDURE pRuleInvalidCompany:
/*------------------------------------------------------------------------------
 Purpose:   Finds records with invalid company code and adds to temp-table
 Notes:     On large tables, this will be slow since we need to scan all records
            If rec_key is indexed, we do limit based on that value
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKeyPrefix AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    FOR EACH company NO-LOCK:
        ASSIGN 
            cCompanyList = cCompanyList + company.company + ",".
    END.
    ASSIGN 
        cCompanyList = TRIM(cCompanyList,",").
        
    CREATE BUFFER hBuffer FOR TABLE ipcFileName. 
    CREATE QUERY hQuery.
     
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr). 
        IF hRecKey:NAME EQ "rec_key" THEN LEAVE.
    END.

    hQuery:ADD-BUFFER(hBuffer).
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hTestField = hBuffer:BUFFER-FIELD (iCtr).
        IF hTestField:NAME EQ "company" THEN LEAVE.
    END.
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
        ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" +
        " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        
        /* Test this here, not in the query, since this can't be indexed */
        IF CAN-DO(cCompanyList,STRING(hTestField:BUFFER-VALUE)) THEN DO:
            hQuery:GET-NEXT().
            NEXT.
        END.
        
        FIND FIRST ttFileList WHERE 
            ttFileList.cFileName EQ ipcFileName AND 
            ttFileList.rRowID EQ hBuffer:ROWID
            NO-ERROR.
        IF NOT AVAIL ttFileList THEN 
        DO:  
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName = ipcFileName
                ttFileList.rRowID = hBuffer:ROWID 
                ttFileList.cRec_key = hRecKey:BUFFER-VALUE. 
        END.        
        ASSIGN 
            iProcessedCount = iProcessedCount + 1
            ttFileList.cRule = ttFileList.cRule + "Invalid Company" + ","
            ttFileList.cMessage = ttFileList.cMessage + STRING(hTestField:BUFFER-VALUE) + ",".
        hQuery:GET-NEXT().
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pRuleInvalidInv-No) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRuleInvalidInv-No Procedure
PROCEDURE pRuleInvalidInv-No:
/*------------------------------------------------------------------------------
 Purpose:   Finds records with invalid inv-no and adds to temp-table
 Notes:     On large tables, this will be slow since we need to scan all records
            If rec_key is indexed, we do limit based on that value    
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKeyPrefix AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    FOR EACH loc NO-LOCK:
        ASSIGN 
            cLocList = cLocList + loc.loc + ",".
    END.
    ASSIGN 
        cLocList = TRIM(cLocList,",").
        
    CREATE BUFFER hBuffer FOR TABLE ipcFileName. 
    CREATE QUERY hQuery.

    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr). 
        IF hRecKey:NAME EQ "rec_key" THEN LEAVE.
    END.

    hQuery:ADD-BUFFER(hBuffer).

    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hCompanyField = hBuffer:BUFFER-FIELD(iCtr). 
        IF hCompanyField:NAME EQ "company" THEN LEAVE.
    END.
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hInvNoField = hBuffer:BUFFER-FIELD(iCtr). 
        IF hInvNoField:NAME EQ "inv-no" THEN LEAVE.
    END.

    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
        ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" +
        " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        
        /* Test this here, not in the query, since this can't be indexed */
        IF CAN-FIND(FIRST inv-head WHERE 
            inv-head.company EQ hCompanyField:BUFFER-VALUE AND 
            inv-head.inv-no EQ INTEGER(hInvNoField:BUFFER-VALUE)) THEN DO:
            hQuery:GET-NEXT().
            NEXT.
        END.        
        IF CAN-FIND(FIRST ar-inv WHERE 
            ar-inv.company EQ hCompanyField:BUFFER-VALUE AND 
            ar-inv.inv-no EQ INTEGER(hInvNoField:BUFFER-VALUE)) THEN DO:
            hQuery:GET-NEXT().
            NEXT.
        END.        
        FIND FIRST ttFileList WHERE 
            ttFileList.cFileName EQ ipcFileName AND 
            ttFileList.rRowID EQ hBuffer:ROWID
            NO-ERROR.
        IF NOT AVAIL ttFileList THEN 
        DO:  
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName = ipcFileName
                ttFileList.rRowID = hBuffer:ROWID 
                ttFileList.cRec_key = hRecKey:BUFFER-VALUE. 
        END.        
        ASSIGN 
            iProcessedCount = iProcessedCount + 1
            ttFileList.cRule = ttFileList.cRule + "Invalid Inv-No" + ","
            ttFileList.cMessage = ttFileList.cMessage + STRING(hInvNoField:BUFFER-VALUE) + ",".
        hQuery:GET-NEXT().
    END.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pRuleInvalidLoc) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRuleInvalidLoc Procedure
PROCEDURE pRuleInvalidLoc:
/*------------------------------------------------------------------------------
 Purpose:   Finds records with invalid company code and adds to temp-table
 Notes:     On large tables, this will be slow since we need to scan all records
            If rec_key is indexed, we do limit based on that value    
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKeyPrefix AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    FOR EACH loc NO-LOCK:
        ASSIGN 
            cLocList = cLocList + loc.loc + ",".
    END.
    ASSIGN 
        cLocList = TRIM(cLocList,",").
        
    CREATE BUFFER hBuffer FOR TABLE ipcFileName. 
    CREATE QUERY hQuery.
     
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr). 
        IF hRecKey:NAME EQ "rec_key" THEN LEAVE.
    END.

    hQuery:ADD-BUFFER(hBuffer).
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN 
            hTestField = hBuffer:BUFFER-FIELD (iCtr).
        IF hTestField:NAME EQ "loc" THEN LEAVE.
    END.
    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
        ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" +
        " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        
        /* Test this here, not in the query, since this can't be indexed */
        IF CAN-DO(cLocList,STRING(hTestField:BUFFER-VALUE)) THEN DO:
            hQuery:GET-NEXT().
            NEXT.
        END.    
        FIND FIRST ttFileList WHERE 
            ttFileList.cFileName EQ ipcFileName AND 
            ttFileList.rRowID EQ hBuffer:ROWID
            NO-ERROR.
        IF NOT AVAIL ttFileList THEN 
        DO:  
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName = ipcFileName
                ttFileList.rRowID = hBuffer:ROWID 
                ttFileList.cRec_key = hRecKey:BUFFER-VALUE. 
        END.        
        ASSIGN 
            iProcessedCount = iProcessedCount + 1
            ttFileList.cRule = ttFileList.cRule + "Invalid Loc" + ","
            ttFileList.cMessage = ttFileList.cMessage + STRING(hTestField:BUFFER-VALUE) + ",".
        hQuery:GET-NEXT().
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-pRuleInvalidOrd-No) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRuleInvalidOrd-No Procedure
PROCEDURE pRuleInvalidOrd-No:
/*------------------------------------------------------------------------------
 Purpose:   Finds records with invalid ord-no and adds to temp-table
 Notes:     On large tables, this will be slow since we need to scan all records
            If rec_key is indexed, we do limit based on that value    
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcRecKeyPrefix AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    FOR EACH loc NO-LOCK:
        ASSIGN 
            cLocList = cLocList + loc.loc + ",".
    END.
    ASSIGN 
        cLocList = TRIM(cLocList,",").
        
    CREATE BUFFER hBuffer FOR TABLE ipcFileName. 
    CREATE QUERY hQuery.

    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hRecKey = hBuffer:BUFFER-FIELD(iCtr). 
        IF hRecKey:NAME EQ "rec_key" THEN LEAVE.
    END.

    hQuery:ADD-BUFFER(hBuffer).

    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hCompanyField = hBuffer:BUFFER-FIELD(iCtr). 
        IF hCompanyField:NAME EQ "company" THEN LEAVE.
    END.
    DO ictr = 1 TO hBuffer:NUM-FIELDS:
        ASSIGN
            hOrdNoField = hBuffer:BUFFER-FIELD(iCtr). 
        IF hOrdNoField:NAME EQ "ord-no" THEN LEAVE.
    END.

    hQuery:QUERY-PREPARE ("FOR EACH " + ipcFileName + " WHERE " +
        ipcFileName + ".rec_key LT '" + ipcRecKeyPrefix + "'" +
        " NO-LOCK").
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST().
    
    DO WHILE NOT hQuery:QUERY-OFF-END:
        
        /* Test this here, not in the query, since this can't be indexed */
        IF CAN-FIND(FIRST oe-ord WHERE 
                    oe-ord.company EQ hCompanyField:BUFFER-VALUE AND 
                    oe-ord.ord-no EQ INTEGER(hOrdNoField:BUFFER-VALUE)) THEN DO:
            hQuery:GET-NEXT().
            NEXT.
        END.
        FIND FIRST ttFileList WHERE 
            ttFileList.cFileName EQ ipcFileName AND 
            ttFileList.rRowID EQ hBuffer:ROWID
            NO-ERROR.
        IF NOT AVAIL ttFileList THEN 
        DO:  
            CREATE ttFileList.
            ASSIGN 
                ttFileList.cFileName = ipcFileName
                ttFileList.rRowID = hBuffer:ROWID 
                ttFileList.cRec_key = hRecKey:BUFFER-VALUE. 
        END.        
        ASSIGN 
            ttFileList.cRule = ttFileList.cRule + "Invalid Ord-No" + ","
            ttFileList.cMessage = ttFileList.cMessage + STRING(hOrdNoField:BUFFER-VALUE) + ",".
        hQuery:GET-NEXT().
    END.
    
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeComplete) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeComplete Procedure
PROCEDURE purgeComplete:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    OUTPUT STREAM sReport TO VALUE (cOutputDir + "\" + "_PurgeReport.csv").
    
    PUT STREAM sReport UNFORMATTED 
        "Table Name,Purged?,Rec Key,Rule,Value,Rule,Value,Rule,Value" + CHR(10).
    FOR EACH ttFileList:
        PUT STREAM sReport UNFORMATTED 
            ttFileList.cFileName + "," +
            STRING(ttFileList.lPurged) + ","
            ttFileList.cRec_Key + ",".
        DO iCtr = 1 TO NUM-ENTRIES(cRule):
            PUT STREAM sReport UNFORMATTED 
                ENTRY(iCtr,cRule) + ","
                ENTRY(iCtr,cMessage) + ",".
        END.
        PUT STREAM sReport UNFORMATTED CHR(10). 
    END.
    OUTPUT STREAM sReport CLOSE.
    
    IF lPurge THEN DO:
        FOR EACH ttFileList 
            BREAK BY ttFileList.cFileName:
                
            IF FIRST-OF(ttFileList.cFileName) THEN DO:
                OUTPUT STREAM sDump TO VALUE (cOutputDir + "\" + ttFileList.cFileName + ".d").
                CREATE BUFFER hBuffer FOR TABLE ttFileList.cFileName. 
                hBuffer:DISABLE-LOAD-TRIGGERS(FALSE).
                CREATE QUERY hQuery.
                hQuery:ADD-BUFFER(hBuffer).
            END.
 
            DO TRANSACTION:
                hQuery:QUERY-PREPARE("FOR EACH " + ttFileList.cFileName +
                                     " WHERE ROWID(" + ttFileList.cFileName + ")" +
                                     " EQ TO-ROWID('" + STRING(ttFileList.rRowID) + "')").
                hQuery:QUERY-OPEN().
                hQuery:GET-FIRST(EXCLUSIVE-LOCK).
                IF hBuffer:AVAILABLE THEN DO:
                    PUT STREAM sDump UNFORMATTED dynExport(hBuffer, " ") SKIP.
                    hBuffer:BUFFER-DELETE().
                END.
            END.
            
            IF LAST-OF(ttFileList.cFileName) THEN DO:
                OUTPUT STREAM sDump CLOSE.
                DELETE OBJECT hQuery.
                DELETE OBJECT hBuffer.
            END.
        END.
    END.
    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeJobs) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeJobs Procedure
PROCEDURE purgeJobs:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcMode AS CHAR NO-UNDO.
    

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF


&IF DEFINED(EXCLUDE-purgeOrphans) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE purgeOrphans Procedure
PROCEDURE purgeOrphans:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:  Calling syntax:
             RUN purgeOrphans IN hPurge (cMode,  /* 'PREVIEW' or 'PURGE' */
                                        cFileName,
                                        cDateLimit,
                                        cOutputDir,
                                        OUTPUT lError,
                                        OUTPUT cMessage).
     
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipcMode AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcFileName AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcDateLimit AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipcOutputDir AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opiProcessedCount AS INT NO-UNDO.
    DEF OUTPUT PARAMETER oplError AS LOG NO-UNDO.
    DEF OUTPUT PARAMETER opcMessage AS CHAR NO-UNDO.
    
    DEF VAR daDateLimit AS DATE NO-UNDO.
    DEF VAR cFieldNameList AS CHAR NO-UNDO.
    
    ASSIGN
        daDateLimit = IF ipcDateLimit NE "" THEN DATE(ipcDateLimit) ELSE TODAY
        cRecKeyPrefix = STRING(YEAR(daDateLimit),"9999") +
                        STRING(MONTH(daDateLimit),"99") +
                        STRING(DAY(daDateLimit),"99")
        iProcessedCount = 0
        lPurge = IF ipcMode EQ "Purge" THEN TRUE ELSE FALSE.
   
    OS-CREATE-DIR VALUE(ipcOutputDir).
    ASSIGN 
        cOutputDir = ipcOutputDir.
         
    RUN pGetFieldList (ipcFileName, OUTPUT cFieldNameList).
    
    IF CAN-DO(cFieldNameList,"company") THEN 
        RUN pRuleBlankCompany (ipcFileName,
                               cRecKeyPrefix,
                               OUTPUT oplError,
                               OUTPUT opcMessage).
    
    IF CAN-DO(cFieldNameList,"loc")  
    AND NOT CAN-DO("item,itemfg",ipcFileName) THEN 
        RUN pRuleBlankLoc (ipcFileName,
                           cRecKeyPrefix,
                           OUTPUT oplError,
                           OUTPUT opcMessage).

    IF CAN-DO(cFieldNameList,"i-no") 
    AND NOT CAN-DO("rm-bin,fg-bin,fg-rcpth,fg-rdtlh",ipcFileName) THEN 
        RUN pRuleBlankI-no (ipcFileName,
                            cRecKeyPrefix,
                            OUTPUT oplError,
                            OUTPUT opcMessage).

    IF CAN-DO(cFieldNameList,"i-no") 
    AND CAN-DO("fg-rcpth,fg-rdtlh",ipcFileName) THEN 
        RUN pRuleBlankI-noMOD1 (ipcFileName,
                                cRecKeyPrefix,
                                OUTPUT oplError,
                                OUTPUT opcMessage).

    IF CAN-DO(cFieldNameList,"ord-no") THEN 
        RUN pRuleInvalidOrd-No (ipcFileName,
                                cRecKeyPrefix,
                                OUTPUT oplError,
                                OUTPUT opcMessage).

    IF CAN-DO(cFieldNameList,"inv-no")  
    AND NOT CAN-DO("rm-bin,fg-bin,fg-rcpth,fg-rdtlh",ipcFileName) THEN 
        RUN pRuleInvalidInv-No (ipcFileName,
                                cRecKeyPrefix,
                                OUTPUT oplError,
                                OUTPUT opcMessage).

    FOR EACH ttFileList:
        ASSIGN 
            ttFileList.cRule = TRIM(ttFileList.cRule,",")
            ttFileList.cMessage = TRIM(ttFileList.cMessage,",")
            ttFileList.lPurge = IF ipcMode EQ "Purge" THEN TRUE ELSE FALSE.
    END.     
    
    ASSIGN 
        opiProcessedCount = iProcessedCount.                  

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF





/* ************************  Function Implementations ***************** */
&IF DEFINED(EXCLUDE-dynExport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION dynExport Procedure
FUNCTION dynExport RETURNS CHARACTER 
  ( INPUT hRecord AS HANDLE , INPUT cDelim AS CHAR ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE hFld     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iCnt     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iExtnt   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTmp     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cArray   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResult  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cLobname AS CHARACTER NO-UNDO.

    IF hRecord:TYPE <> "BUFFER" THEN
        RETURN ?.

    DO iCnt = 1 TO hRecord:NUM-FIELDS:

        ASSIGN 
            hFld = hRecord:BUFFER-FIELD(iCnt).

        /* Handle EXPORT for large objects by writing them out to .blb files. Omit this section in Progress 9 
         * EXPORT adds extra "" for output compatible with the INPUT statement.
         * Names for blobs are not guaranteed the same as the static EXPORT statement, IMPORT handles them correctly. 
        */
        IF hFld:DATA-TYPE = "clob" OR hFld:DATA-TYPE = "blob" THEN 
        DO:
            IF hFld:BUFFER-VALUE = ? THEN 
            DO:
                cResult = cResult + "?" + cDelim.
            END.
            ELSE 
            DO:
                cLobname = hFld:NAME +
                    (IF hFld:DATA-TYPE = "clob" THEN "!" + GET-CODEPAGE(hFld:BUFFER-VALUE) + "!" ELSE "") 
                    + hRecord:TABLE + "_" + STRING(hRecord:RECID) + ".blb".
                COPY-LOB FROM hFld:BUFFER-VALUE TO FILE cLobname NO-CONVERT.
                cResult = cResult + QUOTER(cLobname) + cDelim.
            END.
            NEXT.
        END.
      
        IF hFld:EXTENT = 0 THEN 
        DO:
            IF hFld:BUFFER-VALUE EQ ? THEN ASSIGN 
                cTmp = "?".
            ELSE 
         
                CASE hFld:DATA-TYPE:
                    WHEN "character" THEN 
                        cTmp = QUOTER(hFld:BUFFER-VALUE).
                    WHEN "raw"  THEN 
                        cTmp = '"' + STRING(hFld:BUFFER-VALUE) + '"'.
                    WHEN "datetime" OR 
                    WHEN "datetime-tz" THEN ASSIGN 
                        cTmp = string(year(hFld:BUFFER-VALUE),"9999") 
                            + "-" + string(month(hFld:BUFFER-VALUE),"99") 
                            + "-" + string(day(hFld:BUFFER-VALUE),"99") 
                            + "T" + substring(string(hFld:BUFFER-VALUE),12).
                    OTHERWISE  
                    cTmp = STRING(hFld:BUFFER-VALUE).
                END CASE.
         
            cResult = cResult + cTmp + cDelim.
        END.
        ELSE 
        DO:
            cArray = "".   
            DO iExtnt = 1 TO hFld:EXTENT:
                IF hFld:BUFFER-VALUE(iExtnt) EQ  ? THEN ASSIGN 
                    cTmp = "?".
                ELSE

                    CASE hFld:DATA-TYPE:
                        WHEN "character" THEN 
                            cTmp = QUOTER(hFld:BUFFER-VALUE(iExtnt)).
                        WHEN "raw"          THEN 
                            cTmp = '"' + STRING(hFld:BUFFER-VALUE(iExtnt)) + '"'.
                        WHEN "datetime" OR 
                        WHEN "datetime-tz" THEN 
                            cTmp = string(year(hFld:BUFFER-VALUE(iExtnt)),"9999") 
                                + "-" + string(month(hFld:BUFFER-VALUE(iExtnt)),"99") 
                                + "-" + string(day(hFld:BUFFER-VALUE(iExtnt)),"99") 
                                + "T" + substring(string(hFld:BUFFER-VALUE(iExtnt)),12).
                        OTHERWISE  
                        cTmp = STRING(hFld:BUFFER-VALUE(iExtnt)).
                    END CASE.

                cArray = cArray + cTmp + cDelim.
            END.
            cResult = cResult + RIGHT-TRIM(cArray,cDelim) + cDelim.
        END.
    END.
    RETURN RIGHT-TRIM(cResult,cDelim).
END FUNCTION.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ENDIF

