/*------------------------------------------------------------------------
    File        : dataGridProc.i
    Purpose     :

    Syntax      :

    Description :

    Author(s)   : Ron Stark
    Created     : Sun Jan 15 15:44:31 EST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cGridColumns AS CHARACTER NO-UNDO.
DEFINE VARIABLE cGridQuery   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lShowQuery   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lDeveloper   AS LOGICAL   NO-UNDO.

ON 'CTRL-ALT-G':U ANYWHERE
DO:
    IF lDeveloper THEN 
    RUN pDataGridDat.        
END.

ON 'CTRL-ALT-Q':U ANYWHERE
DO:
    IF lDeveloper THEN DO: 
        lShowQuery = NOT lShowQuery.
        MESSAGE 
            "Show Query" STRING(lShowQuery,"YES/NO")
                VIEW-AS ALERT-BOX.        
    END. /* if ldeveloper */
END.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FIND FIRST users NO-LOCK
     WHERE users.user_id EQ USERID("ASI")
     NO-ERROR.
IF AVAILABLE users THEN
lDeveloper = users.developer.

IF SEARCH ("developer.dat") NE ? THEN 
lDeveloper = YES.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pApplyFilterHandler:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER sender AS System.Object NO-UNDO.
  DEFINE INPUT PARAMETER e      AS System.EventArgs NO-UNDO.
  
  DEFINE VARIABLE oFilterValues AS Consultingwerk.Framework.Collections.CharacterDictionary NO-UNDO.
  DEFINE VARIABLE cGridErrors   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cCellColumn   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE hCellColumn   AS HANDLE    NO-UNDO.
  DEFINE VARIABLE cBuffers      AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cQuery        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE jdx           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE hBuffer       AS HANDLE    NO-UNDO EXTENT 10.

  IF VALID-OBJECT (oRenderedBrowseControl) AND cGridQuery NE "" THEN DO:
      ASSIGN
        oFilterValues = CAST (sender, RenderedBrowseWithSearchControl):FilterValues
        cQuery        = cGridQuery
        cQuery        = REPLACE (cQuery, "%company%", g_company)
        cQuery        = REPLACE (cQuery, "%loc%"    , g_loc    )
        &IF DEFINED(dataGridInclude) NE 0 &THEN
        {Advantzware/WinKit/dataGridInclude.i {{&dataGridInclude}}}
        &ENDIF
        cGridErrors   = SEARCH ("dataGrid/" + REPLACE (THIS-PROCEDURE:NAME,".w",".dat")).
        cGridErrors   = REPLACE (cGridErrors, ".dat", ".err")
        .
      DO idx = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME} :
          ASSIGN
            hCellColumn = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx)
            cCellColumn = (IF INDEX (oFilterValues:Keys,":") EQ 0 THEN ""
                           ELSE hCellColumn:TABLE + ":")
                        + hCellColumn:NAME
            .
          IF CAN-DO (oFilterValues:Keys,cCellColumn) THEN
          cQuery = cQuery + (IF INDEX (cQuery, "WHERE") EQ 0 THEN " WHERE "    ELSE " AND ")
                 + hCellColumn:TABLE + "." + hCellColumn:NAME
                 + (IF hCellColumn:DATA-TYPE EQ "Character" THEN  " BEGINS ~"" ELSE " EQ " )
                 + oFilterValues:GetValue (cCellColumn)
                 + (IF hCellColumn:DATA-TYPE EQ "Character" THEN  "~""         ELSE ""     )
                 .
      END. /* do idx */
      
      &IF DEFINED(EXTERNAL-TABLES) &THEN
      {Advantzware/WinKit/externalTables.i {&EXTERNAL-TABLES}}
      DO idx = 1 TO EXTENT (hBuffer) :
          IF NOT VALID-HANDLE (hBuffer[idx]) THEN LEAVE.
          DO jdx = 1 TO hBuffer[idx]:NUM-FIELDS :
              cQuery = REPLACE (cQuery, "%"
                     + hBuffer[idx]:NAME + "."
                     + hBuffer[idx]:BUFFER-FIELD (jdx):NAME + "%",
                       hBuffer[idx]:BUFFER-FIELD (jdx):BUFFER-VALUE ).
          END. /* do jdx */
      END. /* do idx */
      &ENDIF
      
      IF lShowQuery THEN 
      MESSAGE
        "Query:" SKIP(1) cQuery SKIP(2)
        "Base Template:" SKIP(1) cGridQuery SKIP(2)
        "Include: {{&dataGridInclude}}"
            VIEW-AS ALERT-BOX.
            
      QUERY {&BROWSE-NAME}:QUERY-PREPARE (cQuery) NO-ERROR.      
      IF ERROR-STATUS:NUM-MESSAGES NE 0 THEN DO: 
          IF lDeveloper THEN DO:
              OUTPUT TO VALUE (cGridErrors).
              PUT UNFORMATTED "PROGRAM: " THIS-PROCEDURE:NAME SKIP(1). 
              DO idx = 1 TO ERROR-STATUS:NUM-MESSAGES:
                  PUT UNFORMATTED "ERROR: " AT 5 ERROR-STATUS:GET-MESSAGE (idx).
              END. /* do idx */
              PUT UNFORMATTED SKIP (1) "QUERY: " AT 5 cQuery SKIP.
              OUTPUT CLOSE.
              OS-COMMAND NO-WAIT notepad.exe VALUE (cGridErrors).
          END. /* if ldeveloper */
      END. /* if error */
      ELSE DO:
          IF SEARCH (cGridErrors) NE ? THEN 
          OS-DELETE VALUE (cGridErrors).
          QUERY {&BROWSE-NAME}:QUERY-OPEN.
          QUERY {&BROWSE-NAME}:GET-FIRST().
      END.
  END. /* valid-object */

END PROCEDURE.

PROCEDURE pCreateDataGridDat:
    DEFINE INPUT PARAMETER ipcDataGridDat     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcDataGridInclude AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hCellColumn AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cIdxNames   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNonIdx     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE CQueryStr   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cFiles      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTableName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRecords    AS INTEGER   NO-UNDO.
    
    DO idx = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME} :
        hCellColumn = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
        FIND FIRST asi._file NO-LOCK
             WHERE asi._file._file-name EQ hCellColumn:TABLE
             NO-ERROR.
        IF AVAILABLE asi._file THEN DO:
            IF NOT CAN-DO (cFiles,hCellColumn:TABLE) THEN
            cFiles = cFiles + hCellColumn:TABLE + ",".
            FIND FIRST asi._field OF asi._file NO-LOCK
                 WHERE asi._field._field-name EQ hCellColumn:NAME
                 NO-ERROR.
            IF AVAILABLE asi._field AND asi._field._extent EQ 0 THEN DO:
                IF CAN-FIND (FIRST asi._index-field OF asi._field) THEN
                cIdxNames   = cIdxNames + hCellColumn:NAME + ",".
                ELSE cNonIdx   = cNonIdx + hCellColumn:NAME + ",".
            END. /* avail _field */
            ELSE cNonIdx   = cNonIdx + hCellColumn:NAME + ",".
        END. /* avail _file */
        ELSE cNonIdx   = cNonIdx + hCellColumn:NAME + ",".
    END. /* do idx */
    ASSIGN
        cIdxNames      = TRIM (cIdxNames,",")
        cNonIdx        = TRIM (cNonIdx,",")
        ipcDataGridDat = REPLACE (SEARCH ("dataGrid/dataGrid.dat"),"dataGrid\dataGrid.dat",ipcDataGridDat)
        cQueryStr      = '{&QUERY-STRING-{&BROWSE-NAME}}'
        cQueryStr      = REPLACE (cQueryStr," NO-LOCK","")
        cQueryStr      = REPLACE (cQueryStr,"WHERE","NO-LOCK WHERE")
        cQueryStr      = REPLACE (cQueryStr," TRUE       AND","")
        cQueryStr      = REPLACE (cQueryStr,"g_company","~"%company%~"")
        cQueryStr      = REPLACE (cQueryStr,"gcompany","~"%company%~"")
        cQueryStr      = REPLACE (cQueryStr,"g_loc","~"%loc%~"")
        cQueryStr      = REPLACE (cQueryStr,"gloc","~"%loc%~"")
        cQueryStr      = REPLACE (cQueryStr,"=","EQ")
        cQueryStr      = REPLACE (cQueryStr,"  "," ")
        cQueryStr      = REPLACE (cQueryStr,"  "," ")
        cQueryStr      = REPLACE (cQueryStr,"WHERE TRUE AND","WHERE")
        cQueryStr      = REPLACE (cQueryStr,"WHERE TRUE","WHERE")
        cQueryStr      =    TRIM (cQueryStr)
        .
    IF ENTRY (NUM-ENTRIES (cQueryStr, " "), cQueryStr, " ") EQ "WHERE" THEN
    cQueryStr = REPLACE (cQueryStr," WHERE","").

    OUTPUT TO VALUE (ipcDataGridDat).
    PUT UNFORMATTED
        cQueryStr SKIP
        cIdxNames SKIP(1)
        "Indexed: " cIdxNames SKIP
        "Non-Idx: " cNonIdx SKIP(1)
        "Generated " STRING (TODAY ,"99.99.9999") " @ "
        STRING (TIME ,"hh:mm:ss am") " by: " USERID ("ASI") SKIP
        .
    cFiles = TRIM (cFiles,",").
    DO idx = 1 TO NUM-ENTRIES (cFiles):
        INPUT FROM VALUE (SEARCH ("dataGrid\dataGrid.dat")) NO-ECHO.
        REPEAT:
            IMPORT cTableName iRecords.
            IF cTableName EQ ENTRY (idx,cFiles) THEN LEAVE.
            iRecords = 0.
        END. /* repeat */
        INPUT CLOSE.
        IF idx GT 1 THEN 
        PUT UNFORMATTED SKIP (1).
        PUT UNFORMATTED "Table: " ENTRY (idx,cFiles) " - Records: " LEFT-TRIM (STRING (iRecords,">>>,>>>,>>9")) SKIP.
        FIND FIRST asi._file NO-LOCK
             WHERE asi._file._file-name EQ ENTRY (idx,cFiles)
             NO-ERROR.
        IF AVAILABLE asi._file THEN DO:
            FOR EACH asi._index OF asi._file NO-LOCK :
                PUT UNFORMATTED SKIP(1) "Index: " AT 5 asi._index._index-name SKIP .
                FOR EACH asi._index-field OF asi._index NO-LOCK,
                    FIRST asi._field OF asi._index-field NO-LOCK :
                    PUT UNFORMATTED "Field: " AT 10 asi._field._field-name SKIP .
                END. /* each _index-field */
            END. /* each _index */
        END. /* if avail */
    END. /* do idx */
    OUTPUT CLOSE.
    RUN util/dataGridDat.w (ipcDataGridDat, ipcDataGridInclude, "{&EXTERNAL-TABLES}", "{&TABLES-IN-QUERY-{&BROWSE-NAME}}").
    RUN pGetDataGridDat (ipcDataGridDat).

END PROCEDURE.

PROCEDURE pCustomizeGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cellName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE gridName      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE oFilterValues AS Consultingwerk.Framework.Collections.CharacterDictionary NO-UNDO.

    IF VALID-OBJECT (oRenderedBrowseControl) THEN DO:
        DO idx = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME} :
            ASSIGN
                gridName = STRING (oRenderedBrowseControl:DisplayLayout:Bands[0]:Columns[idx - 1])
                cellName = IF NUM-ENTRIES (gridName,":") EQ 1 THEN gridName ELSE ENTRY (2,gridName,":")
                .
            IF CAN-DO (cGridColumns,cellName) THEN NEXT .
            oRenderedBrowseControl:DisplayLayout:Bands[0]:Columns[gridName]:FilterOperandStyle = Infragistics.Win.UltraWinGrid.FilterOperandStyle:None NO-ERROR .
        END. /* do idx */
    END. /* valid-object */

END PROCEDURE.

PROCEDURE pDataGridDat:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cDataGridDat     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDataGridInclude AS CHARACTER NO-UNDO.

  IF VALID-OBJECT (oRenderedBrowseControl) THEN DO:
      ASSIGN
        cDataGridDat     = SEARCH ("dataGrid/" + REPLACE (THIS-PROCEDURE:NAME,".w",".dat"))
        cDataGridInclude = SEARCH ("dataGrid/" + REPLACE (THIS-PROCEDURE:NAME,".w",".i"  ))
        .
      IF cDataGridDat NE ? THEN DO: 
          RUN util/dataGridDat.w (cDataGridDat, cDataGridInclude, "{&EXTERNAL-TABLES}", "{&TABLES-IN-QUERY-{&BROWSE-NAME}}").
          RUN pGetDataGridDat (cDataGridDat).
      END. /* if ne ? */
  END. /* valid-object */


END PROCEDURE.

PROCEDURE pDataGridInit:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cDataGridDat     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDataGridInclude AS CHARACTER NO-UNDO.

  IF VALID-OBJECT (oRenderedBrowseControl) THEN DO:
      ASSIGN
        cDataGridDat     = "dataGrid/" + REPLACE (THIS-PROCEDURE:NAME,".w",".dat")
        cDataGridInclude = "dataGrid/" + REPLACE (THIS-PROCEDURE:NAME,".w",".i"  )
        .
      IF SEARCH (cDataGridDat) EQ ? AND lDeveloper THEN
      RUN pCreateDataGridDat (cDataGridDat, cDataGridInclude, USERID("ASI")).
      IF SEARCH (cDataGridDat) NE ? THEN DO:
          RUN pGetDataGridDat (cDataGridDat).
          ASSIGN
              cGridQuery = REPLACE (cGridQuery,"%company%", g_company)
              cGridQuery = REPLACE (cGridQuery,"%loc%"    , g_loc    )
              .
          IF Consultingwerk.Util.ProcedureHelper:HasEntry(THIS-PROCEDURE, "pApplyFilterHandler") THEN
              CAST (oRenderedBrowseControl, Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseWithSearchControl):ApplyFilter:Subscribe ("pApplyFilterHandler") .
          IF Consultingwerk.Util.ProcedureHelper:HasEntry(THIS-PROCEDURE, "pCustomizeGrid"     ) THEN
              RUN pCustomizeGrid IN THIS-PROCEDURE .
      END. /* if search ne ? */
  END. /* valid-object */

END PROCEDURE.

PROCEDURE pGetDataGridDat:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcDataGridDat AS CHARACTER NO-UNDO.
    
    INPUT FROM VALUE(SEARCH(ipcDataGridDat)) NO-ECHO.
    IMPORT UNFORMATTED cGridQuery.
    IMPORT UNFORMATTED cGridColumns.
    INPUT CLOSE.

END PROCEDURE.

PROCEDURE InitializeGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    IF NOT VALID-OBJECT (oRenderedBrowseControl) THEN
        RETURN .

    /*
    // Ability to not allow user to group (Based on the number of records that would be loaded)
    oRenderedBrowseControl:DisplayLayout:ViewStyleBand =
        Infragistics.Win.UltraWinGrid.ViewStyleBand:Horizontal .

    // Disable sort on a per column base
    oRenderedBrowseControl:DisplayLayout:Bands[0]:Columns["industry"]:SortIndicator =
        Infragistics.Win.UltraWinGrid.SortIndicator:Disabled .
    */

END PROCEDURE.
