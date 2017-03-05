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

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE pApplyFilterHandler:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER sender AS System.Object NO-UNDO. 
  DEFINE INPUT PARAMETER e      AS System.EventArgs NO-UNDO. 
  
  DEFINE VARIABLE oFilterValues AS Consultingwerk.Framework.Collections.CharacterDictionary NO-UNDO. 
  DEFINE VARIABLE hCellColumn   AS HANDLE    NO-UNDO.
  DEFINE VARIABLE cQuery        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
  
  IF VALID-OBJECT (oRenderedBrowseControl) AND cGridQuery NE "" THEN DO:
      ASSIGN 
        oFilterValues = CAST (sender, RenderedBrowseWithSearchControl):FilterValues
        cQuery = cGridQuery
        .
      DO idx = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME} :
          hCellColumn = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
          IF NOT CAN-DO (oFilterValues:Keys,hCellColumn:NAME) THEN NEXT .
          cQuery = cQuery + "AND "
                 + hCellColumn:TABLE + "."
                 + hCellColumn:NAME
                 + (IF hCellColumn:DATA-TYPE EQ "Character" THEN  " BEGINS ~"" ELSE " EQ ")
                 + oFilterValues:GetValue (hCellColumn:NAME)
                 + (IF hCellColumn:DATA-TYPE EQ "Character" THEN "~"" ELSE "")
                 .             
      END. /* do idx */
  
      QUERY {&BROWSE-NAME}:QUERY-PREPARE (cQuery).
      QUERY {&BROWSE-NAME}:QUERY-OPEN.
  END. /* valid-object */

END PROCEDURE.

PROCEDURE pCreateDataGridDat:
    DEFINE INPUT PARAMETER ipcGridSearch AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hCellColumn AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cIdxNames   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cNonIdx     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE CQueryStr   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx         AS INTEGER   NO-UNDO.
    
    DO idx = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME} :
        hCellColumn = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
        FIND FIRST asi._file NO-LOCK
             WHERE asi._file._file-name EQ hCellColumn:TABLE
             NO-ERROR.
        IF AVAILABLE asi._file THEN DO: 
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
        cIdxNames     = TRIM (cIdxNames,",")
        cNonIdx       = TRIM (cNonIdx,",")
        ipcGridSearch = REPLACE (SEARCH ("dataGrid/dataGrid.dat"),"dataGrid\dataGrid.dat",ipcGridSearch)
        cQueryStr     = '{&QUERY-STRING-{&BROWSE-NAME}}'
        cQueryStr     = REPLACE (cQueryStr," NO-LOCK","")
        cQueryStr     = REPLACE (cQueryStr,"WHERE","NO-LOCK WHERE")
        cQueryStr     = REPLACE (cQueryStr,"g_company","%company%")
        cQueryStr     = REPLACE (cQueryStr,"=","EQ")
        .
    OUTPUT TO VALUE (ipcGridSearch).
    PUT UNFORMATTED cQueryStr SKIP.
    PUT UNFORMATTED cIdxNames SKIP.
    PUT UNFORMATTED "// Indexed: " cIdxNames SKIP.
    PUT UNFORMATTED "// Non-Idx: " cNonIdx SKIP.
    PUT UNFORMATTED "// Generated " STRING (TODAY, "99.99.9999") " @ " STRING (TIME, "hh:mm:ss am") " by: ".
    OUTPUT CLOSE.
    OS-COMMAND SILENT notepad.exe VALUE (ipcGridSearch).
    
END PROCEDURE.    

PROCEDURE pCustomizeGrid:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cellName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx      AS INTEGER   NO-UNDO.
    
    IF VALID-OBJECT (oRenderedBrowseControl) THEN DO:
        DO idx = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME} :
            cellName = STRING (oRenderedBrowseControl:DisplayLayout:Bands[0]:Columns[idx - 1]) .
            IF CAN-DO (cGridColumns,cellName) THEN NEXT .
            oRenderedBrowseControl:DisplayLayout:Bands[0]:Columns[cellName]:FilterOperandStyle = Infragistics.Win.UltraWinGrid.FilterOperandStyle:None NO-ERROR .
        END. /* do idx */
    END. /* valid-object */

END PROCEDURE.

PROCEDURE pDataGridInit:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cGridSearch AS CHARACTER NO-UNDO.
  
  IF VALID-OBJECT (oRenderedBrowseControl) THEN DO: 
      cGridSearch = "dataGrid/" + REPLACE (THIS-PROCEDURE:NAME,".w",".dat").
      IF SEARCH (cGridSearch) EQ ? AND CAN-DO("ASI,NoSweat",USERID("ASI")) THEN
      RUN pCreateDataGridDat (cGridSearch).
      IF SEARCH (cGridSearch) NE ? THEN DO:      
          INPUT FROM VALUE(SEARCH(cGridSearch)) NO-ECHO.
          IMPORT UNFORMATTED cGridQuery.
          IMPORT UNFORMATTED cGridColumns.
          INPUT CLOSE.
          ASSIGN 
              cGridQuery = REPLACE (cGridQuery,"%company%", "~"" + g_company + "~"")
              cGridQuery = REPLACE (cGridQuery,"%loc%","~""      + g_loc     + "~"")
              .
          IF Consultingwerk.Util.ProcedureHelper:HasEntry(THIS-PROCEDURE, "pApplyFilterHandler") THEN 
              CAST (oRenderedBrowseControl, Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseWithSearchControl):ApplyFilter:Subscribe ("pApplyFilterHandler") .          
          IF Consultingwerk.Util.ProcedureHelper:HasEntry(THIS-PROCEDURE, "pCustomizeGrid") THEN 
              RUN pCustomizeGrid IN THIS-PROCEDURE .
      END. /* if search */
  END. /* valid-object */    

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
