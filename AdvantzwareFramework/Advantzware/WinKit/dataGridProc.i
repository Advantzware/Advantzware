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
  DEFINE VARIABLE cellColumn    AS HANDLE    NO-UNDO.
  DEFINE VARIABLE cQuery        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
  
  IF VALID-OBJECT (oRenderedBrowseControl) AND cGridQuery NE "" THEN DO:
      ASSIGN 
        oFilterValues = CAST (sender, RenderedBrowseWithSearchControl):FilterValues
        cQuery = cGridQuery
        .
      DO idx = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME} :
          cellColumn = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
          IF NOT CAN-DO (oFilterValues:Keys,cellColumn:NAME) THEN NEXT .
          cQuery = cQuery + "AND "
                 + cellColumn:TABLE + "."
                 + cellColumn:NAME
                 + (IF cellColumn:DATA-TYPE EQ "Character" THEN  " BEGINS ~"" ELSE " EQ ")
                 + oFilterValues:GetValue (cellColumn:NAME)
                 + (IF cellColumn:DATA-TYPE EQ "Character" THEN "~"" ELSE "")
                 .             
      END. /* do idx */
  
      QUERY {&BROWSE-NAME}:QUERY-PREPARE (cQuery).
      QUERY {&BROWSE-NAME}:QUERY-OPEN.
  END. /* valid-object */

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
      INPUT FROM VALUE(SEARCH("dataGrid/dataGrid.dat")) NO-ECHO.
      REPEAT:
          IMPORT UNFORMATTED cGridSearch.
          IF cGridSearch EQ THIS-PROCEDURE:NAME THEN DO:
              IMPORT UNFORMATTED cGridColumns.
              IMPORT UNFORMATTED cGridQuery.
              LEAVE.
          END. /* if cgridsearch */
      END. /* repeat */
      INPUT CLOSE.
      ASSIGN 
          cGridQuery = REPLACE (cGridQuery,"%company%", "~"" + g_company + "~"")
          cGridQuery = REPLACE (cGridQuery,"%loc%","~""      + g_loc     + "~"")
          .
      IF Consultingwerk.Util.ProcedureHelper:HasEntry(THIS-PROCEDURE, "pApplyFilterHandler") THEN 
          CAST (oRenderedBrowseControl, Consultingwerk.WindowIntegrationKit.Controls.RenderedBrowseWithSearchControl):ApplyFilter:Subscribe ("pApplyFilterHandler") .          
      IF Consultingwerk.Util.ProcedureHelper:HasEntry(THIS-PROCEDURE, "pCustomizeGrid") THEN 
          RUN pCustomizeGrid IN THIS-PROCEDURE .
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
