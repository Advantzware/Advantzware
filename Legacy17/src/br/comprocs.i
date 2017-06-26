&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
&SCOPED-DEFINE maxColumns 200
DEF VAR cellName AS CHAR NO-UNDO EXTENT {&maxColumns}.
DEF VAR cellWidth AS DECI NO-UNDO EXTENT {&maxColumns}.
DEF VAR lMemoExist AS LOG NO-UNDO.
DEF VAR cThisFunct AS CHAR NO-UNDO.
DEF VAR iPassThru AS INT NO-UNDO.
DEF VAR hDataSource AS HANDLE NO-UNDO.
DEF VAR lChange AS LOG.
DEF VAR cField AS CHAR NO-UNDO.
DEF VAR hHandle AS HANDLE NO-UNDO.
DEF VAR hPopupHandle AS HANDLE NO-UNDO.
DEF VAR hMenuLevel1 AS HANDLE EXTENT 25 NO-UNDO.
DEF VAR hMenuLevel2 AS HANDLE EXTENT 100 NO-UNDO.
DEF VAR iMaxRecords AS INT.
DEF VAR cBaseFileName AS CHAR NO-UNDO.
DEF VAR old-rowid AS rowid NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEFINE new global SHARED VARIABLE amemo-handle   AS HANDLE NO-UNDO.
def var lmemo-disp as logical no-undo init yes.
ASSIGN
    iPassThru = {1}.
DEF FRAME fQuestion
    iMaxRecords LABEL "Enter the number of records to send to Excel"
    WITH OVERLAY THREE-D SIDE-LABELS WIDTH 100 VIEW-AS DIALOG-BOX.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */
ON CTRL-END OF br_table IN FRAME F-Main DO:
    APPLY 'END' TO br_table.
END.

ON CTRL-HOME OF br_table IN FRAME F-Main DO:
    APPLY 'HOME' TO br_table.
END.

ON ctrl-A OF br_table IN FRAME F-Main DO:
  br_table:SELECT-ALL().
   RUN ipSpecific IN THIS-PROCEDURE. 
END.

ON ctrl-D OF br_table IN FRAME F-Main DO:
    RUN deleteRecord IN WIDGET-HANDLE(ENTRY({1},DYNAMIC-FUNCTION('getDataTarget' IN hDataSource))).
END.

ON END OF br_table IN FRAME F-Main DO:
    {src/adm2/brsend.i}
    APPLY 'value-changed' TO br_table.
END.

ON HOME OF br_table IN FRAME F-Main DO:
    {src/adm2/brshome.i}
    APPLY 'value-changed' TO br_table.
END.

ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
OR RETURN OF br_table DO:
    RUN ipChooseSelect IN THIS-PROCEDURE.
END.

ON OFF-END OF br_table IN FRAME F-Main DO:
    DEF VAR iBatch AS INTEGER NO-UNDO.
    {src/adm2/brsoffnd.i}
END.

ON OFF-HOME OF br_table IN FRAME F-Main DO:
    {src/adm2/brsoffhm.i}
END.

ON ROW-ENTRY OF br_table IN FRAME F-Main DO:
    {src/adm2/brsentry.i}
END.

ON ROW-LEAVE OF br_table IN FRAME F-Main DO:
    {src/adm2/brsleave.i}
END.

ON SCROLL-NOTIFY OF br_table IN FRAME F-Main DO:
    {src/adm2/brsscrol.i}
END.

ON START-SEARCH OF br_table IN FRAME F-Main DO:
    DEF VAR AscDes AS CHAR NO-UNDO.
    DEF VAR hColumn AS HANDLE NO-UNDO.
    ASSIGN 
        hHandle = br_table:CURRENT-COLUMN
        cField = hHandle:NAME.
    
    if cField = "Description1"
    or cField = "Description2" then do:
        IF cField = "Description1" THEN assign 
            cField = "Description[1]".
        if cField = "Description2" then assign
            cField = "Description[2]".
        IF INDEX(DYNAMIC-FUNCTION('getQuerySort':U IN hDataSource),"descending") <> 0 THEN ASSIGN
            AscDes = "".
        ELSE ASSIGN 
            AscDes = "descending". 
        DYNAMIC-FUNCTION('setQuerySort':U IN hdatasource, "by " + cField + " " + ascdes).
        run chgqry in hDatasource.      
    end.            
    else DO:
        sortchg = DYNAMIC-FUNCTION('setSort':U, INPUT cField).
    end.
        IF INDEX(DYNAMIC-FUNCTION('getQuerySort':U IN hDataSource),"descending") <> 0 THEN ASSIGN
            AscDes = "DES".
        ELSE ASSIGN 
            AscDes = "ASC". 
        DO i = 1 TO br_table:NUM-COLUMNS:
            hColumn = br_table:GET-BROWSE-COLUMN(i).
            hColumn:LABEL-BGCOLOR = 14.
        END.
        ASSIGN
            hHandle:LABEL-BGCOLOR = IF AscDes = "ASC" THEN 10 ELSE 12.
END.

ON VALUE-CHANGED OF br_table IN FRAME F-Main DO:
    {src/adm2/brschnge.i}
    lmemo-disp = no.
    condition = "".
    RUN ipSpecific IN THIS-PROCEDURE.
    /* RUN SetState in h_parent.
    RUN StatusUpdate in h_parent. */
    RUN ipShowMemo IN THIS-PROCEDURE.
END.

&IF "{&Module}" = "op":U &THEN
    {op/brprocs.i iPassThru}
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeBrowserState Include 
PROCEDURE ChangeBrowserState :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER iState AS CHAR.

    IF iState = "enable" THEN ASSIGN
        BROWSE br_table:SENSITIVE = TRUE.
    ELSE IF iState = "disable" THEN ASSIGN
        BROWSE br_table:SENSITIVE = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disableAll Include 
PROCEDURE disableAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER lDisable AS LOG NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Include  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Include 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
    DEF VAR AscDes AS CHAR.
    DEF VAR cOrigSort AS CHAR.
    DEF VAR lSortSet AS LOG.
    DEF VAR cOptSort AS CHAR.
    DEF VAR vThisRowid AS CHAR.

  RUN SUPER.
    
  /* Code placed here will execute AFTER standard behavior.    */
    APPLY 'row-display' TO br_table IN FRAME {&FRAME-NAME}.
    RUN ipSpecific IN THIS-PROCEDURE.
    RUN setCellColumns IN THIS-PROCEDURE.
    
    ASSIGN
        hDataSource = DYNAMIC-FUNCTION('getDataSource').
    
    RUN ipSetSort IN THIS-PROCEDURE (OUTPUT cOptSort).
    IF cOptSort = "" 
    OR cOptSort = "BY " THEN. 
    ELSE DO:
        DYNAMIC-FUNCTION('setQuerySort':U IN hdatasource, cOptSort).
        RUN chgqry IN hDataSource.
    END.
    
    IF INDEX(DYNAMIC-FUNCTION('getQuerySort':U IN hDataSource),"descending") <> 0 THEN ASSIGN
        AscDes = "DES".
    ELSE ASSIGN 
        AscDes = "ASC". 

    ASSIGN
        cOrigSort =
            SUBSTRING(
                DYNAMIC-FUNCTION('getQuerySort':U IN hDataSource),
                INDEX(DYNAMIC-FUNCTION('getQuerySort':U IN hDataSource),"by") + 3).
    IF NUM-ENTRIES(cOrigSort," ") > 1 THEN ASSIGN
        cOrigSort = ENTRY(1,cOrigSort," ")
        cOrigSort = ENTRY(2,cOrigSort,".").
    ELSE IF NUM-ENTRIES(cOrigSort,".") > 1 THEN ASSIGN
        cOrigSort = ENTRY(2,cOrigSort,".").
    ELSE ASSIGN
        cOrigSort = "".

    DO i = 1 TO br_table:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        hColumn = br_table:GET-BROWSE-COLUMN(i).
        IF hColumn:NAME = cOrigSort THEN ASSIGN
            hColumn:LABEL-BGCOLOR = IF AscDes = "ASC" THEN 10 ELSE 13.
        ELSE ASSIGN 
            hColumn:LABEL-BGCOLOR = 14.
    END.
    DO i = 1 TO br_table:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        hColumn = br_table:GET-BROWSE-COLUMN(i).
        IF hColumn:LABEL-BGCOLOR = 10 
        OR hColumn:LABEL-BGCOLOR = 13 THEN
            ASSIGN lSortSet = TRUE.
    END.
    IF NOT lSortSet THEN DO:
        hColumn = br_table:GET-BROWSE-COLUMN(1).
        ASSIGN 
            hColumn:LABEL-BGCOLOR = 10.
    END.

    RUN ipCreatePopup IN THIS-PROCEDURE.
    ASSIGN
        FRAME {&FRAME-NAME}:POPUP-MENU = hPopupHandle.
    APPLY 'value-changed' TO br_table.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseCustomize Include 
PROCEDURE ipChooseCustomize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        br_table:COLUMN-MOVABLE IN FRAME {&FRAME-NAME} = TRUE
        br_table:COLUMN-RESIZABLE = TRUE
        br_table:ALLOW-COLUMN-SEARCHING = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseExcel Include 
PROCEDURE ipChooseExcel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ipDoWhat AS CHAR.

DEF VAR cTable              AS CHARACTER    NO-UNDO.
DEF VAR cFields             AS CHARACTER    NO-UNDO.
DEF VAR cQueryPosition      AS CHARACTER    NO-UNDO.
DEF VAR hQuery              AS HANDLE       NO-UNDO.
DEF VAR hBuffer             AS HANDLE       NO-UNDO.
DEF VAR hField              AS HANDLE       NO-UNDO.
DEF VAR iFields             AS INTEGER      NO-UNDO.
DEF VAR iRow                AS INTEGER      NO-UNDO.
DEF VAR cRow                AS CHARACTER    NO-UNDO INITIAL "A".
DEF VAR chExcelApplication  AS COM-HANDLE   NO-UNDO.
DEF VAR chWorkbook          AS COM-HANDLE   NO-UNDO.
DEF VAR chWorksheet         AS COM-HANDLE   NO-UNDO.
DEF VAR hThisField          AS HANDLE       NO-UNDO.
DEF VAR hThisColumn         AS HANDLE       NO-UNDO.
DEF VAR iCtr2               AS INT          NO-UNDO.
DEF VAR iCtr3               AS INT          NO-UNDO.
DEF VAR cColList            AS CHAR         NO-UNDO.
DEF VAR iColumn             AS INT          NO-UNDO.
DEF VAR lAvailable          AS LOG          NO-UNDO.
DEF VAR lExtentCleared      AS LOG          NO-UNDO.
DEF VAR cLastField          AS CHAR         NO-UNDO.
DEF VAR iBrowseCount        AS INT          NO-UNDO.
DEF VAR tWidth              AS INT          NO-UNDO.
DEF VAR iNumRecs AS INT NO-UNDO.
DEF VAR cQueryWhere AS CHAR.

    ASSIGN
        hQuery = br_table:QUERY IN FRAME {&FRAME-NAME}
        hBuffer = hQuery:GET-BUFFER-HANDLE(1)
        hquery:CACHE = 0.
    IF hQuery:GET-FIRST() = FALSE THEN 
        RETURN.

    cQueryWhere = DYNAMIC-FUNCTION('getQueryWhere':U IN hDataSource).
    cQueryWhere = REPLACE(cQueryWhere,"NO-LOCK  INDEXED-REPOSITION","").
    cQueryWhere = REPLACE(cQueryWhere,"FOR EACH " + cBaseFileName,"").
    cQueryWhere = REPLACE(cQueryWhere,"'",'"').
    cQueryWhere = TRIM(cQueryWhere).
    IF LENGTH(cQueryWhere) > 80 THEN ASSIGN
        cQueryWhere = SUBSTRING(cQueryWhere,1,70) + "...(more)".
    
    SESSION:SET-WAIT-STATE("general").
    STATUS INPUT "Opening Excel. Please wait...".
    CREATE "Excel.Application" chExcelApplication.
    STATUS INPUT "Exporting data. Please wait...".

    ASSIGN 
        chWorkbook                 = chExcelApplication:Workbooks:Add()
        chWorkSheet                = chExcelApplication:Sheets:Item(1)
        cTable                     = hBuffer:TABLE
        chWorkSheet:NAME           = cTable
        cColList                   = "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,"
        cColList                   = cColList + "AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ,"
        cColList                   = cColList + "BA,BB,BC,BD,BE,BF,BG,BH,BI,BJ,BK,BL,BM,BN,BO,BP,BQ,BR,BS,BT,BU,BV,BW,BX,BY,BZ,"
        cColList                   = cColList + "CA,CB,CC,CD,CE,CF,CG,CH,CI,CJ,CK,CL,CM,CN,CO,CP,CQ,CR,CS,CT,CU,CV,CW,CX,CY,CZ,"
        cColList                   = cColList + "DA,DB,DC,DD,DE,DF,DG,DH,DI,DJ,DK,DL,DM,DN,DO,DP,DQ,DR,DS,DT,DU,DV,DW,DX,DY,DZ,"
        cColList                   = cColList + "EA,EB,EC,ED,EE,EF,EG,EH,EI,EJ,EK,EL,EM,EN,EO,EP,EQ,ER,ES,ET,EU,EV,EW,EX,EY,EZ,"
        cColList                   = cColList + "FA,FB,FC,FD,FE,FF,FG,FH,FI,FJ,FK,FL,FM,FN,FO,FP,FQ,FR,FS,FT,FU,FV,FW,FX,FY,FZ,"
        cColList                   = cColList + "GA,GB,GC,GD,GE,GF,GG,GH,GI,GJ,GK,GL,GM,GN,GO,GP,GQ,GR,GS,GT,GU,GV,GW,GX,GY,GZ"
        iRow = 1.
       
    DO iFields = 1 TO br_table:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
        ASSIGN hThisColumn = br_table:GET-BROWSE-COLUMN(iFields).
        chWorkSheet:range(ENTRY(iFields,cColList) + STRING(iRow)):Value = (hThisColumn:LABEL).
        chWorkSheet:range(ENTRY(iFields,cColList) + STRING(iRow)):Font:Bold = TRUE.
    END.

    RUN fetchFirst IN hDataSource.
    /* check if any records */
    lAvailable = DYNAMIC-FUNCTION('getQueryPosition':U IN hDataSource) <> "NoRecordAvailable":U.

    IF lAvailable THEN DO:    /* now loop through all available records */
        REPEAT WHILE lAvailable:
            IF NOT hBuffer:AVAILABLE THEN LEAVE.
            cQueryPosition = DYNAMIC-FUNCTION('getQueryPosition':U IN hDataSource).
            lAvailable = NOT CAN-DO("LastRecord,OnlyRecord":U, cQueryPosition).
            DO:
                ASSIGN 
                    iRow = iRow + 1
                    iColumn = 1.
                DO iFields = 1 TO br_table:NUM-COLUMNS:
                    ASSIGN 
                        hThisColumn = br_table:GET-BROWSE-COLUMN(iFields).
                    IF hThisColumn:NAME = cLastField THEN DO:
                        lExtentCleared = FALSE.
                        NEXT.
                    END.
                    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
                        hField = hBuffer:BUFFER-FIELD(iCtr).
                        IF iRow = 2 THEN DO:
                            chWorkSheet:COLUMNS(ENTRY(iColumn,cColList)):ColumnWidth = hField:WIDTH.
                            chWorkSheet:COLUMNS(ENTRY(iColumn,cColList)):NumberFormat = IF hField:DATA-TYPE = "integer" THEN "#######0":U
                                                                                ELSE IF hField:DATA-TYPE = "decimal" THEN "##,###,###,##0.00":U
                                                                                ELSE IF hField:DATA-TYPE = "date" THEN "MM/DD/YY":U
                                                                                ELSE "@":U.
                            tWidth = tWidth + chWorkSheet:COLUMNS(ENTRY(iColumn,cColList)):ColumnWidth.
                        END.
                        IF hThisColumn:NAME = hField:NAME THEN DO:
                            IF hField:EXTENT < 2 THEN DO:
                                chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iRow)):Value = hField:BUFFER-VALUE.
                                iColumn = iColumn + 1.
                                cLastField = hThisColumn:NAME.
                            END.
                            ELSE DO iCtr2 = 1 TO hField:EXTENT:
                                chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iRow)):Value = hField:BUFFER-VALUE(iCtr2).
                                iColumn = iColumn + 1.
                                lExtentCleared = YES.
                                cLastField = hThisColumn:NAME.
                            END.
                        END.
                    END.
                END.
            END.
            RUN fetchNext IN hDataSource.
        END.
    END.
    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        chWorksheet:COLUMNS(ENTRY(iCtr,cColList)):AutoFit.
    END.

    DO:
        chWorksheet:PageSetup:FirstPageNumber = 1.
        chWorksheet:PageSetup:LeftHeader = "Table: " + cBaseFileName + CHR(10) + cQueryWhere.
        chWorksheet:PageSetup:LeftFooter = "(c)2015, Foresight Software LLC".                    
        chWorksheet:PageSetup:RightHeader = "MXP Browser2Excel" + CHR(10) + "Print Date: " +
                                                STRING(MONTH(TODAY),"99") + "/" +
                                                STRING(DAY(TODAY),"99") + "/" +
                                                STRING(YEAR(TODAY),"9999").
        chWorksheet:PageSetup:RightFooter = "Page: &P".
        chWorksheet:PageSetup:PrintGridlines = TRUE.
        chWorksheet:PageSetup:PrintTitleRows = "$1:$1".
        chWorksheet:PageSetup:ORIENTATION = IF tWidth < 550 THEN 1 ELSE 2.
    END.
    hQuery:REPOSITION-TO-ROW(1).
    
    STATUS DEFAULT "".
    SESSION:SET-WAIT-STATE("").
    
    IF ipDoWhat = "View" THEN ASSIGN    
        chExcelApplication:Visible = TRUE.
    ELSE IF ipDoWhat = "Print" THEN DO:
        SYSTEM-DIALOG PRINTER-SETUP.
        chWorksheet:PrintOut(,,,,SESSION:PRINTER-NAME,,).
    END.
    
    RELEASE OBJECT chWorkSheet        NO-ERROR.
    RELEASE OBJECT chWorkBook         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseExcel3 Include 
PROCEDURE ipChooseExcel3 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cFieldList AS CHAR.
    DEFINE VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO EXTENT {&maxColumns}.
    DEFINE VARIABLE userName AS CHARACTER NO-UNDO EXTENT {&maxColumns}.
    DEFINE VARIABLE userWidth AS DECIMAL NO-UNDO EXTENT {&maxColumns}.
    DEFINE VARIABLE numColumns AS INTEGER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.

    /* save original settings in order to do default restore */
    numColumns = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
    DO i = 1 TO numColumns:
        ASSIGN
            cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i)
            cFieldList = cFieldList + cellColumn[i]:NAME + ",".
    END. /* do i */
    
    ASSIGN
        cFieldList = TRIM(cFieldList,",")
        iMaxRecords = 65536.
    /* 
    UPDATE 
        iMaxRecords 
        WITH FRAME fQuestion.
    */
    RUN transferToExcel IN hDataSource
    ( INPUT cFieldList /* CHARACTER */,
      INPUT NO /* LOGICAL */,
      INPUT YES /* LOGICAL */,
      INPUT iMaxRecords /* INTEGER */). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseFind Include 
PROCEDURE ipChooseFind :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR AscDes AS CHAR.
    DEF VAR cOrigSort AS CHAR.
    DEF VAR lSortSet AS LOG.
    DEF VAR cOptSort AS CHAR.
    
    FIND dictdb._file WHERE 
        dictdb._file._file-name = cBaseFileName 
        NO-LOCK NO-ERROR.
    IF NOT AVAIL dictdb._file THEN DO: 
        MESSAGE 
            "Cannot create Find Information for this File" 
            VIEW-AS ALERT-BOX ERROR.
        RETURN .
    END.
    ELSE RUN br/dFilter.w (INPUT cBaseFileName,
                      INPUT hDataSource).
    
    RUN ipValueChanged IN THIS-PROCEDURE.                                

    ASSIGN
        cOrigSort =
            SUBSTRING(
                DYNAMIC-FUNCTION('getQuerySort':U IN hDataSource),
                INDEX(DYNAMIC-FUNCTION('getQuerySort':U IN hDataSource),"by") + 3).
    IF NUM-ENTRIES(cOrigSort," ") > 1 THEN ASSIGN
        cOrigSort = ENTRY(1,cOrigSort," ")
        cOrigSort = ENTRY(2,cOrigSort,".").
    ELSE IF NUM-ENTRIES(cOrigSort,".") > 1 THEN ASSIGN
        cOrigSort = ENTRY(2,cOrigSort,".").
    ELSE ASSIGN
        cOrigSort = "".
    
    DO i = 1 TO br_table:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        hColumn = br_table:GET-BROWSE-COLUMN(i).
        IF hColumn:NAME = cOrigSort THEN ASSIGN
            hColumn:LABEL-BGCOLOR = IF AscDes = "ASC" THEN 10 ELSE 13.
        ELSE ASSIGN 
            hColumn:LABEL-BGCOLOR = 14.
    END.
    DO i = 1 TO br_table:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        hColumn = br_table:GET-BROWSE-COLUMN(i).
        IF hColumn:LABEL-BGCOLOR = 10 
        OR hColumn:LABEL-BGCOLOR = 13 THEN
            ASSIGN lSortSet = TRUE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseHelp Include 
PROCEDURE ipChooseHelp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN pt/ubrhelp.p ( "main":U ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseMemo Include 
PROCEDURE ipChooseMemo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cOldFileName AS CHAR NO-UNDO.
    DEF VAR cOldKeyList AS CHAR NO-UNDO.
    def var hOldWindow as handle no-undo.
    assign hOldWindow = current-window:handle.

    IF wfilename = "order" THEN ASSIGN 
        wfilename = "customer":U
        wkeylist  = order.cust-no + ",":U            .
    IF wfilename = "service" THEN  ASSIGN 
        wfilename = "customer":U
        wkeylist  = service.cust-no + ",":U.
    assign
        current-window = holdwindow.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChoosePrint Include 
PROCEDURE ipChoosePrint :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTable              AS CHARACTER    NO-UNDO.
DEF VAR cFields             AS CHARACTER    NO-UNDO.
DEF VAR hQuery              AS HANDLE       NO-UNDO.
DEF VAR hBuffer             AS HANDLE       NO-UNDO.
DEF VAR hField              AS HANDLE       NO-UNDO.
DEF VAR iFields             AS INTEGER      NO-UNDO.
DEF VAR iRow                AS INTEGER      NO-UNDO.
DEF VAR cRow                AS CHARACTER    NO-UNDO INITIAL "A".
DEF VAR chExcelApplication  AS COM-HANDLE   NO-UNDO.
DEF VAR chWorkbook          AS COM-HANDLE   NO-UNDO.
DEF VAR chWorksheet         AS COM-HANDLE   NO-UNDO.
DEF VAR hThisField          AS HANDLE       NO-UNDO.
DEF VAR hThisColumn         AS HANDLE       NO-UNDO.
DEF VAR iCtr2               AS INT          NO-UNDO.
DEF VAR iCtr3               AS INT          NO-UNDO.
DEF VAR cColList            AS CHAR         NO-UNDO.
DEF VAR iColumn             AS INT          NO-UNDO.
DEF VAR lExtentCleared      AS LOG          NO-UNDO.
DEF VAR cLastField          AS CHAR         NO-UNDO.
DEF VAR iBrowseCount        AS INT          NO-UNDO.
DEF VAR tWidth              AS INT          NO-UNDO.
DEF VAR iNumRecs AS INT NO-UNDO.

    SYSTEM-DIALOG PRINTER-SETUP.

    ASSIGN
        hQuery = br_table:QUERY IN FRAME {&FRAME-NAME}
        hBuffer = hQuery:GET-BUFFER-HANDLE(1).
    IF hQuery:GET-FIRST() = FALSE THEN 
        RETURN.

    STATUS DEFAULT "Exporting data. Please wait...".
    SESSION:SET-WAIT-STATE("general").
    CREATE "Excel.Application" chExcelApplication.

    ASSIGN 
        chWorkbook                 = chExcelApplication:Workbooks:Add()
        chWorkSheet                = chExcelApplication:Sheets:Item(1)
        cTable                     = hBuffer:TABLE
        chWorkSheet:NAME           = cTable
        cColList                   = "A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z,"
        cColList                   = cColList + "AA,AB,AC,AD,AE,AF,AG,AH,AI,AJ,AK,AL,AM,AN,AO,AP,AQ,AR,AS,AT,AU,AV,AW,AX,AY,AZ,"
        cColList                   = cColList + "BA,BB,BC,BD,BE,BF,BG,BH,BI,BJ,BK,BL,BM,BN,BO,BP,BQ,BR,BS,BT,BU,BV,BW,BX,BY,BZ,"
        cColList                   = cColList + "CA,CB,CC,CD,CE,CF,CG,CH,CI,CJ,CK,CL,CM,CN,CO,CP,CQ,CR,CS,CT,CU,CV,CW,CX,CY,CZ,"
        cColList                   = cColList + "DA,DB,DC,DD,DE,DF,DG,DH,DI,DJ,DK,DL,DM,DN,DO,DP,DQ,DR,DS,DT,DU,DV,DW,DX,DY,DZ,"
        cColList                   = cColList + "EA,EB,EC,ED,EE,EF,EG,EH,EI,EJ,EK,EL,EM,EN,EO,EP,EQ,ER,ES,ET,EU,EV,EW,EX,EY,EZ,"
        cColList                   = cColList + "FA,FB,FC,FD,FE,FF,FG,FH,FI,FJ,FK,FL,FM,FN,FO,FP,FQ,FR,FS,FT,FU,FV,FW,FX,FY,FZ,"
        cColList                   = cColList + "GA,GB,GC,GD,GE,GF,GG,GH,GI,GJ,GK,GL,GM,GN,GO,GP,GQ,GR,GS,GT,GU,GV,GW,GX,GY,GZ"
        iRow = 1.
       
    DO iFields = 1 TO br_table:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
        ASSIGN hThisColumn = br_table:GET-BROWSE-COLUMN(iFields).
        chWorkSheet:range(ENTRY(1,cColList) + STRING(iRow)):Value = (hThisColumn:LABEL).
        chWorkSheet:range(ENTRY(1,cColList) + STRING(iRow)):Font:Bold = TRUE.
        chWorkSheet:COLUMNS(ENTRY(1,cColList)):ColumnWidth = 20.
        iRow = iRow + 1.
    END.
    iRow = 1.

    DO iCtr3 = 1 TO br_table:NUM-SELECTED-ROWS:
        br_table:FETCH-SELECTED-ROW(iCtr3).
        DO:
            ASSIGN 
                iRow = 1
                iColumn = 2.
            IF iCtr3 MODULO 100 = 0 THEN
                STATUS DEFAULT "Exporting data. Please wait... (" + string(iCtr3) +
                " of " + STRING(iNumRecs) + " records exported)". .
            DO iFields = 1 TO br_table:NUM-COLUMNS:
                ASSIGN 
                    hThisColumn = br_table:GET-BROWSE-COLUMN(iFields).
                IF hThisColumn:NAME = cLastField THEN DO:
                    lExtentCleared = FALSE.
                    NEXT.
                END.
                DO iCtr = 1 TO hBuffer:NUM-FIELDS:
                    hField = hBuffer:BUFFER-FIELD(iCtr).
                    IF iRow = 2 THEN DO:
                        chWorkSheet:COLUMNS(ENTRY(iColumn,cColList)):NumberFormat = IF hField:DATA-TYPE = "integer" THEN "#######0":U
                                                                               ELSE IF hField:DATA-TYPE = "decimal" THEN "##,###,###,##0.00":U
                                                                               ELSE IF hField:DATA-TYPE = "date" THEN "MM/DD/YY":U
                                                                               ELSE "@":U.
                    END.
                    IF hThisColumn:NAME = hField:NAME THEN DO:
                        IF hField:EXTENT < 2 THEN DO:
                            chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iRow)):Value = hField:BUFFER-VALUE.
                            chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iRow)):NumberFormat = IF hField:DATA-TYPE = "integer" THEN "#######0":U
                                                                               ELSE IF hField:DATA-TYPE = "decimal" THEN "##,###,###,##0.00":U
                                                                               ELSE IF hField:DATA-TYPE = "date" THEN "MM/DD/YY":U
                                                                               ELSE "@":U.
                            iRow = iRow + 1.
                            cLastField = hThisColumn:NAME.
                        END.
                        ELSE DO iCtr2 = 1 TO hField:EXTENT:
                            chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iRow)):Value = hField:BUFFER-VALUE(iCtr2).
                            chWorkSheet:range(ENTRY(iColumn,cColList) + STRING(iRow)):NumberFormat = IF hField:DATA-TYPE = "integer" THEN "#######0":U
                                                                               ELSE IF hField:DATA-TYPE = "decimal" THEN "##,###,###,##0.00":U
                                                                               ELSE IF hField:DATA-TYPE = "date" THEN "MM/DD/YY":U
                                                                               ELSE "@":U.
                            iRow = iRow + 1.
                            lExtentCleared = YES.
                            cLastField = hThisColumn:NAME.
                        END.
                    END.
                END.
            END.
        END.
    END.

    DO iCtr = 1 TO hBuffer:NUM-FIELDS:
        chWorksheet:COLUMNS(ENTRY(iCtr,cColList)):AutoFit.
    END.
    
    DO:
        chWorksheet:PageSetup:LeftHeader = "MXP Browser2Excel" + chr(10) + "UserId: " + 
                            ENTRY(1,USERID(LDBNAME(1)),"@") + " | DB: " + DBNAME.
        chWorksheet:PageSetup:LeftFooter = "(c) 2015, Foresight Software LLC".                    
        chWorksheet:PageSetup:RightHeader = "Table: " + cBaseFileName.
        chWorksheet:PageSetup:FirstPageNumber = 1.
        chWorksheet:PageSetup:RightFooter = "Print Date: " +
                                                STRING(MONTH(TODAY),"99") + "/" +
                                                STRING(DAY(TODAY),"99") + "/" +
                                                STRING(YEAR(TODAY),"9999") + CHR(10) +
                                            "Page: &P".
        chWorksheet:PageSetup:PrintGridlines = TRUE.
        chWorksheet:PageSetup:ORIENTATION = 1.
        chWorksheet:PrintOut(,,,,SESSION:PRINTER-NAME,,).
    END.
    /*
    ASSIGN    
        chExcelApplication:Visible = TRUE.
    
    hQuery:REPOSITION-TO-ROW(1).
    */
    SESSION:SET-WAIT-STATE("").
    STATUS DEFAULT "".
    RELEASE OBJECT chWorkSheet        NO-ERROR.
    RELEASE OBJECT chWorkBook         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseRefresh Include 
PROCEDURE ipChooseRefresh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  &IF "{&Module}" = "op":U &THEN
    ASSIGN 
        init-val = "".
     find op-control where rowid(op-control) = op-control-rowid no-lock no-error.
    {pt/setinit ar-entity op-control.ar-entity} 
    {pt/setinit order-no 0} 
    {pt/setinit cust-po ''} 
    {pt/setinit cust-no ''} 
    {pt/setinit ship-no ''} 
    {pt/setinit Department ''} 
    {pt/setinit estimate-no ''} 
    {pt/setinit sent-entity ''} 
    {pt/setinit sent-order ''} 
    {pt/setinit sent-by ''} 
    {pt/setinit in-entity op-control.in-entity} 
    {pt/setinit pps-no 0} 
    {pt/setinit quote-no 0} 
   &ENDIF 
  &if "{&Module}" = "DB":U &then
   
     find op-control where rowid(op-control) = op-control-rowid no-lock no-error.
   {pt/setinit ar-entity op-control.ar-entity} 
   {pt/setinit document 0} 
   {pt/setinit order-no 0} 
   {pt/setinit cust-no ''} 
   {pt/setinit yr 0} 
   {pt/setinit prd 0} 
   {pt/setinit inv-credit YES} 
   {pt/setinit seq-no 0} 
   {pt/setinit estimate-no ''} 
   {pt/setinit entity-code op-control.ar-entity} 
   {pt/setinit in-entity op-control.in-entity} 
    {pt/setinit jrnl-delete NO} 
  &endif
    RUN ipSpecific in THIS-PROCEDURE.
    RUN chgqry IN hDataSource.
    RUN moveit IN hDataSource (INPUT old-rowid).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseRestore Include 
PROCEDURE ipChooseRestore :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN restoreCellColumns IN THIS-PROCEDURE.
    ASSIGN
        br_table:COLUMN-MOVABLE IN FRAME {&FRAME-NAME} = FALSE
        br_table:COLUMN-RESIZABLE = FALSE
        br_table:ALLOW-COLUMN-SEARCHING = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseSave Include 
PROCEDURE ipChooseSave :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN saveCellColumns IN THIS-PROCEDURE.
    ASSIGN
        br_table:COLUMN-MOVABLE IN FRAME {&FRAME-NAME} = FALSE
        br_table:COLUMN-RESIZABLE = FALSE
        br_table:ALLOW-COLUMN-SEARCHING = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseSelect Include 
PROCEDURE ipChooseSelect :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN ipSpecific IN THIS-PROCEDURE.
    RUN select-proc IN THIS-PROCEDURE.

    IF INDEX(functionname,".inq") <> 0  or
        functionname BEGINS "b."
        THEN do:
        if functionname <> "dispatch.inq" THEN
        RUN close-it-up IN h_parent.
    end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipChooseZoom Include 
PROCEDURE ipChooseZoom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iKilled AS INTEGER NO-UNDO.
    
    ASSIGN
        inZoom = true 
        hrun = SELF:PRIVATE-DATA
        functionname = hrun.
    
    RUN pt/mxpmenu4.p (INPUT 97, INPUT CURRENT-WINDOW:HANDLE, OUTPUT iKilled).
    
    ASSIGN
        inZoom = false.
                                
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipCreatePopup Include 
PROCEDURE ipCreatePopup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    CREATE MENU hPopupHandle
        ASSIGN
            POPUP-ONLY = TRUE.
    
    CREATE MENU-ITEM hMenuLevel1[1]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "Set Filter (FIND)"
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN ipChooseFind IN THIS-PROCEDURE.
        END TRIGGERS.

    CREATE SUB-MENU hMenuLevel1[2]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "Related Functions (ZOOM)".

    ASSIGN
        iCtr = 1.
    
    FIND FIRST z_funct WHERE
        z_funct.runpgm = ENTRY(2,PROGRAM-NAME(1),' ')
        NO-LOCK NO-ERROR.
    IF AVAIL z_funct THEN ASSIGN
        cThisFunct = z_funct.funct-name.
    ELSE ASSIGN
        cThisFunct = functionname.
    FIND FIRST z_zoom-ln WHERE
        z_zoom-ln.funct-name = cThisFunct AND
        (z_zoom-ln.user-id = entry(1,userid("ptdb":U),"@") OR
        z_zoom-ln.user-id = "")
        NO-LOCK NO-ERROR.
    IF AVAIL z_zoom-ln THEN DO:
        FOR EACH z_zoom-ln WHERE 
            z_zoom-ln.funct-name = cThisFunct AND
            (z_zoom-ln.user-id = entry(1,userid("ptdb":U),"@") OR
            z_zoom-ln.user-id = ""):
        
            FIND FIRST z_funct WHERE
                z_funct.funct-name = z_zoom-ln.zoom-funct
                NO-LOCK NO-ERROR.

            IF AVAIL z_funct THEN DO:
                CREATE MENU-ITEM hMenuLevel2[iCtr]
                    ASSIGN
                        PARENT = hMenuLevel1[2]
                        LABEL = z_funct.funct-desc
                        PRIVATE-DATA = z_funct.funct-name
                    TRIGGERS:
                        ON CHOOSE PERSISTENT RUN ipChooseZoom IN THIS-PROCEDURE.
                    END TRIGGERS.
                ASSIGN iCtr = iCtr + 1.
            END.
        END.
    END.
    ELSE DO:
        ASSIGN
            hMenuLevel1[2]:SENSITIVE = FALSE.
    END.
        
    CREATE MENU-ITEM hMenuLevel1[3]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "View Memos"
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN ipChooseMemo IN THIS-PROCEDURE.
        END TRIGGERS.

    CREATE MENU-ITEM hMenuLevel1[4]
        ASSIGN
            PARENT = hPopupHandle
            SUBTYPE = "Rule".

    CREATE MENU-ITEM hMenuLevel1[5]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "Print This Record"
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN ipChoosePrint IN THIS-PROCEDURE.
        END TRIGGERS.

    CREATE MENU-ITEM hMenuLevel1[6]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "Send to Excel"
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN ipChooseExcel IN THIS-PROCEDURE ("View").
        END TRIGGERS.

    CREATE MENU-ITEM hMenuLevel1[7]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "Print"
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN ipChooseExcel IN THIS-PROCEDURE ("Print").
        END TRIGGERS.
    
    CREATE MENU-ITEM hMenuLevel1[8]
        ASSIGN
            PARENT = hPopupHandle
            SUBTYPE = "Rule".

    CREATE MENU-ITEM hMenuLevel1[9]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "Refresh Browse"
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN ipChooseRefresh IN THIS-PROCEDURE.
        END TRIGGERS.

    CREATE MENU-ITEM hMenuLevel1[10]
        ASSIGN
            PARENT = hPopupHandle
            SUBTYPE = "Rule".
            
    CREATE MENU-ITEM hMenuLevel1[11]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "Customize Browser"
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN ipChooseCustomize IN THIS-PROCEDURE.
        END TRIGGERS.

    CREATE MENU-ITEM hMenuLevel1[12]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "Save Browser Settings"
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN ipChooseSave IN THIS-PROCEDURE.
        END TRIGGERS.

    CREATE MENU-ITEM hMenuLevel1[13]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "Restore Browser Settings"
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN ipChooseRestore IN THIS-PROCEDURE.
        END TRIGGERS.

    CREATE MENU-ITEM hMenuLevel1[14]
        ASSIGN
            PARENT = hPopupHandle
            SUBTYPE = "Rule".
            
    CREATE MENU-ITEM hMenuLevel1[15]
        ASSIGN
            PARENT = hPopupHandle
            LABEL = "Help"
        TRIGGERS:
            ON CHOOSE PERSISTENT RUN ipChooseHelp IN THIS-PROCEDURE.
        END TRIGGERS.

    CREATE MENU-ITEM hMenuLevel1[16]
        ASSIGN
            PARENT = hPopupHandle
            SUBTYPE = "Rule".

    &IF "{&Module}" = "op":U &THEN
        {op/brpopup.i iPassThru}
    &ENDIF
    &IF "{&Module}" = "RR":U &then 
        {po/brpopup.i }
     &endif
     &IF "{&Module}" = "FF":U &then 
        {ff/brpopup.i }
     &endif
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipSetSort Include 
PROCEDURE ipSetSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER cSortString AS CHAR.
    DEF VAR j AS INT.

    FIND FIRST z_join-value NO-LOCK
        WHERE z_join-value.file-name = ENTRY(2,PROGRAM-NAME(1),' ')
        AND z_join-value.key-list = "x" +  ENTRY(1,USERID("ptdb"),"@") NO-ERROR.

    IF AVAILABLE z_join-value THEN DO:
        ASSIGN
            cSortString = "BY ".
        DO j = 1 TO NUM-ENTRIES(z_join-value.rec-image):
            IF j > 1 
            AND (ENTRY(j - 1,z_join-value.rec-image) = "ASC" 
            OR ENTRY(j - 1,z_join-value.rec-image) = "DESC"
            OR ENTRY(j - 1,z_join-value.rec-image) = "descending"
            OR ENTRY(j - 1,z_join-value.rec-image) = "ascending") THEN ASSIGN
                cSortString = cSortString + "BY ".
            ASSIGN
                cSortString = cSortString + ENTRY(j,z_join-value.rec-image) + " ".
        END. /* do j */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipShowMemo Include 
PROCEDURE ipShowMemo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cOldFileName AS CHAR NO-UNDO.
    DEF VAR cOldKeyList AS CHAR NO-UNDO.
    def var hOldWindow as handle no-undo.
    assign hOldWindow = current-window:handle.
    
    IF CAN-FIND (FIRST z_memos WHERE 
             z_memos.file-name = wfilename AND 
             z_memos.key-list = wkeylist AND 
             CAN-DO(z_memos.can-read,ENTRY(1,USERID("ptdb":U),"@")))
    THEN ASSIGN
        lMemoExist = TRUE.
    ELSE ASSIGN
        lMemoExist = FALSE.

    assign 
        cOldFileName = wFileNAME
        cOldKeyList = wkeylist.

    &IF "{&Module}" = "op":U &THEN
        IF avail order 
        and CAN-FIND(FIRST z_memos WHERE 
                     z_memos.file-name = "order" AND 
                     z_memos.key-list = string(order.order-no) + ",":U AND 
                     z_memos.must-see = YES AND 
                     INDEX(z_memos.form-list,"X") <> 0 AND
                     CAN-DO(z_memos.can-read,ENTRY(1,USERID("ptdb":U),"@")))
        AND NOT updateactive 
        and valid-handle(h_vorders) THEN do:
            assign 
                wfilename = "order"
                wkeylist = order.ar-entity + "," + order.cust-no + ",":U.
            if ENTRY(2,PROGRAM-NAME(1),' ') = "op/border.w":U then 
                run pt/dmemoview.w.
        end.
        IF avail order 
        and CAN-FIND(FIRST z_memos WHERE 
                     z_memos.file-name = "customer" AND 
                     z_memos.key-list = string(order.cust-no) + ",":U AND 
                     z_memos.must-see = YES AND 
                     INDEX(z_memos.form-list,"X") <> 0 AND
                     CAN-DO(z_memos.can-read,ENTRY(1,USERID("ptdb":U),"@")))
        AND NOT updateactive 
        and valid-handle(h_vorders) THEN do:
            assign 
                wfilename = "customer"
                wkeylist = order.cust-no + ",":U.
            if ENTRY(2,PROGRAM-NAME(1),' ') = "op/border.w":U then 
                run pt/dmemoview.w.
        end.
    &ENDIF 
    
    &IF "{&Module}" = "ar":U &THEN
        IF avail customer 
        and CAN-FIND(FIRST z_memos WHERE 
                     z_memos.file-name = "customer" AND 
                     z_memos.key-list = customer.cust-no + ",":U AND 
                     z_memos.must-see = YES AND 
                     CAN-DO(z_memos.can-read,ENTRY(1,USERID("ptdb":U),"@")))
        /* AND NOT updateactive */ THEN  do:
            assign 
                wfilename = "customer"
                wkeylist = customer.cust-no + ",":U.
            if ENTRY(2,PROGRAM-NAME(1),' ') = "ar/bcustomer.w":U then 
                run pt/dmemoview.w.
        end.
    &ENDIF 
    
    &IF "{&Module}" = "db":U &THEN
        IF avail invoice 
        and CAN-FIND(FIRST z_memos WHERE 
                     z_memos.file-name = "customer" AND 
                     z_memos.key-list = invoice.cust-no + ",":U AND 
                     z_memos.must-see = YES AND 
                     CAN-DO(z_memos.can-read,ENTRY(1,USERID("ptdb":U),"@")))
        THEN do:
            assign 
                wfilename = "customer"
                wkeylist = invoice.cust-no + ",":U.
            if ENTRY(2,PROGRAM-NAME(1),' ') = "op/binvoice.w":U then 
                run pt/dmemoview.w.
        end.
    &ENDIF 
    
    &IF "{&Module}" = "qt":U &THEN
        IF avail quote 
        and CAN-FIND(FIRST z_memos WHERE 
                     z_memos.file-name = "customer" AND 
                     z_memos.key-list = quote.cust-no + ",":U AND 
                     z_memos.must-see = YES AND 
                     CAN-DO(z_memos.can-read,ENTRY(1,USERID("ptdb":U),"@")))
         THEN do:
            assign 
                wfilename = "customer"
                wkeylist = quote.cust-no + ",":U.
           if ENTRY(2,PROGRAM-NAME(1),' ') = "op/bquote.w":U then 
                run pt/dmemoview.w.
        end.
    &ENDIF 
    
    &IF "{&Module}" = "BP":U &THEN
        IF avail blank-po 
        and CAN-FIND(FIRST z_memos WHERE 
                     z_memos.file-name = "vendor" AND 
                     z_memos.key-list = blank-po.vendor-code + ",":U AND 
                     z_memos.must-see = YES AND 
                     CAN-DO(z_memos.can-read,ENTRY(1,USERID("ptdb":U),"@")))
        THEN do:
            assign 
                wfilename = "vendor"
                wkeylist = blank-po.vendor-code + ",":U.
            if ENTRY(2,PROGRAM-NAME(1),' ') = "po/bblankpo.w":U then 
                run pt/dmemoview.w.
        end.
    &ENDIF 
    
    &IF "{&Module}" = "PO":U &THEN
        IF avail po 
        and CAN-FIND(FIRST z_memos WHERE 
                     z_memos.file-name = "vendor" AND 
                     z_memos.key-list = po.vendor-code + ",":U AND 
                     z_memos.must-see = YES AND 
                     CAN-DO(z_memos.can-read,ENTRY(1,USERID("ptdb":U),"@")))
        THEN do:
            assign 
                wfilename = "vendor"
                wkeylist = po.vendor-code + ",":U.
            if ENTRY(2,PROGRAM-NAME(1),' ') = "op/bpo.w":U then 
                run pt/dmemoview.w.
        end.
    &ENDIF 
  
    &IF "{&Module}" = "FF":U &THEN
        IF avail service 
        and CAN-FIND(FIRST z_memos WHERE 
                     z_memos.file-name = "service" AND 
                     z_memos.key-list = service.service-no AND 
                     z_memos.must-see = YES AND 
                     CAN-DO(z_memos.can-read,ENTRY(1,USERID("ptdb":U),"@")))
        THEN do:
            assign 
                wfilename = "service"
                wkeylist = service.service-no.
            if ENTRY(2,PROGRAM-NAME(1),' ') = "ff/bservice.w":U then 
                run pt/dmemoview.w.
        end.
    &ENDIF 

    ASSIGN 
        current-window = hOldWindow.

    IF VALID-HANDLE(hMenuLevel1[3]) THEN ASSIGN
        hMenuLevel1[3]:SENSITIVE = lMemoExist.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ipValueChanged Include 
PROCEDURE ipValueChanged :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    APPLY 'value-changed' TO br_table IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE restoreCellColumns Include 
PROCEDURE restoreCellColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO EXTENT {&maxColumns}.
    DEFINE VARIABLE numColumns AS INTEGER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.
  
    FIND FIRST z_join-value EXCLUSIVE-LOCK
        WHERE z_join-value.file-name EQ ENTRY(2,PROGRAM-NAME(1),' ')
        AND z_join-value.key-list EQ ENTRY(1,USERID("ptdb"),"@") NO-ERROR.
    IF AVAILABLE z_join-value THEN DELETE z_join-value.
    /* change columns back to default settings */
    numColumns = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
    DO i = 1 TO numColumns:
        cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
        DO j = 1 TO numColumns:
            IF cellColumn[i]:NAME EQ cellName[j] AND (i NE j OR
            cellColumn[i]:WIDTH-PIXELS NE cellWidth[j]) THEN DO:
                cellColumn[i]:WIDTH-PIXELS = cellWidth[j].
                IF j NE i THEN
                    {&BROWSE-NAME}:MOVE-COLUMN(i,j) IN FRAME {&FRAME-NAME}.
                /* backup one and recheck, so we don't skip anything */
                IF i NE 1 THEN i = i - 1.
            END. /* if changed */
        END. /* do j */
    END. /* do i */
    MESSAGE 'Browser Defaults Restored.' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveCellColumns Include 
PROCEDURE saveCellColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE numColumns AS INTEGER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    /* find the unique program and user record holding column settings */
    /* column field-name,column width,column field-name,column width, ... */
    FIND FIRST z_join-value EXCLUSIVE-LOCK
        WHERE z_join-value.file-name EQ ENTRY(2,PROGRAM-NAME(1),' ')
        AND z_join-value.key-list EQ ENTRY(1,USERID("ptdb"),"@") NO-ERROR.
    IF NOT AVAILABLE z_join-value THEN DO:
        CREATE z_join-value.
        ASSIGN
            z_join-value.file-name = ENTRY(2,PROGRAM-NAME(1),' ')
            z_join-value.key-list = ENTRY(1,USERID("ptdb"),"@").
    END. /* if not avail */
    ASSIGN
        numColumns = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}
        z_join-value.rec-image = ''.
    DO i = 1 TO numColumns:
        z_join-value.rec-image = z_join-value.rec-image +
            {&BROWSE-NAME}:GET-BROWSE-COLUMN(i):NAME + ',' +
            STRING({&BROWSE-NAME}:GET-BROWSE-COLUMN(i):WIDTH-PIXELS) + ','.
    END. /* do i */
    RELEASE z_join-value.
    MESSAGE 'Browser Changes Saved!' VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Select-Proc Include 
PROCEDURE Select-Proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR old-value# AS CHAR NO-UNDO.
    old-value# = fr-value .
   
    RUN CreateFind IN hGenRules (INPUT cBaseFileName,
                                 INPUT-OUTPUT  ff1 ,
                                 INPUT-OUTPUT  ff2,
                                 INPUT-OUTPUT  ff3 ,
                                 INPUT-OUTPUT  ff4 ,
                                 INPUT-OUTPUT  ff5 ,
                                 INPUT-OUTPUT  ff6 ,
                                 INPUT-OUTPUT  ff7 ,
                                 INPUT-OUTPUT  ff8 ,
                                 INPUT-OUTPUT  ff9 ,
                                 INPUT-OUTPUT  ff10 ,
                                 INPUT-OUTPUT  ff11 ,
                                 INPUT-OUTPUT  ff12 ,
                                 INPUT  file-name ,
                                 INPUT-OUTPUT  key-list ).

    IF NUM-ENTRIES(key-list) >= 1 AND use-field = ENTRY(1,key-list) THEN fr-value = STRING(ff1). 
    IF NUM-ENTRIES(key-list) >= 2 AND use-field = ENTRY(2,key-list) THEN fr-value = STRING(ff2). 
    IF NUM-ENTRIES(key-list) >= 3 AND use-field = ENTRY(3,key-list) THEN fr-value = STRING(ff3). 
    IF NUM-ENTRIES(key-list) >= 4 AND use-field = ENTRY(4,key-list) THEN fr-value = STRING(ff4). 
    IF NUM-ENTRIES(key-list) >= 5 AND use-field = ENTRY(5,key-list) THEN fr-value = STRING(ff5).
    IF NUM-ENTRIES(key-list) >= 6 AND use-field = ENTRY(6,key-list) THEN fr-value = STRING(ff6). 
    IF NUM-ENTRIES(key-list) >= 7 AND use-field = ENTRY(7,key-list) THEN fr-value = STRING(ff7). 
    IF NUM-ENTRIES(key-list) >= 8 AND use-field = ENTRY(8,key-list) THEN fr-value = STRING(ff8). 
    IF NUM-ENTRIES(key-list) >= 9 AND use-field = ENTRY(9,key-list) THEN fr-value = STRING(ff9). 
    IF NUM-ENTRIES(key-list) >= 10 AND use-field = ENTRY(10,key-list) THEN fr-value = STRING(ff10). 
    IF NUM-ENTRIES(key-list) >= 11 AND use-field = ENTRY(11,key-list) THEN fr-value = STRING(ff11). 
    IF NUM-ENTRIES(key-list) >= 12 AND use-field = ENTRY(12,key-list) THEN fr-value = STRING(ff12). 

    IF old-value# NE fr-value 
    AND VALID-HANDLE(xfocus#)
    AND xfocus#:type = "fill-in":U
    THEN DO:
        xfocus#:SCREEN-VALUE = fr-value.
        APPLY "value-changed":U TO xfocus#.
    END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setCellColumns Include 
PROCEDURE setCellColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cellColumn AS WIDGET-HANDLE NO-UNDO EXTENT {&maxColumns}.
    DEFINE VARIABLE userName AS CHARACTER NO-UNDO EXTENT {&maxColumns}.
    DEFINE VARIABLE userWidth AS DECIMAL NO-UNDO EXTENT {&maxColumns}.
    DEFINE VARIABLE numColumns AS INTEGER NO-UNDO.
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE j AS INTEGER NO-UNDO.

    /* save original settings in order to do default restore */
    numColumns = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
    DO i = 1 TO numColumns:
        ASSIGN
            cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i)
            cellName[i] = cellColumn[i]:NAME
            cellWidth[i] = cellColumn[i]:WIDTH-PIXELS.
    END. /* do i */
    /* find the unique program and user record holding column settings */
    /* column field-name,column width,column field-name,column width, ... */

    FIND FIRST z_join-value NO-LOCK
        WHERE z_join-value.file-name EQ ENTRY(2,PROGRAM-NAME(1),' ')
        AND z_join-value.key-list EQ ENTRY(1,USERID("ptdb"),"@") NO-ERROR.
    IF AVAILABLE z_join-value THEN DO:
        i = 0.
        /* get user cell column order and size */
        DO j = 1 TO NUM-ENTRIES(z_join-value.rec-image) - 1 BY 2:
            ASSIGN
                i = i + 1
                userName[i] = ENTRY(j,z_join-value.rec-image)
                userWidth[i] = DECIMAL(ENTRY(j + 1,z_join-value.rec-image)).
        END. /* do j */
        /* change default columns to user order and size */
        DO i = 1 TO numColumns:
            cellColumn[i] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(i).
            DO j = 1 TO numColumns:
                /* check if any column order or size changes exist */
                IF cellColumn[i]:NAME EQ userName[j] AND (i NE j OR
                cellColumn[i]:WIDTH-PIXELS NE userWidth[j]) THEN DO:
                    cellColumn[i]:WIDTH-PIXELS = userWidth[j]. /* set column size */
                    IF i NE j THEN /* move columns */
                        {&BROWSE-NAME}:MOVE-COLUMN(i,j) IN FRAME {&FRAME-NAME}.
                    /* backup one and recheck, so we don't skip anything */
                    IF i NE 1 THEN i = i - 1.
                END. /* if changed */
            END. /* do j */
        END. /* do i */
    END. /* if avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

