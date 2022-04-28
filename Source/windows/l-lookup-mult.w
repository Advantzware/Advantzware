&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: windows\l-lookup.w

  Description: This is a generic dynamic lookup supporting single DB table

  Input Parameters:
    ip-title       :Title of the lookup screen
    ip-lookupField :The source field for which the lookup screen is called for
    ip-table-list  :DB Table from which data is to be fetched
    ip-fieldList   :List of fields which are required in the query
    ip-displayList :List of fields which should be displayed in the browse
    ip-labelList   :List of field labels to override the default database 
                    field label
    ip-formatList  :List of field formats to override the default database 
                    field format
    ip-widthList   :List of browse column width values to override the default 
                    column width in browse
    ip-filterList  :List of fields for which field level search needs to be 
                    enabled
    ip-sortList    :List of fields for which sorting needs to be enabled
    ip-queryString :Where clause to select specific records
    ip-outList     :List of fields for which the value is required to be 
                    returned when a row is selected in the browse
    ip-recLimit    :Max Record Limit to prevent run away query
    ip-subjectID   :Dynamic Subject ID
    ip-userid      :Dynamic User ID
    ip-paramValueID:Dynamic Parameter Value ID

  Output Parameters:
    op-returnFields:Pipe separated list of return field name and value as
                    output based on previous input list
    op-lookupField :Single return value which is to be returned from 
                    the lookup - this will populate in the field from 
                    where the lookup was opened
    op-recVal      :RecID of the row selected when a row is selected 
                    in the browse

  Author: Mithun Porandla

  Created: 4th March 2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE INPUT  PARAMETER ip-title        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-lookupField  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-table-list   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-fieldList    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-displayList  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-labelList    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-formatList   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-widthList    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-filterList   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-sortList     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-queryString  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-outList      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-recLimit     AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ip-subjectID    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ip-userid       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-paramValueID AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER op-returnFields AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-lookupField  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-recVal       AS RECID     NO-UNDO.
{sys/inc/var.i}

DEFINE VARIABLE cFilterValue    AS CHARACTER NO-UNDO EXTENT 1000.
DEFINE VARIABLE cCustListQuery  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustListTable  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFocusValue     AS CHARACTER NO-UNDO.
DEFINE VARIABLE h_brbuffer      AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_browser       AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_brquery       AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_brtt          AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_buffer        AS HANDLE    NO-UNDO EXTENT.
DEFINE VARIABLE lContainsWhere  AS LOGICAL   NO-UNDO EXTENT.
DEFINE VARIABLE h_dialogFrame   AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_query         AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_tt            AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_ttbuffer      AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_ttquery       AS HANDLE    NO-UNDO.
DEFINE VARIABLE hCalcColumn     AS HANDLE    NO-UNDO EXTENT 1000.
DEFINE VARIABLE hColumn         AS HANDLE    NO-UNDO EXTENT 1000.
DEFINE VARIABLE hDynCalcField   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hFilterField    AS HANDLE    NO-UNDO EXTENT 1000.
DEFINE VARIABLE hStatusField    AS HANDLE    NO-UNDO EXTENT 1000.
DEFINE VARIABLE iRowCount       AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-count        AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-maxBrRows    AS INTEGER   NO-UNDO INITIAL 30.
DEFINE VARIABLE li-pageCount    AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE li-pageRecCount AS INTEGER   NO-UNDO INITIAL 30.
DEFINE VARIABLE ls-queryString  AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-sortBy       AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-sortType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-continue     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ll-filterFirst  AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE ll-filterFlag   AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE ll-filterOpen   AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE ll-ttLoaded     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE ll-useMatches   AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE lRowSelectable  AS LOGICAL   NO-UNDO EXTENT 1000 INITIAL YES.
DEFINE VARIABLE lUseCustList    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rDynValueColumn AS ROWID     NO-UNDO EXTENT 1000.
DEFINE VARIABLE h_focus         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hWidget         AS HANDLE    NO-UNDO EXTENT 1000.


RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.

{sys/ref/CustList.i NEW}
{AOA/dynBL/pBuildCustList.i}
{AOA/includes/pGetDynParamValue.i}
{methods/template/brwcustomdef.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME br-table

/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnClear ls-search useWildcards br-table ~
btnOK bt-clear bt-filter bt-cancel bt-next bt-ok bt-prev 
&Scoped-Define DISPLAYED-OBJECTS useWildcards 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnClear useWildcards btnOK 
&Scoped-define List-2 ls-search bt-clear 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD generateFilterQuery Dialog-Frame 
FUNCTION generateFilterQuery RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD generateSearchQuery Dialog-Frame 
FUNCTION generateSearchQuery RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSearchValue Dialog-Frame 
FUNCTION getSearchValue RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON bt-cancel AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/exit_white.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel"
     BGCOLOR 8 .

DEFINE BUTTON bt-clear 
     IMAGE-UP FILE "Graphics/32x32/undo_32.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON bt-filter 
     IMAGE-UP FILE "Graphics/32x32/filter_and_sort.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Toggle Column Filters".

DEFINE BUTTON bt-next 
     IMAGE-UP FILE "Graphics/32x32/navigate_close.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_close_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Next" 
     SIZE 8 BY 1.91 TOOLTIP "Page Down"
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "OK"
     BGCOLOR 8 .

DEFINE BUTTON bt-prev 
     IMAGE-UP FILE "Graphics/32x32/navigate_open.png":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_open_disabled.png":U NO-FOCUS FLAT-BUTTON
     LABEL "&Prev" 
     SIZE 8 BY 1.91 TOOLTIP "Page Up"
     BGCOLOR 8 .

DEFINE BUTTON btnClear 
     IMAGE-UP FILE "Graphics/32x32/undo_32.png":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON btnOK 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U NO-FOCUS FLAT-BUTTON
     LABEL "OK" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE VARIABLE ls-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 49 BY 1.14 NO-UNDO.

DEFINE VARIABLE useWildcards AS LOGICAL INITIAL no 
     LABEL "Use Wildcards" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY 1 NO-UNDO.


/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table Dialog-Frame _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 112 BY 25.29 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnClear AT ROW 1.24 COL 18 WIDGET-ID 34
     ls-search AT ROW 1.71 COL 25 COLON-ALIGNED WIDGET-ID 32
     useWildcards AT ROW 1.71 COL 42 WIDGET-ID 38
     br-table AT ROW 4.52 COL 1 WIDGET-ID 200
     btnOK AT ROW 1.24 COL 10 WIDGET-ID 36
     bt-clear AT ROW 1.24 COL 10 WIDGET-ID 24
     bt-filter AT ROW 1.24 COL 2 WIDGET-ID 2
     bt-cancel AT ROW 1.24 COL 105 WIDGET-ID 22
     bt-next AT ROW 1.24 COL 85 WIDGET-ID 26
     bt-ok AT ROW 1.24 COL 97 WIDGET-ID 28
     bt-prev AT ROW 1.24 COL 77 WIDGET-ID 30
     SPACE(28.00) SKIP(26.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 
         TITLE BGCOLOR 22 FGCOLOR 1 "Help Information" WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB br-table useWildcards Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON bt-clear IN FRAME Dialog-Frame
   2                                                                    */
ASSIGN 
       bt-clear:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnClear IN FRAME Dialog-Frame
   1                                                                    */
ASSIGN 
       btnClear:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btnOK IN FRAME Dialog-Frame
   1                                                                    */
ASSIGN 
       btnOK:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN ls-search IN FRAME Dialog-Frame
   NO-DISPLAY 2                                                         */
ASSIGN 
       ls-search:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX useWildcards IN FRAME Dialog-Frame
   1                                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Help Information */
DO:
    RUN pClose.
    APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-table
&Scoped-define SELF-NAME br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table Dialog-Frame
ON DEFAULT-ACTION OF br-table IN FRAME Dialog-Frame
DO:
    APPLY "CHOOSE":U TO bt-ok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table Dialog-Frame
ON PAGE-DOWN OF br-table IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE":U TO bt-next.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table Dialog-Frame
ON PAGE-UP OF br-table IN FRAME Dialog-Frame
DO:
  APPLY "CHOOSE":U TO bt-prev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table Dialog-Frame
ON RETURN OF br-table IN FRAME Dialog-Frame
DO:
    APPLY "DEFAULT-ACTION":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table Dialog-Frame
ON ROW-DISPLAY OF br-table IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    DEFINE VARIABLE jdx AS INTEGER NO-UNDO.

    iRowCount = iRowCount + 1.
    /* if a status field, run status field comparison */
    DO idx = 1 TO h_brbuffer:NUM-FIELDS:
        IF VALID-HANDLE(hStatusField[idx]) THEN DO:
            FIND FIRST dynValueColumn NO-LOCK
                 WHERE ROWID(dynValueColumn) EQ rDynValueColumn[idx]
                 NO-ERROR.
            IF NOT AVAILABLE dynValueColumn THEN NEXT.
            IF dynValueColumn.isStatusField AND
               dynValueColumn.textColor NE dynValueColumn.cellColor AND
               DYNAMIC-FUNCTION("fDynStatusField" IN hDynCalcField,
                   h_brquery:HANDLE,
                   h_brbuffer:NAME + "." + ENTRY(2,dynValueColumn.colName,"."),
                   dynValueColumn.statusCompare,
                   dynValueColumn.compareValue) THEN DO:
                lRowSelectable[iRowCount] = INDEX(dynValueColumn.statusAction,"Unselectable") EQ 0.
                IF dynValueColumn.statusAction BEGINS "Row" THEN
                DO jdx = 1 TO h_brbuffer:NUM-FIELDS:
                    IF VALID-HANDLE(hColumn[jdx]) THEN
                    ASSIGN
                        hColumn[jdx]:FGCOLOR = dynValueColumn.textColor
                        hColumn[jdx]:BGCOLOR = dynValueColumn.cellColor
                        .
                END. /* else */
                ELSE IF dynValueColumn.statusAction BEGINS "Cell" THEN
                ASSIGN
                    hStatusField[dynValueColumn.sortOrder]:FGCOLOR = dynValueColumn.textColor
                    hStatusField[dynValueColumn.sortOrder]:BGCOLOR = dynValueColumn.cellColor
                    .
            END. /* if fDynStatusField */
        END. /* if valid-handle */
    END. /* do idx */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-table Dialog-Frame
ON START-SEARCH OF br-table IN FRAME Dialog-Frame
DO:
        {methods/template/sortindicator.i} 
   IF INDEX(ip-sortList, REPLACE(h_browser:CURRENT-COLUMN:NAME, "&", ".")) > 0 THEN DO:
       IF ls-sortBy = h_browser:CURRENT-COLUMN:NAME THEN
            ls-sortType = IF ls-sortType = "DESCENDING" THEN "" ELSE "DESCENDING".

       ASSIGN 
          ls-sortBy = h_browser:CURRENT-COLUMN:NAME.

       IF ll-filterOpen THEN
          RUN openFilterQuery.
       ELSE
          RUN openSearchQuery.
   END.
   {methods/template/sortindicatorend.i} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-clear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-clear Dialog-Frame
ON CHOOSE OF bt-clear IN FRAME Dialog-Frame /* Reset */
DO:
    ls-search:SCREEN-VALUE = "".  
    RUN openSearchQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-filter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filter Dialog-Frame
ON CHOOSE OF bt-filter IN FRAME Dialog-Frame
DO: 
    IF NOT ll-ttLoaded THEN DO:
        MESSAGE "Large set of records available. Toggling search mode is disabled"
                VIEW-AS ALERT-BOX INFORMATION.
        
        RETURN NO-APPLY.    
    END.    
    RUN resizeFilterFrame. 
    IF ll-filterOpen THEN
        RUN openFilterQuery.
    ELSE
        RUN openSearchQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-next Dialog-Frame
ON CHOOSE OF bt-next IN FRAME Dialog-Frame /* Next */
DO:
    bt-prev:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
    IF ll-filterOpen THEN
        RUN nextPageFilter(INPUT h_query,
                     INPUT h_buffer).    
    ELSE
        RUN nextPage(INPUT h_ttquery,
                     INPUT h_ttbuffer).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-ok Dialog-Frame
ON CHOOSE OF bt-ok IN FRAME Dialog-Frame
DO:
    DEFINE VARIABLE h_lfield AS HANDLE NO-UNDO.
    
    IF h_browser:NUM-SELECTED-ROWS GT 0 AND h_brBuffer:AVAILABLE THEN DO:
        IF lRowSelectable[h_browser:FOCUSED-ROW] THEN DO:
            DO li-count = 1 TO NUM-ENTRIES(ip-outList):
                ASSIGN
                    h_lfield        = h_brbuffer:BUFFER-FIELD(REPLACE(ENTRY(li-count,ip-outList), ".", "&")):HANDLE
                    op-returnFields = op-returnFields + ENTRY(li-count,ip-outList) + "|"
                                    + (IF h_lfield:DATA-TYPE EQ "DATE" THEN
                                          (IF h_lfield:BUFFER-VALUE EQ ? THEN "" 
                                           ELSE h_lfield:BUFFER-VALUE)
                                       ELSE h_lfield:BUFFER-VALUE)
                                    + "|"
                                    .
            END.
            ASSIGN
                op-lookupField = h_brbuffer:BUFFER-FIELD(REPLACE(ip-lookupField, ".", "&")):BUFFER-VALUE
                op-recVal      = h_brbuffer:BUFFER-FIELD("recid"):BUFFER-VALUE
                .
        END. /* if lrowselectable */
        ELSE DO:
            MESSAGE
                "Selection of this Record is not allowed"
            VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END. /* else */
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prev Dialog-Frame
ON CHOOSE OF bt-prev IN FRAME Dialog-Frame /* Prev */
DO: 
    bt-next:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
    IF ll-filterOpen THEN
        RUN prevPageFilter(INPUT h_query,
                     INPUT h_buffer).    
    ELSE
        RUN prevPage(INPUT h_ttquery,
                     INPUT h_ttbuffer).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClear Dialog-Frame
ON CHOOSE OF btnClear IN FRAME Dialog-Frame /* Reset */
DO:
    RUN resetFilterObjects.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOK Dialog-Frame
ON CHOOSE OF btnOK IN FRAME Dialog-Frame /* OK */
DO:
    RUN openFilterQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ls-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ls-search Dialog-Frame
ON VALUE-CHANGED OF ls-search IN FRAME Dialog-Frame /* Search */
DO:
    RUN openSearchQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME useWildcards
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL useWildcards Dialog-Frame
ON VALUE-CHANGED OF useWildcards IN FRAME Dialog-Frame /* Use Wildcards */
DO:
    RUN toggleMatches.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /* grab screen value as a starting value */
    cFocusValue = FOCUS:SCREEN-VALUE NO-ERROR.
    /* if screen value is HI-VALUE, clear so user sees all entries */
    IF cFocusValue EQ CHR(254) OR cFocusValue EQ ? THEN
    cFocusValue = "".
    RUN validateParameters NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    RETURN ERROR.
    RUN init.
    RUN enable_UI.
    RUN resizeWindow. 
    IF ll-filterFirst THEN DO:
        RUN resizeFilterFrame.        
        RUN openFilterQuery.
    END.    
    ELSE DO WITH FRAME {&FRAME-NAME}:
        RUN buildTempTable.
        RUN openSearchQuery.
        HIDE {&List-1}.
        ENABLE {&List-2}.
        APPLY 'ENTRY' TO ls-search IN FRAME {&FRAME-NAME}.
    END.  
    RUN customizeBrowse.
    IF VALID-HANDLE(h_focus) THEN
    APPLY "ENTRY":U TO h_focus.
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN pClose.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addBrowseCols Dialog-Frame 
PROCEDURE addBrowseCols :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE h_colHandle AS HANDLE NO-UNDO.
    
    DO li-count = 1 TO h_brbuffer:NUM-FIELDS:
        h_brbuffer:BUFFER-FIELD(li-count):VALIDATE-EXPRESSION = "".
    END.

    DO li-count = 1 TO NUM-ENTRIES(ip-displayList):
        h_browser:ADD-LIKE-COLUMN(h_brbuffer:NAME + "." + REPLACE(ENTRY(li-count, ip-displayList),".", "&")).
        h_colHandle = h_browser:GET-BROWSE-COLUMN(li-count):HANDLE.

        IF INDEX(ip-sortList, ENTRY(li-count, ip-displayList)) GT 0 THEN
        h_colHandle:LABEL-BGCOLOR = 14.           
        IF ip-widthList NE "" AND 
           NUM-ENTRIES(ip-widthList) GE li-count AND
           ENTRY(li-count, ip-widthList) NE "" THEN
        h_colHandle:WIDTH-CHARS = INTEGER(ENTRY(li-count, ip-widthList)).
        IF h_colHandle:DATA-TYPE EQ "DATE" AND
           INDEX(ip-filterList, h_colHandle:NAME) GT 0 AND 
           (h_colHandle:WIDTH-CHARS LT 20 OR
            h_colHandle:WIDTH-CHARS EQ ?) THEN
        h_colHandle:WIDTH-CHARS = 20.
        IF h_colHandle:DATA-TYPE EQ "CHARACTER" THEN
        ASSIGN
            h_colHandle:WIDTH-CHARS = FONT-TABLE:GET-TEXT-WIDTH-CHARS(STRING(FILL("X",256),h_colHandle:FORMAT))
            h_colHandle:WIDTH-CHARS = MAX(h_colHandle:WIDTH-CHARS,LENGTH(h_colHandle:LABEL)) + 2
            .
            .
        IF ip-SubjectID NE 0 THEN DO:
            hColumn[li-count] = h_colHandle.
            FIND FIRST dynValueColumn NO-LOCK
                 WHERE dynValueColumn.subjectID    EQ ip-subjectID
                   AND dynValueColumn.user-id      EQ ip-userID
                   AND dynValueColumn.paramValueID EQ ip-paramValueID
                   AND dynValueColumn.colName      EQ h_colHandle:NAME
                   AND dynValueColumn.isCalcField  EQ YES
                   AND dynValueColumn.calcProc     NE ""
                 NO-ERROR.
            IF AVAILABLE dynValueColumn THEN
            ASSIGN
                hCalcColumn[li-count]     = h_colHandle
                rDynValueColumn[li-count] = ROWID(dynValueColumn)
                .
            FIND FIRST dynValueColumn NO-LOCK
                 WHERE dynValueColumn.subjectID     EQ ip-subjectID
                   AND dynValueColumn.user-id       EQ ip-userID
                   AND dynValueColumn.paramValueID  EQ ip-paramValueID
                   AND dynValueColumn.isStatusField EQ YES
                   AND LOOKUP(h_colHandle:NAME,dynValueColumn.colName,".") EQ 2
                 NO-ERROR.
            IF AVAILABLE dynValueColumn THEN
            ASSIGN
                hStatusField[li-count]    = h_colHandle
                rDynValueColumn[li-count] = ROWID(dynValueColumn)
                .
        END. /* if ne 0 */
    END. /* do li-count */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addFilterObjects Dialog-Frame 
PROCEDURE addFilterObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE h_field     AS HANDLE  NO-UNDO.
    DEFINE VARIABLE h_fillin    AS HANDLE  NO-UNDO.
    DEFINE VARIABLE h_combobox  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE h_browseCol AS HANDLE  NO-UNDO.
    DEFINE VARIABLE h_calendar  AS HANDLE  NO-UNDO.
    DEFINE VARIABLE h_colNum    AS INTEGER NO-UNDO.
    
    IF ip-filterList NE "" THEN DO:
        hWidget = ?.
        DO li-count = 1 TO NUM-ENTRIES(ip-filterList):      
            ASSIGN 
                h_field     = h_ttbuffer:BUFFER-FIELD(REPLACE(ENTRY(li-count,ip-filterList), ".","&"))
                h_colNum    = LOOKUP(ENTRY(li-count,ip-filterList), ip-displayList)
                h_browseCol = h_browser:GET-BROWSE-COL(h_colNum):HANDLE
                .                

            IF ip-subjectID NE 0 THEN DO:
                FIND FIRST dynValueColumn NO-LOCK
                    WHERE dynValueColumn.subjectID         EQ ip-subjectID
                      AND dynValueColumn.user-id           EQ ip-userID
                      AND dynValueColumn.paramValueID      EQ ip-paramValueID
                      AND dynValueColumn.isFilterInitField EQ YES
                      AND dynValueColumn.colName           EQ ENTRY(li-count,ip-filterList)
                    NO-ERROR.
                IF AVAILABLE dynValueColumn THEN
                    ASSIGN
                        hFilterField[li-count] = h_browseCol
                        cFilterValue[li-count] = dynValueColumn.filterInitValue
                        .
            END.

            IF VALID-HANDLE(h_field) THEN DO:
                IF h_field:DATA-TYPE EQ "CHARACTER" OR
                   h_field:DATA-TYPE EQ "INTEGER"   OR
                   h_field:DATA-TYPE EQ "DECIMAL"   OR
                   h_field:DATA-TYPE EQ "DATE" THEN DO:
                    CREATE FILL-IN h_fillin
                        ASSIGN 
                        FRAME        = FRAME {&FRAME-NAME}:HANDLE
                        ROW          = h_browser:ROW - 1.26
                        X            = h_browseCol:X
                        SENSITIVE    = TRUE
                        WIDTH-PIXELS = h_browseCol:WIDTH-PIXELS + 4
                        DATA-TYPE    = h_field:DATA-TYPE
                        FORMAT       = h_field:FORMAT
                        VISIBLE      = TRUE
                        SCREEN-VALUE = ""
                        PRIVATE-DATA = h_field:NAME
                        TRIGGERS:
                            ON ENTRY  PERSISTENT RUN pSetBGColor (h_fillin, 14).
                            ON LEAVE  PERSISTENT RUN pSetBGColor (h_fillin, ?).
                            ON RETURN PERSISTENT RUN applyFilter.
                        END TRIGGERS.
                    hWidget[li-count] = h_fillin:HANDLE.
                    IF li-count EQ 1 THEN
                    h_focus = h_fillin.
                    IF h_field:DATA-TYPE EQ "DATE" THEN DO:
                        h_fillin:WIDTH-CHARS = 16.                      
                        CREATE BUTTON h_calendar
                            ASSIGN 
                            FRAME        = FRAME {&FRAME-NAME}:HANDLE                         
                            ROW          = h_browser:ROW - 1.28
                            COLUMN       = h_fillin:COLUMN + h_fillin:WIDTH
                            WIDTH        = 4.6
                            HEIGHT       = h_fillin:HEIGHT                          
                            SENSITIVE    = TRUE
                            VISIBLE      = TRUE
                            PRIVATE-DATA = h_field:NAME
                            TRIGGERS:
                                ON CHOOSE PERSISTENT RUN chooseDate (h_calendar:PRIVATE-DATA).
                            END TRIGGERS.            
                        hWidget[li-count] = h_fillin:HANDLE.
                        h_calendar:LOAD-IMAGE-UP("Graphics/16x16/calendar.bmp").          
                    END.
                END.
                ELSE IF h_field:DATA-TYPE EQ "LOGICAL" THEN DO:
                        CREATE COMBO-BOX h_combobox
                            ASSIGN 
                            FRAME           = FRAME {&FRAME-NAME}:HANDLE
                            ROW             = h_browser:ROW - 1.26
                            X               = h_browseCol:X
                            WIDTH-PIXELS    = h_browseCol:WIDTH-PIXELS + 4
                            SENSITIVE       = TRUE
                            VISIBLE         = TRUE
                            INNER-LINES     = 3
                            LIST-ITEM-PAIRS = "All,1,"
                                    + ENTRY(1,h_field:FORMAT,"/") + ",2,"
                                    + ENTRY(2,h_field:FORMAT,"/") + ",3"
                            PRIVATE-DATA    = h_field:NAME
                            SCREEN-VALUE    = "1"
                            TRIGGERS:
                                ON VALUE-CHANGED PERSISTENT RUN openFilterQuery.
                            END TRIGGERS.
                        hWidget[li-count] = h_fillin:HANDLE.
                    END.
            END.
        END.
        
        bt-ok:HANDLE:MOVE-TO-TOP().
        bt-cancel:HANDLE:MOVE-TO-TOP().
        bt-prev:HANDLE:MOVE-TO-TOP().
        bt-next:HANDLE:MOVE-TO-TOP().
 
    END.
    CLEAR FRAME {&FRAME-NAME} NO-PAUSE.
    /* set any filter init values */
    DO li-count = 1 TO NUM-ENTRIES(ip-filterList):
        IF VALID-HANDLE(hFilterField[li-count]) THEN
            hWidget[li-count]:SCREEN-VALUE = cFilterValue[li-count].
    END. /* do li-count */
    IF cFocusValue NE "0" AND cFocusValue NE "" THEN 
        h_focus:SCREEN-VALUE = cFocusValue.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE applyFilter Dialog-Frame 
PROCEDURE applyFilter :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    APPLY "CHOOSE":U TO btnOK IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE attachQuery Dialog-Frame 
PROCEDURE attachQuery :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    
    IF ip-sortList <> "" THEN
        ASSIGN 
            ls-sortBy   = REPLACE(ENTRY(1,ENTRY(1,ip-sortList),"|"), ".", "&")
            ls-sortType = IF NUM-ENTRIES(ENTRY(1,ip-sortList),"|") EQ 2 THEN 
                              "DESCENDING"
                          ELSE
                              "".

    h_brquery:QUERY-PREPARE(
        "FOR EACH" + " " +
        h_brbuffer:NAME + " " +
        "NO-LOCK" + 
        REPLACE(cCustListQuery, "&1", h_brbuffer:NAME)
        ).

    h_brquery:QUERY-OPEN().
                                        
    h_browser:QUERY = h_brquery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buildTempTable Dialog-Frame 
PROCEDURE buildTempTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE h_field    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE ls-allData AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iBuffer    AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cLocalCustListQuery  AS CHARACTER NO-UNDO.

    ASSIGN
        cLocalCustListQuery = cCustListQuery
        cLocalCustListQuery = REPLACE(cLocalCustListQuery, "&1.", "")
        cLocalCustListQuery = REPLACE(cLocalCustListQuery, "&", ".")
        .
                
    ls-queryString = ip-queryString
                   + cLocalCustListQuery.
                    
    h_query:QUERY-PREPARE (ls-queryString).
    
    h_query:QUERY-OPEN().        
    h_query:GET-FIRST().

    IF NOT h_buffer[1]:AVAILABLE THEN
    MESSAGE "No records found for table" h_buffer[1]:NAME VIEW-AS ALERT-BOX.    
    SESSION:SET-WAIT-STATE("GENERAL").
    REPEAT:
        IF h_query:QUERY-OFF-END THEN LEAVE.
        ls-allData = "".
        h_ttbuffer:BUFFER-CREATE().
        
        DO li-count = 1 TO NUM-ENTRIES(ip-fieldList):
            IF ENTRY(li-count, ip-fieldList) BEGINS "Calc" THEN
                NEXT.
                
            iBuffer = LOOKUP(ENTRY(1, ENTRY(li-count, ip-fieldList), "."), ip-table-list). 
            
            h_ttbuffer:BUFFER-FIELD (REPLACE(ENTRY(li-count, ip-fieldList), ".", "&")):BUFFER-VALUE = h_buffer[iBuffer]:BUFFER-FIELD(ENTRY(2, ENTRY(li-count, ip-fieldList), ".")):BUFFER-VALUE.
        END.
        
        DO li-count = 1 TO NUM-ENTRIES(ip-displayList):
            IF ENTRY(li-count, ip-fieldList) BEGINS "Calc" THEN
                NEXT.
                
            iBuffer = LOOKUP(ENTRY(1, ENTRY(li-count, ip-displayList), "."), ip-table-list). 
            
            h_field = h_buffer[iBuffer]:BUFFER-FIELD(ENTRY(2, ENTRY(li-count, ip-displayList), ".")):HANDLE NO-ERROR.
            IF VALID-HANDLE(h_field) THEN DO:
                IF h_field:DATA-TYPE = "LOGICAL" THEN 
                ls-allData = ls-allData + STRING(h_field:BUFFER-VALUE, h_field:FORMAT) + "|".
                ELSE IF h_field:DATA-TYPE = "DATE" THEN 
                ls-allData = ls-allData + STRING((IF h_field:BUFFER-VALUE = ? THEN '' ELSE h_field:BUFFER-VALUE), h_field:FORMAT) + "|".
                ELSE 
                ls-allData = ls-allData + STRING(h_field:BUFFER-VALUE) + "|".
            END.
/*            RUN pCalcFields (h_brtt, h_ttbuffer).*/
        END.        
        ASSIGN
            h_ttbuffer:BUFFER-FIELD("allData"):BUFFER-VALUE = ls-allData
            h_ttbuffer:BUFFER-FIELD("recid"):BUFFER-VALUE   = h_buffer[iBuffer]:RECID
            .

        h_query:GET-NEXT().
    END.    
    SESSION:SET-WAIT-STATE("").    
    ll-ttLoaded = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE chooseDate Dialog-Frame 
PROCEDURE chooseDate :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-widget-field    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.

    DEFINE VARIABLE h_widget     AS HANDLE    NO-UNDO.

    RUN nosweat/popupcal2.w (OUTPUT calendarDate).
    IF calendarDate NE '' THEN DO:
        h_widget = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.
        DO WHILE VALID-HANDLE(h_widget):
            IF h_widget:TYPE = "FILL-IN" AND
               h_widget:DATA-TYPE = "DATE" AND
               h_widget:PRIVATE-DATA = ip-widget-field THEN DO:
                h_widget:SCREEN-VALUE = calendarDate.
                LEAVE.
            END.
  
            h_widget = h_widget:NEXT-SIBLING.
        END.    
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createTempTables Dialog-Frame 
PROCEDURE createTempTables :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE h_field    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE ls-label   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-format  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE li-lookup  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ll-success AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iBuffer    AS INTEGER   NO-UNDO.
    
    CREATE TEMP-TABLE h_tt.
    CREATE TEMP-TABLE h_brtt.
    
    DO li-count = 1 TO NUM-ENTRIES(ip-fieldList):
        iBuffer = LOOKUP(ENTRY(1, ENTRY(li-count, ip-fieldList), "."), ip-table-list). 
        
        IF NOT ENTRY(li-count,ip-fieldList) BEGINS "Calc" THEN DO:
            h_field = h_buffer[iBuffer]:BUFFER-FIELD(ENTRY(2, ENTRY(li-count,ip-fieldList), ".")).

            ASSIGN ls-label  = h_field:LABEL
                   ls-format = h_field:FORMAT
                   li-lookup = 0
                   .    
            li-lookup = LOOKUP(h_field:NAME, ip-displayList).
    
            IF li-lookup > 0 THEN DO:
                IF ip-labelList <> "" AND 
                   NUM-ENTRIES(ip-labelList) >= li-lookup AND
                   ENTRY(li-lookup,ip-labelList) <> "" THEN
                ls-label = ENTRY(li-lookup,ip-labelList).
    
                IF ip-formatList <> "" AND 
                   NUM-ENTRIES(ip-formatList) >= li-lookup AND
                   ENTRY(li-lookup,ip-formatList) <> "" THEN
                ls-format = ENTRY(li-lookup,ip-formatList).
    
            END.
            
            ll-success = h_tt:ADD-NEW-FIELD(REPLACE(ENTRY(li-count, ip-fieldList), ".","&"),
                                            h_field:DATA-TYPE,
                                            h_field:EXTENT,
                                            ls-format,
                                            h_field:DEFAULT-VALUE,
                                            ls-label) NO-ERROR.
    
            /* use default field format in case supplied format is incorrect */
            IF NOT ll-success THEN
                h_tt:ADD-NEW-FIELD(REPLACE(ENTRY(li-count, ip-fieldList), ".","&"),
                                   h_field:DATA-TYPE,
                                   h_field:EXTENT,
                                   h_field:FORMAT,
                                   h_field:DEFAULT-VALUE,
                                   ls-label) NO-ERROR.
        END. /* if valid-handle */
        ELSE IF ip-SubjectID NE 0 THEN DO:
            FIND FIRST dynValueColumn NO-LOCK
                 WHERE dynValueColumn.subjectID    EQ ip-subjectID
                   AND dynValueColumn.user-id      EQ ip-userID
                   AND dynValueColumn.paramValueID EQ ip-paramValueID
                   AND dynValueColumn.colName      EQ ENTRY(li-count,ip-fieldList)
                   AND dynValueColumn.isCalcField  EQ YES
                   AND dynValueColumn.calcProc     NE ""
                 NO-ERROR.
            IF NOT AVAILABLE dynValueColumn THEN NEXT.
            h_tt:ADD-NEW-FIELD(
                dynValueColumn.colName,
                dynValueColumn.dataType,
                0,
                dynValueColumn.colFormat,
                "",
                dynValueColumn.colLabel
                ) NO-ERROR.
        END. /* else */
    END. /* do li-count */
    
    h_tt:ADD-NEW-FIELD("allData", "CHARACTER").
    h_tt:ADD-NEW-FIELD("recid", "RECID").
    h_tt:TEMP-TABLE-PREPARE("ttData").
    
    h_brtt:CREATE-LIKE(h_tt).
    h_brtt:TEMP-TABLE-PREPARE("ttResultSet").

    h_ttbuffer = h_tt:DEFAULT-BUFFER-HANDLE.
    h_ttquery:SET-BUFFERS(h_ttbuffer).
    IF lUseCustList THEN
    h_ttquery:ADD-BUFFER(BUFFER ttCustList:HANDLE).

    h_brbuffer = h_brtt:DEFAULT-BUFFER-HANDLE.
    h_brquery:SET-BUFFERS(h_brbuffer).
    IF lUseCustList THEN
    h_brquery:ADD-BUFFER(BUFFER ttCustList:HANDLE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE customizeBrowse Dialog-Frame 
PROCEDURE customizeBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    h_browser:ALLOW-COLUMN-SEARCHING = TRUE.
        
/*    h_browser:BGCOLOR = 8.*/
    h_browser:SENSITIVE = YES.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY useWildcards 
      WITH FRAME Dialog-Frame.
  ENABLE btnClear ls-search useWildcards btnOK bt-clear bt-filter bt-cancel 
         bt-next bt-ok bt-prev 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init Dialog-Frame 
PROCEDURE init :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cCompany       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustListField AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustListID    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cStartCustList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iBuffer        AS INTEGER   NO-UNDO.
    
    IF ip-subjectID NE 0 THEN DO:
        RUN pGetDynParamValue (ip-subjectID, "", "", 0).
        IF dynParamValue.useCustList OR dynParamValue.CustListID NE "" THEN DO:
            FIND FIRST dynValueColumn NO-LOCK
                 WHERE dynValueColumn.subjectID     EQ dynParamValue.subjectID
                   AND dynValueColumn.user-id       EQ dynParamValue.user-id
                   AND dynValueColumn.prgmName      EQ dynParamValue.prgmName
                   AND dynValueColumn.paramValueID  EQ dynParamValue.paramValueID
                   AND dynValueColumn.CustListField EQ YES
                 NO-ERROR.
            IF AVAILABLE dynValueColumn THEN DO:
                cCustListID = dynParamValue.CustListID.
                IF cCustListID EQ "" THEN
                RUN spGetSessionParam ("CustListID", OUTPUT cCustListID).
                RUN spSetSessionParam ("CustListID", "").
                RUN spGetSessionParam ("Company", OUTPUT cCompany).
                RUN pBuildCustList (
                    cCompany,
                    cCustListID,
                    OUTPUT cStartCustList,
                    OUTPUT cEndCustList,
                    OUTPUT lUseCustList
                    ).
                IF lUseCustList THEN
                ASSIGN
                    cCustListField = REPLACE(dynValueColumn.colName,".", "&")
                    cCustListTable = ENTRY(1, dynValueColumn.colName, ".")
                    cCustListQuery = ", FIRST ttCustList WHERE ttCustList.cust-no EQ &1."
                                   + cCustListField
                                   + " AND ttCustList.log-fld EQ YES "
                                   .
            END. /* if avail */
        END. /* if custlistid */
    END. /* if ip-subjectid */

    CREATE QUERY h_query.

    EXTENT(h_buffer) = NUM-ENTRIES(ip-queryString).
    EXTENT(lContainsWhere) = EXTENT(h_buffer).
    
    DO iBuffer = 1 TO NUM-ENTRIES(ip-table-list):
        CREATE BUFFER h_buffer[iBuffer] FOR TABLE ENTRY(iBuffer, ip-table-list).
        h_query:ADD-BUFFER(h_buffer[iBuffer]).
    END.    
    
    DO iBuffer = 1 TO NUM-ENTRIES(ip-queryString):        
        IF INDEX(ENTRY(iBuffer, ip-queryString), "WHERE") GT 0 THEN
            lContainsWhere[iBuffer] = TRUE.
    END.

    IF lUseCustList THEN
    h_query:ADD-BUFFER(BUFFER ttCustList:HANDLE).
    CREATE QUERY h_ttquery.    
    CREATE QUERY h_brquery.
    ASSIGN
        h_browser = br-table:HANDLE IN FRAME {&FRAME-NAME}    
        h_dialogFrame = FRAME {&FRAME-NAME}:HANDLE
        h_dialogFrame:TITLE = ip-title
        .    
        
    RUN validateRecordLimit (OUTPUT ll-filterFirst).    
    RUN createTempTables.
    RUN attachQuery.
    RUN addBrowseCols.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextPage Dialog-Frame 
PROCEDURE nextPage PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER h_ipquery  AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER h_ipbuffer AS HANDLE NO-UNDO.
        
    DEFINE VARIABLE li-count    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBuffer     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iFieldCount AS INTEGER NO-UNDO.
    
    h_brbuffer:EMPTY-TEMP-TABLE().

    ASSIGN
        li-pageCount    = li-pageCount + 1
        li-pageRecCount = li-maxBrRows
        iRowCount       = 0
        lRowSelectable  = YES
        .    
    bt-next:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.   
    DO li-count = 1 TO li-maxBrRows:
        h_ipquery:GET-NEXT().
        IF h_ipquery:QUERY-OFF-END THEN DO:
            bt-next:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.        
            LEAVE.
        END.
        li-pageRecCount = li-pageRecCount - 1.
         
        h_brbuffer:BUFFER-CREATE().

        DO iFieldCount = 1 TO NUM-ENTRIES(ip-fieldList):
            iBuffer = LOOKUP(ENTRY(1, ENTRY(iFieldCount, ip-fieldList), "."), ip-table-list). 
            
            h_brbuffer:BUFFER-FIELD (REPLACE(ENTRY(iFieldCount, ip-fieldList), ".", "&")):BUFFER-VALUE = h_ipbuffer:BUFFER-FIELD(REPLACE(ENTRY(iFieldCount, ip-fieldList), ".", "&")):BUFFER-VALUE.
        END.

        RUN pCalcFields (h_brquery, h_brbuffer).
    
        IF ll-filterOpen THEN
        h_brbuffer:BUFFER-FIELD("recid"):BUFFER-VALUE = h_ipbuffer:RECID.               
    END.
    
    h_brquery:QUERY-OPEN().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextPageFilter Dialog-Frame 
PROCEDURE nextPageFilter :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER h_ipquery  AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER h_ipbuffer AS HANDLE NO-UNDO EXTENT.
        
    DEFINE VARIABLE li-count    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBuffer     AS INTEGER NO-UNDO.
    DEFINE VARIABLE iFieldCount AS INTEGER NO-UNDO.
    
    h_brbuffer:EMPTY-TEMP-TABLE().

    ASSIGN
        li-pageCount    = li-pageCount + 1
        li-pageRecCount = li-maxBrRows
        iRowCount       = 0
        lRowSelectable  = YES
        .    
    bt-next:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.   
    DO li-count = 1 TO li-maxBrRows:
        h_ipquery:GET-NEXT().
        IF h_ipquery:QUERY-OFF-END THEN DO:
            bt-next:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.        
            LEAVE.
        END.
        li-pageRecCount = li-pageRecCount - 1.
         
        h_brbuffer:BUFFER-CREATE().

        DO iFieldCount = 1 TO NUM-ENTRIES(ip-fieldList):
            iBuffer = LOOKUP(ENTRY(1, ENTRY(iFieldCount, ip-fieldList), "."), ip-table-list). 
            IF iBuffer GT 0 THEN
            h_brbuffer:BUFFER-FIELD (REPLACE(ENTRY(iFieldCount, ip-fieldList), ".", "&")):BUFFER-VALUE = h_ipbuffer[iBuffer]:BUFFER-FIELD(ENTRY(2, ENTRY(iFieldCount, ip-fieldList), ".")):BUFFER-VALUE.
        END.

        RUN pCalcFields (h_brquery, h_brbuffer).
    
        IF ll-filterOpen AND iBuffer GT 0 THEN
        h_brbuffer:BUFFER-FIELD("recid"):BUFFER-VALUE = h_ipbuffer[iBuffer]:RECID.               
    END.
    
    h_brquery:QUERY-OPEN().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openFilterQuery Dialog-Frame 
PROCEDURE openFilterQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    ASSIGN ls-queryString = generateFilterQuery().
                       
    h_query:QUERY-PREPARE (ls-queryString).
    h_query:QUERY-OPEN().

    bt-prev:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
    
    li-pageCount = 0.
    
    RUN nextPageFilter (h_query, h_buffer).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openSearchQuery Dialog-Frame 
PROCEDURE openSearchQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN ls-queryString = generateSearchQuery().
                  
    h_ttquery:QUERY-PREPARE (ls-queryString).
    h_ttquery:QUERY-OPEN().
    
    bt-prev:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
    
    li-pageCount = 0.
    
    RUN nextPage(h_ttquery, h_ttbuffer).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalcFields Dialog-Frame 
PROCEDURE pCalcFields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphQuery  AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER iphBuffer AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCalcParam   AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx          AS INTEGER   NO-UNDO.

    DO idx = 1 TO h_brbuffer:NUM-FIELDS:
        /* if a calculated field, run calculation */
        IF VALID-HANDLE(hCalcColumn[idx]) THEN DO:
            FIND FIRST dynValueColumn NO-LOCK
                 WHERE ROWID(dynValueColumn) EQ rDynValueColumn[idx]
                 NO-ERROR.
            IF NOT AVAILABLE dynValueColumn THEN NEXT.
            cCalcParam = "".
            DO jdx = 1 TO NUM-ENTRIES(dynValueColumn.calcParam,"|"):
                cCalcParam[1] = ENTRY(jdx,dynValueColumn.calcParam,"|").

                IF NUM-ENTRIES(cCalcParam[1],".") GT 1 THEN
                cCalcParam[2] = cCalcParam[2] + h_brbuffer:NAME + "." + REPLACE(cCalcParam[1], ".", "&") + "|".
                ELSE
                cCalcParam[2] = cCalcParam[2] + cCalcParam[1] + "|".
            END.
            cCalcParam[2] = TRIM(cCalcParam[2],"|").

            RUN spDynCalcField IN hDynCalcField (
                iphQuery:HANDLE,
                dynValueColumn.calcProc,
                cCalcParam[2],
                dynValueColumn.dataType,
                dynValueColumn.colFormat,
                OUTPUT cBufferValue
                ).
            iphBuffer:BUFFER-FIELD(dynValueColumn.colName):BUFFER-VALUE = cBufferValue.
        END. /* if valid-handle */
    END. /* do idx */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pClose Dialog-Frame
PROCEDURE pClose:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iBuffer AS INTEGER NO-UNDO.
    
    IF VALID-HANDLE(h_query) THEN DO:
        IF h_query:IS-OPEN THEN
        h_query:QUERY-CLOSE().
        DELETE OBJECT h_query.
    END.    
    
    IF VALID-HANDLE(h_brquery) THEN DO:
        IF h_brquery:IS-OPEN THEN
        h_brquery:QUERY-CLOSE().
        DELETE OBJECT h_brquery.
    END.    

    IF VALID-HANDLE(h_ttquery) THEN DO:
        IF h_ttquery:IS-OPEN THEN
        h_ttquery:QUERY-CLOSE().
        DELETE OBJECT h_ttquery.
    END.    

    IF VALID-HANDLE(h_tt) THEN
    DELETE OBJECT h_tt.
    
    IF VALID-HANDLE(h_brtt) THEN
    DELETE OBJECT h_brtt.
    
    IF VALID-HANDLE(hDynCalcField) THEN
    DELETE OBJECT hDynCalcField.

    DO iBuffer = 1 TO EXTENT(h_buffer):
        IF VALID-HANDLE(h_buffer[iBuffer]) THEN
            DELETE OBJECT h_buffer[iBuffer].
    END.    
      
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prevPage Dialog-Frame 
PROCEDURE prevPage PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER h_ipquery  AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER h_ipbuffer AS HANDLE NO-UNDO.

    DEFINE VARIABLE li-count    AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iBuffer     AS INTEGER NO-UNDO.       
    DEFINE VARIABLE iFieldCount AS INTEGER NO-UNDO.
    
    h_brbuffer:EMPTY-TEMP-TABLE().
    
    ASSIGN
        iRowCount      = 0
        lRowSelectable = YES
        .
    h_ipquery:REPOSITION-BACKWARD ((li-maxBrRows * 2) - li-pageRecCount).
    DO li-count = 1 TO li-maxBrRows:
        h_ipquery:GET-NEXT().
        IF h_ipquery:QUERY-OFF-END THEN LEAVE.
         
        h_brbuffer:BUFFER-CREATE().

        DO iFieldCount = 1 TO NUM-ENTRIES(ip-fieldList):
            iBuffer = LOOKUP(ENTRY(1, ENTRY(iFieldCount, ip-fieldList), "."), ip-table-list). 
            
            h_brbuffer:BUFFER-FIELD (REPLACE(ENTRY(iFieldCount, ip-fieldList), ".", "&")):BUFFER-VALUE = h_ipbuffer:BUFFER-FIELD(REPLACE(ENTRY(iFieldCount, ip-fieldList), ".", "&")):BUFFER-VALUE.
        END.

        RUN pCalcFields (h_brquery, h_brbuffer).

        IF ll-filterOpen THEN
        h_brbuffer:BUFFER-FIELD("recid"):BUFFER-VALUE = h_ipbuffer:RECID.             

    END.
    h_brquery:QUERY-OPEN().
    
    ASSIGN
        li-pageRecCount = 0
        li-pageCount    = li-pageCount - 1
        .    
    IF li-pageCount = 1 THEN
    bt-prev:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prevPageFilter Dialog-Frame 
PROCEDURE prevPageFilter :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER h_ipquery  AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER h_ipbuffer AS HANDLE NO-UNDO EXTENT.

    DEFINE VARIABLE li-count    AS INTEGER NO-UNDO. 
    DEFINE VARIABLE iBuffer     AS INTEGER NO-UNDO.       
    DEFINE VARIABLE iFieldCount AS INTEGER NO-UNDO.
    
    h_brbuffer:EMPTY-TEMP-TABLE().
    
    ASSIGN
        iRowCount      = 0
        lRowSelectable = YES
        .
    h_ipquery:REPOSITION-BACKWARD ((li-maxBrRows * 2) - li-pageRecCount).
    DO li-count = 1 TO li-maxBrRows:
        h_ipquery:GET-NEXT().
        IF h_ipquery:QUERY-OFF-END THEN LEAVE.
         
        h_brbuffer:BUFFER-CREATE().

        DO iFieldCount = 1 TO NUM-ENTRIES(ip-fieldList):
            iBuffer = LOOKUP(ENTRY(1, ENTRY(iFieldCount, ip-fieldList), "."), ip-table-list). 
            
            h_brbuffer:BUFFER-FIELD (REPLACE(ENTRY(iFieldCount, ip-fieldList), ".", "&")):BUFFER-VALUE = h_ipbuffer[iBuffer]:BUFFER-FIELD(ENTRY(2, ENTRY(iFieldCount, ip-fieldList), ".")):BUFFER-VALUE.
        END.

        RUN pCalcFields (h_brquery, h_brbuffer).

        IF ll-filterOpen THEN
        h_brbuffer:BUFFER-FIELD("recid"):BUFFER-VALUE = h_ipbuffer[iBuffer]:RECID.             

    END.
    h_brquery:QUERY-OPEN().
    
    ASSIGN
        li-pageRecCount = 0
        li-pageCount    = li-pageCount - 1
        .    
    IF li-pageCount = 1 THEN
    bt-prev:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetBGColor Dialog-Frame 
PROCEDURE pSetBGColor :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphWidget  AS HANDLE  NO-UNDO.
    DEFINE INPUT PARAMETER ipiBGColor AS INTEGER NO-UNDO.
    
    iphWidget:BGCOLOR = ipiBGColor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resetFilterObjects Dialog-Frame 
PROCEDURE resetFilterObjects :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE h_widget AS HANDLE NO-UNDO.

    CLEAR FRAME {&FRAME-NAME} NO-PAUSE.  
    h_widget = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.
    DO WHILE VALID-HANDLE(h_widget):
        IF h_widget:TYPE = "COMBO-BOX" THEN
            h_widget:SCREEN-VALUE = "1".  
        h_widget = h_widget:NEXT-SIBLING.
    END.        
    RUN openFilterQuery.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeFilterFrame Dialog-Frame 
PROCEDURE resizeFilterFrame :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    DO idx = 1 TO EXTENT(hWidget):
        IF NOT VALID-HANDLE(hWidget[idx]) THEN LEAVE.
        hWidget[idx]:HIDDEN = ll-filterOpen.
        RUN pSetBGColor (hWidget[idx], ?).
    END.  
    IF ll-filterOpen THEN DO WITH FRAME {&FRAME-NAME}:
        HIDE {&List-1}.
        ENABLE {&List-2}.
        APPLY 'ENTRY' TO ls-search.
    END.
    ELSE DO WITH FRAME {&FRAME-NAME}:
        HIDE {&List-2}.
        ENABLE {&List-1}.
        IF NOT ll-filterFlag THEN DO:
            ll-filterFlag = TRUE.
            RUN addFilterObjects.
        END.
        IF VALID-HANDLE(h_focus) THEN
        APPLY "ENTRY":U TO h_focus.
    END.
    ll-filterOpen = NOT ll-filterOpen.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resizeWindow Dialog-Frame 
PROCEDURE resizeWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE li-width-pixels   AS INTEGER NO-UNDO.
    DEFINE VARIABLE li-add-width      AS INTEGER NO-UNDO.
  
    DO li-count = 1 TO h_browser:NUM-COLUMNS:
        li-width-pixels = li-width-pixels + 
                        h_browser:GET-BROWSE-COLUMN(li-count):WIDTH-PIXELS.
    END.

    li-add-width = li-width-pixels + (h_browser:NUM-COLUMNS * 4) + 19 - h_browser:WIDTH-PIXELS.
    IF li-width-pixels + (h_browser:NUM-COLUMNS * 4) + 19 > h_browser:WIDTH-PIXELS THEN
        ASSIGN 
            h_dialogFrame:WIDTH-PIXELS = li-width-pixels + h_browser:NUM-COLUMNS * 4 + 29
            h_browser:WIDTH-PIXELS     = li-width-pixels + h_browser:NUM-COLUMNS * 4 + 19
            bt-cancel:HANDLE:X IN FRAME {&FRAME-NAME} = bt-cancel:HANDLE:X IN FRAME {&FRAME-NAME} + li-add-width
            bt-ok:HANDLE:X IN FRAME {&FRAME-NAME} = bt-ok:HANDLE:X IN FRAME {&FRAME-NAME} + li-add-width.
    ELSE
        h_browser:FIT-LAST-COLUMN = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE toggleMatches Dialog-Frame 
PROCEDURE toggleMatches :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    ASSIGN ll-useMatches = NOT ll-useMatches.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateParameters Dialog-Frame 
PROCEDURE validateParameters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ls-fields AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cFieldTable AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cField      AS CHARACTER NO-UNDO.
    
    /* check if table input parameter is empty */
    IF ip-table-list = "" THEN DO:
        MESSAGE "DB tables name not supplied"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.
    
    /* check if table input parameter is valid */
    DO li-count = 1 TO NUM-ENTRIES(ip-table-list):
        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name = ENTRY(li-count, ip-table-list)
             NO-ERROR.
        IF NOT AVAILABLE ASI._file THEN DO:
            MESSAGE "Incorrect DB table " + ENTRY(li-count, ip-table-list) + " passed as input parameter"
                VIEW-AS ALERT-BOX.
            RETURN ERROR.
        END.
    END.
    
    /* check if field list input paramater is empty */
    IF ip-fieldList = "" THEN DO:
        MESSAGE "Field list is not supplied"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.
    
    /* check if fields supplied in the fields list are available in the table */
    ls-fields = ''.
    DO li-count = 1 TO NUM-ENTRIES(ip-fieldList):
        IF ENTRY(li-count,ip-fieldList) BEGINS "Calc" THEN NEXT.
        
        ASSIGN
            cFieldTable = ENTRY(1, ENTRY(li-count,ip-fieldList), ".")
            cField      = ENTRY(2, ENTRY(li-count,ip-fieldList), ".")
            .

        IF LOOKUP( cFieldTable, ip-table-list) EQ 0 THEN DO:
            ls-fields = ls-fields + " " + cField.
            NEXT.
        END.

        FIND FIRST ASI._file NO-LOCK
             WHERE ASI._file._file-name = cFieldTable
             NO-ERROR.
        IF NOT AVAILABLE ASI._file THEN DO:
            ls-fields = ls-fields + " " + cField.
            NEXT.        
        END.
        
        FIND FIRST ASI._field NO-LOCK
             WHERE ASI._field._Field-Name = cField
               AND ASI._field._file-recid = RECID(ASI._file) NO-ERROR.
        IF NOT AVAILABLE ASI._field THEN
            ls-fields = ls-fields + " " + ENTRY(li-count,ip-fieldList).    
    END.    
    
    IF ls-fields <> "" THEN DO:
        MESSAGE "Fields [ " + ls-fields + " ] are not available in the table " + ip-table-list
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.
    
    /* check if lookup field input parameter is empty */
    IF ip-lookupField = "" THEN DO:
        MESSAGE "Lookup field not supplied"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.    
    END.

    ASSIGN
        cFieldTable = ENTRY(1, ip-lookupField, ".")
        cField      = ENTRY(2, ip-lookupField, ".")
        .
    
    IF LOOKUP( cFieldTable, ip-table-list) EQ 0 THEN DO:
        MESSAGE "Lookup field [ " + ip-lookupField + " ] is not available in the table " + ip-table-list
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.

    FIND FIRST ASI._file NO-LOCK
         WHERE ASI._file._file-name = cFieldTable
         NO-ERROR.
    IF NOT AVAILABLE ASI._file THEN DO:
        MESSAGE "Lookup field's [ " + ip-lookupField + " ] table is not valid "
            VIEW-AS ALERT-BOX.
        RETURN ERROR.      
    END.
    
    FIND FIRST ASI._field NO-LOCK
         WHERE ASI._field._Field-Name = cField
           AND ASI._field._file-recid = RECID(ASI._file) NO-ERROR.
    IF NOT AVAILABLE ASI._field THEN DO:
        MESSAGE "Lookup field [ " + ip-lookupField + " ] is not available in the table " + ip-table-list
            VIEW-AS ALERT-BOX.
        RETURN ERROR.           
    END.

    /* check if display list input paramater is empty */
    IF ip-fieldList = "" THEN DO:
        MESSAGE "Display field list not supplied"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.

    /* check if display fields are available in the field list */
    DO li-count = 1 TO NUM-ENTRIES(ip-displayList):
        IF INDEX(ip-fieldList, ENTRY(li-count,ip-displayList)) = 0 THEN
            ls-fields = ls-fields + " " + ENTRY(li-count,ip-displayList).
    END.

    IF ls-fields <> "" THEN DO:
        MESSAGE "Display fields [ " + ls-fields + " ] are not availablle in the field list"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.
    
    /* check if sort list input paramater is empty */
    IF ip-sortList = "" THEN DO:
        MESSAGE "Sort field list not supplied"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.
    
    /* check if sort list fields are available in the display list */
    DO li-count = 1 TO NUM-ENTRIES(ip-sortList):
        IF INDEX(ip-displayList, ENTRY(1,ENTRY(li-count,ip-sortList),"|")) = 0 THEN
            ls-fields = ls-fields + " " + ENTRY(li-count,ip-sortList).
    END.

    IF ls-fields <> "" THEN DO:
        MESSAGE "Sort fields [ " + ls-fields + " ] are not availablle in the display list"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.

    /* check if filter fields are available in the display list */
    DO li-count = 1 TO NUM-ENTRIES(ip-filterList):
        IF INDEX(ip-displayList, ENTRY(li-count,ip-filterList)) = 0 THEN
            ls-fields = ls-fields + " " + ENTRY(li-count,ip-filterList).
    END.

    IF ls-fields <> "" THEN DO:
        MESSAGE "Filter fields [ " + ls-fields + " ] are not availablle in the display list"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validateRecordLimit Dialog-Frame 
PROCEDURE validateRecordLimit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER ip-filterFirst   AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE h_lquery        AS HANDLE NO-UNDO.    

    DEFINE VARIABLE ls-lqueryString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBufferCount    AS INTEGER   NO-UNDO.

    DEFINE VARIABLE cLocalCustListQuery  AS CHARACTER NO-UNDO.

    IF ip-recLimit LE 0 THEN DO:
        ip-filterFirst = TRUE.
        RETURN.
    END.

    ASSIGN
        cLocalCustListQuery = cCustListQuery
        cLocalCustListQuery = REPLACE(cLocalCustListQuery, "&1.", "")
        cLocalCustListQuery = REPLACE(cLocalCustListQuery, "&", ".")
        .

    ls-lqueryString = ip-queryString
                    + cLocalCustListQuery.

    CREATE QUERY h_lquery.
    
    DO iBufferCount = 1 TO EXTENT(h_buffer):
        h_lquery:ADD-BUFFER(h_buffer[iBufferCount]).         
    END.

    IF lUseCustList THEN
        h_lquery:ADD-BUFFER(BUFFER ttCustList:HANDLE).
           
    h_lquery:QUERY-PREPARE(ls-lqueryString).
    h_lquery:QUERY-OPEN().
 
    h_lquery:GET-FIRST().
    REPEAT:
        IF h_lquery:QUERY-OFF-END THEN
            LEAVE.
        iCount = iCount + 1.
        
        IF iCount GE ip-recLimit THEN DO:
            ip-filterFirst = TRUE.
            LEAVE.
        END.
        h_lquery:GET-NEXT().
    END.

    DELETE OBJECT h_lquery.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION generateFilterQuery Dialog-Frame 
FUNCTION generateFilterQuery RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE h_widget  AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE ls-QueryString       AS CHARACTER NO-UNDO EXTENT.
    DEFINE VARIABLE ls-returnQueryString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-datatypeString    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ld-dateValidation    AS DATE      NO-UNDO.
    DEFINE VARIABLE ls-comboboxValue     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iBuffer              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cLocalCustListQuery  AS CHARACTER NO-UNDO.
    
    h_widget = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.

    EXTENT(ls-QueryString) = EXTENT(h_buffer).
    
    DO WHILE VALID-HANDLE(h_widget):
        ls-datatypeString = "".

        IF h_widget:TYPE = "FILL-IN"  AND 
           h_widget:SCREEN-VALUE <> "" AND
           h_widget:DATA-TYPE <> "DATE" THEN DO:
            IF h_widget:DATA-TYPE = "CHARACTER" THEN
                ls-datatypeString = IF ll-useMatches THEN 
                                     "MATCHES" + " " + "'*" + h_widget:SCREEN-VALUE + "*'"
                                     ELSE 
                                     "BEGINS" + " " + "'" + h_widget:SCREEN-VALUE + "'".
            ELSE IF h_widget:DATA-TYPE = "INTEGER" THEN
                ls-datatypeString = "=" + " " + "INTEGER" + "('" + h_widget:SCREEN-VALUE + "')".
            ELSE IF h_widget:DATA-TYPE = "DECIMAL" THEN
                ls-datatypeString = "=" + " " + "DECIMAL" + "('" + h_widget:SCREEN-VALUE + "')".

            iBuffer = LOOKUP(ENTRY(1, h_widget:PRIVATE-DATA, "&"), ip-table-list). 
            
            ASSIGN
              ls-QueryString[iBuffer] = ls-QueryString[iBuffer] + " " + 
                              (IF ls-QueryString[iBuffer] = "" AND NOT lContainsWhere[iBuffer] THEN "WHERE" ELSE "AND") + " " +
                              REPLACE(h_widget:PRIVATE-DATA, "&", ".") + " " +
                              ls-datatypeString + " ".  
        END.
        ELSE IF h_widget:TYPE = "FILL-IN" AND h_widget:DATA-TYPE = "DATE" THEN DO:
            IF h_widget:SCREEN-VALUE = "" THEN
                ls-datatypeString = "=" + " " + "?".
            ELSE DO:                
                ld-dateValidation = DATE(h_widget:SCREEN-VALUE) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR AND ld-dateValidation <> ? THEN
                   ls-datatypeString = "=" + " " + "DATE" + "(" + h_widget:SCREEN-VALUE + ")".
                ELSE DO:
                   h_widget = h_widget:NEXT-SIBLING.
                   NEXT.
                END.                                
            END.
            
            iBuffer = LOOKUP(ENTRY(1, h_widget:PRIVATE-DATA, "&"), ip-table-list). 
            
            ASSIGN
                ls-QueryString[iBuffer] = ls-QueryString[iBuffer] + " " + 
                                      (IF ls-QueryString[iBuffer] = "" AND NOT lContainsWhere[iBuffer] THEN "WHERE" ELSE "AND") + " " +
                                      REPLACE(h_widget:PRIVATE-DATA, "&", ".") + " " +
                                      ls-datatypeString + " ".  
        
        END.
        ELSE IF h_widget:TYPE = "COMBO-BOX" AND h_widget:SCREEN-VALUE <> "1" THEN DO:
            iBuffer = LOOKUP(ENTRY(1, h_widget:PRIVATE-DATA, "&"), ip-table-list). 
            
            ASSIGN 
                ls-comboboxValue     = h_widget:SCREEN-VALUE
                ls-QueryString[iBuffer] = ls-QueryString[iBuffer] + " " + 
                                       (IF ls-QueryString[iBuffer] = "" AND NOT lContainsWhere[iBuffer] THEN "WHERE" ELSE "AND") + " " +
                                       REPLACE(h_widget:PRIVATE-DATA, "&", ".") + " " +
                                       "=" + " " + 
                                       (IF ls-comboboxValue = "2" THEN "TRUE" ELSE "FALSE")
                                       + " ".
        END.
        h_widget = h_widget:NEXT-SIBLING.
    END.
    
    DO iBuffer = 1 TO EXTENT(h_buffer):
        ls-returnQueryString = ls-returnQueryString 
                             + ENTRY(iBuffer, ip-queryString) + " " 
                             + ls-QueryString[iBuffer] + ",".
    END.
    
    ls-returnQueryString = TRIM(ls-returnQueryString, ",").
    
    ASSIGN
        cLocalCustListQuery = cCustListQuery
        cLocalCustListQuery = REPLACE(cLocalCustListQuery, "&1.", "")
        cLocalCustListQuery = REPLACE(cLocalCustListQuery, "&", ".")
        .
    
    ls-returnQueryString = ls-returnQueryString 
                         + cLocalCustListQuery 
                         + " BY" + " " + REPLACE(ls-sortBy, "&", ".") + " "
                         + ls-sortType
                         .

    RETURN ls-returnQueryString.
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION generateSearchQuery Dialog-Frame 
FUNCTION generateSearchQuery RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ls-returnQueryString  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-searchValue        AS CHARACTER NO-UNDO.
  
    ASSIGN ls-searchValue = getSearchValue()
           ls-returnQueryString = "FOR EACH" + " " + h_ttbuffer:NAME + " "
                                + (IF ls-searchValue = "" THEN "" ELSE "WHERE" + " "
                                + h_ttbuffer:NAME + "." + "allData MATCHES" + " "
                                + ls-searchValue) + " "
                                + REPLACE(cCustListQuery, "&1", h_ttbuffer:NAME)
                                + "BY" + " " + h_ttbuffer:NAME + "." + ls-sortBy + " "
                                + ls-sortType
                                .

    RETURN ls-returnQueryString. /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSearchValue Dialog-Frame 
FUNCTION getSearchValue RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ls-searchValue    AS CHARACTER   NO-UNDO.
  
    ls-searchValue = TRIM(REPLACE(ls-search:SCREEN-VALUE IN FRAME {&FRAME-NAME}," ", "*")).
    ls-searchValue = IF ls-searchValue = "" THEN "" ELSE "'*" + ls-searchValue + "*'".
  
    RETURN ls-searchValue.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

