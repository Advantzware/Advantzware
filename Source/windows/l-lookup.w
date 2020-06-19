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
    ip-table       :DB Table from which data is to be fetched
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
DEFINE INPUT  PARAMETER ip-table        AS CHARACTER NO-UNDO.
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
 
DEFINE VARIABLE h_query            AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_ttquery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_brquery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_buffer           AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_ttbuffer         AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_brbuffer         AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_tt               AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_brtt             AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_browser          AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_dialogFrame      AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_filterFrame      AS HANDLE    NO-UNDO.
DEFINE VARIABLE h_firstFilterField AS HANDLE    NO-UNDO. 
DEFINE VARIABLE li-count           AS INTEGER   NO-UNDO.
DEFINE VARIABLE li-maxBrRows       AS INTEGER   NO-UNDO INITIAL 30.
DEFINE VARIABLE li-pageCount       AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE li-pageRecCount    AS INTEGER   NO-UNDO INITIAL 30.
DEFINE VARIABLE ls-sortBy          AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-sortType        AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-queryString     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ll-filterOpen      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE ll-filterFirst     AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE ll-filterFlag      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE ll-ttLoaded        AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE ll-useMatches      AS LOGICAL   NO-UNDO INITIAL FALSE.
DEFINE VARIABLE ll-continue        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rDynValueColumn    AS ROWID     NO-UNDO EXTENT 1000.
DEFINE VARIABLE hCalcColumn        AS HANDLE    NO-UNDO EXTENT 1000.
DEFINE VARIABLE hDynCalcField      AS HANDLE    NO-UNDO.

RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.

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
&Scoped-Define ENABLED-OBJECTS bt-cancel bt-clear ls-search br-table ~
bt-next bt-ok bt-prev 
&Scoped-Define DISPLAYED-OBJECTS ls-search 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

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
     IMAGE-UP FILE "Graphics/32x32/navigate_cross.ico":U NO-FOCUS
     LABEL "Cancel" 
     SIZE 8 BY 1.91 TOOLTIP "Cancel"
     BGCOLOR 8 .

DEFINE BUTTON bt-clear 
     IMAGE-UP FILE "Graphics/32x32/undo_32.ico":U
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset".

DEFINE BUTTON bt-next 
     IMAGE-UP FILE "Graphics/32x32/navigate_down2.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_down2_disabled.ico":U NO-FOCUS
     LABEL "&Next" 
     SIZE 8 BY 1.91 TOOLTIP "Page Down"
     BGCOLOR 8 .

DEFINE BUTTON bt-ok AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/navigate_check.ico":U NO-FOCUS
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "OK"
     BGCOLOR 8 .

DEFINE BUTTON bt-prev 
     IMAGE-UP FILE "Graphics/32x32/navigate_up2.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/navigate_up2_disabled.ico":U NO-FOCUS
     LABEL "&Prev" 
     SIZE 8 BY 1.91 TOOLTIP "Page Up"
     BGCOLOR 8 .

DEFINE VARIABLE ls-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search" 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1.14 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 56 BY 1.91.

DEFINE BUTTON bt-filter 
     IMAGE-UP FILE "Graphics/32x32/filter_and_sort.ico":U
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Toggle Column Filters".


/* Browse definitions                                                   */
DEFINE BROWSE br-table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-table Dialog-Frame _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 112 BY 25.29 ROW-HEIGHT-CHARS .62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     bt-cancel AT ROW 1.24 COL 104 WIDGET-ID 22
     bt-clear AT ROW 1.24 COL 11 WIDGET-ID 24
     ls-search AT ROW 1.62 COL 27 COLON-ALIGNED WIDGET-ID 32
     br-table AT ROW 4.52 COL 1 WIDGET-ID 200
     bt-next AT ROW 1.24 COL 86 WIDGET-ID 26
     bt-ok AT ROW 1.24 COL 95 WIDGET-ID 28
     bt-prev AT ROW 1.24 COL 77 WIDGET-ID 30
     RECT-1 AT ROW 1.24 COL 20 WIDGET-ID 20
     SPACE(37.00) SKIP(26.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         BGCOLOR 15 FGCOLOR 1 
         TITLE BGCOLOR 15 FGCOLOR 1 "Help Information" WIDGET-ID 100.

DEFINE FRAME filter-frame
     bt-filter AT ROW 1.24 COL 2 WIDGET-ID 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 1
         SIZE 10 BY 2.38 WIDGET-ID 300.


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
/* REPARENT FRAME */
ASSIGN FRAME filter-frame:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB br-table ls-search Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       bt-clear:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       ls-search:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME filter-frame
   UNDERLINE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Help Information */
DO:     
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
ON START-SEARCH OF br-table IN FRAME Dialog-Frame
DO:
   IF INDEX(ip-sortList, h_browser:CURRENT-COLUMN:NAME) > 0 THEN DO:
       IF ls-sortBy = h_browser:CURRENT-COLUMN:NAME THEN
            ls-sortType = IF ls-sortType = "DESCENDING" THEN "" ELSE "DESCENDING".

       ASSIGN 
          ls-sortBy = h_browser:CURRENT-COLUMN:NAME.

       IF ll-filterOpen THEN
          RUN openFilterQuery.
       ELSE
          RUN openSearchQuery.
   END.
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


&Scoped-define FRAME-NAME filter-frame
&Scoped-define SELF-NAME bt-filter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-filter Dialog-Frame
ON CHOOSE OF bt-filter IN FRAME filter-frame
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


&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define SELF-NAME bt-next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-next Dialog-Frame
ON CHOOSE OF bt-next IN FRAME Dialog-Frame /* Next */
DO:
    bt-prev:HANDLE:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
    IF ll-filterOpen THEN
        RUN nextPage(INPUT h_query,
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
       DO li-count = 1 TO NUM-ENTRIES(ip-outList):
           ASSIGN
               h_lfield        = h_brbuffer:BUFFER-FIELD(ENTRY(li-count,ip-outList)):HANDLE
               op-returnFields = op-returnFields + h_lfield:NAME + "|"
                               + (IF h_lfield:DATA-TYPE EQ "DATE" THEN
                                     (IF h_lfield:BUFFER-VALUE EQ ? THEN "" 
                                      ELSE h_lfield:BUFFER-VALUE)
                                  ELSE h_lfield:BUFFER-VALUE)
                               + "|"
                               .
       END.
       ASSIGN
          op-lookupField = h_brbuffer:BUFFER-FIELD(ip-lookupField):BUFFER-VALUE
          op-recVal      = h_brbuffer:BUFFER-FIELD("recid"):BUFFER-VALUE
          .
    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bt-prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bt-prev Dialog-Frame
ON CHOOSE OF bt-prev IN FRAME Dialog-Frame /* Prev */
DO: 
    bt-next:HANDLE:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.
    IF ll-filterOpen THEN
        RUN prevPage(INPUT h_query,
                     INPUT h_buffer).    
    ELSE
        RUN prevPage(INPUT h_ttquery,
                     INPUT h_ttbuffer).
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
    IF ip-subjectID NE 0 THEN DO:
    END. /* if subject id ne 0 */
    RUN validateParameters NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        RETURN ERROR.
    END.
    RUN init.
    RUN enable_UI.    
    RUN resizeWindow.    
    IF ll-filterFirst THEN DO:
        RUN resizeFilterFrame.        
        RUN openFilterQuery.    
    END.    
    ELSE DO:
        RUN buildTempTable.
        RUN openSearchQuery.
        APPLY 'ENTRY' TO ls-search IN FRAME {&FRAME-NAME}.
    END.  
    ASSIGN 
        bt-clear:HANDLE:SENSITIVE IN FRAME {&FRAME-NAME}  = TRUE
        ls-search:HANDLE:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
        bt-prev:HANDLE:SENSITIVE IN FRAME {&FRAME-NAME}   = FALSE
        .  
    RUN customizeBrowse.  
    WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
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
        h_browser:ADD-LIKE-COLUMN(h_brbuffer:NAME + "." + ENTRY(li-count, ip-displayList)).
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
        IF ip-SubjectID NE 0 THEN DO:
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
    DEFINE VARIABLE h_field        AS HANDLE    NO-UNDO.
    DEFINE VARIABLE h_fillin       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE h_combobox     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE h_togglebox    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE h_browseCol    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE h_calendar     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE h_btnClear     AS HANDLE    NO-UNDO.
    DEFINE VARIABLE h_btnOK        AS HANDLE    NO-UNDO.      
    DEFINE VARIABLE h_colNum       AS INTEGER   NO-UNDO.
    
    IF ip-filterList <> "" THEN DO:
        DO li-count = 1 TO NUM-ENTRIES(ip-filterList):
            ASSIGN 
                h_field = h_ttbuffer:BUFFER-FIELD(ENTRY(li-count,ip-filterList))
                h_colNum = LOOKUP(ENTRY(li-count,ip-filterList), ip-displayList)
                h_browseCol = h_browser:GET-BROWSE-COL(h_colNum):HANDLE.
                
            IF VALID-HANDLE(h_field) THEN DO:
                IF h_field:DATA-TYPE = "CHARACTER" OR
                   h_field:DATA-TYPE = "INTEGER"   OR
                   h_field:DATA-TYPE = "DECIMAL"   OR
                   h_field:DATA-TYPE = "DATE" THEN DO:

                   CREATE FILL-IN h_fillin
                   ASSIGN FRAME    = h_filterFrame
                       ROW          = h_browser:ROW - 1.26
                       X            = h_browseCol:X
                       SENSITIVE    = TRUE
                       WIDTH-PIXELS = h_browseCol:WIDTH-PIXELS + 4
                       DATA-TYPE    = h_field:DATA-TYPE
                       FORMAT       = h_field:FORMAT
                       VISIBLE      = TRUE
                       SCREEN-VALUE = ""
                       PRIVATE-DATA = h_field:NAME.

                   IF h_field:DATA-TYPE = "DATE" THEN DO:
                       h_fillin:WIDTH-CHARS = 16.
                      
                       CREATE BUTTON h_calendar
                       ASSIGN FRAME    = h_filterFrame                          
                           ROW          = h_browser:ROW - 1.28
                           COLUMN       = h_fillin:COLUMN + h_fillin:WIDTH
                           WIDTH        = 4.6
                           HEIGHT       = h_fillin:HEIGHT                          
                           SENSITIVE    = TRUE
                           VISIBLE      = TRUE
                           PRIVATE-DATA = h_field:NAME
                           TRIGGERS:
                               ON CHOOSE PERSISTENT RUN chooseDate IN THIS-PROCEDURE ( INPUT h_calendar:PRIVATE-DATA).
                           END TRIGGERS.            
                           
                       h_calendar:LOAD-IMAGE-UP("Graphics/16x16/calendar.bmp").          
                   END.
                   
                   IF li-count = 1 THEN
                       ASSIGN h_firstFilterField = h_fillin.
                END.
                ELSE IF h_field:DATA-TYPE = "LOGICAL" THEN DO:
                   CREATE COMBO-BOX h_combobox
                   ASSIGN FRAME       = h_filterFrame
                       ROW             = h_browser:ROW - 1.26
                       X               = h_browseCol:X
                       WIDTH-PIXELS    = h_browseCol:WIDTH-PIXELS + 4
                       SENSITIVE       = TRUE
                       VISIBLE         = TRUE
                       INNER-LINES     = 3
                       LIST-ITEM-PAIRS = "All,1" + "," + 
                                         ENTRY(1,h_field:FORMAT,"/") + "," + "2" + "," +
                                         ENTRY(2,h_field:FORMAT,"/") + "," + "3"
                       PRIVATE-DATA    = h_field:NAME
                       SCREEN-VALUE    = "1"
                       TRIGGERS:
                           ON VALUE-CHANGED PERSISTENT RUN openFilterQuery IN THIS-PROCEDURE.
                       END TRIGGERS
                       .                      
                END.
            END.
        END.
        
        CREATE TOGGLE-BOX h_togglebox
        ASSIGN FRAME = h_filterFrame
            LABEL     = "Use Wildcards"
            Y         = bt-filter:Y IN FRAME filter-frame + 10
            COLUMN    = 46
            SENSITIVE = TRUE
            VISIBLE   = TRUE
            TRIGGERS:
                ON VALUE-CHANGED PERSISTENT RUN toggleMatches IN THIS-PROCEDURE.
            END TRIGGERS.
           
        CREATE BUTTON h_btnClear
        ASSIGN FRAME = h_filterFrame
            LABEL     = "Reset"
            ROW       = bt-filter:ROW IN FRAME filter-frame
            COLUMN    = 24
            WIDTH     = bt-cancel:WIDTH IN FRAME {&FRAME-NAME}
            HEIGHT    = bt-cancel:HEIGHT IN FRAME {&FRAME-NAME}
            SENSITIVE = TRUE
            VISIBLE   = TRUE
            TRIGGERS:
               ON CHOOSE PERSISTENT RUN resetFilterObjects IN THIS-PROCEDURE.
            END TRIGGERS.
        h_btnClear:LOAD-IMAGE("Graphics/32x32/undo_32.ico").

        CREATE BUTTON h_btnOK
        ASSIGN FRAME = h_filterFrame
           LABEL     = "Find"
           ROW       = bt-filter:ROW IN FRAME filter-frame
           COLUMN    = 11
           WIDTH     = bt-ok:WIDTH IN FRAME {&FRAME-NAME}
           HEIGHT    = bt-ok:HEIGHT IN FRAME {&FRAME-NAME}
           SENSITIVE = TRUE
           VISIBLE   = TRUE
           TRIGGERS:
              ON CHOOSE PERSISTENT RUN openFilterQuery IN THIS-PROCEDURE.
           END TRIGGERS.
        h_btnOK:LOAD-IMAGE("Graphics/32x32/magnifying_glass.ico").

        bt-ok:HANDLE:MOVE-TO-TOP().
        bt-cancel:HANDLE:MOVE-TO-TOP().
        bt-prev:HANDLE:MOVE-TO-TOP().
        bt-next:HANDLE:MOVE-TO-TOP().
 
    END.
    CLEAR FRAME filter-frame NO-PAUSE.
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
            ls-sortBy   = ENTRY(1,ENTRY(1,ip-sortList),"|")
            ls-sortType = IF NUM-ENTRIES(ENTRY(1,ip-sortList),"|") EQ 2 THEN 
                              "DESCENDING"
                          ELSE
                              "".

    h_brquery:QUERY-PREPARE("FOR EACH" + " " + h_brbuffer:NAME + " " + "NO-LOCK").
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
  
    ls-queryString = "FOR EACH " + h_buffer:NAME + " NO-LOCK" + " "
                   + IF ip-queryString EQ "" THEN "" 
                     ELSE "WHERE" + " " + ip-queryString. 
    h_query:QUERY-PREPARE (ls-queryString).
    h_query:QUERY-OPEN().        
    h_query:GET-FIRST().
    IF NOT h_buffer:AVAILABLE THEN
    MESSAGE "No records found for table" h_buffer:NAME VIEW-AS ALERT-BOX.    
    SESSION:SET-WAIT-STATE("GENERAL").
    REPEAT:
        IF h_query:QUERY-OFF-END THEN LEAVE.
        ls-allData = "".
        h_ttbuffer:BUFFER-CREATE().
        h_ttbuffer:BUFFER-COPY(h_buffer).        
        DO li-count = 1 TO NUM-ENTRIES(ip-displayList):
            h_field = h_buffer:BUFFER-FIELD(ENTRY(li-count, ip-displayList)):HANDLE NO-ERROR.
            IF VALID-HANDLE(h_field) THEN DO:
                IF h_field:DATA-TYPE = "LOGICAL" THEN 
                ls-allData = ls-allData + STRING(h_field:BUFFER-VALUE, h_field:FORMAT) + "|".
                ELSE IF h_field:DATA-TYPE = "DATE" THEN 
                ls-allData = ls-allData + STRING((IF h_field:BUFFER-VALUE = ? THEN '' ELSE h_field:BUFFER-VALUE), h_field:FORMAT) + "|".
                ELSE 
                ls-allData = ls-allData + STRING(h_field:BUFFER-VALUE) + "|".
            END.
/*            RUN pCalcFields (h_ttbuffer).*/
        END.        
        ASSIGN
            h_ttbuffer:BUFFER-FIELD("allData"):BUFFER-VALUE = ls-allData
            h_ttbuffer:BUFFER-FIELD("recid"):BUFFER-VALUE   = h_buffer:RECID
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
        h_widget = h_filterFrame:FIRST-CHILD:FIRST-CHILD.
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
    
    CREATE TEMP-TABLE h_tt.
    CREATE TEMP-TABLE h_brtt.
    
    DO li-count = 1 TO NUM-ENTRIES(ip-fieldList):
        h_field = h_buffer:BUFFER-FIELD(ENTRY(li-count,ip-fieldList)) NO-ERROR.
        
        IF VALID-HANDLE(h_field) THEN DO:
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
            
            ll-success = h_tt:ADD-NEW-FIELD(h_field:NAME,
                                            h_field:DATA-TYPE,
                                            h_field:EXTENT,
                                            ls-format,
                                            h_field:DEFAULT-VALUE,
                                            ls-label) NO-ERROR.
    
            /* use default field format in case supplied format is incorrect */
            IF NOT ll-success THEN
                h_tt:ADD-NEW-FIELD(h_field:NAME,
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

    h_brbuffer = h_brtt:DEFAULT-BUFFER-HANDLE.
    h_brquery:SET-BUFFERS(h_brbuffer).

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
  HIDE FRAME filter-frame.
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
  DISPLAY ls-search 
      WITH FRAME Dialog-Frame.
  ENABLE bt-cancel bt-clear ls-search bt-next bt-ok bt-prev 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  ENABLE bt-filter 
      WITH FRAME filter-frame.
  {&OPEN-BROWSERS-IN-QUERY-filter-frame}
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
    
    CREATE BUFFER h_buffer FOR TABLE ip-table.

    CREATE QUERY h_query.
    h_query:SET-BUFFERS(h_buffer).

    CREATE QUERY h_ttquery.
    
    CREATE QUERY h_brquery.
     
    h_browser = br-table:HANDLE IN FRAME {&FRAME-NAME}.
    
    h_dialogFrame = FRAME {&FRAME-NAME}:HANDLE.
    h_dialogFrame:TITLE = ip-title.

    h_filterFrame = FRAME filter-frame:HANDLE.
    
    RUN validateRecordLimit(OUTPUT ll-filterFirst).
    
    RUN createTempTables.
    RUN attachQuery.
    RUN addBrowseCols.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextPage Dialog-Frame 
PROCEDURE nextPage :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER h_ipquery  AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER h_ipbuffer AS HANDLE NO-UNDO.
        
    h_brbuffer:EMPTY-TEMP-TABLE().

    ASSIGN
        li-pageCount    = li-pageCount + 1
        li-pageRecCount = li-maxBrRows
        .    
    bt-next:HANDLE:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.   
    DO li-count = 1 TO li-maxBrRows:
        h_ipquery:GET-NEXT().
        IF h_ipquery:QUERY-OFF-END THEN DO:
            bt-next:HANDLE:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.        
            LEAVE.
        END.
        li-pageRecCount = li-pageRecCount - 1.
         
        h_brbuffer:BUFFER-CREATE().
        h_brbuffer:BUFFER-COPY(h_ipbuffer).

        RUN pCalcFields (h_brbuffer).
    
        IF ll-filterOpen THEN
        h_brbuffer:BUFFER-FIELD("recid"):BUFFER-VALUE = h_ipbuffer:RECID.               
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

    bt-prev:HANDLE:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
    
    li-pageCount = 0.
    
    RUN nextPage ( INPUT h_query,
                   INPUT h_buffer).
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
    
    bt-prev:HANDLE:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
    
    li-pageCount = 0.
    
    RUN nextPage( INPUT h_ttquery,
                  INPUT h_ttbuffer).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCalcFields Dialog-Frame 
PROCEDURE pCalcFields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iphBuffer AS HANDLE NO-UNDO.
    
    DEFINE VARIABLE cBufferValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCalcParam   AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE idx          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx          AS INTEGER   NO-UNDO.

    DO idx = 1 TO h_brbuffer:NUM-FIELDS:
        IF VALID-HANDLE(hCalcColumn[idx]) THEN DO:
            FIND FIRST dynValueColumn NO-LOCK
                 WHERE ROWID(dynValueColumn) EQ rDynValueColumn[idx]
                 NO-ERROR.
            IF NOT AVAILABLE dynValueColumn THEN NEXT.
/*            cCalcParam = dynValueColumn.calcParam.*/
            DO jdx = 1 TO NUM-ENTRIES(dynValueColumn.calcParam,"|"):
                cCalcParam[1] = ENTRY(jdx,dynValueColumn.calcParam,"|").
                IF NUM-ENTRIES(cCalcParam[1],".") GT 1 THEN
                cCalcParam[2] = cCalcParam[2] + h_brbuffer:NAME + "." + ENTRY(2,cCalcParam[1],".") + "|".
                ELSE
                cCalcParam[2] = cCalcParam[2] + cCalcParam[1] + "|".
/*                ENTRY(jdx,cCalcParam,"|") = REPLACE(ENTRY(jdx,cCalcParam,"|"),ENTRY(1,ENTRY(jdx,cCalcParam,"|"),"."),h_brbuffer:NAME).*/
            END.
            cCalcParam[2] = TRIM(cCalcParam[2],"|").
            RUN spDynCalcField IN hDynCalcField (
                h_brquery:HANDLE,
                dynValueColumn.calcProc,
                cCalcParam[2],
                dynValueColumn.dataType,
                dynValueColumn.colFormat,
                OUTPUT cBufferValue
                ).
            iphBuffer:BUFFER-FIELD(dynValueColumn.colName):BUFFER-VALUE = cBufferValue.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prevPage Dialog-Frame 
PROCEDURE prevPage :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER h_ipquery  AS HANDLE NO-UNDO.
    DEFINE INPUT PARAMETER h_ipbuffer AS HANDLE NO-UNDO.
        
    h_brbuffer:EMPTY-TEMP-TABLE().
    
    h_ipquery:REPOSITION-BACKWARD ((li-maxBrRows * 2) - li-pageRecCount).
    DO li-count = 1 TO li-maxBrRows:
        h_ipquery:GET-NEXT().
        IF h_ipquery:QUERY-OFF-END THEN LEAVE.
         
        h_brbuffer:BUFFER-CREATE().
        h_brbuffer:BUFFER-COPY(h_ipbuffer).  

        RUN pCalcFields (h_brbuffer).

        IF ll-filterOpen THEN
        h_brbuffer:BUFFER-FIELD("recid"):BUFFER-VALUE = h_ipbuffer:RECID.             

    END.
    h_brquery:QUERY-OPEN().
    
    ASSIGN
        li-pageRecCount = 0
        li-pageCount    = li-pageCount - 1
        .    
    IF li-pageCount = 1 THEN
    bt-prev:HANDLE:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

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
    DEFINE VARIABLE h_widget  AS HANDLE NO-UNDO.

    CLEAR FRAME filter-frame NO-PAUSE.
  
    h_widget = h_filterFrame:FIRST-CHILD:FIRST-CHILD.
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
    IF NOT ll-filterOpen THEN DO:
        ASSIGN
            h_filterFrame:BGCOLOR        = ?
            h_filterFrame:VIRTUAL-HEIGHT = 3.24
            h_filterFrame:VIRTUAL-WIDTH  = h_browser:WIDTH
            h_filterFrame:HEIGHT = h_filterFrame:VIRTUAL-HEIGHT
            h_filterFrame:WIDTH  = h_filterFrame:VIRTUAL-WIDTH.
    
        IF NOT ll-filterFlag THEN DO:
            ll-filterFlag = TRUE.
            RUN addFilterObjects.
        END.
        
        IF VALID-HANDLE(h_firstFilterField) THEN
            APPLY 'ENTRY' TO h_firstFilterField.
    END.
    ELSE DO:
        ASSIGN
            h_filterFrame:BGCOLOR        = ?
            h_filterFrame:VIRTUAL-HEIGHT = 2.2
            h_filterFrame:VIRTUAL-WIDTH  = 9.5
            h_filterFrame:HEIGHT = h_filterFrame:VIRTUAL-HEIGHT
            h_filterFrame:WIDTH  = h_filterFrame:VIRTUAL-WIDTH.
            
        APPLY 'ENTRY' TO ls-search IN FRAME {&FRAME-NAME}.
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
            h_dialogFrame:WIDTH-PIXELS = li-width-pixels + h_browser:NUM-COLUMNS * 4 + 19 + 10
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
    
    /* check if table input parameter is empty */
    IF ip-table = "" THEN DO:
        MESSAGE "DB table name not supplied"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.
    
    /* check if table input parameter is valid */
    FIND FIRST ASI._file NO-LOCK
         WHERE ASI._file._file-name = ip-table AND
               ASI._file._Tbl-Type  = "T" NO-ERROR.
    IF NOT AVAILABLE ASI._file THEN DO:
        MESSAGE "Incorrect DB table " + ip-table + " passed as input parameter"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
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
        FIND FIRST ASI._field NO-LOCK
             WHERE ASI._field._Field-Name = ENTRY(li-count,ip-fieldList)
               AND ASI._field._file-recid = RECID(_file) NO-ERROR.
        IF NOT AVAILABLE ASI._field THEN
            ls-fields = ls-fields + " " + ENTRY(li-count,ip-fieldList).    
    END.    
    
    IF ls-fields <> "" THEN DO:
        MESSAGE "Fields [ " + ls-fields + " ] are not available in the table " + ip-table
            VIEW-AS ALERT-BOX.
        RETURN ERROR.
    END.
    
    /* check if lookup field input parameter is empty */
    IF ip-lookupField = "" THEN DO:
        MESSAGE "Lookup field not supplied"
            VIEW-AS ALERT-BOX.
        RETURN ERROR.    
    END.
    
    /* check if lookup field input parameter is available in table */
    FIND FIRST ASI._field NO-LOCK
         WHERE ASI._field._Field-Name = ip-lookupField
           AND ASI._field._file-recid = RECID(_file) NO-ERROR.
    IF NOT AVAILABLE ASI._field THEN DO:
        MESSAGE "Lookup field [ " + ip-lookupField + " ] is not available in the table " + ip-table
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
    
    IF ip-recLimit LE 0 THEN DO:
        ip-filterFirst = TRUE.
        RETURN.
    END.
    
    ls-lqueryString = "FOR EACH" + " " +
                    h_buffer:NAME + " " +
                    "NO-LOCK" + " " + 
                    IF ip-queryString = "" THEN "" ELSE "WHERE" + " " + ip-queryString. 
                    "NO-LOCK". 
    
    CREATE QUERY h_lquery.
    h_lquery:SET-BUFFERS(h_buffer).                
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
    
    DEFINE VARIABLE ls-returnQueryString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-datatypeString    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ld-dateValidation    AS DATE      NO-UNDO.
    DEFINE VARIABLE ls-comboboxValue     AS CHARACTER NO-UNDO.
    
    h_widget = h_filterFrame:FIRST-CHILD:FIRST-CHILD.

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

            ASSIGN
              ls-returnQueryString = ls-returnQueryString + " " + 
                              (IF ls-returnQueryString = "" AND ip-queryString = "" THEN "WHERE" ELSE "AND") + " " +
                              h_buffer:NAME + "." + h_widget:PRIVATE-DATA + " " +
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
            
            ASSIGN
                ls-returnQueryString = ls-returnQueryString + " " + 
                                      (IF ls-returnQueryString = "" AND ip-queryString = "" THEN "WHERE" ELSE "AND") + " " +
                                      h_buffer:NAME + "." + h_widget:PRIVATE-DATA + " " +
                                      ls-datatypeString + " ".  
        
        END.
        ELSE IF h_widget:TYPE = "COMBO-BOX" AND h_widget:SCREEN-VALUE <> "1" THEN DO:
            ASSIGN 
                ls-comboboxValue     = h_widget:SCREEN-VALUE
                ls-returnQueryString = ls-returnQueryString + " " + 
                                       (IF ls-returnQueryString = "" AND ip-queryString = "" THEN "WHERE" ELSE "AND") + " " +
                                       h_buffer:NAME + "." + h_widget:PRIVATE-DATA + " " +
                                       "=" + " " + 
                                       IF ls-comboboxValue = "2" THEN "TRUE" ELSE "FALSE".
        END.
        h_widget = h_widget:NEXT-SIBLING.
    END.
    
    ls-returnQueryString = "FOR EACH" + " " + 
                    h_buffer:NAME + " " +
                    "NO-LOCK" + " " + 
                    (IF ip-queryString = "" THEN "" ELSE "WHERE" + " " + ip-queryString) + " " +                     
                    ls-returnQueryString + " " +
                    "BY" + " " + h_buffer:NAME + "." + ls-sortBy + " " + 
                    ls-sortType.
    
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
           ls-returnQueryString = "FOR EACH" + " " + h_ttbuffer:NAME + " " +
                                  (IF ls-searchValue = "" THEN "" ELSE "WHERE" + " " +
                                  h_ttbuffer:NAME + "." + "allData MATCHES" + " " +
                                  ls-searchValue) + " " +
                                  "BY" + " " + h_ttbuffer:NAME + "." + ls-sortBy + " " +
                                  ls-sortType.

    RETURN ls-returnQueryString.   /* Function return value. */

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

