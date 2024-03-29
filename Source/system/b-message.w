&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  util/b-message.w

  Decription: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */


&SCOPED-DEFINE winReSize
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}

DEFINE BUFFER bf-zMessage FOR zMessage.

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var            AS CHARACTER NO-UNDO.

DEFINE               VARIABLE char-hdl                AS CHARACTER NO-UNDO.
DEFINE               VARIABLE columnCount             AS INTEGER   NO-UNDO.
DEFINE               VARIABLE idx                     AS INTEGER   NO-UNDO.
DEFINE               VARIABLE ilogic                  AS LOG       NO-UNDO.
DEFINE               VARIABLE iSecurityLevel          AS INTEGER   NO-UNDO.
DEFINE               VARIABLE lActive                 AS LOG       NO-UNDO.
DEFINE               VARIABLE ll-first                AS LOG       INIT YES NO-UNDO.
DEFINE               VARIABLE ll-show-all             AS LOG       NO-UNDO.
DEFINE               VARIABLE ll-sort-asc             AS LOG       INIT YES NO-UNDO.
DEFINE               VARIABLE lv-cust-no              AS CHARACTER NO-UNDO.
DEFINE               VARIABLE lv-sort-by              AS CHARACTER INIT "Message ID" NO-UNDO.
DEFINE               VARIABLE lv-sort-by-lab          AS CHARACTER INIT "Message ID" NO-UNDO.
DEFINE               VARIABLE lvFirstRowID            AS ROWID     NO-UNDO.
DEFINE               VARIABLE lvLastRowID             AS ROWID     NO-UNDO.
DEFINE               VARIABLE phandle                 AS HANDLE    NO-UNDO.
DEFINE               VARIABLE useColors               AS CHARACTER NO-UNDO.
DEFINE               VARIABLE v-called-setCellColumns AS LOG       NO-UNDO.
DEFINE               VARIABLE v-col-move              AS LOG       INIT YES NO-UNDO.

ASSIGN 
    cocode = g_company
    locode = g_loc.

FIND FIRST users NO-LOCK WHERE 
    users.user_id EQ USERID(LDBNAME(1)) 
    NO-ERROR.
IF AVAILABLE users THEN ASSIGN 
        iSecurityLevel = users.securityLevel.


&SCOPED-DEFINE key-phrase TRUE

&SCOPED-DEFINE for-each1                          ~
    FOR EACH zMessage                               ~
        WHERE {&key-phrase}                       ~
          AND zMessage.currSecLevel    LE iSecurityLevel   ~
          AND (IF fi_pro-name BEGINS '*' THEN  zMessage.msgID MATCHES (fi_pro-name + "*") ~
              ELSE zMessage.msgID BEGINS fi_pro-name) ~
          AND (IF fi_module BEGINS '*' THEN zMessage.module MATCHES (fi_module + "*") ~
               ELSE zMessage.module BEGINS fi_module) ~
          AND (zMessage.hotkey          BEGINS fi_hotkey OR fi_hotkey EQ "ALL") ~
          AND (IF fi_desc BEGINS '*'     THEN zMessage.msgName MATCHES (fi_desc + "*") ~
              ELSE zMessage.msgName BEGINS fi_desc) ~
          AND (IF fi_currMessage BEGINS '*'    THEN zMessage.currMessage MATCHES (fi_currMessage + "*") ~
              ELSE zMessage.currMessage       BEGINS fi_currMessage) ~
          AND zMessage.currSecLevel    GE  fi_seclevel 

&SCOPED-DEFINE for-eachblank ~
    FOR EACH zMessage ~
        WHERE {&key-phrase} ~
        AND zMessage.currSecLevel LE iSecurityLevel

&SCOPED-DEFINE sortby-log                                                                                                                                  ~
    IF lv-sort-by EQ "msgID"      THEN zMessage.msgID ELSE ~
    IF lv-sort-by EQ "Module"           THEN zMessage.module ELSE ~
    IF lv-sort-by EQ "hotkey"           THEN zMessage.hotkey ELSE ~
    IF lv-sort-by EQ "msgName"      THEN zMessage.msgName ELSE ~
    IF lv-sort-by EQ "currMessage"            THEN zMessage.currMessage ELSE ~
    IF lv-sort-by EQ "currSecLevel"    THEN string(zMessage.currSecLevel,"9999999") ELSE ""

&SCOPED-DEFINE sortby BY zMessage.msgID

&SCOPED-DEFINE sortby-phrase-asc ~
    BY ({&sortby-log}) ~
    {&sortby}

&SCOPED-DEFINE sortby-phrase-desc ~
    BY ({&sortby-log}) DESC ~
    {&sortby}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES zMessage

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table zMessage.msgID ~
zMessage.module zMessage.hotkey zMessage.msgName zMessage.currMessage ~
zMessage.currSecLevel 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH zMessage WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH zMessage WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table zMessage
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table zMessage


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fi_pro-name fi_module fi_hotkey ~
fi_desc fi_currMessage fi_seclevel btn_go btn_show Browser-Table 
&Scoped-Define DISPLAYED-OBJECTS fi_pro-name fi_module fi_hotkey fi_desc ~
fi_currMessage fi_seclevel fi_sort-by 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS
><EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS>
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE>
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_go 
    LABEL "&Go" 
    SIZE 12 BY 1
    FONT 6.

DEFINE BUTTON btn_show 
    LABEL "&Show All" 
    SIZE 12 BY 1
    FONT 6.

DEFINE VARIABLE fi_hotkey      AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
    VIEW-AS COMBO-BOX INNER-LINES 5
    LIST-ITEMS "All" 
    DROP-DOWN-LIST
    SIZE 10 BY 1 TOOLTIP "Select Hot Key Filter" NO-UNDO.

DEFINE VARIABLE fi_desc        AS CHARACTER FORMAT "X(48)":U 
    VIEW-AS FILL-IN 
    SIZE 35 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_module      AS CHARACTER FORMAT "X(4)":U 
    VIEW-AS FILL-IN 
    SIZE 10 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_currMessage AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 31.4 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_pro-name    AS CHARACTER FORMAT "X(32)":U 
    VIEW-AS FILL-IN 
    SIZE 31 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_seclevel    AS INTEGER   FORMAT ">>>>":U INITIAL 0 
    VIEW-AS FILL-IN 
    SIZE 9.6 BY 1
    BGCOLOR 15 NO-UNDO.

DEFINE VARIABLE fi_sort-by     AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 34.6 BY 1
    BGCOLOR 14 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
    zMessage SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
    QUERY Browser-Table NO-LOCK DISPLAY
    zMessage.msgID COLUMN-LABEL "Message ID" FORMAT "x(32)":U
    WIDTH 17.8 LABEL-BGCOLOR 14
    zMessage.module FORMAT "x(3)":U WIDTH 13 LABEL-BGCOLOR 14
    zMessage.hotkey FORMAT "x(3)":U WIDTH 13 LABEL-BGCOLOR 14
    zMessage.msgName COLUMN-LABEL "Message Name" FORMAT "x(48)":U WIDTH 37.2 LABEL-BGCOLOR 14
    zMessage.currMessage COLUMN-LABEL "Current Message" FORMAT "x(256)":U WIDTH 33.4
    LABEL-BGCOLOR 14
    zMessage.currSecLevel COLUMN-LABEL "Sec. Lvl." FORMAT ">999":U
    WIDTH 13.2 LABEL-BGCOLOR 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 136.0 BY 14.20
         FGCOLOR 1  FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
    fi_pro-name AT ROW 2.19 COL 1 COLON-ALIGNED NO-LABELS WIDGET-ID 48
    fi_module AT ROW 2.19 COL 33 COLON-ALIGNED NO-LABELS WIDGET-ID 2
    fi_hotkey AT ROW 2.19 COL 44.4 COLON-ALIGNED NO-LABELS WIDGET-ID 40
    fi_desc AT ROW 2.19 COL 55.6 COLON-ALIGNED NO-LABELS WIDGET-ID 16
    fi_currMessage AT ROW 2.19 COL 91.6 COLON-ALIGNED NO-LABELS
    fi_seclevel AT ROW 2.19 COL 124 COLON-ALIGNED NO-LABELS
    btn_go AT ROW 3.62 COL 1.8 WIDGET-ID 4
    btn_show AT ROW 3.62 COL 15.2 WIDGET-ID 10
    fi_sort-by AT ROW 3.62 COL 56 COLON-ALIGNED NO-LABELS WIDGET-ID 12
    Browser-Table AT ROW 5.52 COL 2.4 HELP
    "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
    "Module" VIEW-AS TEXT
    SIZE 11 BY .71 AT ROW 1.24 COL 35.2 WIDGET-ID 24
    FGCOLOR 9 FONT 6
    "Message Name" VIEW-AS TEXT
    SIZE 20 BY .71 AT ROW 1.24 COL 57.6 WIDGET-ID 38
    FGCOLOR 9 FONT 6
    "Current Message" VIEW-AS TEXT
    SIZE 20 BY .71 AT ROW 1.24 COL 93.6
    FGCOLOR 9 FONT 6
    "Sec. Lvl" VIEW-AS TEXT
    SIZE 10.4 BY .71 AT ROW 1.24 COL 126.2
    FGCOLOR 9 FONT 6
    "Hot Key" VIEW-AS TEXT
    SIZE 10 BY .71 AT ROW 1.24 COL 47.2 WIDGET-ID 34
    FGCOLOR 9 FONT 6
  /*  "Click on Column Title to Sort" VIEW-AS TEXT
    SIZE 28 BY .95 AT ROW 3.62 COL 94 WIDGET-ID 14 */
    "Message ID" VIEW-AS TEXT
    SIZE 18 BY .71 AT ROW 1.24 COL 3 WIDGET-ID 42
    FGCOLOR 9 FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1 SCROLLABLE 
    BGCOLOR 15 FGCOLOR 1 
    DEFAULT-BUTTON btn_go WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN 
DO:
    MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
        VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 19.52
         WIDTH              = 139.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/navbrows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table fi_sort-by F-Main */
ASSIGN 
    FRAME F-Main:SCROLLABLE = FALSE
    FRAME F-Main:HIDDEN     = TRUE.

ASSIGN 
    Browser-Table:PRIVATE-DATA IN FRAME F-Main           = "2"
    Browser-Table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE
    Browser-Table:COLUMN-RESIZABLE IN FRAME F-Main       = TRUE.

/* SETTINGS FOR FILL-IN fi_sort-by IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.zMessage"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "zMessage"
     _FldNameList[1]   > asi.zMessage.msgID
"zMessage.msgID" "Message ID" "x(32)" "character" ? ? ? 14 ? ? no ? no no "29.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.zMessage.module
"zMessage.module" ? ? "character" ? ? ? 14 ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.zMessage.hotkey
"zMessage.hotkey" ? ? "character" ? ? ? 14 ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.zMessage.msgName
"zMessage.msgName" ? ? "character" ? ? ? 14 ? ? no ? no no "35.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.zMessage.currMessage
"zMessage.currMessage" "Notes" ? "character" ? ? ? 14 ? ? no ? no no "31.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.zMessage.currSecLevel
"zMessage.currSecLevel" "Sec. Lvl." ? "integer" ? ? ? 14 ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main B-table-Win
ON HELP OF FRAME F-Main
    DO:
        DEFINE VARIABLE lv-handle AS HANDLE NO-UNDO.
        DEFINE VARIABLE char-val  AS cha    NO-UNDO.
        DEFINE VARIABLE rec-val   AS RECID  NO-UNDO. 
        DEFINE VARIABLE city-val  AS cha    NO-UNDO. 
        DEFINE VARIABLE state-val AS cha    NO-UNDO. 

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
    DO:
        DEFINE VARIABLE phandle  AS HANDLE NO-UNDO.
        DEFINE VARIABLE char-hdl AS cha    NO-UNDO.
        {methods/run_link.i "container-source" "select-page" "(2)"}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
    DO:
        /* This code displays initial values for newly added or copied rows. */
        {src/adm/template/brsentry.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
    DO:
        /* Do not disable this code or no updates will take place except
         by pressing the Save button on an Update SmartPanel. */
        {src/adm/template/brsleave.i}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON START-SEARCH OF Browser-Table IN FRAME F-Main
    DO:
		{methods/template/sortindicator.i} 
        /*RUN startSearch.*/
        DEFINE VARIABLE lh-column     AS HANDLE    NO-UNDO.
        DEFINE VARIABLE lv-column-nam AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lv-column-lab AS CHARACTER NO-UNDO.

        ASSIGN
            lh-column     = {&BROWSE-NAME}:CURRENT-COLUMN 
            lv-column-nam = lh-column:NAME
            lv-column-lab = lh-column:LABEL.

        IF lv-sort-by EQ lv-column-nam THEN ll-sort-asc = NOT ll-sort-asc.
        ELSE
            ASSIGN
                lv-sort-by     = lv-column-nam
                lv-sort-by-lab = lv-column-lab.

        APPLY 'END-SEARCH' TO {&BROWSE-NAME}.
        RUN dispatch ("open-query").
		{methods/template/sortindicatorend.i} 
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
    DO:
        /* This ADM trigger code must be preserved in order to notify other
           objects when the browser's current row changes. */
        {src/adm/template/brschnge.i}
    /*{methods/template/local/setvalue.i}*/
  
  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go B-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Go */
    DO:
  
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN
                fi_pro-name
                fi_module                                               
                fi_hotkey                                                 
                fi_desc  
                fi_currMessage
                fi_seclevel 
                ll-first = NO 
                .                                             
     
            RUN dispatch ("open-query").
    
            GET FIRST Browser-Table .
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_show
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_show B-table-Win
ON CHOOSE OF btn_show IN FRAME F-Main /* Show All */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ll-show-all = YES.
            APPLY "choose" TO btn_go.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}
{methods/ctrl-a_browser.i}
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

&SCOPED-DEFINE cellColumnDat browsers-util

{methods/browsers/setCellColumns.i}
/* Ticket# : 92946
   Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
fi_sort-by:HIDDEN  = TRUE.
fi_sort-by:VISIBLE = FALSE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item B-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
    /*------------------------------------------------------------------------------
      Purpose:     Dispatched to this procedure when the Record-
                   Source has a new row available.  This procedure
                   tries to get the new row (or foriegn keys) from
                   the Record-Source and process it.
      Parameters:  <none>
    ------------------------------------------------------------------------------*/

    /* Define variables needed by this internal procedure.             */
    {src/adm/template/row-head.i}

    /* Process the newly available records (i.e. display fields,
       open queries, and/or pass records on to any RECORD-TARGETS).    */
    {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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
    HIDE FRAME F-Main.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win 
PROCEDURE export-xl :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE first-msg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE last-msg  AS CHARACTER NO-UNDO.

    IF AVAIL zMessage THEN
     ASSIGN
        last-msg = zMessage.msgID
        first-msg = zMessage.msgID .

    RUN system/MessageExp.w(INPUT first-msg,INPUT last-msg) .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE first-query B-table-Win 
PROCEDURE first-query :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    IF ll-first THEN 
    DO:
        RUN set-defaults.
        RUN query-first.
    END.
    ELSE
        RUN query-go.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getCellColumns B-table-Win 
PROCEDURE getCellColumns :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    columnCount = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
    DO idx = 1 TO columnCount:
        cellColumn[idx] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(idx).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields B-table-Win 
PROCEDURE local-display-fields :
    /*------------------------------------------------------------------------------
          Purpose:     Override standard ADM method
          Notes:       
        ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
  
    DO WITH FRAME {&FRAME-NAME}:
        fi_sort-by:SCREEN-VALUE = TRIM(lv-sort-by-lab)               + " " +
            TRIM(STRING(ll-sort-asc,"As/Des")) + "cending".
    END.
                    
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-first B-table-Win 
PROCEDURE local-get-first :
    /*------------------------------------------------------------------------------
          Purpose:     Override standard ADM method
          Notes:       
        ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-first':U ) .

/* Code placed here will execute AFTER standard behavior.    */
/*{methods/template/local/setvalue.i}*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-last B-table-Win 
PROCEDURE local-get-last :
    /*------------------------------------------------------------------------------
          Purpose:     Override standard ADM method
          Notes:       
        ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-last':U ) .

/* Code placed here will execute AFTER standard behavior.    */
/*{methods/template/local/setvalue.i}*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-next B-table-Win 
PROCEDURE local-get-next :
    /*------------------------------------------------------------------------------
          Purpose:     Override standard ADM method
          Notes:       
        ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-next':U ) .

/* Code placed here will execute AFTER standard behavior.    */
/* {methods/template/local/setvalue.i}*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-get-prev B-table-Win 
PROCEDURE local-get-prev :
    /*------------------------------------------------------------------------------
          Purpose:     Override standard ADM method
          Notes:       
        ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-prev':U ) .

/* Code placed here will execute AFTER standard behavior.    */
/*{methods/template/local/setvalue.i}*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
    /*------------------------------------------------------------------------------
          Purpose:     Override standard ADM method
          Notes:       
        ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
    RUN setCellColumns.
    /* Code placed here will execute AFTER standard behavior.    */

    fi_hotkey:LIST-ITEMS IN FRAME {&FRAME-NAME} = "All".
    FOR EACH bf-zMessage WHERE zMessage.currSecLevel LE iSecurityLevel :
        ilogic = fi_hotkey:ADD-LAST (bf-zMessage.hotkey) IN FRAME {&frame-name}.
    END. /* each bf-zMessage */
        
    fi_hotkey:SCREEN-VALUE = fi_hotkey:ENTRY(1) .

    APPLY 'ENTRY':U TO fi_pro-name IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
    /*------------------------------------------------------------------------------
          Purpose:     Override standard ADM method
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li AS INTEGER NO-UNDO.

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

    /* Code placed here will execute AFTER standard behavior.    */

    IF ll-show-all THEN 
    DO:
        RUN set-defaults.
    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
           {&for-eachblank} NO-LOCK
            
        IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.
    END.
    ELSE RUN first-query.

    IF AVAILABLE {&first-table-in-query-{&browse-name}} THEN 
    DO:
        RUN dispatch ("display-fields").
        RUN dispatch ("row-changed").

    END.

    /*RUN set-rec_key.*/
    ll-show-all = NO .
  
    APPLY "value-changed" TO BROWSE {&browse-name}.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
    /*------------------------------------------------------------------------------
          Purpose:     Override standard ADM method
          Notes:       
        ------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    IF AVAILABLE zMessage THEN APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE move-columns B-table-Win 
PROCEDURE move-columns :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
            Browser-Table:COLUMN-MOVABLE   = v-col-move
            Browser-Table:COLUMN-RESIZABLE = v-col-move
            v-col-move                     = NOT v-col-move.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE navigate-browser B-table-Win 
PROCEDURE navigate-browser :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipNavType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opNavType AS CHARACTER NO-UNDO.
 
    IF ipNavType NE '' THEN
        CASE ipNavType:
            WHEN 'F' THEN RUN dispatch ('get-first':U).
            WHEN 'L' THEN RUN dispatch ('get-last':U).
            WHEN 'N' THEN RUN dispatch ('get-next':U).
            WHEN 'P' THEN RUN dispatch ('get-prev':U).
            WHEN 'G' THEN RUN lookup-eb.
        END CASE.
    
    IF ROWID(zMessage) EQ lvLastRowID THEN
        opNavType = 'L'.
      
    IF ROWID(zMessage) EQ lvFirstRowID THEN
        opNavType = IF opNavType EQ 'L' THEN 'B' ELSE 'F'.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE query-first B-table-Win 
PROCEDURE query-first :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li AS INTEGER NO-UNDO.
  
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                      NO-LOCK
            
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE query-go B-table-Win 
PROCEDURE query-go :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li AS INTEGER NO-UNDO.
  
     
     &SCOPED-DEFINE open-query                   ~
          OPEN QUERY {&browse-name}               ~
            {&for-each1}                          ~
               NO-LOCK

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.
 
    RUN dispatch IN this-procedure ("open-query").
  
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

    RUN dispatch IN this-procedure ("row-changed").
    APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query2 B-table-Win 
PROCEDURE repo-query2 :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

    DEFINE VARIABLE li AS INTEGER NO-UNDO.



    RUN set-defaults.
    
    REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

    RUN dispatch IN this-procedure ("row-changed").
 
    APPLY "value-changed" TO BROWSE {&browse-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
    /*------------------------------------------------------------------------------
      Purpose:     Send record ROWID's for all tables used by
                   this file.
      Parameters:  see template/snd-head.i
    ------------------------------------------------------------------------------*/

    /* Define variables needed by this internal procedure.               */
    {src/adm/template/snd-head.i}

    /* For each requested table, put it's ROWID in the output list.      */
    {src/adm/template/snd-list.i "zMessage"}

    /* Deal with any unexpected table requests before closing.           */
    {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-defaults B-table-Win 
PROCEDURE set-defaults :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN
    
            fi_pro-name:SCREEN-VALUE    = ""
            fi_module:SCREEN-VALUE      = ""
            fi_hotkey:SCREEN-VALUE      = "ALL"
            fi_desc:SCREEN-VALUE        = ""
            fi_currMessage:SCREEN-VALUE = ""
            fi_seclevel:SCREEN-VALUE    = "0"
            fi_pro-name                 = ""
            fi_module                   = ""
            fi_hotkey                   = ""
            fi_desc                     = ""
            fi_currMessage              = ""
            fi_seclevel                 = 0
            .     
        
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-focus B-table-Win 
PROCEDURE set-focus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBrowseFocus B-table-Win 
PROCEDURE setBrowseFocus :
    /*------------------------------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        ------------------------------------------------------------------------------*/
    APPLY 'ENTRY':U TO BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
PROCEDURE state-changed :
    /* -----------------------------------------------------------
          Purpose:     
          Parameters:  <none>
          Notes:       
        -------------------------------------------------------------*/
    DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

    CASE p-state:
        /* Object instance CASEs can go here to replace standard behavior
           or add new cases. */
        {src/adm/template/bstates.i}
    END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

