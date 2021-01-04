&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: Util/FixPriceMtxFlg.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Sewa Singh

  Created: 06th June 2019

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
&SCOPED-DEFINE yellowColumnsName ttOePrmtx

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE lReTrigger AS LOGICAL NO-UNDO INITIAL FALSE.


DEFINE TEMP-TABLE ttOePrmtx NO-UNDO
    FIELDS company     AS CHARACTER
    FIELDS cust-no     AS CHARACTER
    FIELDS custShipID  AS CHARACTER
    FIELDS i-no        AS CHARACTER
    FIELDS custype     AS CHARACTER
    FIELDS online      AS LOGICAL 
    FIELD  procat      AS CHARACTER
    FIELD  eff-date    AS DATE
    FIELD  exp-date    AS DATE 
    FIELD  onlineapply AS LOGICAL 
    FIELD  eventRowID  AS ROWID
    .


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttOePrmtx

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 ttOePrmtx.onlineapply ttOePrmtx.company ttOePrmtx.cust-no ttOePrmtx.custShipID ttOePrmtx.i-no ttOePrmtx.custype ttOePrmtx.online ~
               ttOePrmtx.procat ttOePrmtx.eff-date ttOePrmtx.exp-date
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 ttOePrmtx.onlineapply   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-2 ttOePrmtx
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-2 ttOePrmtx
&Scoped-define SELF-NAME BROWSE-2
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH ttOePrmtx     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY {&SELF-NAME} FOR EACH ttOePrmtx    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 ttOePrmtx
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 ttOePrmtx


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-13 btSave btExit btFilter ~
begin_cust-no BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS begin_cust-no fi_sortby

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VARIABLE C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btExit 
    IMAGE-UP FILE "Graphics/32x32/exit_white.png":U
    LABEL "Exit" 
    SIZE 11 BY 2.62 TOOLTIP "Exit".

DEFINE BUTTON btSave 
    IMAGE-UP FILE "Graphics/32x32/floppy_disk.png":U
    LABEL "Export" 
    SIZE 11 BY 2.62 TOOLTIP "Save Data".

DEFINE BUTTON btFilter 
    IMAGE-UP FILE "Graphics/32x32/magnifying_glass.ico":U
    LABEL "Filter" 
    SIZE 9 BY 2.14 TOOLTIP "Filter".

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)":U 
    LABEL "Customer#" 
    VIEW-AS FILL-IN 
    SIZE 30 BY 1 TOOLTIP "Begin Customer"
    FONT 35 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-13
    EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
    SIZE 136 BY 3.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
    ttOePrmtx SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 C-Win _FREEFORM
    QUERY BROWSE-2 NO-LOCK DISPLAY
    ttOePrmtx.onlineapply COLUMN-LABEL "[ ] All" 
    WIDTH 10 VIEW-AS TOGGLE-BOX
    ttOePrmtx.company COLUMN-LABEL "Company" FORMAT "x(3)":U
    WIDTH 15 LABEL-BGCOLOR 14
    ttOePrmtx.cust-no COLUMN-LABEL "Customer" FORMAT "x(8)":U
    WIDTH 20 LABEL-BGCOLOR 14
    ttOePrmtx.custShipID COLUMN-LABEL "Ship Id" FORMAT "x(8)":U
    WIDTH 20 LABEL-BGCOLOR 14
    ttOePrmtx.i-no COLUMN-LABEL "Item No" FORMAT "x(15)":U WIDTH 32 LABEL-BGCOLOR 14
    ttOePrmtx.custype COLUMN-LABEL "Type" FORMAT "x(8)":U WIDTH 15 LABEL-BGCOLOR 14
    ttOePrmtx.online COLUMN-LABEL "OnLine" FORMAT "Yes/No":U LABEL-BGCOLOR 14
    ttOePrmtx.procat COLUMN-LABEL "Cat  " FORMAT "x(5)":U  WIDTH 11 LABEL-BGCOLOR 14
    ttOePrmtx.eff-date COLUMN-LABEL "Effective Date" FORMAT "99/99/9999":U LABEL-BGCOLOR 14
    ttOePrmtx.exp-date COLUMN-LABEL "Exp Date" FORMAT "99/99/9999":U LABEL-BGCOLOR 14
      ENABLE ttOePrmtx.onlineapply
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 180.2 BY 23.1
         FONT 34 ROW-HEIGHT-CHARS .9 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
    btSave AT ROW 1.86 COL 117.6 WIDGET-ID 44
    btExit AT ROW 1.86 COL 130 WIDGET-ID 2
    btFilter AT ROW 2.14 COL 57.2 WIDGET-ID 18
    begin_cust-no AT ROW 2.71 COL 21.6 COLON-ALIGNED WIDGET-ID 24
    fi_sortby AT ROW 3.71 COL 143 COLON-ALIGNED NO-LABEL
    BROWSE-2 AT ROW 5.52 COL 6.8 WIDGET-ID 200
    "Filter" VIEW-AS TEXT
    SIZE 7 BY .62 AT ROW 1.29 COL 14 WIDGET-ID 30
    FONT 35
    RECT-13 AT ROW 1.62 COL 7 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
    SIDE-LABELS NO-UNDERLINE THREE-D 
    AT COL 1 ROW 1
    SIZE 186 BY 28.57
    BGCOLOR 15  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
    CREATE WINDOW C-Win ASSIGN
        HIDDEN             = YES
        TITLE              = "Update Price Matrix OnLine"
        HEIGHT             = 28.57
        WIDTH              = 187.6
        MAX-HEIGHT         = 33.57
        MAX-WIDTH          = 273.2
        VIRTUAL-HEIGHT     = 33.57
        VIRTUAL-WIDTH      = 273.2
        RESIZE             = YES
        SCROLL-BARS        = NO
        STATUS-AREA        = NO
        BGCOLOR            = ?
        FGCOLOR            = ?
        KEEP-FRAME-Z-ORDER = YES
        THREE-D            = YES
        MESSAGE-AREA       = NO
        SENSITIVE          = YES.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 begin_cust-no DEFAULT-FRAME */
ASSIGN 
    BROWSE-2:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
    THEN C-Win:HIDDEN = NO.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.


/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttOePrmtx
      BY ttOePrmtx.i-no DESCENDING.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Fix Price Matrix */
    OR ENDKEY OF {&WINDOW-NAME} ANYWHERE 
    DO:
        /* This case occurs when the user presses the "Esc" key.
           In a persistently run window, just ignore this.  If we did not, the
           application would exit. */
        IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Fix Price Matrix */
    DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust-no C-Win
ON HELP OF begin_cust-no IN FRAME DEFAULT-FRAME /* Customer# */
    DO:
        DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
  
        RUN system/openlookup.p (
            g_company, 
            "cust-no", /* lookup field */
            0,   /* Subject ID */
            "",  /* User ID */
            0,   /* Param value ID */
            OUTPUT returnFields, 
            OUTPUT lookupField, 
            OUTPUT recVal
            ). 
    
        IF lookupField NE "" THEN 
        DO:
            begin_cust-no:SCREEN-VALUE = lookupField.
        
            APPLY "LEAVE" TO SELF.
        END.  
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&Scoped-define SELF-NAME BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON DEFAULT-ACTION OF BROWSE-2 IN FRAME DEFAULT-FRAME
    DO:
    
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-2 C-Win
ON START-SEARCH OF BROWSE-2 IN FRAME DEFAULT-FRAME
    DO:
        IF SELF:CURRENT-COLUMN:NAME EQ "onlineapply" THEN 
        DO:
            lReTrigger = NOT lReTrigger.
        
            FOR EACH ttOePrmtx:
                ttOePrmtx.onlineapply = lReTrigger.
            END.
        
            SELF:CURRENT-COLUMN:LABEL = IF lReTrigger THEN
                "[*] All "
                ELSE
                "[ ] All ".
                                        
            {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}    
        END.    
            RUN startsearch.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&Scoped-define SELF-NAME btExit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btExit C-Win
ON CHOOSE OF btExit IN FRAME DEFAULT-FRAME /* Exit */
    DO:
        APPLY "WINDOW-CLOSE" TO CURRENT-WINDOW.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSave C-Win
ON CHOOSE OF btSave IN FRAME DEFAULT-FRAME /* Export */
    DO:
    
        FOR EACH ttOePrmtx NO-LOCK:

            FIND FIRST oe-prmtx EXCLUSIVE-LOCK
                WHERE ROWID(oe-prmtx) EQ eventRowID NO-ERROR .
            IF AVAILABLE oe-prmtx AND ttOePrmtx.onlineapply EQ YES THEN
                ASSIGN oe-prmtx.online = NOT oe-prmtx.online .
        END.
        RELEASE oe-prmtx .
    
        APPLY "CHOOSE":U TO btFilter.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btFilter C-Win
ON CHOOSE OF btFilter IN FRAME DEFAULT-FRAME /* Filter */
    DO:
        IF begin_cust-no:SCREEN-VALUE EQ ""  THEN RETURN.
    
        EMPTY TEMP-TABLE ttOePrmtx.
        FOR EACH  oe-prmtx NO-LOCK
            WHERE oe-prmtx.company EQ g_company
            AND oe-prmtx.cust-no EQ begin_cust-no:SCREEN-VALUE
            BY oe-prmtx.i-no DESCENDING:
            CREATE ttOePrmtx.
            BUFFER-COPY oe-prmtx TO ttOePrmtx.
            ttOePrmtx.eventRowID = ROWID(oe-prmtx).        
        END.
    
        {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 

    {custom/yellowColumns.i}
    RUN enable_UI.

    /* Ticket# : 92946
   Hiding this widget for now, as browser's column label should be indicating the column which is sorted by */
    fi_sortby:HIDDEN  = TRUE.
    fi_sortby:VISIBLE = FALSE.
    /*APPLY "CHOOSE" TO btFilter.    */
    APPLY "entry" TO begin_cust-no IN FRAME {&FRAME-NAME}. 
   
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
    /*------------------------------------------------------------------------------
      Purpose:     DISABLE the User Interface
      Parameters:  <none>
      Notes:       Here we clean-up the user-interface by deleting
                   dynamic widgets we have created and/or hide 
                   frames.  This procedure is usually called when
                   we are ready to "clean-up" after running.
    ------------------------------------------------------------------------------*/
    /* Delete the WINDOW we created */
    IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
        THEN DELETE WIDGET C-Win.
    IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
    DISPLAY begin_cust-no 
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    ENABLE RECT-13 btSave btExit btFilter begin_cust-no BROWSE-2 
        WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
    {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
    VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

