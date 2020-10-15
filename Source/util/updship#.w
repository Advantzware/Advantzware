&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: fixShipto.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Ron Stark

  Created: 02/26/2019

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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
&SCOPED-DEFINE yellowColumnsName updship#
{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}
{methods/template/brwcustomdef.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = g_company
    locode = g_loc.
DEFINE VARIABLE cChar AS CHARACTER.
DEFINE VARIABLE lCheckPro AS LOGICAL NO-UNDO .
DEFINE TEMP-TABLE tt-oe-shipto 
    FIELD tt-recid    AS RECID
    FIELD ship-id     LIKE shipto.ship-id
    FIELD ship-name   LIKE shipto.ship-name
    FIELD ship-add    AS CHARACTER 
    FIELD ship-city   LIKE shipto.ship-city
    FIELD ship-state  LIKE shipto.ship-state
    FIELD ship-zip   LIKE shipto.ship-zip
    FIELD i-count     AS INTEGER
    FIELD ship-stat   AS LOGICAL 
    FIELD IS-SELECTED AS LOG       COLUMN-LABEL "" VIEW-AS TOGGLE-BOX
    .
DEFINE BUFFER bf-shipto FOR shipto .

 IF INDEX(PROGRAM-NAME(2),"viewers/shipto.") GT 0 THEN
     ASSIGN lCheckPro = YES .



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME browse-machine

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-oe-shipto

/* Definitions for BROWSE browse-machine                                */
&Scoped-define FIELDS-IN-QUERY-browse-machine tt-oe-shipto.IS-SELECTED tt-oe-shipto.ship-id tt-oe-shipto.ship-name tt-oe-shipto.ship-add tt-oe-shipto.ship-city tt-oe-shipto.ship-state tt-oe-shipto.ship-zip tt-oe-shipto.ship-stat
&Scoped-define ENABLED-FIELDS-IN-QUERY-browse-machine tt-oe-shipto.IS-SELECTED   
&Scoped-define ENABLED-TABLES-IN-QUERY-browse-machine tt-oe-shipto
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-browse-machine tt-oe-shipto
&Scoped-define SELF-NAME browse-machine
&Scoped-define QUERY-STRING-browse-machine FOR EACH tt-oe-shipto      NO-LOCK ~
      ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-browse-machine OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-shipto      NO-LOCK  ~
      ~{&SORTBY-PHRASE} .
&Scoped-define TABLES-IN-QUERY-browse-machine tt-oe-shipto
&Scoped-define FIRST-TABLE-IN-QUERY-browse-machine tt-oe-shipto


/* Definitions for FRAME F-Main                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-browse-machine}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-17 RECT-18 begin_cust browse-machine ~
fi_ship-id rd_active tb_post-change btn-process btn-cancel 
&Scoped-Define DISPLAYED-OBJECTS begin_cust fi_ship-id rd_active ~
tb_post-change  fi_sortby

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,F1                                */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-cancel 
     LABEL "Ca&ncel" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btn-process 
     LABEL "&Start Process" 
     SIZE 18 BY 1.14.

DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Change Selected ShipTo Ids for Customer" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fi_ship-id AS CHARACTER FORMAT "X(8)":U 
     LABEL "To Ship To" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE rd_active AS CHARACTER INITIAL "I" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Set Old Ship To to Inactive", "I",
"Delete Old Ship To", "D"
     SIZE 61.4 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 18.29.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 9.43.

DEFINE VARIABLE tb_post-change AS LOGICAL INITIAL no 
     LABEL "Include Posted/Closed Records (Order,Release,Bol,Invoice)" 
     VIEW-AS TOGGLE-BOX
     SIZE 64 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sortby    AS CHARACTER FORMAT "X(256)":U 
    VIEW-AS FILL-IN 
    SIZE 47 BY 1
    BGCOLOR 14 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browse-machine FOR 
      tt-oe-shipto SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browse-machine
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browse-machine C-Win _FREEFORM
  QUERY browse-machine NO-LOCK DISPLAY
      tt-oe-shipto.IS-SELECTED COLUMN-LABEL ''  VIEW-AS TOGGLE-BOX 
    tt-oe-shipto.ship-id FORMAT "x(8)" COLUMN-LABEL "Ship To ID" WIDTH 12 LABEL-BGCOLOR 14
    tt-oe-shipto.ship-name FORMAT "X(30)" COLUMN-LABEL "Ship To Name" LABEL-BGCOLOR 14
    tt-oe-shipto.ship-add FORMAT "X(40)" COLUMN-LABEL "Ship To Address"  LABEL-BGCOLOR 14
    tt-oe-shipto.ship-city COLUMN-LABEL "City" LABEL-BGCOLOR 14
    tt-oe-shipto.ship-state COLUMN-LABEL "State" LABEL-BGCOLOR 14
    tt-oe-shipto.ship-zip COLUMN-LABEL "Zip" LABEL-BGCOLOR 14
    tt-oe-shipto.ship-stat COLUMN-LABEL "Inactive" FORMAT "Yes/No" LABEL-BGCOLOR 14
      ENABLE tt-oe-shipto.IS-SELECTED
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 89 BY 9.05 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     begin_cust AT ROW 6.24 COL 50 COLON-ALIGNED HELP
          "Enter Beginning Customer" WIDGET-ID 10
     browse-machine AT ROW 7.76 COL 4
     fi_ship-id AT ROW 17.38 COL 40 COLON-ALIGNED
     rd_active AT ROW 18.62 COL 20.6 NO-LABEL WIDGET-ID 16
     tb_post-change AT ROW 19.67 COL 21.4 WIDGET-ID 22
     btn-process AT ROW 21.62 COL 21
     btn-cancel AT ROW 21.62 COL 53
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 5.29 COL 3.6
     "" VIEW-AS TEXT
          SIZE 2.2 BY .95 AT ROW 1.95 COL 88
          BGCOLOR 11 
     RECT-17 AT ROW 4.86 COL 1
     RECT-18 AT ROW 7.52 COL 1 WIDGET-ID 20
     fi_sortby AT ROW 3.76 COL 10 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 22.24.

DEFINE FRAME FRAME-B
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 1 COL 3.6
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 88.8 BY .95 AT ROW 3.76 COL 1
          BGCOLOR 11 
     "You MUST perform a database backup before running this procedure!" VIEW-AS TEXT
          SIZE 84 BY .95 AT ROW 1.95 COL 6.6
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "This process may take hours.  Please let the process complete!" VIEW-AS TEXT
          SIZE 76 BY .95 AT ROW 2.91 COL 10.6
          BGCOLOR 11 FGCOLOR 12 FONT 5
     "" VIEW-AS TEXT
          SIZE 7 BY .95 AT ROW 2.91 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 3 BY .95 AT ROW 1.95 COL 1
          BGCOLOR 11 
     "" VIEW-AS TEXT
          SIZE 6.2 BY .95 AT ROW 2.91 COL 82.8
          BGCOLOR 11 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 95.8 BY 3.81
         BGCOLOR 11 .


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
         TITLE              = "Update Ship To Number"
         HEIGHT             = 22.24
         WIDTH              = 95.8
         MAX-HEIGHT         = 22.24
         MAX-WIDTH          = 98.2
         VIRTUAL-HEIGHT     = 22.24
         VIRTUAL-WIDTH      = 98.2
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("Graphics\asiicon.ico":U) THEN
    MESSAGE "Unable to load icon: Graphics\asiicon.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB browse-machine begin_cust F-Main */
ASSIGN 
       btn-cancel:PRIVATE-DATA IN FRAME F-Main     = 
                "ribbon-button".

ASSIGN 
       btn-process:PRIVATE-DATA IN FRAME F-Main     = 
                "ribbon-button".

ASSIGN 
       tb_post-change:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

ASSIGN 
    browse-machine:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
    fi_sortby:HIDDEN IN FRAME F-Main = TRUE.

/* SETTINGS FOR FRAME FRAME-B
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browse-machine
/* Query rebuild information for BROWSE browse-machine
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt-oe-shipto
     NO-LOCK SORTBY-PHRASE .
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE browse-machine */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Update Ship To Number */
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
ON WINDOW-CLOSE OF C-Win /* Update Ship To Number */
DO:
        /* This event will close the window and terminate the procedure.  */
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        RETURN NO-APPLY.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browse-machine C-Win
ON START-SEARCH OF browse-machine IN FRAME F-Main
    DO:
        RUN startSearch.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browse-machine C-Win
ON ROW-DISPLAY OF browse-machine IN FRAME F-Main
DO:
    &scoped-define exclude-row-display true 
    {methods/template/brwrowdisplay.i}   
        
  IF AVAIL tt-oe-shipto AND tt-oe-shipto.ship-stat EQ YES THEN DO:
      ASSIGN 
          tt-oe-shipto.ship-id:BGCOLOR IN BROWSE {&BROWSE-NAME}      = 3 
          tt-oe-shipto.ship-name:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 3 
          tt-oe-shipto.ship-add:BGCOLOR IN BROWSE {&BROWSE-NAME}     = 3 
          tt-oe-shipto.ship-city:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 3 
          tt-oe-shipto.ship-state:BGCOLOR IN BROWSE {&BROWSE-NAME}   = 3 
          tt-oe-shipto.ship-zip:BGCOLOR IN BROWSE {&BROWSE-NAME}     = 3 
          tt-oe-shipto.ship-stat:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 3 .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME begin_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_cust C-Win
ON LEAVE OF begin_cust IN FRAME F-Main /* Change Selected ShipTo Ids for Customer */
DO:
        DO WITH FRAME {&FRAME-NAME}:
      
            ASSIGN begin_cust.
            CLOSE QUERY browse-machine.
            RUN build-table. 
            OPEN QUERY browse-machine FOR EACH tt-oe-shipto  NO-LOCK BY tt-oe-shipto.ship-id.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-cancel C-Win
ON CHOOSE OF btn-cancel IN FRAME F-Main /* Cancel */
DO:
        APPLY "close" TO THIS-PROCEDURE.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-process
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-process C-Win
ON CHOOSE OF btn-process IN FRAME F-Main /* Start Process */
DO:
        DEFINE VARIABLE v-process AS LOG NO-UNDO.
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.
        END.
  
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ cocode
            AND cust.cust-no EQ begin_cust
            NO-ERROR .

        IF NOT AVAILABLE cust THEN 
        DO:
            MESSAGE "You must enter a valid customer number" VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO begin_cust.
            RETURN NO-APPLY.
        END.

        FIND FIRST bf-shipto NO-LOCK
            WHERE bf-shipto.company EQ cocode
            AND bf-shipto.cust-no EQ cust.cust-no
            AND bf-shipto.ship-id EQ fi_ship-id:SCREEN-VALUE
            NO-ERROR.

        IF NOT AVAILABLE bf-shipto THEN 
        DO:
            MESSAGE "You must enter a valid Ship Id" VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO fi_ship-id.
            RETURN NO-APPLY.
        END.

        i = 0 .
        FOR EACH tt-oe-shipto WHERE tt-oe-shipto.IS-SELECTED:
            i = i + 1.
        END.

        IF i = 0  THEN 
        DO:
            MESSAGE "Please Select Record ..."
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK .
            RETURN .
        END.


        MESSAGE "Are you sure you want to " + TRIM(c-win:TITLE)
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE v-process.

        IF v-process THEN RUN run-process.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ship-id C-Win
ON HELP OF fi_ship-id IN FRAME F-Main /* To Ship To */
DO:
        DEFINE VARIABLE char-val AS cha   NO-UNDO.
        DEFINE VARIABLE rec-val  AS RECID NO-UNDO.
        ASSIGN fi_ship-id .
        RUN windows/l-shipto.w (g_company,g_loc,begin_cust:screen-value,fi_ship-id,OUTPUT char-val).
        IF char-val <> "" THEN
            ASSIGN fi_ship-id:SCREEN-VALUE = ENTRY(1,char-val).

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_ship-id C-Win
ON LEAVE OF fi_ship-id IN FRAME F-Main /* To Ship To */
DO:
        DO WITH FRAME {&FRAME-NAME}:
      
            ASSIGN fi_ship-id.
        END.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_active
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_active C-Win
ON LEAVE OF rd_active IN FRAME F-Main
DO:
        ASSIGN rd_active.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_active C-Win
ON VALUE-CHANGED OF rd_active IN FRAME F-Main
DO:
    /*{custom/chgfont.i}*/
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_post-change
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_post-change C-Win
ON VALUE-CHANGED OF tb_post-change IN FRAME F-Main /* Include Posted/Closed Records (Order,Release,Bol,Invoice) */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browse-machine
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3helpw.i}
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
    RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

{methods/template/brwcustom.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

    /* security check need {methods/prgsecur.i} in definition section */
    IF access-close THEN 
    DO:
        APPLY "close" TO THIS-PROCEDURE.
        RETURN .
    END.
    {custom/yellowColumns.i}
    RUN enable_UI.
    {methods/nowait.i}

    IF INDEX(PROGRAM-NAME(2),"viewers/shipto.") NE 0 THEN do:
        {custom/usrprint.i}
    END.

    CLOSE QUERY browse-machine.
    RUN build-table. 
    DO WITH FRAME {&FRAME-NAME}:
         
        OPEN QUERY browse-machine FOR EACH tt-oe-shipto
            NO-LOCK BY tt-oe-shipto.ship-id.
    END.


    /* DO WITH FRAME {&FRAME-NAME}:
        APPLY "entry" TO fi_ship-id.
      END.*/
  
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-table C-Win 
PROCEDURE build-table :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        EMPTY TEMP-TABLE tt-oe-shipto .     
        FOR EACH shipto NO-LOCK
            WHERE shipto.company = cocode  
            AND shipto.cust-no = begin_cust:screen-value 
            :
            FIND FIRST tt-oe-shipto WHERE tt-oe-shipto.tt-recid = RECID(shipto)
                NO-ERROR.
            IF NOT AVAILABLE tt-oe-shipto THEN 
            DO:
                CREATE tt-oe-shipto.
                ASSIGN 
                    tt-oe-shipto.tt-recid   = RECID(shipto)
                    tt-oe-shipto.ship-id    = shipto.ship-id  
                    tt-oe-shipto.ship-name  = shipto.ship-name
                    tt-oe-shipto.ship-add   = shipto.ship-add[1]
                    tt-oe-shipto.ship-city  = shipto.ship-city  
                    tt-oe-shipto.ship-state = shipto.ship-state
                    tt-oe-shipto.ship-zip   = shipto.ship-zip 
                    tt-oe-shipto.ship-stat  = IF shipto.statusCode EQ "I" THEN YES ELSE NO
                    .
            END.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY begin_cust fi_ship-id rd_active tb_post-change 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE RECT-17 RECT-18 begin_cust browse-machine fi_ship-id rd_active 
         tb_post-change btn-process btn-cancel 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW FRAME FRAME-B IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/*DEFINE VARIABLE lRelease AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lBol     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lOrder   AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInv     AS LOGICAL NO-UNDO.*/

    SESSION:SET-WAIT-STATE("General").

    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN {&displayed-objects}.
    END.

    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ begin_cust
        NO-ERROR .
  
    FIND FIRST bf-shipto NO-LOCK
        WHERE bf-shipto.company EQ cocode
        AND bf-shipto.cust-no EQ cust.cust-no
        AND bf-shipto.ship-id EQ fi_ship-id:SCREEN-VALUE
        NO-ERROR.
    
    IF fi_ship-id NE "" THEN 
    DO:

        FOR EACH tt-oe-shipto WHERE tt-oe-shipto.IS-SELECTED:
            FOR EACH shipto EXCLUSIVE-LOCK 
                WHERE shipto.company EQ cocode AND 
                recid(shipto) EQ tt-oe-shipto.tt-recid :
              
              
                FIND FIRST eb
                    WHERE eb.company EQ cocode
                    USE-INDEX cust NO-LOCK NO-ERROR.
            
                DO WHILE AVAILABLE eb:
                    cChar = eb.loc.
            
                    FOR EACH eb
                        WHERE eb.company EQ cocode
                        AND eb.loc     EQ cChar
                        AND eb.cust-no EQ cust.cust-no
                        AND eb.ship-id EQ shipto.ship-id
                        USE-INDEX cust
            
                        TRANSACTION:
            
                        eb.ship-id = bf-shipto.ship-id .
                        {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Estimate # ' + string(eb.est-no) "}
            
                        ASSIGN
                            eb.ship-no      = bf-shipto.ship-no
                            eb.ship-name    = bf-shipto.ship-name
                            eb.ship-addr[1] = bf-shipto.ship-addr[1]
                            eb.ship-addr[2] = bf-shipto.ship-addr[2]
                            eb.ship-city    = bf-shipto.ship-city
                            eb.ship-state   = bf-shipto.ship-state
                            eb.ship-zip     = bf-shipto.ship-zip.
                    END.
            
                    RELEASE eb.
            
                    FIND FIRST eb
                        WHERE eb.company EQ cocode
                        AND eb.loc     GT cChar
                        USE-INDEX cust NO-LOCK NO-ERROR.
                END.
            
                FOR EACH quotehd
                    WHERE quotehd.company EQ cocode
                    AND quotehd.cust-no EQ cust.cust-no
                    AND quotehd.ship-id EQ shipto.ship-id
            
                    TRANSACTION:
                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Quote # ' + string(quotehd.q-no) "}
            
                    quotehd.ship-id = bf-shipto.ship-id.
            
                    ASSIGN
                        quotehd.shipto[1] = bf-shipto.ship-name
                        quotehd.shipto[2] = bf-shipto.ship-addr[1]
                        quotehd.shipto[3] = bf-shipto.ship-addr[2]
                        quotehd.shipto[4] = bf-shipto.ship-city + ", " + bf-shipto.ship-state +
                                     "  " + bf-shipto.ship-zip.
                END.

                FOR EACH quoteitm
                    WHERE quoteitm.company EQ cocode
                    AND quoteitm.cust-no EQ cust.cust-no
                    AND quoteitm.ship-id EQ shipto.ship-id
            
                    TRANSACTION:
                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Quote # ' + string(quoteitm.q-no) "}
            
                    quoteitm.ship-id = bf-shipto.ship-id.
                    quoteitm.shipto = bf-shipto.ship-addr[1] .
                        
                END.
            
                FOR EACH oe-rel
                    WHERE oe-rel.company EQ cocode
                    AND oe-rel.cust-no EQ cust.cust-no
                    AND oe-rel.ship-id EQ shipto.ship-id
                    TRANSACTION:
                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) "}
            
                    oe-rel.ship-id = bf-shipto.ship-id .
            
                    oe-rel.ship-no = bf-shipto.ship-no .
                END.
            
                FOR EACH oe-relh
                    WHERE oe-relh.company EQ cocode
                    AND (oe-relh.posted  EQ NO OR tb_post-change)
                    AND oe-relh.cust-no EQ cust.cust-no
                    AND oe-relh.ship-id EQ shipto.ship-id
                    USE-INDEX post
                    TRANSACTION:

                    {custom/statusMsg.i "'Processing Ship To # ' + string(shipto.ship-id) + ' Release # ' + string(oe-relh.rel-no) "}
            
                    oe-relh.ship-id = bf-shipto.ship-id.
            
                    oe-relh.ship-no = bf-shipto.ship-no.
                END.
            
                FOR EACH oe-bolh
                    WHERE oe-bolh.company EQ cocode
                    AND (oe-bolh.posted  EQ NO OR tb_post-change)
                    AND oe-bolh.cust-no EQ cust.cust-no 
                    AND oe-bolh.ship-id EQ shipto.ship-id
                    USE-INDEX post
            
                    TRANSACTION:
                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Bol # ' + string(oe-bolh.bol-no) "}
            
                    oe-bolh.ship-id = bf-shipto.ship-id.
            
                    oe-bolh.ship-no = bf-shipto.ship-no.
                END.
            
                FOR EACH ar-inv
                    WHERE ar-inv.company EQ cocode
                    AND (ar-inv.posted  EQ NO OR tb_post-change)
                    AND ar-inv.cust-no EQ cust.cust-no
                    AND ar-inv.ship-id EQ shipto.ship-id
                    USE-INDEX posted
            
                    TRANSACTION:

                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Invoice # ' + string(ar-inv.inv-no) "}
            
                    ar-inv.ship-id = bf-shipto.ship-id.
                END.
            
                FOR EACH oe-ord
                    WHERE oe-ord.company EQ cocode
                    AND oe-ord.cust-no EQ cust.cust-no
                    AND oe-ord.ship-id EQ shipto.ship-id
                    AND (oe-ord.opened EQ YES OR tb_post-change )
                    TRANSACTION:

                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Order # ' + string(oe-ord.ord-no) "}
               
                    oe-ord.ship-id = bf-shipto.ship-id .
                                                  
                END.
                FOR EACH oe-ordl
                    WHERE oe-ordl.company EQ cocode
                    AND oe-ordl.cust-no EQ cust.cust-no
                    AND oe-ordl.ship-id EQ shipto.ship-id
                    AND (oe-ordl.opened EQ YES OR tb_post-change )
                    TRANSACTION:

                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Order # ' + string(oe-ordl.ord-no) "}
               
                    oe-ordl.ship-id = bf-shipto.ship-id .
                                                  
                END.
                
                FOR EACH po-ord
                    WHERE po-ord.company EQ cocode
                    AND po-ord.cust-no EQ cust.cust-no
                    AND po-ord.cust-no NE "" 
                    AND po-ord.ship-id EQ shipto.ship-id
                    AND (po-ord.stat NE "C" OR tb_post-change )
                    TRANSACTION:
                 
                    {custom/statusMsg.i "'Processing Shipto # ' + string(shipto.ship-id) + ' Purchase Order # ' + string(po-ord.po-no) "}
               
                      ASSIGN
                        po-ord.ship-id = bf-shipto.ship-id     
                        po-ord.ship-name = bf-shipto.ship-name   
                        po-ord.ship-addr[1] = bf-shipto.ship-addr[1]
                        po-ord.ship-addr[2] = bf-shipto.ship-addr[2]
                        po-ord.ship-city = bf-shipto.ship-city   
                        po-ord.ship-state = bf-shipto.ship-state  
                        po-ord.ship-zip = bf-shipto.ship-zip    
                        po-ord.ship-no = bf-shipto.ship-no  
                        po-ord.loc = bf-shipto.loc   . 
                                                  
                END.

                FOR EACH sys-ctrl-shipto 
                    WHERE sys-ctrl-shipto.company EQ cocode
                      AND sys-ctrl-shipto.cust-vend EQ YES 
                      AND sys-ctrl-shipto.cust-vend-no EQ cust.cust-no
                      AND sys-ctrl-shipto.ship-id EQ shipto.ship-id 
                    TRANSACTION:
                    
                      ASSIGN
                       sys-ctrl-shipto.ship-id = bf-shipto.ship-id     .
                END.

            
                DO TRANSACTION:
                    IF rd_active EQ "I" THEN
                        shipto.statusCode = "I".
                    ELSE IF rd_active EQ "D" THEN 
                        DO:
                            DELETE shipto .
                        END.
                END.

            END.  /* for each shipto */
        END.  /* for each tt-oe-shipto */
    END.  /* fi_ship-id ne "" */  
    RELEASE shipto .
    RELEASE oe-ord .
    RELEASE eb .
    RELEASE ar-inv .
    RELEASE oe-bolh .
    RELEASE oe-relh .
    RELEASE quotehd .
    RELEASE oe-rel .
    RELEASE quoteitm .
    RELEASE oe-ordl .
    RELEASE po-ord .
    RELEASE sys-ctrl-shipto .

    STATUS DEFAULT "".

    SESSION:SET-WAIT-STATE("").

    MESSAGE TRIM(c-win:TITLE) + " Process Complete..." VIEW-AS ALERT-BOX.

    IF lCheckPro THEN
        APPLY "close" TO THIS-PROCEDURE.
    ELSE DO:
        CLOSE QUERY browse-machine.
        RUN build-table. 
        OPEN QUERY browse-machine FOR EACH tt-oe-shipto  NO-LOCK BY tt-oe-shipto.ship-id.
    END.

    RETURN NO-APPLY.
/* end ---------------------------------- copr. 2001  advanced software, inc. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

