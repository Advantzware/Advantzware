&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: mfprgrms.w

  Description: attach programs to UDF Group

  Input Parameters: group

  Output Parameters: 

  Author: Ron Stark

  Created: 12.5.2016
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

&IF DEFINED(UIB_is_Running) EQ 0 &THEN
DEFINE INPUT PARAMETER ipMFGroup AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ioplSavePrompt AS LOGICAL NO-UNDO.
&ELSE
DEFINE VARIABLE ipMFGroup AS CHARACTER NO-UNDO INITIAL "ASIJobCL".
DEFINE VARIABLE ioplSavePrompt AS LOGICAL NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */

{UDF/mfttdefs.i &NEW="SHARED"}

DEFINE TEMP-TABLE ttblMFPrgrms LIKE ttMFPrgrms.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME browseMFPrgrms

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttblMFPrgrms prgrms

/* Definitions for BROWSE browseMFPrgrms                                */
&Scoped-define FIELDS-IN-QUERY-browseMFPrgrms ttblMFPrgrms.prgmname ttblMFPrgrms.prgtitle   
&Scoped-define ENABLED-FIELDS-IN-QUERY-browseMFPrgrms   
&Scoped-define SELF-NAME browseMFPrgrms
&Scoped-define QUERY-STRING-browseMFPrgrms FOR EACH ttblMFPrgrms WHERE ttblMFPrgrms.mfgroup EQ ipMFGroup
&Scoped-define OPEN-QUERY-browseMFPrgrms OPEN QUERY {&SELF-NAME} FOR EACH ttblMFPrgrms WHERE ttblMFPrgrms.mfgroup EQ ipMFGroup.
&Scoped-define TABLES-IN-QUERY-browseMFPrgrms ttblMFPrgrms
&Scoped-define FIRST-TABLE-IN-QUERY-browseMFPrgrms ttblMFPrgrms


/* Definitions for BROWSE browsePrgrms                                  */
&Scoped-define FIELDS-IN-QUERY-browsePrgrms prgrms.prgmname prgrms.prgtitle 
&Scoped-define ENABLED-FIELDS-IN-QUERY-browsePrgrms 
&Scoped-define QUERY-STRING-browsePrgrms FOR EACH prgrms ~
      WHERE (prgrms.prgmname BEGINS srchPrgmName OR ~
srchPrgmName EQ "") AND ~
(prgrms.prgTitle BEGINS srchPrgTitle OR ~
srchPrgTitle EQ "") NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-browsePrgrms OPEN QUERY browsePrgrms FOR EACH prgrms ~
      WHERE (prgrms.prgmname BEGINS srchPrgmName OR ~
srchPrgmName EQ "") AND ~
(prgrms.prgTitle BEGINS srchPrgTitle OR ~
srchPrgTitle EQ "") NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-browsePrgrms prgrms
&Scoped-define FIRST-TABLE-IN-QUERY-browsePrgrms prgrms


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-browseMFPrgrms}~
    ~{&OPEN-QUERY-browsePrgrms}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnSave srchPrgmName srchPrgTitle ~
browseMFPrgrms btnClearSrchPrgmName browsePrgrms btnClearSrchPrgTitle ~
btnRestore btnExit 
&Scoped-Define DISPLAYED-OBJECTS mfGroup srchPrgmName srchPrgTitle 

/* Custom List Definitions                                              */
/* moveButtons,topButtons,bottomButtons,List-4,List-5,List-6            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnClearSrchPrgmName 
     IMAGE-UP FILE "Graphics/16x16/keyboard_key_x.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.2 BY 1 TOOLTIP "Clear Program Search".

DEFINE BUTTON btnClearSrchPrgTitle 
     IMAGE-UP FILE "Graphics/16x16/keyboard_key_x.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.2 BY 1 TOOLTIP "Clear Program Search".

DEFINE BUTTON btnExit AUTO-END-KEY 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "E&xit" 
     SIZE 8 BY 1.91 TOOLTIP "Exit"
     FONT 4.

DEFINE BUTTON btnRestore 
     IMAGE-UP FILE "Graphics/32x32/refresh.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Reset" 
     SIZE 8 BY 1.91 TOOLTIP "Reset"
     FONT 4.

DEFINE BUTTON btnSave AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "Save" 
     SIZE 8 BY 1.91 TOOLTIP "Save"
     FONT 4.

DEFINE VARIABLE mfGroup AS CHARACTER FORMAT "X(256)":U 
     LABEL "Group" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE srchPrgmName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search Program" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE srchPrgTitle AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY browseMFPrgrms FOR 
      ttblMFPrgrms SCROLLING.

DEFINE QUERY browsePrgrms FOR 
      prgrms SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE browseMFPrgrms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browseMFPrgrms Dialog-Frame _FREEFORM
  QUERY browseMFPrgrms DISPLAY
      ttblMFPrgrms.prgmname FORMAT "x(20)"
 ttblMFPrgrms.prgtitle
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 25.24
         TITLE "UDF Attached Programs" FIT-LAST-COLUMN.

DEFINE BROWSE browsePrgrms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS browsePrgrms Dialog-Frame _STRUCTURED
  QUERY browsePrgrms NO-LOCK DISPLAY
      prgrms.prgmname FORMAT "X(20)":U
      prgrms.prgtitle FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 79 BY 25.24
         TITLE "Programs" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnSave AT ROW 1 COL 145 HELP
          "Save" WIDGET-ID 8
     mfGroup AT ROW 2.19 COL 14 COLON-ALIGNED WIDGET-ID 10
     srchPrgmName AT ROW 3.14 COL 80 COLON-ALIGNED HELP
          "Search Program" WIDGET-ID 14
     srchPrgTitle AT ROW 3.14 COL 101 COLON-ALIGNED HELP
          "Search Title" NO-LABEL WIDGET-ID 16
     browseMFPrgrms AT ROW 4.33 COL 1 WIDGET-ID 200
     btnClearSrchPrgmName AT ROW 3.14 COL 99 HELP
          "Clear Program Search" WIDGET-ID 18
     browsePrgrms AT ROW 4.33 COL 82 WIDGET-ID 100
     btnClearSrchPrgTitle AT ROW 3.14 COL 157 HELP
          "Clear Program Search" WIDGET-ID 20
     btnRestore AT ROW 1 COL 137 HELP
          "Reset" WIDGET-ID 12
     btnExit AT ROW 1 COL 153 HELP
          "Exit Design Layout Window" WIDGET-ID 4
     SPACE(0.20) SKIP(26.66)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "User Defined Fields Attached Programs"
         CANCEL-BUTTON btnExit.


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
/* BROWSE-TAB browseMFPrgrms srchPrgTitle Dialog-Frame */
/* BROWSE-TAB browsePrgrms btnClearSrchPrgmName Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN mfGroup IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       mfGroup:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browseMFPrgrms
/* Query rebuild information for BROWSE browseMFPrgrms
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttblMFPrgrms WHERE ttblMFPrgrms.mfgroup EQ ipMFGroup.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE browseMFPrgrms */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE browsePrgrms
/* Query rebuild information for BROWSE browsePrgrms
     _TblList          = "nosweat.prgrms"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "(prgrms.prgmname BEGINS srchPrgmName OR
srchPrgmName EQ """") AND
(prgrms.prgTitle BEGINS srchPrgTitle OR
srchPrgTitle EQ """")"
     _FldNameList[1]   > nosweat.prgrms.prgmname
"prgrms.prgmname" ? "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = nosweat.prgrms.prgtitle
     _Query            is OPENED
*/  /* BROWSE browsePrgrms */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* User Defined Fields Attached Programs */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseMFPrgrms
&Scoped-define SELF-NAME browseMFPrgrms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browseMFPrgrms Dialog-Frame
ON DEFAULT-ACTION OF browseMFPrgrms IN FRAME Dialog-Frame /* UDF Attached Programs */
DO:
    DELETE ttblMFPrgrms.
    {&OPEN-QUERY-browseMFPrgrms}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browsePrgrms
&Scoped-define SELF-NAME browsePrgrms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL browsePrgrms Dialog-Frame
ON DEFAULT-ACTION OF browsePrgrms IN FRAME Dialog-Frame /* Programs */
DO:
    IF NOT CAN-FIND(FIRST ttblMFPrgrms
                    WHERE ttblMFPrgrms.mfgroup  EQ ipMFGroup
                      AND ttblMFPrgrms.prgmname EQ prgrms.prgmname) THEN DO:
        CREATE ttblMFPrgrms.
        ASSIGN
            ttblMFPrgrms.mfgroup  = ipMFGroup
            ttblMFPrgrms.prgmname = prgrms.prgmname
            ttblMFPrgrms.prgtitle = prgrms.prgtitle
            .
    END.
    {&OPEN-QUERY-browseMFPrgrms}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearSrchPrgmName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearSrchPrgmName Dialog-Frame
ON CHOOSE OF btnClearSrchPrgmName IN FRAME Dialog-Frame
DO:
    ASSIGN
        srchPrgmName:SCREEN-VALUE = ""
        srchPrgmName
        .
    APPLY "ENTRY":U TO srchPrgmName.
    APPLY "VALUE-CHANGED":U TO srchPrgmName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnClearSrchPrgTitle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnClearSrchPrgTitle Dialog-Frame
ON CHOOSE OF btnClearSrchPrgTitle IN FRAME Dialog-Frame
DO:
    ASSIGN
        srchPrgTitle:SCREEN-VALUE = ""
        srchPrgTitle
        .
    APPLY "ENTRY":U TO srchPrgTitle.
    APPLY "VALUE-CHANGED":U TO srchPrgTitle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRestore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRestore Dialog-Frame
ON CHOOSE OF btnRestore IN FRAME Dialog-Frame /* Reset */
DO:
    RUN loadMFPrgrms.
    {&OPEN-QUERY-browseMFPrgrms}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave Dialog-Frame
ON CHOOSE OF btnSave IN FRAME Dialog-Frame /* Save */
DO:
    RUN saveMFPrgrms.
    ioplSavePrompt = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME srchPrgmName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL srchPrgmName Dialog-Frame
ON ENTRY OF srchPrgmName IN FRAME Dialog-Frame /* Search Program */
DO:
    {&SELF-NAME}:SET-SELECTION(1,256).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL srchPrgmName Dialog-Frame
ON VALUE-CHANGED OF srchPrgmName IN FRAME Dialog-Frame /* Search Program */
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-browsePrgrms}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME srchPrgTitle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL srchPrgTitle Dialog-Frame
ON ENTRY OF srchPrgTitle IN FRAME Dialog-Frame
DO:
    {&SELF-NAME}:SET-SELECTION(1,256).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL srchPrgTitle Dialog-Frame
ON VALUE-CHANGED OF srchPrgTitle IN FRAME Dialog-Frame
DO:
    ASSIGN {&SELF-NAME}.
    {&OPEN-QUERY-browsePrgrms}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME browseMFPrgrms
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN loadMFPrgrms.
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY mfGroup srchPrgmName srchPrgTitle 
      WITH FRAME Dialog-Frame.
  ENABLE btnSave srchPrgmName srchPrgTitle browseMFPrgrms btnClearSrchPrgmName 
         browsePrgrms btnClearSrchPrgTitle btnRestore btnExit 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadMFPrgrms Dialog-Frame 
PROCEDURE loadMFPrgrms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    mfGroup = ipMFGroup.

    &IF DEFINED(UIB_is_Running) NE 0 &THEN
    DEFINE VARIABLE i AS INTEGER NO-UNDO.

    EMPTY TEMP-TABLE ttMFPrgrms.

    FOR EACH mfgroup NO-LOCK:
        DO i = 2 TO NUM-ENTRIES(mfgroup.mfgroup_data,"|"):
            FIND FIRST prgrms NO-LOCK
                 WHERE prgrms.prgmname EQ ENTRY(i,mfgroup.mfgroup_data,"|")
                 NO-ERROR.
            IF NOT AVAILABLE prgrms THEN NEXT.
            CREATE ttMFPrgrms.
            ASSIGN
                ttMFPrgrms.mfgroup  = ENTRY(1,mfgroup.mfgroup_data,"|")
                ttMFPrgrms.prgmname = prgrms.prgmname
                ttMFPrgrms.prgtitle = prgrms.prgtitle
                .
        END. /* do i */
    END. /* each mfgroup */
    &ENDIF
    
    EMPTY TEMP-TABLE ttblMFPrgrms.

    FOR EACH ttMFPrgrms
        WHERE ttMFPrgrms.mfgroup EQ ipMFGroup
        :
        CREATE ttblMFPrgrms.
        BUFFER-COPY ttMFPrgrms TO ttblMFPrgrms.
    END. /* each ttmfprgrms */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE saveMFPrgrms Dialog-Frame 
PROCEDURE saveMFPrgrms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ttMFPrgrms
        WHERE ttMFPrgrms.mfgroup EQ ipMFGroup
        :
        DELETE ttMFPrgrms.
    END. /* each ttmfprgrms */

    FOR EACH ttblMFPrgrms:
        CREATE ttMFPrgrms.
        BUFFER-COPY ttblMFPrgrms TO ttMFPrgrms.
    END. /* each ttblmfprgrms */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

