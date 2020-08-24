&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: scr-rpt.w
   
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

&Scoped-define program-id scr-rpt.

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE INPUT PARAMETER list-name  AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER title-name AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-font-no AS INTEGER   NO-UNDO.
DEFINE INPUT PARAMETER ip-orient  AS CHARACTER NO-UNDO.

DEFINE VARIABLE li-font-no AS INTEGER   NO-UNDO INITIAL 10.
DEFINE VARIABLE lv-ornt    AS CHARACTER NO-UNDO INITIAL "P".
DEFINE VARIABLE gcompany   AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
{custom/getcmpny.i}

ASSIGN
    li-font-no = ip-font-no
    lv-ornt = ip-orient
    .
DO TRANSACTION:
   {sys/inc/notepad.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnMinimize btnMaximize ed-scr-view btnApply ~
widthSize heightSize btn-close btn-font btn-print btn-save 
&Scoped-Define DISPLAYED-OBJECTS ed-scr-view widthSize heightSize 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-close AUTO-GO 
     IMAGE-UP FILE "Graphics/32x32/door_exit.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Exit".

DEFINE BUTTON btn-font 
     IMAGE-UP FILE "Graphics/32x32/font.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Font".

DEFINE BUTTON btn-print 
     IMAGE-UP FILE "Graphics/32x32/printer.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Print".

DEFINE BUTTON btn-save 
     IMAGE-UP FILE "Graphics/32x32/floppy_disk.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Save As".

DEFINE BUTTON btnApply 
     IMAGE-UP FILE "Graphics/32x32/arrow_spread.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Apply".

DEFINE BUTTON btnMaximize 
     IMAGE-UP FILE "Graphics/32x32/arrow_spread2.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Maximize".

DEFINE BUTTON btnMinimize 
     IMAGE-UP FILE "Graphics/32x32/arrow_join2.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 8 BY 1.91 TOOLTIP "Minimize".

DEFINE VARIABLE ed-scr-view AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 159 BY 26.19
     FONT 9 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 34 BY 2.33.

DEFINE VARIABLE heightSize AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 29 MAX-VALUE 48 HORIZONTAL 
     TIC-MARKS BOTH FREQUENCY 1
     SIZE 38 BY 1.91 TOOLTIP "Set Width" NO-UNDO.

DEFINE VARIABLE widthSize AS INTEGER INITIAL 0 
     VIEW-AS SLIDER MIN-VALUE 160 MAX-VALUE 380 HORIZONTAL 
     TIC-MARKS BOTH FREQUENCY 10
     SIZE 38 BY 1.91 TOOLTIP "Width Size" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     btnMinimize AT ROW 27.43 COL 96 WIDGET-ID 22
     btnMaximize AT ROW 27.43 COL 87 WIDGET-ID 20
     ed-scr-view AT ROW 1 COL 2 NO-LABEL
     btnApply AT ROW 27.43 COL 40 WIDGET-ID 18
     widthSize AT ROW 27.43 COL 2 NO-LABEL WIDGET-ID 14
     heightSize AT ROW 27.43 COL 49 NO-LABEL WIDGET-ID 16
     btn-close AT ROW 27.43 COL 152
     btn-font AT ROW 27.43 COL 144
     btn-print AT ROW 27.43 COL 128
     btn-save AT ROW 27.43 COL 136
     RECT-1 AT ROW 27.19 COL 127 WIDGET-ID 2
     SPACE(0.00) SKIP(0.05)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Screen Viewer".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDialog
   Allow: Basic,Browse,DB-Fields,Query,Smart
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB D-Dialog 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX D-Dialog
   FRAME-NAME                                                           */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME D-Dialog
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog /* Screen Viewer */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  RUN pSaveSettings.
  APPLY "GO":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-close D-Dialog
ON CHOOSE OF btn-close IN FRAME D-Dialog
DO:
    RUN pSaveSettings.
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-font
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-font D-Dialog
ON CHOOSE OF btn-font IN FRAME D-Dialog
DO:
  FONT-TABLE:NUM-ENTRIES = 99.
  SYSTEM-DIALOG FONT 9 FIXED-ONLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-print D-Dialog
ON CHOOSE OF btn-print IN FRAME D-Dialog
DO:
    RUN custom/prntproc.p (list-name, li-font-no, lv-ornt).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-save D-Dialog
ON CHOOSE OF btn-save IN FRAME D-Dialog
DO:
    DEFINE VARIABLE OKpressed AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.

    init-dir = "c:\temp\" .
    SYSTEM-DIALOG GET-FILE list-name
        TITLE   "Enter Listing Name to SAVE AS ..."
        FILTERS "Listing Files (*.rpt)" "*.rpt",
                "All Files (*.*)" "*.*"
        INITIAL-DIR init-dir
        ASK-OVERWRITE
        CREATE-TEST-FILE
        SAVE-AS
        USE-FILENAME
        UPDATE OKpressed.
    IF NOT OKpressed THEN
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnApply
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnApply D-Dialog
ON CHOOSE OF btnApply IN FRAME D-Dialog
DO:
    RUN pWinReSize (heightSize, widthSize).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMaximize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMaximize D-Dialog
ON CHOOSE OF btnMaximize IN FRAME D-Dialog
DO:
    ASSIGN
        heightSize = SESSION:HEIGHT - 1
        widthSize  = SESSION:WIDTH
        .
    RUN pWinReSize (heightSize, widthSize).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnMinimize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnMinimize D-Dialog
ON CHOOSE OF btnMinimize IN FRAME D-Dialog
DO:
    ASSIGN
        heightSize = heightSize:MIN-VALUE
        widthSize  = widthSize:MIN-VALUE
        .
    RUN pWinReSize (heightSize, widthSize).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME heightSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL heightSize D-Dialog
ON VALUE-CHANGED OF heightSize IN FRAME D-Dialog
DO:
    ASSIGN {&SELF-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME widthSize
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL widthSize D-Dialog
ON VALUE-CHANGED OF widthSize IN FRAME D-Dialog
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

IF notepad-log THEN DO:
    IF notepad-chr EQ "" THEN DO: /* task 02101509 */
&IF DEFINED(FWD-VERSION) > 0 &THEN
        open-mime-resource "text/plain" STRING("file:///" + list-name) FALSE.
&ELSE
        OS-COMMAND NO-WAIT notepad VALUE(list-name).
&ENDIF
        RETURN.
    END.
    ELSE DO:
        FIND FIRST usergrps NO-LOCK
             WHERE usergrps.usergrps EQ "Notepad" 
             NO-ERROR.
        IF AVAILABLE usergrps AND LOOKUP(STRING(USERID(LDBNAME(1))),usergrps.users) <> 0 THEN DO:
&IF DEFINED(FWD-VERSION) > 0 &THEN
            open-mime-resource "text/plain" STRING("file:///" + list-name) FALSE.
&ELSE
            OS-COMMAND NO-WAIT notepad VALUE(list-name).
&ENDIF
            RETURN.
        END.
        ELSE DO:
            IF NOT AVAILABLE usergrps THEN DO:
                CREATE usergrps.
                ASSIGN
                    usergrps.usergrps = "Notepad"
                    usergrps.dscr     = "NOTEPAD USERS"
                    .
            END. /* if not avail */
        END.  /* not avail usergrps */
    END. /* else do  notepad-chr = "" */
END.  /* if notepad-log */
ASSIGN
    heightSize:MAX-VALUE = SESSION:HEIGHT - 1
    widthSize:MAX-VALUE  = SESSION:WIDTH
    .
RUN pGetSettings.
ed-scr-view:READ-FILE(list-name).

{src/adm/template/dialogmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects D-Dialog  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available D-Dialog  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI D-Dialog  _DEFAULT-DISABLE
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
  HIDE FRAME D-Dialog.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI D-Dialog  _DEFAULT-ENABLE
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
  DISPLAY ed-scr-view widthSize heightSize 
      WITH FRAME D-Dialog.
  ENABLE btnMinimize btnMaximize ed-scr-view btnApply widthSize heightSize 
         btn-close btn-font btn-print btn-save 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize D-Dialog 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ed-scr-view:READ-FILE(list-name) IN FRAME {&frame-name}.
  FRAME {&frame-name}:title = title-name.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pGetSettings D-Dialog 
PROCEDURE pGetSettings :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx     AS INTEGER NO-UNDO.
    
    FIND FIRST user-print NO-LOCK
         WHERE user-print.program-id EQ "{&program-id}"
           AND user-print.user-id    EQ USERID("ASI")
         NO-ERROR.
    IF AVAILABLE user-print THEN DO:
        DO idx = 1 TO EXTENT(user-print.field-name):
            IF user-print.field-name[idx] EQ "" THEN LEAVE.
            CASE user-print.field-name[idx]:
                WHEN "WindowHeight" THEN
                heightSize = DECIMAL(user-print.field-value[idx]).
                WHEN "WindowWidth" THEN
                widthSize  = DECIMAL(user-print.field-value[idx]).
            END CASE.
        END. /* do idx */
    END. /* if avail */
    RUN pWinReSize (heightSize, widthSize).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSaveSettings D-Dialog 
PROCEDURE pSaveSettings :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE idx AS INTEGER NO-UNDO.
    
    DO TRANSACTION:
        FIND FIRST user-print EXCLUSIVE-LOCK
             WHERE user-print.program-id EQ "{&program-id}"
               AND user-print.user-id    EQ USERID("ASI")
             NO-ERROR.
        IF NOT AVAILABLE user-print THEN DO:
            CREATE user-print.
            ASSIGN
                user-print.program-id = "{&program-id}"
                user-print.user-id    = USERID("ASI")
                .
        END. /* not avail */
        ASSIGN
            user-print.field-name  = ""
            user-print.field-value = ""
            user-print.field-label = ""
            .
        ASSIGN
            idx = idx + 1
            user-print.field-name[idx]  = "WindowWidth"
            user-print.field-label[idx] = "WindowWidth"
            user-print.field-value[idx] = STRING(FRAME {&FRAME-NAME}:WIDTH)
            idx = idx + 1
            user-print.field-name[idx]  = "WindowHeight"
            user-print.field-label[idx] = "WindowHeight"
            user-print.field-value[idx] = STRING(FRAME {&FRAME-NAME}:HEIGHT)
            .
        FIND CURRENT user-print NO-LOCK.
    END. /* do trans */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pWinReSize D-Dialog 
PROCEDURE pWinReSize :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdHeight AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdWidth  AS DECIMAL NO-UNDO.

    SESSION:SET-WAIT-STATE("General").
    IF ipdHeight LT 28.57 THEN
    ipdHeight = 28.57.
    IF ipdWidth  LT 160   THEN
    ipdWidth  = 160.
    DO WITH FRAME {&FRAME-NAME}:
        HIDE FRAME {&FRAME-NAME}.
        ASSIGN
            RECT-1:HIDDEN              = YES
            btn-print:HIDDEN           = YES
            btn-save:HIDDEN            = YES
            btn-font:HIDDEN            = YES
            btn-close:HIDDEN           = YES
            FRAME {&FRAME-NAME}:HEIGHT = ipdHeight
            FRAME {&FRAME-NAME}:WIDTH  = ipdWidth
            ed-scr-view:HEIGHT         = FRAME {&FRAME-NAME}:HEIGHT - 4.2
            ed-scr-view:WIDTH          = FRAME {&FRAME-NAME}:WIDTH  - 3
            btnApply:ROW               = FRAME {&FRAME-NAME}:HEIGHT - 3
            heightSize:ROW             = btnApply:ROW
            widthSize:ROW              = btnApply:ROW
            btnMaximize:ROW            = btnApply:ROW
            btnMinimize:ROW            = btnApply:ROW
            RECT-1:ROW                 = FRAME {&FRAME-NAME}:HEIGHT - 2.94
            btn-print:ROW              = RECT-1:ROW    + .24
            btn-save:ROW               = btn-print:ROW
            btn-font:ROW               = btn-save:ROW
            btn-close:ROW              = btn-font:ROW
            RECT-1:COL                 = FRAME {&FRAME-NAME}:WIDTH  - 35
            btn-print:COL              = RECT-1:COL    + 1
            btn-save:COL               = btn-print:COL + btn-print:WIDTH
            btn-font:COL               = btn-save:COL  + btn-save:WIDTH
            btn-close:COL              = btn-font:COL  + btn-font:WIDTH
            .
        VIEW FRAME {&FRAME-NAME}.
        ASSIGN
            RECT-1:HIDDEN    = NO
            btn-print:HIDDEN = NO
            btn-save:HIDDEN  = NO
            btn-font:HIDDEN  = NO
            btn-close:HIDDEN = NO
            .
    END. /* do with */
    SESSION:SET-WAIT-STATE("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records D-Dialog  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartDialog, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed D-Dialog 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

