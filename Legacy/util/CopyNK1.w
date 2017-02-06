&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: batch\contexp.w

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
{methods/defines/hndldefs.i}
{methods/prgsecur.i}
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}
 DEF BUFFER bf-sys-ctrl FOR sys-ctrl .
 DEF BUFFER bf-sys-ctrl-shipto FOR sys-ctrl-shipto .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS from_company to_company Btn_OK Btn_Cancel ~
Btn_Help 
&Scoped-Define DISPLAYED-OBJECTS from_company to_company 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Help" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE from_company AS CHARACTER FORMAT "X(8)":U 
     LABEL "From Company#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 6.

DEFINE VARIABLE to_company AS CHARACTER FORMAT "X(3)" INITIAL "" 
     LABEL "To Company#" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     FONT 6 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     from_company AT ROW 4.86 COL 23.2 COLON-ALIGNED WIDGET-ID 16
     to_company AT ROW 4.86 COL 65.2 COLON-ALIGNED WIDGET-ID 24
     Btn_OK AT ROW 10.19 COL 30
     Btn_Cancel AT ROW 10.19 COL 48
     Btn_Help AT ROW 10.19 COL 84
     "Utiltity Copy the NK1 Parameters from Company to Company" VIEW-AS TEXT
          SIZE 74 BY .62 AT ROW 2.81 COL 4
          FONT 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.4 BY 13.43.


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
         TITLE              = "Copy NK1 Parameters from Company to Company"
         COLUMN             = 54.6
         ROW                = 7.48
         HEIGHT             = 13.52
         WIDTH              = 101
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 119.6
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 119.6
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{advantzware/winkit/embedwindow-nonadm.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Export Contacts */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Export Contacts */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME DEFAULT-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DEFAULT-FRAME C-Win
ON HELP OF FRAME DEFAULT-FRAME
DO:
  def var char-val as cha no-undo.
  def var look-recid as recid no-undo.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lw-focus AS WIDGET-HANDLE NO-UNDO.
  lw-focus = FOCUS.                         
    /*case lw-focus:name :
         WHEN "from_company" THEN DO:
              RUN windows/l-custact.w (gcompany, from_company:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
              FIND cust WHERE RECID(cust) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL cust THEN DO:
               from_company:SCREEN-VALUE = cust.cust-no.
              END.
         END.  
         WHEN "to_company" THEN DO:
              RUN windows/l-custact.w (gcompany, to_company:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
              FIND cust WHERE RECID(cust) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL cust THEN DO:
               to_company:SCREEN-VALUE = cust.cust-no.
              END.
         END.
    end case.*/
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
DO: /* Call Help Function (or a simple message). */
MESSAGE "Help for File: {&FILE-NAME}":U VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK C-Win
ON CHOOSE OF Btn_OK IN FRAME DEFAULT-FRAME /* OK */
DO:
  def var i as int initial 0 no-undo.
  def var num-imports as int initial 0 no-undo.
  DEF VAR excelheader AS CHAR NO-UNDO.

  SESSION:SET-WAIT-STATE("GENERAL").

  IF to_company:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Company Must be Enter " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO to_company .
        RETURN NO-APPLY.
  END.
  IF from_company:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Company Must be Enter " VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO from_company .
       RETURN NO-APPLY.
  END.

  FOR EACH bf-sys-ctrl WHERE bf-sys-ctrl.company = from_company NO-LOCK :

      FIND FIRST sys-ctrl WHERE sys-ctrl.NAME = bf-sys-ctrl.NAME 
            AND  sys-ctrl.company = to_company NO-LOCK NO-ERROR .

      IF NOT AVAIL sys-ctrl THEN DO:
        create sys-ctrl.
        buffer-copy bf-sys-ctrl except bf-sys-ctrl.company to sys-ctrl.
        ASSIGN sys-ctrl.company = to_company .


        FOR EACH bf-sys-ctrl-shipto OF bf-sys-ctrl  NO-LOCK :
           create sys-ctrl-shipto.
           buffer-copy bf-sys-ctrl-shipto except bf-sys-ctrl-shipto.company to sys-ctrl-shipto.
           ASSIGN sys-ctrl-shipto.company = to_company .
        END.
      END.

  END.


  SESSION:SET-WAIT-STATE("").

  message  "NK-1 Parameter Copy " + to_company + " Company successfully" view-as alert-box.
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME from_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL from_company C-Win
ON LEAVE OF from_company IN FRAME DEFAULT-FRAME /* From Company# */
DO:
   assign {&self-name}.
    FIND FIRST company WHERE company.company = from_company:SCREEN-VALUE NO-LOCK NO-ERROR.
              IF NOT AVAIL company THEN DO:         
                  MESSAGE   from_company  "is not a valid company" VIEW-AS ALERT-BOX ERROR.
                  RETURN NO-APPLY.
              END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME to_company
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL to_company C-Win
ON LEAVE OF to_company IN FRAME DEFAULT-FRAME /* To Company# */
DO:
   assign {&self-name}.
   IF to_company:SCREEN-VALUE = "" THEN DO:
       MESSAGE "Company Must be Enter " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
   END.

   FIND FIRST company WHERE company.company = to_company:SCREEN-VALUE NO-LOCK NO-ERROR.
              IF NOT AVAIL company THEN DO:         
                  MESSAGE   to_company  "is not a valid company" VIEW-AS ALERT-BOX ERROR.
                  RETURN NO-APPLY.
              END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

SESSION:SET-WAIT-STATE("").

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   RUN disable_UI.
   {Advantzware/WinKit/closewindow-nonadm.i}
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF access-close THEN DO:
     APPLY "close" TO THIS-PROCEDURE.
     RETURN .
  END.

  RUN enable_UI.

  {methods/nowait.i}
     FIND FIRST usercomp WHERE
      usercomp.user_id = USERID("ASI") AND
      usercomp.loc = '' AND
      usercomp.company_default = YES
      NO-LOCK NO-ERROR.

  IF AVAIL usercomp THEN 
          ASSIGN FROM_company:SCREEN-VALUE = usercomp.company .

   APPLY "entry" TO from_company.
  {Advantzware/WinKit/embedfinalize-nonadm.i}
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
  DISPLAY from_company to_company 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE from_company to_company Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tit-code C-Win 
PROCEDURE valid-tit-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
   /* IF (tit-code:SCREEN-VALUE) <> "" AND
       NOT CAN-FIND(FIRST titlcode
                WHERE titlcode.titlcode  EQ (tit-code:SCREEN-VALUE))
    THEN DO:
      MESSAGE  string(tit-code:SCREEN-VALUE) + " is  invalid, please re-enter..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO tit-code.
      RETURN ERROR.
    END.*/
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

