&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File              : util/MoveEst.w

  Description       : Move Estimate Files

  Input Parameters  : <none>

  Output Parameters : <none>

  Author            : Eric Panchenko

  Created           : July 10, 2007

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
{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}
{sys/inc/var.i new shared}

assign
 cocode = gcompany.

/* Variables */
DEFINE VARIABLE vcStartingPath  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE vcEndPath  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE viFileCount AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-42 fi_OldPath fi_NewPath fi_date-start ~
fi_date-end btnOk BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS fi_OldPath fi_NewPath fi_date-start ~
fi_date-end 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnOk AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fi_date-end AS DATE FORMAT "99/99/9999":U 
     LABEL "End Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_date-start AS DATE FORMAT "99/99/9999":U 
     LABEL "Start Date" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_NewPath AS CHARACTER FORMAT "x(40)":U 
     LABEL "To Folder" 
     VIEW-AS FILL-IN 
     SIZE 41.4 BY 1 NO-UNDO.

DEFINE VARIABLE fi_OldPath AS CHARACTER FORMAT "x(40)":U 
     LABEL "From Folder" 
     VIEW-AS FILL-IN 
     SIZE 41.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58 BY 3.95.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fi_OldPath AT ROW 3.14 COL 14.6 COLON-ALIGNED
     fi_NewPath AT ROW 4.43 COL 14.6 COLON-ALIGNED
     fi_date-start AT ROW 7.43 COL 12 COLON-ALIGNED
     fi_date-end AT ROW 7.43 COL 39.6 COLON-ALIGNED
     btnOk AT ROW 12.19 COL 14
     BtnCancel AT ROW 12.19 COL 36
     "Move Estimate Files" VIEW-AS TEXT
          SIZE 19.4 BY .62 AT ROW 2.1 COL 6.6
     RECT-42 AT ROW 2.38 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 64.4 BY 12.62.


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
         TITLE              = "Move Estimate Files"
         HEIGHT             = 12.62
         WIDTH              = 64.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN
       BtnCancel:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


ASSIGN
       btnOk:PRIVATE-DATA IN FRAME DEFAULT-FRAME     = 
                "ribbon-button".


IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Move Estimate Files */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Move Estimate Files */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Cancel */
DO:
    APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* OK */
DO:
   DEF VAR v-process AS LOG NO-UNDO.

   ASSIGN fi_OldPath fi_NewPath fi_date-start
          fi_date-end
          FILE-INFO:FILE-NAME = fi_OldPath.

   IF FILE-INFO:FILE-TYPE EQ ? THEN
   DO:
       MESSAGE "Old Path is invalid."
           VIEW-AS ALERT-BOX ERROR BUTTONS OK.
       APPLY "ENTRY":U TO fi_OldPath IN FRAME {&FRAME-NAME}.
       LEAVE.
   END.

   FILE-INFO:FILE-NAME = fi_NewPath.

   IF FILE-INFO:FILE-TYPE EQ ? THEN
   DO:
      MESSAGE "New Path is Blank."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "ENTRY":U TO fi_NewPath IN FRAME {&FRAME-NAME}.
      LEAVE.
   END.

   IF fi_OldPath EQ fi_NewPath THEN
   DO:
      MESSAGE "From Folder and To Folder are Equivalent."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "ENTRY":U TO fi_OldPath IN FRAME {&FRAME-NAME}.
      LEAVE.
   END.

   IF fi_date-start EQ ? THEN
   DO:
      MESSAGE "Invalid Start Date."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "ENTRY":U TO fi_date-start IN FRAME {&FRAME-NAME}.
      LEAVE.
   END.

   IF fi_date-end EQ ? THEN
   DO:
      MESSAGE "Invalid End Date."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "ENTRY":U TO fi_date-end IN FRAME {&FRAME-NAME}.
      LEAVE.
   END.

   IF fi_date-start > fi_date-end THEN
   DO:
      MESSAGE "Invalid Date Range."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
      APPLY "ENTRY":U TO fi_date-start IN FRAME {&FRAME-NAME}.
      LEAVE.
   END.

   message "Are you sure you want move estimate files from " fi_OldPath
          "to" fi_NewPath + "?"       
          view-as alert-box question button yes-no update v-process.

  if v-process then run run-process.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_NewPath
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_NewPath C-Win
ON HELP OF fi_NewPath IN FRAME DEFAULT-FRAME /* To Folder */
DO:
   DEFINE VARIABLE oServer AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE oFolder AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE oParent AS COM-HANDLE NO-UNDO.
   DEFINE VARIABLE cFolder AS CHARACTER NO-UNDO.
   DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

   CREATE 'Shell.Application' oServer.

   oFolder = oServer:BrowseForFolder(CURRENT-WINDOW:HWND,'Select To Folder',0).

   IF VALID-HANDLE(oFolder) = True THEN
   DO:
      ASSIGN cFolder = oFolder:Title
             oParent = oFolder:ParentFolder
             iCount = 0.

      REPEAT:
         IF iCount >= oParent:Items:Count THEN
            LEAVE.
         ELSE
            IF oParent:Items:Item(iCount):Name = cFolder THEN
            DO:
               fi_NewPath:SCREEN-VALUE = oParent:Items:Item(iCount):Path.
               LEAVE.
            END.
         ASSIGN iCount = iCount + 1.
       END.
    END.

    RELEASE OBJECT oParent NO-ERROR.
    RELEASE OBJECT oFolder NO-ERROR.
    RELEASE OBJECT oServer NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_OldPath
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_OldPath C-Win
ON HELP OF fi_OldPath IN FRAME DEFAULT-FRAME /* From Folder */
DO:
  DEFINE VARIABLE oServer AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE oFolder AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE oParent AS COM-HANDLE NO-UNDO.
  DEFINE VARIABLE cFolder AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

  CREATE 'Shell.Application' oServer.

  oFolder = oServer:BrowseForFolder(CURRENT-WINDOW:HWND,'Select From Folder',0).

  IF VALID-HANDLE(oFolder) = True THEN
  DO:
     ASSIGN cFolder = oFolder:Title
            oParent = oFolder:ParentFolder
            iCount = 0.

     REPEAT:
        IF iCount >= oParent:Items:Count THEN
           LEAVE.
        ELSE
           IF oParent:Items:Item(iCount):Name = cFolder THEN
           DO:
              fi_OldPath:SCREEN-VALUE = oParent:Items:Item(iCount):Path.
              LEAVE.
           END.
        ASSIGN iCount = iCount + 1.
      END.
   END.

   RELEASE OBJECT oParent NO-ERROR.
   RELEASE OBJECT oFolder NO-ERROR.
   RELEASE OBJECT oServer NO-ERROR.
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

   ASSIGN
      fi_date-start = TODAY
      fi_date-end = TODAY.

   RUN enable_UI.

   FIND first sys-ctrl where
        sys-ctrl.company eq cocode AND
        sys-ctrl.name    eq "CEBROWSE"
        no-lock no-error.

   IF AVAIL sys-ctrl THEN
      fi_NewPath:SCREEN-VALUE = sys-ctrl.char-fld.

   apply 'entry':u to fi_OldPath.

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
  DISPLAY fi_OldPath fi_NewPath fi_date-start fi_date-end 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-42 fi_OldPath fi_NewPath fi_date-start fi_date-end btnOk 
         BtnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-process C-Win 
PROCEDURE run-process :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR vcFile AS CHAR NO-UNDO EXTENT 3.
   DEF VAR vcExt AS CHAR NO-UNDO. 
   DEF VAR vcCommand AS CHAR NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:

     SESSION:SET-WAIT-STATE ("general").

     /* Clean up input data. */
      ASSIGN
         vcStartingPath  = right-trim (fi_OldPath:screen-value, '~/~\')
         vcEndPath  = right-trim (fi_NewPath:screen-value, '~/~\')
         viFileCount   = 0.

      INPUT FROM OS-DIR(vcStartingPath).
      REPEAT:
         IMPORT vcFile.
         /* Skip current and parent dir */
         IF vcFile[1] EQ '.':U OR vcFile[1] EQ '..':U THEN NEXT.

         /* Only move estimate files */
         IF vcFile[3] BEGINS 'F':U THEN DO:

            vcExt = SUBSTRING(vcFile[1],LENGTH(vcFile[1]) - 3).

            IF LENGTH(vcExt) EQ 4 AND
               SUBSTRING(vcExt,1,1) EQ "." AND
               LOOKUP(SUBSTRING(vcExt,2,1),"v,a,s,p,q,z,b,x,n") > 0 AND
               LOOKUP(SUBSTRING(vcExt,3,1),"0,1,2,3,4,5,6,7,8,9") > 0 AND
               LOOKUP(SUBSTRING(vcExt,4,1),"0,1,2,3,4,5,6,7,8,9") > 0 THEN
               DO:
                  FILE-INFO:FILE-NAME = vcFile[2].

                  IF FILE-INFO:FILE-MOD-DATE GE fi_date-start AND
                     FILE-INFO:FILE-MOD-DATE LE fi_date-end THEN
                  DO:
                     ASSIGN
                       viFileCount = viFileCount + 1.
                       vcCommand = "move /y " + vcFile[2] + " " + vcEndPath.

                     STATUS DEFAULT 'Copying: ' + vcFile[2].

                     OS-COMMAND SILENT VALUE (vcCommand).
                  END.
               END.
         END.
      END.

      INPUT CLOSE.

      STATUS DEFAULT ''.

      MESSAGE 'Moved ' viFileCount ' estimate file(s).'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

