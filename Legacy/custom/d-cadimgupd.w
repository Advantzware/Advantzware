&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipRowId  AS ROWID NO-UNDO.

/* Local Variable Definitions ---                                       */


DEFINE VARIABLE ldummy  AS LOG       NO-UNDO.
DEFINE VARIABLE i       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCount  AS INTEGER   NO-UNDO .
DEFINE VARIABLE cadFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdCadProcs AS HANDLE.
RUN custom/CadImgProcs.p PERSISTENT SET hdCadProcs.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-30 RECT-7 RECT-8 tb_addcad tb_boximg ~
file_name Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS est_no tot_form tb_addcad tb_boximg ~
file_name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ImageName Dialog-Frame 
FUNCTION ImageName RETURNS CHARACTER
    (ipImageFileName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "Cancel" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "OK" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE VARIABLE est_no    AS CHARACTER FORMAT "X(10)" 
    LABEL "Estimate" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1.

DEFINE VARIABLE file_name AS CHARACTER FORMAT "X(256)":U 
    LABEL "File Path" 
    VIEW-AS FILL-IN 
    SIZE 39 BY 1 NO-UNDO.

DEFINE VARIABLE tot_form  AS CHARACTER FORMAT "X(11)":U 
    LABEL "# of Forms" 
    VIEW-AS FILL-IN 
    SIZE 21 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-30
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 90.2 BY .01.

DEFINE RECTANGLE RECT-7
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 91 BY 9.76.

DEFINE RECTANGLE RECT-8
    EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
    SIZE 85 BY .15.

DEFINE VARIABLE tb_addcad AS LOGICAL INITIAL YES 
    LABEL "Add .ARD Extension to CAD #" 
    VIEW-AS TOGGLE-BOX
    SIZE 52 BY 1 NO-UNDO.

DEFINE VARIABLE tb_boximg AS LOGICAL INITIAL YES 
    LABEL "Update Box Design Image File path and filename - (CAD#).jpg" 
    VIEW-AS TOGGLE-BOX
    SIZE 69 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
    est_no AT ROW 1.71 COL 13 COLON-ALIGNED
    tot_form AT ROW 1.71 COL 48 COLON-ALIGNED
    tb_addcad AT ROW 3.48 COL 15 WIDGET-ID 60
    tb_boximg AT ROW 4.71 COL 15 WIDGET-ID 62
    file_name AT ROW 6.43 COL 30 COLON-ALIGNED
    Btn_OK AT ROW 8.52 COL 51
    Btn_Cancel AT ROW 8.52 COL 70.6
    RECT-30 AT ROW 2.91 COL 1.2 WIDGET-ID 56
    RECT-7 AT ROW 1.24 COL 2 WIDGET-ID 58
    RECT-8 AT ROW 2.95 COL 5 WIDGET-ID 64
    SPACE(3.79) SKIP(8.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
    SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
    TITLE "CAD and BOX Design Update"
    DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
ASSIGN 
    FRAME Dialog-Frame:SCROLLABLE = FALSE
    FRAME Dialog-Frame:HIDDEN     = TRUE.

/* SETTINGS FOR FILL-IN est_no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
    tb_addcad:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

ASSIGN 
    tb_boximg:PRIVATE-DATA IN FRAME Dialog-Frame = "parm".

/* SETTINGS FOR FILL-IN tot_form IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Bank Info */
    DO:
        APPLY "END-ERROR":U TO SELF.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
    DO:
        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&displayed-objects}.

           IF SEARCH(file_name) EQ ?  THEN DO:
               MESSAGE "File Path or file Invalid, try help..." VIEW-AS ALERT-BOX INFORMATION .
                APPLY "entry" TO file_name . 
                RETURN NO-APPLY .
           END.
        END.

    RUN pUpdateCadOnCorrugated IN hdCadProcs(ipRowId,tb_addcad,tb_boximg,ImageName(file_name),file_name) .

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_addcad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_addcad Dialog-Frame
ON VALUE-CHANGED OF tb_addcad IN FRAME Dialog-Frame /* Add .ARD Extension to CAD # */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_boximg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_boximg Dialog-Frame
ON VALUE-CHANGED OF tb_boximg IN FRAME Dialog-Frame /* Update Box Design Image File path and filename - (CAD#).jpg */
    DO:
        ASSIGN {&self-name}.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME file_name
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL file_name Dialog-Frame
ON HELP OF file_name IN FRAME Dialog-Frame /* Update Box Design Image File path and filename - (CAD#).jpg */
    DO:
        DEFINE VARIABLE initDir   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE okClicked AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE cRtnChar  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lRecFound AS LOGICAL   NO-UNDO.

        RUN sys/ref/nk1look.p (INPUT ipcCompany, "CEUpdateCAD", "C" /* Logical */, NO /* check by cust */, 
            INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
            OUTPUT cRtnChar, OUTPUT lRecFound).
        IF lRecFound THEN
            initDir = cRtnChar NO-ERROR. 
        cadFile = ''.

        SYSTEM-DIALOG GET-FILE cadfile 
            TITLE 'Select Image File to insert'
            FILTERS 'JPG Files    (*.jpg)' '*.jpg',
            'Bitmap files (*.bmp)' '*.bmp',
            'JPEG Files   (*.jpeg)' '*.jpeg',
            'TIF Files    (*.tif)' '*.tif',
            'All Files    (*.*) ' '*.*'
            INITIAL-DIR initDir
            MUST-EXIST USE-FILENAME UPDATE okClicked.
        IF okClicked THEN
            file_name:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cadfile /*imageName(cadfile)*/ .
  
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
   
    FIND FIRST est NO-LOCK
        WHERE ROWID(est) EQ ipRowId NO-ERROR .

    IF AVAILABLE est THEN 
    DO:
        est_no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = est.est-no  .
        est_no = est.est-no .
        iCount = 0 .

        FOR EACH ef NO-LOCK
            WHERE ef.company EQ est.company 
            AND ef.est-no EQ est.est-no :
            iCount = iCount + 1 .
        END.

        tot_form:SCREEN-VALUE IN FRAME {&FRAME-NAME}   = string(iCount) .
        tot_form = STRING(iCount) .

    END.
  
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
    DISPLAY est_no tot_form tb_addcad tb_boximg file_name 
        WITH FRAME Dialog-Frame.
    ENABLE RECT-30 RECT-7 RECT-8 tb_addcad tb_boximg file_name Btn_OK Btn_Cancel 
        WITH FRAME Dialog-Frame.
    VIEW FRAME Dialog-Frame.
    {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


  /* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ImageName Dialog-Frame 
FUNCTION ImageName RETURNS CHARACTER
    (ipImageFileName AS CHARACTER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    ASSIGN
        ipImageFileName = SUBSTR(ipImageFileName,1,R-INDEX(ipImageFileName,'.') - 1)
        ipImageFileName = SUBSTR(ipImageFileName,R-INDEX(ipImageFileName,'\') + 1).
    RETURN ipImageFileName.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
