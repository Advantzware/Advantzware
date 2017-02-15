&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File:   Printer driver bypass flag is added from scr-rpt.w

  Description: from cntnrdlg.w - ADM SmartDialog Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
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

def input parameter list-name as cha no-undo.
def input parameter title-name as cha no-undo.
DEF INPUT PARAMETER ip-font-no AS INT NO-UNDO.
DEF INPUT PARAM ip-orient AS cha NO-UNDO.
DEF INPUT PARAM ip-driver-bypass AS LOG NO-UNDO.  /* window printer driver bypass */
def var ll-log as log no-undo.

DEF VAR li-font-no AS INT INIT 10 NO-UNDO.
DEF VAR lv-ornt AS cha INIT "P" NO-UNDO.

ASSIGN li-font-no = ip-font-no
       lv-ornt = ip-orient.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDialog
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME D-Dialog

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ed-scr-view btn-print btn-save btn-font ~
btn-close 
&Scoped-Define DISPLAYED-OBJECTS ed-scr-view 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn-close AUTO-END-KEY 
     LABEL "&Close" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-font 
     LABEL "&Font" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-print 
     LABEL "&Print" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn-save 
     LABEL "&Save As" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE ed-scr-view AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 143.8 BY 21.43
     FONT 9 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     ed-scr-view AT ROW 1 COL 1 NO-LABEL
     btn-print AT ROW 22.91 COL 14
     btn-save AT ROW 22.91 COL 47
     btn-font AT ROW 22.91 COL 76
     btn-close AT ROW 22.91 COL 111
     SPACE(18.80) SKIP(0.89)
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
                                                                        */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

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
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-close
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-close D-Dialog
ON CHOOSE OF btn-close IN FRAME D-Dialog /* Close */
DO:
  APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-font
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-font D-Dialog
ON CHOOSE OF btn-font IN FRAME D-Dialog /* Font */
DO:
  FONT-TABLE:NUM-ENTRIES = 20.
  SYSTEM-DIALOG FONT 9 FIXED-ONLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-print
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-print D-Dialog
ON CHOOSE OF btn-print IN FRAME D-Dialog /* Print */
DO:
  DEFINE VARIABLE printok AS LOGICAL NO-UNDO.
  DEFINE VARIABLE list-text AS CHARACTER FORMAT "x(176)" NO-UNDO.
  DEFINE VARIABLE result AS LOGICAL NO-UNDO.
  
  /*SYSTEM-DIALOG PRINTER-SETUP UPDATE printok.
  IF NOT printok THEN
  RETURN NO-APPLY.
  */
  /* Use Progress Print. Always use Font#9 in Registry (set above) */
  /*RUN 'adecomm/_osprint.p' (INPUT ?, INPUT list-name,
                            INPUT 3, INPUT 1, INPUT 0, INPUT 0, OUTPUT result).
                            */

  IF ip-driver-bypass THEN DO: /* send file to printer directly */
     RUN custom/d-print.w (list-name).
  END.
  ELSE RUN custom/prntproc.p (list-name, li-font-no, lv-ornt).
 

  

/*
  OUTPUT TO PRINTER.
  INPUT FROM VALUE(list-name) NO-ECHO.
  REPEAT:
    IMPORT UNFORMATTED list-text.
    IF ASC(SUBSTR(list-text,1,1)) = 12 THEN
    DO:
      PAGE.
      list-text = SUBSTR(list-text,2).
    END.
    IF list-text NE "" THEN
    PUT UNFORMATTED list-text SKIP.
    ELSE
    PUT UNFORMATTED SKIP(1).
    list-text = "".
  END.
  INPUT CLOSE.
  OUTPUT CLOSE.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn-save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn-save D-Dialog
ON CHOOSE OF btn-save IN FRAME D-Dialog /* Save As */
DO:
  DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
  DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

  ASSIGN
   /* list-name = screen-file-name + IF screen-file-name NE "" THEN "rpt" ELSE ""
      init-dir = "users\" + USERID("NOSWEAT").
   */
    init-dir = "c:\temp\" .
  SYSTEM-DIALOG GET-FILE list-name
      TITLE      "Enter Listing Name to SAVE AS ..."
      FILTERS    "Listing Files (*.rpt)" "*.rpt",
                 "All Files (*.*)" "*.*"
      INITIAL-DIR init-dir
      ASK-OVERWRITE
      CREATE-TEST-FILE
      SAVE-AS
      USE-FILENAME
      UPDATE OKpressed.
  IF NOT OKpressed THEN
  RETURN NO-APPLY.
  /*ASSIGN
    screen-file-name = SUBSTR(list-name,R-INDEX(list-name,"\") + 1,
        R-INDEX(list-name,".") - R-INDEX(list-name,"\"))
    ldummy = screen-report:SAVE-FILE(list-name)
    {&WINDOW-NAME}:TITLE = "Screen Viewer - '" + screen-file-name + "'".
  */  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

ll-log = ed-scr-view:read-file(list-name).

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
  DISPLAY ed-scr-view 
      WITH FRAME D-Dialog.
  ENABLE ed-scr-view btn-print btn-save btn-font btn-close 
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
  ll-log = ed-scr-view:read-file(list-name) in frame {&frame-name}.
  frame {&frame-name}:title = title-name.
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

