&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------

  File: 

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

&SCOPED-DEFINE WinKitDontEmbed

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

&SCOPED-DEFINE defType OUTPUT PARAMETER
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&SCOPED-DEFINE defType VARIABLE
&ENDIF

DEFINE {&defType} op-prt-note AS LOG NO-UNDO.
DEFINE {&defType} op-prt-box AS LOG NO-UNDO.
DEFINE {&defType} op-from-dept AS CHAR  NO-UNDO.
DEFINE {&defType} op-to-dept AS CHAR  NO-UNDO.
DEFINE {&defType} op-dest AS INT NO-UNDO.
DEFINE {&defType} op-font AS INT NO-UNDO.
DEFINE {&defType} op-ornt AS CHAR NO-UNDO.
DEFINE {&defType} op-lines AS INT NO-UNDO.
DEFINE {&defType} op-error AS LOG NO-UNDO.
DEFINE {&defType} op-contribution AS LOG NO-UNDO.

{sys/inc/var.i new shared}
{methods/prgsecur.i}

ASSIGN cocode = g_company
       locode = g_loc.

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
&Scoped-Define ENABLED-OBJECTS RECT-8 tb_prt-note from_dept to_dept ~
tb_prt-box tb-contribution rd-dest lv-ornt lv-font-no lines-per-page Btn_OK ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS tb_prt-note from_dept to_dept tb_prt-box ~
tb-contribution rd-dest lv-ornt lv-font-no lines-per-page lv-font-name 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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

DEFINE VARIABLE from_dept AS CHARACTER FORMAT "X(256)":U 
     LABEL "From Department" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>9":U INITIAL 99 
     LABEL "Lines Per Page" 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)" 
     VIEW-AS FILL-IN 
     SIZE 59 BY 1 NO-UNDO.

DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15" 
     LABEL "Font" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE to_dept AS CHARACTER FORMAT "X(256)":U INITIAL "zz" 
     LABEL "To Department" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Portrait", "P",
"Landscape", "L"
     SIZE 32 BY .95 NO-UNDO.

DEFINE VARIABLE rd-dest AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "To Printer", 1,
"To Screen", 2,
"To File", 3,
"To Fax", 4,
"To Email", 5,
"To Port Directly", 6
     SIZE 19 BY 7.38 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 28 BY 8.57.

DEFINE VARIABLE tb-contribution AS LOGICAL INITIAL no 
     LABEL "Print Contribution $" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-box AS LOGICAL INITIAL no 
     LABEL "Print Box design?" 
     VIEW-AS TOGGLE-BOX
     SIZE 39 BY .81 NO-UNDO.

DEFINE VARIABLE tb_prt-note AS LOGICAL INITIAL yes 
     LABEL "Print Manufacturing Department note?" 
     VIEW-AS TOGGLE-BOX
     SIZE 48 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     tb_prt-note AT ROW 2.43 COL 12
     from_dept AT ROW 3.62 COL 22 COLON-ALIGNED
     to_dept AT ROW 3.62 COL 57 COLON-ALIGNED
     tb_prt-box AT ROW 5.05 COL 12
     tb-contribution AT ROW 5.05 COL 38 WIDGET-ID 2
     rd-dest AT ROW 7.67 COL 6 NO-LABEL
     lv-ornt AT ROW 8.14 COL 38 NO-LABEL
     lv-font-no AT ROW 9.33 COL 36 COLON-ALIGNED
     lines-per-page AT ROW 9.33 COL 69 COLON-ALIGNED
     lv-font-name AT ROW 10.52 COL 36 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 16 COL 20
     Btn_Cancel AT ROW 16 COL 70
     "Selection Parameters" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 1.24 COL 3
          BGCOLOR 2 
     "Output Destination" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 6.95 COL 7
     RECT-8 AT ROW 6.71 COL 4
     SPACE(66.39) SKIP(2.19)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Estimate Print"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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

/* SETTINGS FOR FILL-IN lv-font-name IN FRAME D-Dialog
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
ON WINDOW-CLOSE OF FRAME D-Dialog /* Estimate Print */
DO:  
  /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel D-Dialog
ON CHOOSE OF Btn_Cancel IN FRAME D-Dialog /* Cancel */
DO:
   op-error = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK D-Dialog
ON CHOOSE OF Btn_OK IN FRAME D-Dialog /* OK */
DO: 
    ASSIGN tb-contribution.
    ASSIGN
      tb_prt-note tb_prt-box from_dept to_dept rd-dest lv-font-no lv-ornt lines-per-page
      op-prt-note = tb_prt-note
      op-prt-box = tb_prt-box
      op-from-dept = from_dept
      op-to-dept = to_dept
      op-dest = rd-dest
      op-font = int(lv-font-no)
      op-ornt = lv-ornt
      op-lines = lines-per-page
      op-error = NO
      op-contribution = tb-contribution.
   RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lines-per-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page D-Dialog
ON HELP OF lines-per-page IN FRAME D-Dialog /* Lines Per Page */
DO:
&IF DEFINED(FWD-VERSION) > 0 &THEN
  open-mime-resource "text/plain" "file:///linesPerPage.txt" false.
&ELSE
  OS-COMMAND NO-WAIT notepad linesPerPage.txt.
&ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lines-per-page D-Dialog
ON LEAVE OF lines-per-page IN FRAME D-Dialog /* Lines Per Page */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-font-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no D-Dialog
ON HELP OF lv-font-no IN FRAME D-Dialog /* Font */
DO:
  DEF VAR char-val AS CHAR NO-UNDO.
  
  RUN windows/l-fonts.w (SELF:SCREEN-VALUE,OUTPUT char-val).
  IF char-val NE "" THEN DO:
    ASSIGN
      SELF:SCREEN-VALUE = ENTRY(1,char-val)
      lv-font-name:SCREEN-VALUE = ENTRY(2,char-val)
      {&SELF-NAME}.
    RUN setLinesPerPage (lv-ornt,lv-font-no,lines-per-page:HANDLE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-font-no D-Dialog
ON LEAVE OF lv-font-no IN FRAME D-Dialog /* Font */
DO:
  ASSIGN {&SELF-NAME}.
  RUN setLinesPerPage (lv-ornt,lv-font-no,lines-per-page:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-ornt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-ornt D-Dialog
ON VALUE-CHANGED OF lv-ornt IN FRAME D-Dialog
DO:
  ASSIGN {&SELF-NAME}.
  RUN setLinesPerPage (lv-ornt,lv-font-no,lines-per-page:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd-dest
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd-dest D-Dialog
ON VALUE-CHANGED OF rd-dest IN FRAME D-Dialog
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

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
  DISPLAY tb_prt-note from_dept to_dept tb_prt-box tb-contribution rd-dest 
          lv-ornt lv-font-no lines-per-page lv-font-name 
      WITH FRAME D-Dialog.
  ENABLE RECT-8 tb_prt-note from_dept to_dept tb_prt-box tb-contribution 
         rd-dest lv-ornt lv-font-no lines-per-page Btn_OK Btn_Cancel 
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
  {custom/usrprint.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLinesPerPage D-Dialog 
PROCEDURE setLinesPerPage :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipOrientation AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipFont AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipLines AS HANDLE NO-UNDO.

  DEFINE VARIABLE orientFont AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lines AS INTEGER NO-UNDO.
  DEFINE VARIABLE linesPerPage AS INTEGER NO-UNDO.
  DEFINE VARIABLE cLPPFile AS CHARACTER NO-UNDO.
  linesPerPage = IF ipOrientation EQ 'P' THEN 66 ELSE 45.
  IF SEARCH('linesPerPage.txt') NE ? THEN DO:
    cLPPFile = SEARCH('linesPerPage.txt').
    INPUT FROM VALUE(cLPPFile) NO-ECHO.
    REPEAT:
      IMPORT orientFont lines.
      IF orientFont EQ ipOrientation + ipFont THEN DO:
        linesPerPage = lines.
        LEAVE.
      END. /* if */
    END. /* repeat */
    INPUT CLOSE.
  END. /* if search */
  ipLines:SCREEN-VALUE = STRING(linesPerPage).
  APPLY 'LEAVE':U TO ipLines.

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

