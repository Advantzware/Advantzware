&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
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

  Author:   Sewa Singh

  Created:  fri aug 9 19:29:35 EST 2019
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
/* DEF OUTPUT PARAM opiQty LIKE job-hdr.qty.    */
/* DEF OUTPUT PARAM oplRunForm AS LOG NO-UNDO.  */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{cecrep/jc-soule.i}
{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

DEF WORKFILE w-jm
   FIELD d-seq    LIKE mach.d-seq
   FIELD rec-id   AS   RECID.

DEF TEMP-TABLE ttItemList
    FIELD i-no LIKE job-hdr.i-no
    FIELD part-no LIKE itemfg.part-no
    FIELD frm AS INTEGER 
    FIELD combo AS LOGICAL
    FIELD itemName AS CHARACTER 
    FIELD blank-no AS INTEGER 
    FIELD IS-SELECTED AS LOGICAL  COLUMN-LABEL "" VIEW-AS TOGGLE-BOX
    FIELD lDie AS LOGICAL  COLUMN-LABEL "Die" VIEW-AS TOGGLE-BOX
    FIELD lCad AS LOGICAL  COLUMN-LABEL "Cad" VIEW-AS TOGGLE-BOX
    FIELD lPlate AS LOGICAL  COLUMN-LABEL "Plate" VIEW-AS TOGGLE-BOX
    FIELD lImage AS LOGICAL  COLUMN-LABEL "Image" VIEW-AS TOGGLE-BOX.

DEF BUFFER bf-job-hdr FOR job-hdr.
DEF BUFFER bf-itemfg FOR itemfg.
DEFINE SHARED VARIABLE lIncludeLastPage AS LOGICAL NO-UNDO .
DEFINE SHARED VARIABLE cRdOptionMclean AS CHARACTER NO-UNDO .
DEFINE SHARED VARIABLE cJobType AS CHARACTER NO-UNDO .
DEFINE SHARED VARIABLE lFSC     AS LOGICAL   NO-UNDO .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-4

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ttItemList

/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 ttItemList.IS-SELECTED ttItemList.frm ttItemList.combo ttItemList.i-no ttItemList.part-no ttItemList.itemName ttItemList.lDie ttItemList.lCad ttItemList.lPlate ttItemList.lImage 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4 ttItemList.IS-SELECTED ttItemList.lDie ttItemList.lCad ttItemList.lPlate ttItemList.lImage 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-4 ttItemList
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-4 ttItemList
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH ttItemList
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH ttItemList.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 ttItemList
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 ttItemList


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-4 cbJobType rd_active tb_include-page ~
Btn_OK Btn_Cancel tb_fsc RECT-42 
&Scoped-Define DISPLAYED-OBJECTS cbJobType fi_job-no rd_active ~
tb_include-page tb_fsc 

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

DEFINE VARIABLE cbJobType AS CHARACTER FORMAT "X(256)":U INITIAL "--" 
     LABEL "Job Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "--","New Order","Revised Order","Repeat Order" 
     DROP-DOWN-LIST
     SIZE 25.6 BY 1 NO-UNDO.

DEFINE VARIABLE fi_job-no AS CHARACTER FORMAT "x(9)" 
     LABEL "Job#" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE rd_active AS CHARACTER INITIAL "M" 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Moorestown", "M",
"Nazareth", "N"
     SIZE 35.4 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-42
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 9.29.

DEFINE VARIABLE tb_fsc AS LOGICAL INITIAL no 
     LABEL "FSC" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE tb_include-page AS LOGICAL INITIAL no 
     LABEL "Include Item List Page" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-4 FOR 
      ttItemList SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 Dialog-Frame _FREEFORM
  QUERY BROWSE-4 DISPLAY
      ttItemList.IS-SELECTED COLUMN-LABEL ''  VIEW-AS TOGGLE-BOX
      ttItemList.frm COLUMN-LABEL 'Frm' WIDTH 5  
      ttItemList.combo COLUMN-LABEL 'Combo' 
      ttItemList.i-no FORMAT "X(15)" LABEL "FG Item #" WIDTH 19
      ttItemList.part-no FORMAT "X(15)" LABEL "Customer Part #" WIDTH 19
      ttItemList.itemName FORMAT "X(30)" LABEL "Item Name"
      ttItemList.lDie COLUMN-LABEL 'Die'  VIEW-AS TOGGLE-BOX
      ttItemList.lCad COLUMN-LABEL 'Cad'  VIEW-AS TOGGLE-BOX
      ttItemList.lPlate COLUMN-LABEL 'Plate'  VIEW-AS TOGGLE-BOX
      ttItemList.lImage COLUMN-LABEL 'Image'  VIEW-AS TOGGLE-BOX
    ENABLE ttItemList.IS-SELECTED
           ttItemList.lDie
           ttItemList.lCad
           ttItemList.lPlate
           ttItemList.lImage
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 59 BY 9.29
         FONT 6 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-4 AT ROW 1.24 COL 52 WIDGET-ID 100
     cbJobType AT ROW 7.62 COL 18.4 COLON-ALIGNED WIDGET-ID 24
     fi_job-no AT ROW 1.95 COL 13 COLON-ALIGNED HELP
          "Enter Job Number."
     rd_active AT ROW 3.67 COL 13.6 NO-LABEL WIDGET-ID 16
     tb_include-page AT ROW 5.14 COL 13.6 WIDGET-ID 22
     Btn_OK AT ROW 11 COL 36
     Btn_Cancel AT ROW 11 COL 64
     tb_fsc AT ROW 6.29 COL 13.6 WIDGET-ID 26
     RECT-42 AT ROW 1.24 COL 5
     SPACE(60.00) SKIP(6.71)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FONT 6
         TITLE "Edit Estimated Sheets for Job/Form"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB BROWSE-4 1 Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BROWSE-4:NUM-LOCKED-COLUMNS IN FRAME Dialog-Frame     = 1.

/* SETTINGS FOR FILL-IN fi_job-no IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH ttItemList.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Edit Estimated Sheets for Job/Form */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEF VAR ld AS DEC NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN 
        lIncludeLastPage = logical(tb_include-page:SCREEN-VALUE) 
        lFSC = LOGICAL(tb_FSC:SCREEN-VALUE)
        cJobType = cbJobType:SCREEN-VALUE
        cRdOptionMclean = rd_active:SCREEN-VALUE 
        .
      IF cJobType EQ "--" THEN cJobType = "".
      FOR EACH ttItemList NO-LOCK:
          FIND FIRST ttSoule EXCLUSIVE-LOCK
              WHERE ttSoule.frm EQ ttItemList.frm 
                AND ttSoule.blank-no EQ ttItemList.blank-no NO-ERROR.
          IF AVAIL ttSoule AND ttItemList.IS-SELECTED THEN
              ASSIGN
              ttSoule.RunForm = YES              
              ttSoule.lPrintDieImage = ttItemList.lDie
              ttSoule.lPrintCadImage = ttItemList.lCad
              ttSoule.lPrintPlateImage = ttItemList.lPlate
              ttSoule.lPrintFGImage = ttItemList.lImage.
          ELSE IF AVAIL ttSoule THEN ttSoule.RunForm = NO.
                     
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rd_active
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rd_active Dialog-Frame
ON LEAVE OF rd_active IN FRAME Dialog-Frame
DO:
        ASSIGN rd_active.
    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_fsc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_fsc Dialog-Frame
ON VALUE-CHANGED OF tb_fsc IN FRAME Dialog-Frame /* FSC */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tb_include-page
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tb_include-page Dialog-Frame
ON VALUE-CHANGED OF tb_include-page IN FRAME Dialog-Frame /* Include Item List Page */
DO:
  assign {&self-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-4
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

  FIND job-hdr WHERE ROWID(job-hdr) EQ ip-rowid NO-LOCK NO-ERROR.

  IF AVAIL job-hdr THEN DO:

      for each bf-job-hdr NO-LOCK 
        where bf-job-hdr.company eq job-hdr.company
          and bf-job-hdr.job     EQ job-hdr.job
          and bf-job-hdr.job-no  EQ job-hdr.job-no
          and bf-job-hdr.job-no2 EQ job-hdr.job-no2:

        RUN pCreateTempTable(bf-job-hdr.i-no,bf-job-hdr.frm,bf-job-hdr.blank-no ) .
          
        fi_job-no = TRIM(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2,"99").
      END.
   
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
  DISPLAY cbJobType fi_job-no rd_active tb_include-page tb_fsc 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-4 cbJobType rd_active tb_include-page Btn_OK Btn_Cancel tb_fsc 
         RECT-42 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateTempTable Dialog-Frame 
PROCEDURE pCreateTempTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcStock AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO .
    DEFINE INPUT PARAMETER ipciBlankNo AS INTEGER NO-UNDO .

    FIND FIRST bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ bf-job-hdr.company
        AND bf-itemfg.i-no    EQ ipcStock
        NO-ERROR.

    CREATE ttItemList.
        ASSIGN
            ttItemList.i-no = ipcStock
            ttItemList.part-no = IF AVAIL bf-itemfg THEN bf-itemfg.part-no ELSE ""
            ttItemList.IS-SELECTED  = YES 
            ttItemList.frm = ipiFormNo
            ttItemList.blank-no = ipciBlankNo 
            ttItemList.combo = (IF AVAIL est AND est.est-type GE 2 THEN YES ELSE NO )
            ttItemList.itemName = IF AVAIL bf-itemfg THEN bf-itemfg.i-name ELSE ""
            ttItemList.lDie = YES
            ttItemList.lCad = YES 
            ttItemList.lPlate = YES
            ttItemList.lImage = YES. 


     CREATE ttSoule.
        ASSIGN
            ttSoule.job-no = bf-job-hdr.job-no
            ttSoule.job-no2 = bf-job-hdr.job-no2
            ttSoule.frm = ipiFormNo
            ttSoule.blank-no = ipciBlankNo
            ttSoule.qty = bf-job-hdr.qty
            ttSoule.i-no = ipcStock
            ttSoule.runForm = YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

