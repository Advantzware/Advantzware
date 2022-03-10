&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS D-Dialog 
/*------------------------------------------------------------------------
  File: est/dboxAltDesign.w
  
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

DEFINE INPUT PARAMETER iprStyleRowid AS ROWID NO-UNDO .
DEFINE INPUT PARAMETER iprEBRowid AS ROWID NO-UNDO .

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i}

{methods/defines/globdefs.i}
{sys/inc/var.i shared}

{system/FormulaProcs.i}

DEFINE VARIABLE iMaxLengthPanel    AS INTEGER   INIT 80 NO-UNDO.
DEFINE VARIABLE hdFormulaProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE cScoreL            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cScoreLTotal       AS CHARACTER NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS btnRebuild btn_right btn_left ~
editor_wcum-score editor_wscore box-image-2 RECT-40 btnCancel 
&Scoped-Define DISPLAYED-OBJECTS iDesignIDAlt cDescription cBox-image ~
clscore clcum-score editor_wcum-score editor_wscore 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 cDescription 
&Scoped-define List-2 clcum-score editor_wcum-score editor_wscore 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "Cancel" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnRebuild 
     LABEL "Rebuild" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_left 
     IMAGE-UP FILE "adm2/image/prev.bmp":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE BUTTON btn_right 
     IMAGE-UP FILE "adm2/image/next.bmp":U
     LABEL "" 
     SIZE 4 BY 1.

DEFINE VARIABLE cbox-text LIKE box-design-hdr.box-text
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 116 BY 12.62
     FONT 2 NO-UNDO.

DEFINE VARIABLE editor_wcum-score AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL
     SIZE 14 BY 12.38
     FONT 2 NO-UNDO.

DEFINE VARIABLE editor_wscore AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-VERTICAL
     SIZE 15 BY 12.38
     FONT 2.

DEFINE VARIABLE cBox-image LIKE box-design-hdr.box-image
     VIEW-AS FILL-IN 
     SIZE 63 BY 1 NO-UNDO.

DEFINE VARIABLE cDescription LIKE box-design-hdr.description
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE VARIABLE clcum-score LIKE box-design-hdr.lcum-score
     VIEW-AS FILL-IN 
     SIZE 116 BY 1
     FONT 2 NO-UNDO.

DEFINE VARIABLE clscore LIKE box-design-hdr.lscore
     VIEW-AS FILL-IN 
     SIZE 116 BY 1
     FONT 2 NO-UNDO.

DEFINE VARIABLE iDesignIDAlt LIKE style.designIDAlt
     VIEW-AS FILL-IN 
     SIZE 6.2 BY 1.1 NO-UNDO.

DEFINE IMAGE box-image-2
     SIZE 117 BY 12.86.

DEFINE RECTANGLE RECT-40
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 149 BY 16.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME D-Dialog
     btnRebuild AT ROW 18.14 COL 48.8 WIDGET-ID 4
     iDesignIDAlt AT ROW 1.24 COL 15 COLON-ALIGNED HELP
          "Enter the alternate design number for this style." WIDGET-ID 2
     cDescription AT ROW 1.24 COL 22 COLON-ALIGNED HELP
          "" NO-LABEL
     cBox-image AT ROW 1.24 COL 75 COLON-ALIGNED HELP
          "" FORMAT "x(200)"
     clscore AT ROW 2.43 COL 2 HELP
          "" NO-LABEL FORMAT "x(210)"
          FONT 2
     btn_right AT ROW 2.43 COL 118
     clcum-score AT ROW 3.38 COL 2 HELP
          "" NO-LABEL FORMAT "x(210)"
          FONT 2
     btn_left AT ROW 3.38 COL 118
     cbox-text AT ROW 4.57 COL 2 HELP
          "" NO-LABEL
          FONT 2
     editor_wcum-score AT ROW 4.81 COL 119 HELP
          "Enter the cumulative width score." NO-LABEL
     editor_wscore AT ROW 4.81 COL 133 NO-LABEL
     btnCancel AT ROW 18.14 COL 69.4 WIDGET-ID 6
     "Total" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 3.33 COL 122
     "W Totals   W Score" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 4.1 COL 123
     "Score:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 2.43 COL 122
     box-image-2 AT ROW 4.57 COL 2
     RECT-40 AT ROW 1 COL 1
     SPACE(0.00) SKIP(2.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         FGCOLOR 1 FONT 6
         TITLE "PO Scores".


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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME D-Dialog:SCROLLABLE       = FALSE
       FRAME D-Dialog:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN cBox-image IN FRAME D-Dialog
   NO-ENABLE LIKE = ASI.box-design-hdr.box-image EXP-FORMAT EXP-HELP EXP-SIZE */
/* SETTINGS FOR EDITOR cbox-text IN FRAME D-Dialog
   NO-DISPLAY NO-ENABLE LIKE = ASI.box-design-hdr.box-text EXP-SIZE     */
ASSIGN 
       cbox-text:HIDDEN IN FRAME D-Dialog           = TRUE
       cbox-text:RETURN-INSERTED IN FRAME D-Dialog  = TRUE.

/* SETTINGS FOR FILL-IN cDescription IN FRAME D-Dialog
   NO-ENABLE 1 LIKE = ASI.box-design-hdr.description EXP-HELP EXP-SIZE  */
/* SETTINGS FOR FILL-IN clcum-score IN FRAME D-Dialog
   NO-ENABLE ALIGN-L 2 LIKE = ASI.box-design-hdr.lcum-score EXP-LABEL EXP-FORMAT EXP-HELP EXP-SIZE */
ASSIGN 
       clcum-score:AUTO-RESIZE IN FRAME D-Dialog      = TRUE.

/* SETTINGS FOR FILL-IN clscore IN FRAME D-Dialog
   NO-ENABLE ALIGN-L LIKE = ASI.box-design-hdr.lscore EXP-LABEL EXP-FORMAT EXP-HELP EXP-SIZE */
ASSIGN 
       clscore:AUTO-RESIZE IN FRAME D-Dialog      = TRUE.

/* SETTINGS FOR EDITOR editor_wcum-score IN FRAME D-Dialog
   2                                                                    */
ASSIGN 
       editor_wcum-score:RETURN-INSERTED IN FRAME D-Dialog  = TRUE
       editor_wcum-score:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR EDITOR editor_wscore IN FRAME D-Dialog
   2                                                                    */
ASSIGN 
       editor_wscore:RETURN-INSERTED IN FRAME D-Dialog  = TRUE
       editor_wscore:READ-ONLY IN FRAME D-Dialog        = TRUE.

/* SETTINGS FOR FILL-IN iDesignIDAlt IN FRAME D-Dialog
   NO-ENABLE LIKE = ASI.style.designIDAlt EXP-HELP EXP-SIZE             */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX D-Dialog
/* Query rebuild information for DIALOG-BOX D-Dialog
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX D-Dialog */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME D-Dialog
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL D-Dialog D-Dialog
ON WINDOW-CLOSE OF FRAME D-Dialog
DO:  
        /* Add Trigger to equate WINDOW-CLOSE to END-ERROR. */
       
        DELETE PROCEDURE hdFormulaProcs.
        APPLY "END-ERROR":U TO SELF.

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel D-Dialog
ON CHOOSE OF btnCancel IN FRAME D-Dialog /* Cancel */
DO:
   apply "window-close" to frame {&frame-name}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRebuild
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRebuild D-Dialog
ON CHOOSE OF btnRebuild IN FRAME D-Dialog /* Rebuild */
DO:
    DO WITH FRAME {&frame-name}:
    END.
  
    RUN pRebuildBox.
        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_left
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_left D-Dialog
ON CHOOSE OF btn_left IN FRAME D-Dialog
DO:
   clscore:SCREEN-VALUE = 
               SUBSTR(cScoreL,1,iMaxLengthPanel).
   clcum-score:SCREEN-VALUE = 
               SUBSTR(cScoreLTotal,1,iMaxLengthPanel).
            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_right
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_right D-Dialog
ON CHOOSE OF btn_right IN FRAME D-Dialog
DO:
  
    ASSIGN  
        clscore:SCREEN-VALUE     = SUBSTR(cScoreL, iMaxLengthPanel + 1,iMaxLengthPanel)
        clcum-score:SCREEN-VALUE = SUBSTR(cScoreLTotal, iMaxLengthPanel + 1,iMaxLengthPanel).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cBox-image
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cBox-image D-Dialog
ON HELP OF cBox-image IN FRAME D-Dialog /* Image File */
DO:
   DEFINE VARIABLE ls-filename AS cha       NO-UNDO.
   DEFINE VARIABLE ll-ok       AS LOG       NO-UNDO.
   DEFINE VARIABLE cInitDir    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llInitDir   AS CHARACTER NO-UNDO.

   RUN sys/ref/nk1look.p (g_company, "DefaultDir", "C", NO, NO, "", "", 
                          OUTPUT cInitDir, OUTPUT llInitDir).
   IF cInitDir NE "" THEN
       ASSIGN
       FILE-INFO:FILE-NAME = cInitDir
      cInitDir            = FILE-INFO:FULL-PATHNAME .
   IF cInitDir = ? THEN cInitDir = "" .

   SYSTEM-DIALOG GET-FILE ls-filename 
                 TITLE "Select Image File to insert"
                 FILTERS "JPG Files    (*.jpg)" "*.jpg",
                         "Bitmap files (*.bmp)" "*.bmp",
                         "JPEG Files   (*.jpeg)" "*.jpeg",
                         "TIF Files    (*.tif)" "*.tif",
                         "All Files    (*.*) " "*.*"
                 INITIAL-DIR cInitDir
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN SELF:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME clscore
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL clscore D-Dialog
ON CURSOR-LEFT OF clscore IN FRAME D-Dialog
DO:

     IF SELF:CURSOR-OFFSET <= 20 THEN DO:
          clcum-score:SCREEN-VALUE = SUBSTRING(cScoreLTotal,1,80)
                     .
      END.
      ELSE IF SELF:CURSOR-OFFSET <= 40 THEN DO:
          clcum-score:SCREEN-VALUE = SUBSTRING(cScoreLTotal,21,80)
                     .
      END.
      ELSE IF SELF:CURSOR-OFFSET <= 50 THEN DO:
          clcum-score:SCREEN-VALUE = SUBSTRING(cScoreLTotal,41,80)
                     .
      END.
      ELSE  DO:
          clcum-score:SCREEN-VALUE = SUBSTRING(cScoreLTotal,51,80)
                     .
      END.
    APPLY LASTKEY .

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL clscore D-Dialog
ON CURSOR-RIGHT OF clscore IN FRAME D-Dialog
DO:

      IF SELF:CURSOR-OFFSET >= 120 THEN DO:
          clcum-score:SCREEN-VALUE = SUBSTRING(cScoreLTotal,51,80)
                     .
      END.
      ELSE IF SELF:CURSOR-OFFSET >= 100 THEN DO:
          clcum-score:SCREEN-VALUE = SUBSTRING(cScoreLTotal,41,80)
                     .
      END.
      ELSE IF SELF:CURSOR-OFFSET >= 80 THEN DO:
          clcum-score:SCREEN-VALUE = SUBSTRING(cScoreLTotal,21,80)
                     .
      END.
      ELSE  DO:
          clcum-score:SCREEN-VALUE = SUBSTRING(cScoreLTotal,1,80)
                     .
      END.

   APPLY LASTKEY. 
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL clscore D-Dialog
ON END OF clscore IN FRAME D-Dialog
DO:
    APPLY LASTKEY.
    clcum-score:SCREEN-VALUE = SUBSTRING(cScoreLTotal,iMaxLengthPanel + 1,iMaxLengthPanel).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL clscore D-Dialog
ON HOME OF clscore IN FRAME D-Dialog
DO:
    APPLY LASTKEY.
    clcum-score:SCREEN-VALUE = SUBSTRING(cScoreLTotal,1,iMaxLengthPanel).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL clscore D-Dialog
ON LEAVE OF clscore IN FRAME D-Dialog
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}
    DEFINE VARIABLE i         AS INTEGER NO-UNDO.
    DEFINE VARIABLE ls-string AS cha     INIT "0,1,2,3,4,5,6,7,8,9" NO-UNDO.
    DO i = 1 TO LENGTH(SELF:screen-value) :
      IF LOOKUP(SUBSTRING(SELF:screen-value,i,1),ls-string) < 0 THEN DO:
         MESSAGE "Invalid Entry. Use Numeric Value Only. " VIEW-AS ALERT-BOX ERROR.
         RETURN NO-APPLY.
      END. 
    END.
    {&methods/lValidateError.i NO}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME editor_wcum-score
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL editor_wcum-score D-Dialog
ON ENTRY OF editor_wcum-score IN FRAME D-Dialog
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK D-Dialog 


/* ***************************  Main Block  *************************** */

{sys/inc/f3helpw.i}
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    /*{src/adm/template/dialogmn.i}*/
    RUN enable_UI.
  
    {methods/nowait.i}     
    DO WITH FRAME {&frame-name}: 
        IF NOT VALID-HANDLE(hdFormulaProcs) THEN
            RUN system/FormulaProcs.p PERSISTENT SET hdFormulaProcs.
         
        RUN pDisplayValue (INPUT iprStyleRowid, INPUT iprEBRowid).
            
    END.
    
    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  DISPLAY iDesignIDAlt cDescription cBox-image clscore clcum-score 
          editor_wcum-score editor_wscore 
      WITH FRAME D-Dialog.
  ENABLE btnRebuild btn_right btn_left editor_wcum-score editor_wscore 
         box-image-2 RECT-40 btnCancel 
      WITH FRAME D-Dialog.
  VIEW FRAME D-Dialog.
  {&OPEN-BROWSERS-IN-QUERY-D-Dialog}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisableField D-Dialog 
PROCEDURE pDisableField :
/*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/       

    DO WITH FRAME {&FRAME-NAME}:
        DISABLE {&ENABLED-FIELDS } .
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisplayValue D-Dialog 
PROCEDURE pDisplayValue PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprStyleID AS ROWID NO-UNDO.
    DEFINE INPUT  PARAMETER iprEBID    AS ROWID NO-UNDO.
    
    DEFINE BUFFER bf-eb  FOR eb.
    
    DO WITH FRAME {&FRAME-NAME}:
    END.
    
    FIND FIRST bf-eb NO-LOCK
        WHERE ROWID(bf-eb) = iprEBID No-ERROR.
    
    FIND FIRST style NO-LOCK 
        WHERE ROWID(style) EQ iprStyleID NO-ERROR.
        
        
    IF NOT AVAILABLE style OR style.formula[20] EQ "" THEN
    DO:
        MESSAGE "PO Scores width Formula not setup on Style"
            VIEW-AS ALERT-BOX.
                
        RETURN.
    END. 
            
    IF AVAILABLE bf-eb THEN
    DO:
        RUN Formula_ParseDesignScores IN hdFormulaProcs (
            INPUT bf-eb.company,
            INPUT bf-eb.est-no,
            INPUT bf-eb.form-no,
            INPUT bf-eb.blank-no,
            INPUT style.designIDAlt,
            INPUT NO,
            OUTPUT TABLE ttScoreLine
            ).
            
        IF NOT CAN-FIND(FIRST ttScoreLine) THEN
        DO:
            MESSAGE "PO Scores not found"
                VIEW-AS ALERT-BOX.
                
            RETURN.
        END.
        
        FIND FIRST box-design-hdr NO-LOCK
            WHERE box-design-hdr.design-no = style.designIDAlt 
            AND box-design-hdr.company = style.company NO-ERROR.
        
        IF AVAILABLE box-design-hdr THEN
        DO:
            
            ASSIGN
                iDesignIDAlt:SCREEN-VALUE = STRING(Style.DesignIDAlt)
                cDescription:SCREEN-VALUE = box-design-hdr.Description
                cBox-image:SCREEN-VALUE   = box-design-hdr.box-image.
                
            IF box-design-hdr.box-image NE "" THEN
                box-image-2:load-image(box-design-hdr.box-image) IN FRAME {&frame-name}.
            
            FOR EACH ttScoreLine NO-LOCK:
                    
                IF ttScoreLine.PanelType = "L" THEN
                    ASSIGN
                        cScoreL                  = ttScoreLine.ScoreLine
                        cScoreLTotal             = ttScoreLine.ScoreLineTotal
                        clscore:SCREEN-VALUE     = SUBSTR(cScoreL,1,iMaxLengthPanel)
                        clcum-score:SCREEN-VALUE = SUBSTR(cScoreLTotal,1,iMaxLengthPanel).
                ELSE
                    ASSIGN
                        editor_wscore:SCREEN-VALUE     = editor_wscore:SCREEN-VALUE + CHR(10) + ttScoreLine.ScoreLine
                        editor_wcum-score:SCREEN-VALUE = editor_wcum-score:SCREEN-VALUE + CHR(10) + ttScoreLine.ScoreLineTotal.
                    
            END. /* FOR EACH ttScoreLine NO-LOCK */
           
        END. /* IF AVAILABLE box-design-hdr THEN  */
        
    END. /* IF AVAILABLE bf-eb THEN */
       

END PROCEDURE.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRebuildBox D-Dialog
PROCEDURE pRebuildBox PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    MESSAGE "This process will rebuild and save the PO Scores. Are you sure to proceed?"
        VIEW-AS ALERT-BOX QUESTION 
        BUTTONS YES-NO UPDATE lChoice AS LOGICAL.
  
    IF NOT lChoice THEN
        RETURN.
           
    /* Build panelHeader and paneDetail records for vaiable width */
    RUN Formula_ReBuildAndSavePanelDetailsForEstimate IN hdFormulaProcs (
        INPUT iprEBRowid
        ).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME





