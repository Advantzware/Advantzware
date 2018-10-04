&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers\attach.w

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

{custom/globdefs.i}

DEF VAR v-est-no AS cha NO-UNDO.
DEF VAR v-i-no AS cha NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR lv-saletool AS CHAR NO-UNDO.
DEF VAR ll-saletool AS LOG NO-UNDO.
DEF VAR llRecFound AS LOG NO-UNDO.
DEF VAR cReturn AS CHAR NO-UNDO.
DEF VAR poPaperClip-int AS INT NO-UNDO.
DEF VAR v-rec-key-list AS CHAR NO-UNDO.

PROCEDURE ShellExecuteA EXTERNAL "shell32":u :
      define input parameter hwnd as long.
      define input parameter lpOperation as char.
      define input parameter lpFile as char.
      define input parameter lpParameters as char.
      define input parameter lpDirectory as char.
      define input parameter nShowCmd as long.
      define return parameter hInstance as long.
END PROCEDURE.

/* Simple use */
    RUN sys/ref/nk1look.p (g_company, "POPaperClip", "I", no, no, "", "", 
                          Output cReturn, output llRecFound).


If llRecFound then
        poPaperClip-INT = INT(cReturn).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES attach
&Scoped-define FIRST-EXTERNAL-TABLE attach


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR attach.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS attach.attach-file attach.run-application ~
attach.est-no attach.i-no 
&Scoped-define ENABLED-TABLES attach
&Scoped-define FIRST-ENABLED-TABLE attach
&Scoped-Define ENABLED-OBJECTS RECT-8 
&Scoped-Define DISPLAYED-FIELDS attach.attach-file attach.run-application ~
attach.run-program attach.est-no attach.i-no attach.creat-date 
&Scoped-define DISPLAYED-TABLES attach
&Scoped-define FIRST-DISPLAYED-TABLE attach


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-ASSIGN-FIELDS attach.run-program attach.creat-date 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "",
     Keys-Supplied = ""':U).
/**************************
</EXECUTING-CODE> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 7.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     attach.attach-file AT ROW 1.29 COL 22 COLON-ALIGNED FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 117 BY 1
     attach.run-application AT ROW 2.67 COL 22.2 COLON-ALIGNED
          LABEL "Open with"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "Word ","Excel ","Acrobat ","MS Paint ","Photo Shop ","Notepad    ","Wordpad   ","Internet Explorer","Windows Default" 
          DROP-DOWN-LIST
          SIZE 31 BY 1
     attach.run-program AT ROW 4.1 COL 22 COLON-ALIGNED NO-LABEL FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 115 BY 1
     attach.est-no AT ROW 5.52 COL 22 COLON-ALIGNED FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     attach.i-no AT ROW 5.52 COL 57 COLON-ALIGNED
          LABEL "FG Item#"
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     attach.creat-date AT ROW 5.52 COL 122 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     RECT-8 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.attach
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 9.48
         WIDTH              = 141.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN attach.attach-file IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN attach.creat-date IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN attach.est-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN attach.i-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR COMBO-BOX attach.run-application IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN attach.run-program IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT                                     */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME attach.attach-file
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL attach.attach-file V-table-Win
ON HELP OF attach.attach-file IN FRAME F-Main /* Attached File */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.
   DEF VAR cInitDir AS CHARACTER NO-UNDO.
   DEF VAR llInitDir AS CHARACTER NO-UNDO.

   RUN sys/ref/nk1look.p (g_company, "DefaultDir", "C", no, no, "", "", 
                          Output cInitDir, output llInitDir).
   IF cInitDir NE "" THEN
       ASSIGN
       FILE-INFO:FILE-NAME = cInitDir
      cInitDir = FILE-INFO:FULL-PATHNAME .
   IF cInitDir = ? THEN cInitDir = "" .

   system-dialog get-file ls-filename 
                 title "Select Image File to insert"
                 filters "All Files    (*.*) " "*.*",
                         "JPG Files    (*.jpg)" "*.jpg",
                         "Bitmap files (*.bmp)" "*.bmp",
                         "JPEG Files   (*.jpeg)" "*.jpeg",
                         "Rich Test Files (*.rtf)" "*.rtf",
                         "MS Word Files  (*.doc)" "*.doc",
                         "MS Word Template Files (*.dot)" "*.dot",
                         "MS Word 2007 Files  (*.docx)" "*.docx",
                         "MS Word 2007 Template Files (*.dotx)" "*.dotx",
                         "MS Excel Files  (*.xls)" "*.xls",
                         "MS Excel 2007 Files  (*.xlsx)" "*.xlsx",
                         "Adobe PDF Files (*.pdf)" "*.pdf"                         
                 initial-dir cInitDir 
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN self:screen-value = ls-filename.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL attach.attach-file V-table-Win
ON LEAVE OF attach.attach-file IN FRAME F-Main /* Attached File */
DO:
    DEF VAR lv-cmd AS cha NO-UNDO.

    IF R-INDEX(SELF:SCREEN-VALUE,".") > 0 THEN 
      CASE SUBSTRING(self:screen-value,R-INDEX(SELF:SCREEN-VALUE,".") + 1)  :
          WHEN "doc" OR WHEN "docx" OR
          WHEN "dot" OR WHEN "dotx" THEN ASSIGN ATTACH.run-application:SCREEN-VALUE = "Word"
                                                ATTACH.run-program:SCREEN-VALUE = "Winword.exe"
                                                ATTACH.run-program:SENSITIVE = NO.
          WHEN "xls" OR WHEN "xlsx" THEN ASSIGN ATTACH.run-application:SCREEN-VALUE = "Excel" 
                                                ATTACH.run-program:SCREEN-VALUE = "Excel.exe"
                                                ATTACH.run-program:SENSITIVE = NO.
          WHEN "pdf" THEN DO:
              RUN custom/runapdf.p (OUTPUT lv-cmd).
              ASSIGN ATTACH.run-application:SCREEN-VALUE = "Acrobat" 
                     ATTACH.run-program:SCREEN-VALUE = lv-cmd 
                     ATTACH.run-program:SENSITIVE = NO.
          END.
          WHEN "jpg" OR WHEN "bmp" THEN ASSIGN ATTACH.run-application:SCREEN-VALUE = "MS Paint"
                                               ATTACH.run-program:SCREEN-VALUE = "mspaint.exe"
                                               ATTACH.run-program:SENSITIVE = NO.
          WHEN "txt" THEN ASSIGN ATTACH.run-application:SCREEN-VALUE = "Notepad"
                                 ATTACH.run-program:SCREEN-VALUE = "notepad.exe"
                                 ATTACH.run-program:SENSITIVE = NO .
          WHEN "html" OR WHEN "htm" THEN ASSIGN ATTACH.run-application:SCREEN-VALUE = "Internet Explorer"
                                                ATTACH.run-program:SCREEN-VALUE = "iexplore.exe"
                                                ATTACH.run-program:SENSITIVE = NO.
          OTHERWISE DO:
              ASSIGN ATTACH.run-application:SCREEN-VALUE = "Windows Default"
                     ATTACH.run-program:SCREEN-VALUE = ""
                     ATTACH.run-program:SENSITIVE = YES.
          END.
      END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME attach.est-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL attach.est-no V-table-Win
ON HELP OF attach.est-no IN FRAME F-Main /* Estimate # */
DO:
   DEF VAR char-val AS cha NO-UNDO.

   RUN windows/l-est.w (g_company,"",SELF:SCREEN-VALUE,OUTPUT char-val).
   IF char-val <> "" THEN DO:
      FIND FIRST eb WHERE RECID(eb) = INT(char-val) NO-LOCK NO-ERROR.
      SELF:SCREEN-VALUE = IF AVAIL eb THEN eb.est-no ELSE "".

   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL attach.est-no V-table-Win
ON LEAVE OF attach.est-no IN FRAME F-Main /* Estimate # */
DO:
  IF LASTKEY <> -1 THEN DO:
      IF LENGTH(SELF:SCREEN-VALUE) < 8 THEN
          SELF:SCREEN-VALUE = FILL(" ",8 - LENGTH(SELF:SCREEN-VALUE)) + SELF:SCREEN-VALUE.
       RUN validate-est-no NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME attach.i-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL attach.i-no V-table-Win
ON HELP OF attach.i-no IN FRAME F-Main /* FG Item# */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    RUN windows/l-itemfg.w (g_company,"",SELF:SCREEN-VALUE,OUTPUT char-val).
    IF char-val <> "" THEN SELF:SCREEN-VALUE = ENTRY(1,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL attach.i-no V-table-Win
ON LEAVE OF attach.i-no IN FRAME F-Main /* FG Item# */
DO:
    IF LASTKEY <> -1 THEN DO:
       RUN validate-i-no NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME attach.run-application
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL attach.run-application V-table-Win
ON VALUE-CHANGED OF attach.run-application IN FRAME F-Main /* Open with */
DO:
   IF SELF:SCREEN-VALUE = "Windows Default" THEN 
      ASSIGN 
        ATTACH.run-program:SCREEN-VALUE = ""
        ATTACH.run-program:SENSITIVE = YES.
   ELSE ASSIGN ATTACH.run-program:SENSITIVE = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME attach.run-program
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL attach.run-program V-table-Win
ON HELP OF attach.run-program IN FRAME F-Main /* Program to excute */
DO:
   def var ls-filename as cha no-undo.
   def var ll-ok as log no-undo.

   system-dialog get-file ls-filename 
                 title "Select Application to open with"
                 filters "Executable Files (*.exe)" "*.exe", 
                         "All Files    (*.*) " "*.*"
                 initial-dir ".\"
                 MUST-EXIST
                 USE-FILENAME
                 UPDATE ll-ok.

    IF ll-ok THEN self:screen-value = CHR(34) + ls-filename + CHR(34).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL attach.run-program V-table-Win
ON LEAVE OF attach.run-program IN FRAME F-Main /* Program to excute */
DO:
  DO WITH FRAME {&FRAME-NAME}:
   IF ATTACH.run-program:SCREEN-VALUE <> "" THEN
    IF SUBSTRING(TRIM(ATTACH.run-program:SCREEN-VALUE),1,1) NE CHR(34) AND
      SUBSTRING(TRIM(ATTACH.run-program:SCREEN-VALUE),LENGTH(TRIM(ATTACH.run-program:SCREEN-VALUE)),1) NE CHR(34) THEN DO:
      ATTACH.run-program:SCREEN-VALUE = CHR(34) + ATTACH.run-program:SCREEN-VALUE + CHR(34).
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
PROCEDURE WinExec EXTERNAL "KERNEL32.DLL":
       DEFINE INPUT PARAMETER programname AS cha.
       DEFINE INPUT PARAMETER visualstyle AS long.
       DEFINE RETURN PARAM statuscode AS LONG.
   END.       


  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "attach"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "attach"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excute-program V-table-Win 
PROCEDURE excute-program :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR lv-cmd AS cha NO-UNDO.
   DEF VAR tInt As Int No-undo.
   DEF VAR ls-image1 AS cha NO-UNDO.
   DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.

   lv-cmd = chr(34) + ATTACH.attach-file + " " + CHR(34).
   RUN ShellExecuteA(0, "open", lv-cmd, "", "", 0, OUTPUT tInt).
   IF tInt LE 32 THEN
   DO:
      IF ATTACH.run-program <> "" THEN do:

          OS-COMMAND SILENT START value(trim(ATTACH.run-program)) value(lv-cmd).
      END.
      ELSE DO:
        ASSIGN ls-image1 = ATTACH.attach-file 
               FILE-INFO:FILE-NAME = ls-image1
               ls-full-img1 = FILE-INFO:FULL-PATHNAME .
         OS-COMMAND START value(ls-full-img1)  .
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-this-saletool V-table-Win 
PROCEDURE is-this-saletool :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-saletool AS LOG NO-UNDO.


  op-saletool = ll-saletool.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rec_key AS cha NO-UNDO.
  DEF BUFFER bf2-oe-ordl FOR oe-ordl.
  DEF BUFFER bf3-oe-ordl FOR oe-ordl.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT ll-saletool THEN DO:
    ASSIGN v-est-no = ""
           v-i-no = "".

    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
    RUN get-ip-rec_key IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rec_key).

    FIND FIRST est WHERE est.rec_key = lv-rec_key NO-LOCK NO-ERROR.

    IF AVAIL est THEN DO:
       v-est-no = est.est-no.
       FOR EACH eb fields(stock-no) WHERE
           eb.company = est.company AND
           eb.est-no = est.est-no NO-LOCK :
           IF eb.stock-no <> "" THEN v-i-no = v-i-no + eb.stock-no + ",". 
       END.

    END.
    ELSE DO:
        FIND FIRST oe-ord WHERE
             oe-ord.rec_key EQ lv-rec_key
             NO-LOCK NO-ERROR.

        IF AVAIL oe-ord THEN
        DO:
            FIND FIRST est WHERE
                 est.company EQ oe-ord.company AND
                 est.est-no EQ oe-ord.est-no
                 NO-LOCK NO-ERROR.

            IF AVAIL est THEN DO:
               v-est-no = est.est-no.
               FOR EACH eb FIELDS(stock-no) WHERE
                   eb.company = est.company AND
                   eb.est-no = est.est-no AND
                   eb.stock-no NE "" NO-LOCK :
                   v-i-no = v-i-no + eb.stock-no + ",". 
               END.
            END.

        END.
        ELSE
        DO:
          FIND FIRST itemfg WHERE itemfg.rec_key = lv-rec_key NO-LOCK NO-ERROR.    
          IF AVAIL itemfg THEN DO:          
              ASSIGN
                 v-i-no = itemfg.i-no
                 v-est-no = itemfg.est-no.
          END.
          ELSE IF CONNECTED("emptrack") THEN
              RUN browsers/addonatt.p(INPUT lv-rec_key,
                                         OUTPUT v-est-no,
                                         OUTPUT v-i-no,
                                         OUTPUT v-rec-key-list).             

          IF v-i-no = "" THEN DO:

              FIND FIRST bf2-oe-ordl WHERE
                 bf2-oe-ordl.rec_key EQ lv-rec_key
                 NO-LOCK NO-ERROR.

              IF AVAIL bf2-oe-ordl THEN DO:

                 FOR EACH bf3-oe-ordl WHERE
                     bf3-oe-ordl.company EQ bf2-oe-ordl.company AND
                     bf3-oe-ordl.ord-no EQ bf2-oe-ordl.ord-no
                     NO-LOCK:

                     v-i-no = v-i-no + bf3-oe-ordl.i-no + ",".
                 END.

              END.
          END. /* v-i-no = "" */
        END. /* not avail oe-ord */
    END. /* not avail est */
    ASSIGN ATTACH.creat-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY)
           ATTACH.est-no:SCREEN-VALUE = v-est-no
           ATTACH.i-no:SCREEN-VALUE = entry(1,v-i-no)
           ATTACH.run-application:SCREEN-VALUE = "Windows Default"
           ATTACH.run-program:SENSITIVE = YES. .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-rec_key AS cha NO-UNDO.
  DEF VAR lv-link-file AS cha NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF adm-adding-record THEN DO:
    IF ll-saletool THEN DO:
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"saletool-source", OUTPUT char-hdl).
      RUN get-saletool IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-saletool).
      ASSIGN
       attach.est-no = lv-saletool
       attach.i-no   = lv-saletool.
    END.

    ELSE DO:
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
      RUN get-ip-rec_key IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rec_key).
      ASSIGN attach.rec_key = lv-rec_key.
      FIND FIRST itemfg WHERE itemfg.rec_key = lv-rec_key NO-LOCK NO-ERROR.

      IF AVAIL itemfg AND poPaperClip-INT EQ 1 THEN
        RUN po/copyItemAttach.p (INPUT "Item", INPUT attach.attach-file, INPUT ROWID(itemfg)).
    END.

    attach.company = g_company.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ATTACH.run-program:SENSITIVE IN FRAME {&FRAME-NAME} = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  {custom/askdel.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    IF ll-saletool THEN
      ASSIGN
       attach.est-no:SENSITIVE = NO
       attach.i-no:SENSITIVE   = NO.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"saletool-source", OUTPUT char-hdl).

  ll-saletool = VALID-HANDLE(WIDGET-HANDLE(char-hdl)).

  DO WITH FRAME {&FRAME-NAME}:
    IF ll-saletool THEN
      ASSIGN
       attach.est-no:HIDDEN = YES
       attach.i-no:HIDDEN   = YES.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN validate-est-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN.
  IF ATTACH.est-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" OR 
     ATTACH.i-no:SCREEN-VALUE <> "" THEN DO:
     RUN validate-i-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ATTACH.run-program:SENSITIVE = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "attach"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE    NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.

  CASE p-state:
      /* Object instance CASEs can go here to replace standard behavior
         or add new cases. */
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-est-no V-table-Win 
PROCEDURE validate-est-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
IF NOT ll-saletool THEN DO:
  IF LASTKEY = -1 and
     ATTACH.est-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
     ATTACH.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN
  DO:
      MESSAGE "Estimate# and FG Item# cannot blank. Enter either or both."           
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ATTACH.est-no.
      RETURN ERROR.
  END.

  IF ATTACH.est-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "" AND
       NOT CAN-FIND(FIRST est WHERE est.company = g_company AND 
               est.est-no = ATTACH.est-no:SCREEN-VALUE IN FRAME {&FRAME-NAME})
  THEN DO:
      MESSAGE "Invalid Estimate#. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ATTACH.est-no.
      RETURN ERROR.
  END.
END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-i-no V-table-Win 
PROCEDURE validate-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
IF NOT ll-saletool THEN DO:
  IF ATTACH.est-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND
     ATTACH.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN
  DO:
      MESSAGE "Estimate# and FG Item# cannot blank. Enter either or both."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ATTACH.est-no.
      RETURN ERROR.
  END.

  IF attach.i-no:SCREEN-VALUE IN FRAME {&frame-name} <> "" and
     NOT CAN-FIND(FIRST itemfg WHERE itemfg.company = g_company AND 
               itemfg.i-no = ATTACH.i-no:SCREEN-VALUE IN FRAME {&FRAME-NAME})
  THEN DO:
      MESSAGE "Invalid FG Item#. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ATTACH.i-no.
      RETURN ERROR.
  END.
END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

