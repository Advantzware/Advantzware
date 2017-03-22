&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/attachpo.w

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
DEFINE VARIABLE ip-rec_key AS CHARACTER NO-UNDO.
{po/d-cp-att.i NEW}
{custom/globdefs.i}
{sys/inc/VAR.i}
{sys/inc/varasgn.i}

DEF VAR v-po-no AS cha NO-UNDO.
DEF VAR v-rec-key-list AS CHAR NO-UNDO.
DEF VAR  h_sel-att AS HANDLE NO-UNDO.
&SCOPED-DEFINE BRWSDEFS attachpo

PROCEDURE ShellExecuteA EXTERNAL "shell32":u :
      define input parameter hwnd as long.
      define input parameter lpOperation as char.
      define input parameter lpFile as char.
      define input parameter lpParameters as char.
      define input parameter lpDirectory as char.
      define input parameter nShowCmd as long.
      define return parameter hInstance as long.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartNavBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target,Navigation-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME Browser-Table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES attach

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table attach.attach-file ~
attach.est-no attach.creat-date 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH attach WHERE ~{&KEY-PHRASE} ~
      AND (attach.company = g_company and ~
attach.rec_key eq v-rec-key-list and ~
v-po-no <> "" and attach.est-no = v-po-no) ~
 NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH attach WHERE ~{&KEY-PHRASE} ~
      AND (attach.company = g_company and ~
attach.rec_key eq v-rec-key-list and ~
v-po-no <> "" and attach.est-no = v-po-no) ~
 NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table attach
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table attach


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 btnRun browse-order ~
btCopyFromItem 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD i-no-pos B-table-Win 
FUNCTION i-no-pos RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btCopyFromItem 
     LABEL "Copy From Item Master" 
     SIZE 27 BY 1.14.

DEFINE BUTTON btnRun 
     IMAGE-UP FILE "Graphics/32x32/media_play.ico":U
     IMAGE-INSENSITIVE FILE "Graphics/32x32/inactive.png":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 7.8 BY 1.38 TOOLTIP "Run Program".

DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 41 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      attach
    FIELDS(attach.attach-file
      attach.est-no
      attach.creat-date) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      attach.attach-file FORMAT "x(50)":U
      attach.est-no COLUMN-LABEL "PO #" FORMAT "x(8)":U
      attach.creat-date FORMAT "99/99/9999":U WIDTH 16.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 16.67
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     btnRun AT ROW 17.67 COL 44
     browse-order AT ROW 17.91 COL 2 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 17.91 COL 66.2 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.91 COL 97.2 HELP
          "CLEAR AUTO FIND Value"
     btCopyFromItem AT ROW 17.91 COL 114 WIDGET-ID 2
     RECT-4 AT ROW 17.67 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 19.52
         WIDTH              = 145.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{methods/template/browser.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB Browser-Table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN auto_find IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       auto_find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RADIO-SET browse-order IN FRAME F-Main
   NO-DISPLAY                                                           */
/* SETTINGS FOR BUTTON Btn_Clear_Find IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Clear_Find:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.attach"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _Where[1]         = "(attach.company = g_company and
attach.rec_key eq v-rec-key-list and
v-po-no <> """" and attach.est-no = v-po-no)
"
     _FldNameList[1]   = asi.attach.attach-file
     _FldNameList[2]   > asi.attach.est-no
"attach.est-no" "PO #" "x(8)" "character" ? ? ? ? ? ? no "Enter PO Number." no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.attach.creat-date
"attach.creat-date" ? ? "date" ? ? ? ? ? ? no ? no no "16.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE Browser-Table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME Browser-Table
&Scoped-define SELF-NAME Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-ENTRY OF Browser-Table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON ROW-LEAVE OF Browser-Table IN FRAME F-Main
DO:
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON VALUE-CHANGED OF Browser-Table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
  {methods/template/local/setvalue.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btCopyFromItem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btCopyFromItem B-table-Win
ON CHOOSE OF btCopyFromItem IN FRAME F-Main /* Copy From Item Master */
DO:
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR cont-hdl AS HANDLE NO-UNDO.
  DEF VAR lv-rec_key AS CHAR NO-UNDO.
  DEF VAR v-sel-item AS CHAR NO-UNDO.
  DEF VAR v-i-no AS CHAR NO-UNDO.
  v-i-no = "".
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
    RUN get-ip-rec_key IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rec_key).
    
    FIND FIRST po-ordl WHERE po-ordl.rec_key = lv-rec_key NO-LOCK NO-ERROR.
    IF AVAIL po-ordl THEN
        v-i-no = po-ordl.i-no.

  FIND FIRST po-ord WHERE po-ord.rec_key EQ v-rec-key-list NO-LOCK NO-ERROR.

  IF AVAIL po-ord THEN DO:
     FOR EACH po-ordl WHERE po-ordl.company = po-ord.company
          AND po-ordl.po-no  = po-ord.po-no
          AND (IF v-i-no GT "" THEN po-ordl.i-no EQ v-i-no
              ELSE TRUE)
          NO-LOCK:
         FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
                             AND itemfg.i-no    = po-ordl.i-no
                           NO-LOCK NO-ERROR.
         IF AVAIL itemfg THEN DO:
            v-sel-item = itemfg.i-no /* itemfg.rec_key */. 
            LEAVE.
         END.
     END.

     RUN po/d-cp-att.w (INPUT v-sel-item).
     RUN set-copy-list.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnRun
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnRun B-table-Win
ON CHOOSE OF btnRun IN FRAME F-Main /* Run */
DO:
  RUN call-attach.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available B-table-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE call-attach B-table-Win 
PROCEDURE call-attach :
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
  RUN ShellExecuteA(0, "open", ATTACH.attach-file, "", "", 0, OUTPUT tInt).
  IF tInt LE 32 THEN
   DO:
   IF ATTACH.run-program <> "" THEN do:       
          OS-COMMAND SILENT START value(trim(ATTACH.run-program)) value(lv-cmd).
      END.
      ELSE DO:
        ASSIGN ls-image1 = ATTACH.attach-file 
               FILE-INFO:FILE-NAME = ls-image1
               ls-full-img1 = FILE-INFO:FULL-PATHNAME .
         OS-COMMAND SILENT START value(ls-full-img1)  .
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI B-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
     v-po-no = ""
     v-rec-key-list = "".

  {methods/run_link.i "CONTAINER-SOURCE" "Get-ip-rec_key" "(OUTPUT ip-rec_key)"}

  {sys/ref/attachpologic.i}
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records B-table-Win  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-copy-list B-table-Win 
PROCEDURE set-copy-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR lv-rec_key AS CHAR NO-UNDO.
DEF VAR v-po-no AS CHAR NO-UNDO.
DEF VAR i AS INT.
DEF BUFFER bf-attach FOR ATTACH.
DEF BUFFER bf-item-attach FOR ATTACH.
DEF VAR lv-from-rec_key AS CHAR NO-UNDO.
/* Accept list of attachments to copy from itemfg and create in PO */

FOR EACH tt-mis WHERE tt-mis.selekt = YES:
    
    FIND bf-item-attach WHERE ROWID(bf-item-attach) = tt-mis.sel-rowid
         NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-item-attach THEN
        NEXT.
    
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
    RUN get-ip-rec_key IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rec_key).

    FIND FIRST po-ord WHERE po-ord.rec_key = lv-rec_key NO-LOCK NO-ERROR.
    
    IF NOT AVAIL po-ord THEN
    DO:
       FIND FIRST po-ordl WHERE po-ordl.rec_key = lv-rec_key NO-LOCK NO-ERROR.
  
       IF AVAIL po-ordl THEN
       DO:
          FIND FIRST po-ord WHERE
               po-ord.company EQ po-ordl.company AND
               po-ord.po-no EQ po-ordl.po-no
               NO-LOCK no-error.

          IF AVAIL po-ord THEN
             lv-rec_key = po-ord.rec_key.
       END.
          
    END.

    FIND FIRST bf-attach WHERE bf-attach.rec_key EQ lv-rec_key 
                           AND bf-attach.attach-file = bf-item-attach.attach-file
                         NO-LOCK NO-ERROR.

    /* Already Attached? */
    IF AVAIL bf-attach THEN
        NEXT.
    
    CREATE bf-attach.
    BUFFER-COPY bf-item-attach EXCEPT rec_key TO bf-attach.

    ASSIGN bf-attach.rec_key = lv-rec_key
           bf-attach.company = g_company.

  ASSIGN v-po-no = "".

/*   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl). */
/*   RUN get-ip-rec_key IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rec_key).                         */
  
/*   FIND FIRST po-ord WHERE po-ord.rec_key = lv-rec_key NO-LOCK NO-ERROR. */
  
  IF AVAIL po-ord THEN
     v-po-no = STRING(po-ord.po-no).
  ELSE
  DO:
     FIND FIRST po-ordl WHERE po-ordl.rec_key = lv-rec_key NO-LOCK NO-ERROR.
  
     IF AVAIL po-ordl THEN
     DO:
        FIND FIRST po-ord WHERE
             po-ord.company EQ po-ordl.company AND
             po-ord.po-no EQ po-ordl.po-no
             NO-LOCK no-error.

        IF AVAIL po-ord THEN
           v-po-no = STRING(po-ord.po-no).
     END.
  END.

  
  ASSIGN bf-attach.creat-date = TODAY
         bf-attach.est-no = v-po-no.
END.
RUN adm-open-query IN THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed B-table-Win 
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
      {src/adm/template/bstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION i-no-pos B-table-Win 
FUNCTION i-no-pos RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN trim(attach.est-no) = trim(v-po-no).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

