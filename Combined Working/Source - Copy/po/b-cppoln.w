&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File:  browsers/<table>.w

  Description: from BROWSER.W - Basic SmartBrowser Object Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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

{custom/globdefs.i}
{sys/inc/VAR.i}
{sys/inc/varasgn.i}

DEF VAR v-est-no AS cha NO-UNDO.
DEF VAR v-i-no AS cha NO-UNDO.
DEF VAR v-i-no-pos AS LOG NO-UNDO.
DEF VAR v-rec-key-list AS CHAR NO-UNDO.
DEF VAR ig AS INT.
DEF VAR ip-company AS CHAR NO-UNDO.
DEF VAR ip-po-no   AS INT NO-UNDO.
DEF VAR h-container AS HANDLE NO-UNDO.
DEF VAR v-order-list AS CHAR NO-UNDO.
DEF VAR v-first-disable-fields AS LOG NO-UNDO.


DEF VAR v-current-row AS ROWID NO-UNDO.
DEF TEMP-TABLE po-ordl LIKE po-ordl.
{fg/fullset.i NEW}

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
&Scoped-define INTERNAL-TABLES po-ordl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table po-ordl.line po-ordl.ord-no ~
po-ordl.cust-no po-ordl.i-no po-ordl.i-name 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table po-ordl.ord-no 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table po-ordl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table po-ordl
&Scoped-define QUERY-STRING-Browser-Table FOR EACH po-ordl WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH po-ordl WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table po-ordl
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table po-ordl


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnRun Browser-Table RECT-4 browse-order ~
auto_find Btn_Clear_Find 

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
DEFINE BUTTON btnRun 
     IMAGE-UP FILE "Graphics/32x32/media_play.ico":U NO-FOCUS FLAT-BUTTON
     LABEL "&Run" 
     SIZE 7 BY 1.38.

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
     SIZE 75 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      po-ordl
    FIELDS(po-ordl.line
      po-ordl.ord-no
      po-ordl.cust-no
      po-ordl.i-no
      po-ordl.i-name) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      po-ordl.line FORMAT "99":U WIDTH 6.2
      po-ordl.ord-no FORMAT ">>>>>9":U WIDTH 31.4
      po-ordl.cust-no FORMAT "x(8)":U WIDTH 14
      po-ordl.i-no FORMAT "x(15)":U
      po-ordl.i-name FORMAT "x(30)":U
  ENABLE
      po-ordl.ord-no
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 16.67
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btnRun AT ROW 17.67 COL 84
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 17.91 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 17.91 COL 101 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 17.91 COL 132 HELP
          "CLEAR AUTO FIND Value"
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
         HEIGHT             = 19.57
         WIDTH              = 145.6.
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
   NO-DISPLAY                                                           */
ASSIGN 
       auto_find:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR RADIO-SET browse-order IN FRAME F-Main
   NO-DISPLAY                                                           */
ASSIGN 
       browse-order:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       btnRun:HIDDEN IN FRAME F-Main           = TRUE.

ASSIGN 
       Btn_Clear_Find:HIDDEN IN FRAME F-Main           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.po-ordl"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _FldNameList[1]   > asi.po-ordl.line
"po-ordl.line" ? ? "integer" ? ? ? ? ? ? no ? no no "6.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > asi.po-ordl.ord-no
"po-ordl.ord-no" ? ? "integer" ? ? ? ? ? ? yes ? no no "31.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.po-ordl.cust-no
"po-ordl.cust-no" ? ? "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = asi.po-ordl.i-no
     _FldNameList[5]   = asi.po-ordl.i-name
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
ON HELP OF Browser-Table IN FRAME F-Main
DO:
DEF VAR lv-item-type AS LOG.
DEF VAR lv-i-no AS CHAR.
DEF VAR lw-focus AS HANDLE.
DEF VAR char-val AS CHAR.
DEF VAR look-recid AS RECID.
     lw-focus  = FOCUS.

    DO WITH FRAME {&FRAME-NAME}:

        
            /* gdm - CHANGED TO ACCOMODATE RM vs FG LOOKUP */
            ASSIGN
              lv-item-type = po-ordl.item-type
              lv-i-no = po-ordl.i-no.       
/*             IF NOT lv-item-type                                                                                                          */
/*               THEN                                                                                                                       */
/*                 RUN windows/l-ordlno.w(g_company, po-ordl.cust-no,"",lv-i-no,lw-focus:SCREEN-VALUE, OUTPUT char-val,OUTPUT look-recid ). */
/*               ELSE                                                                                                                       */
/*                 RUN windows/l-ordmno.w(g_company, po-ordl.cust-no,"",lv-i-no,lw-focus:SCREEN-VALUE, OUTPUT char-val,OUTPUT look-recid ). */
            RUN windows/l-ordlno.w(g_company, 
                                   po-ordl.cust-no,
                                   "",
                                   lv-i-no,
                                   "", 
                                   OUTPUT char-val,
                                   OUTPUT look-recid ).

            IF char-val <> "" THEN ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                                          /*po-ordl.cust-no  = ENTRY(2,char-val) */.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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

/*     /* Do not disable this code or no updates will take place except */
/*      by pressing the Save button on an Update SmartPanel. */         */
   /*  {src/adm/template/brsleave.i}                                      */
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


&Scoped-define SELF-NAME po-ordl.ord-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ordl.ord-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF po-ordl.ord-no IN BROWSE Browser-Table /* Customer Order Number */
DO:
    DEF VAR a AS CHAR NO-UNDO.
    DEF VAR b AS CHAR NO-UNDO.

    IF LASTKEY NE -1 THEN DO:
        RUN valid-order NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
        IF AVAIL po-ordl THEN
            po-ordl.ord-no = integer(po-ordl.ord-no:SCREEN-VALUE IN BROWSE Browser-Table).
        IF (LASTKEY EQ 13 OR LASTKEY = 9) THEN DO:
            a = po-ordl.LINE:SCREEN-VALUE IN BROWSE Browser-Table.         
            RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
            run get-link-handle in adm-broker-hdl(this-procedure,"tableio-source", output char-hdl).
            b = po-ordl.LINE:SCREEN-VALUE IN BROWSE Browser-Table.

            IF a = b THEN DO:
              APPLY 'down-arrow' TO BROWSE Browser-Table. 
              b = po-ordl.LINE:SCREEN-VALUE IN BROWSE Browser-Table.              
            END.
                
            IF integer(b) - integer(a) GT 1 THEN
                APPLY 'up-arrow' TO BROWSE Browser-Table.  

            b = po-ordl.LINE:SCREEN-VALUE IN BROWSE Browser-Table.
            IF a NE b THEN DO:
                APPLY 'up-arrow' TO BROWSE Browser-Table.
                run start-update in widget-handle(char-hdl).
            END.
              
        END.

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

      /*
run get-link-handle in adm-broker-hdl(this-procedure,"consource-source",output char-hdl).
      
h-container = HANDLE(char-hdl). */
DEFINE VARIABLE hProc AS HANDLE NO-UNDO.
hProc = SESSION:FIRST-PROCEDURE.
DO WHILE VALID-HANDLE(hProc):

    IF index(hProc:FILE-NAME, "v-purord") GT 0 THEN
        LEAVE. /* found it. */
    hProc = hProc:NEXT-SIBLING.
END.

IF VALID-HANDLE(hProc) THEN DO:
    h-container =  hProc.
END.


IF VALID-HANDLE(h-container) THEN
RUN get-parameters IN h-container (OUTPUT ip-company, OUTPUT ip-po-no).


FOR EACH asi.po-ordl WHERE asi.po-ordl.company = ip-company
                       AND asi.po-ordl.po-no   = ip-po-no NO-LOCK.
   ig = ig + 1.
   IF ig GT 500 THEN
       LEAVE.
   CREATE po-ordl.
   BUFFER-COPY  asi.po-ordl EXCEPT ASI.po-ordl.ord-no TO po-ordl .
END.

v-first-disable-fields = YES.
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

  lv-cmd = chr(34) + ATTACH.attach-file + " " + CHR(34).
  OS-COMMAND /*NO-WAIT*/ SILENT START value(trim(ATTACH.run-program)) value(lv-cmd). 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-results B-table-Win 
PROCEDURE get-results :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER op-order-val-list AS CHAR NO-UNDO.
FOR EACH po-ordl.
    op-order-val-list = op-order-val-list + 
        "|" + string(po-ordl.line) + "," + STRING(po-ordl.ord-no) .
END.
op-order-val-list = TRIM(op-order-val-list, "|").
  IF VALID-HANDLE(h-container) THEN
     RUN set-order-list IN h-container (INPUT op-order-val-list).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-apply-entry B-table-Win 
PROCEDURE local-apply-entry :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'apply-entry':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record B-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN valid-order NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields B-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR tab-source AS HANDLE.
DEF VAR char-hdl AS CHAR.
  /* Code placed here will execute PRIOR to standard behavior. */

IF v-first-disable-fields THEN DO:
    v-first-disable-fields = NO.
    RETURN.
END.

/*   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'tableio-source':U,OUTPUT char-hdl). */
/*   tab-source = HANDLE(char-hdl).                                                             */
/*   {util/tmsg.i ""prior_to_disable_fields"" VALID-HANDLE(tab-source)}                         */
/*   RUN notify ('apply-entry, TABLEIO-SOURCE':U).                                              */
/*   RUN notify ('apply-entry, NAVIGATION-SOURCE':U).                                           */
/*   APPLY 'entry' TO auto_find IN FRAME {&FRAME-NAME}.                                         */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-end-update B-table-Win 
PROCEDURE local-end-update :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'end-update':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
    IF LASTKEY NE -1 THEN DO:
        RUN valid-order NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN 
            RETURN.
    END.
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'get-next':U ) .
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit B-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  ASSIGN v-est-no = ""
         v-i-no = ""
         v-rec-key-list = ""
         v-current-row  = ?.
 /*
  {methods/run_link.i "CONTAINER-SOURCE" "Get-ip-rec_key" "(OUTPUT ip-rec_key)"}
  {sys/ref/attachlogic.i}
   */
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-changed B-table-Win 
PROCEDURE local-row-changed :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-po-ordl FOR po-ordl.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-changed':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST bf-po-ordl 
      WHERE bf-po-ordl.LINE EQ INTEGER(po-ordl.LINE:SCREEN-VALUE IN BROWSE {&browse-name} )
      NO-ERROR.
  IF AVAIL bf-po-ordl THEN
      bf-po-ordl.ord-no = INTEGER(po-ordl.ord-no:SCREEN-VALUE IN BROWSE {&browse-name} ).
  RUN get-results (OUTPUT v-order-list).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
    IF LASTKEY NE -1 THEN DO:
        RUN valid-order NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

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
  {src/adm/template/snd-list.i "po-ordl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-cont-handle B-table-Win 
PROCEDURE set-cont-handle :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAMETER ip-handle AS HANDLE.
 h-container = ip-handle.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-order B-table-Win 
PROCEDURE valid-order :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ll-ans           AS LOGICAL     NO-UNDO.
DEFINE VARIABLE ll-order-warned  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cItemType        AS CHARACTER   NO-UNDO.
DEF BUFFER xpo-ordl FOR po-ordl.
DEF BUFFER b-po-ord FOR po-ord.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST itemfg 
        WHERE itemfg.company = cocode 
          AND itemfg.i-no = po-ordl.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN
        cItemType = "FG".
    /* FINISHED GOODS */
    IF INT(po-ordl.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}) NE 0 AND
       cItemType EQ "FG" THEN DO:

      FIND FIRST oe-ordl
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ INT(po-ordl.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
            AND oe-ordl.i-no    EQ po-ordl.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
          NO-LOCK NO-ERROR.

      IF NOT AVAIL oe-ordl THEN
      FOR EACH oe-ordl NO-LOCK
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ INT(po-ordl.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}),
          FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ oe-ordl.company
            AND itemfg.i-no    EQ oe-ordl.i-no
            AND itemfg.isaset:
        RUN fg/fullset.p (ROWID(itemfg)).
        IF CAN-FIND(FIRST tt-fg-set
                    WHERE tt-fg-set.part-no EQ po-ordl.i-no:SCREEN-VALUE IN BROWSE {&browse-name})
        THEN LEAVE.
      END.
      FOR EACH tt-fg-set:
        DELETE tt-fg-set.
      END.

      IF NOT AVAIL oe-ordl THEN DO:
        APPLY "entry" TO po-ordl.ord-no.
        MESSAGE "FG Item# not on order, please try again..."
            VIEW-AS ALERT-BOX ERROR.
        RETURN ERROR.
      END.

      IF NOT ll-order-warned THEN DO:
        FIND FIRST xpo-ordl
            WHERE xpo-ordl.company EQ cocode
              AND xpo-ordl.i-no    EQ po-ordl.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
              AND xpo-ordl.ord-no  EQ INT(po-ordl.ord-no:SCREEN-VALUE IN BROWSE {&browse-name})
              AND CAN-FIND(FIRST b-po-ord
                           WHERE b-po-ord.company EQ cocode
                             AND b-po-ord.po-no   EQ xpo-ordl.po-no)
              AND RECID(xpo-ordl)  NE RECID(po-ordl)               
            USE-INDEX item NO-LOCK NO-ERROR.
                  
        ll-ans = NOT AVAIL xpo-ordl.
              
        IF NOT ll-ans THEN
        MESSAGE "Purchase order " +
                TRIM(STRING(xpo-ordl.po-no,">>>>>>>>")) +
                " already exists for order/item, continue?"
                VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE ll-ans.
              
        IF NOT ll-ans THEN DO:
          APPLY "entry" TO po-ordl.ord-no.
          RETURN ERROR.
        END.

        ELSE
          ASSIGN
           ll-order-warned              = YES
           po-ordl.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = oe-ordl.cust-no
           po-ordl.ord-no:SCREEN-VALUE IN BROWSE {&browse-name}  = string(oe-ordl.ord-no).
      END.
    END. /* FG END */
    ELSE DO: /* RAW MATERIALS */
        
        FIND FIRST oe-ord NO-LOCK
          WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ INT(po-ordl.ord-no:SCREEN-VALUE) NO-ERROR.
        IF AVAIL oe-ord 
          THEN
            ASSIGN
              ll-order-warned              = YES
              po-ordl.cust-no:SCREEN-VALUE IN BROWSE {&browse-name} = oe-ord.cust-no
              po-ordl.ord-no:SCREEN-VALUE IN BROWSE {&browse-name} = string(oe-ord.ord-no).

    END.
  END.
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
  RETURN ( trim(attach.est-no) = trim(v-est-no) /*or 
          lookup(attach.i-no,v-i-no) > 0*/ ).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

