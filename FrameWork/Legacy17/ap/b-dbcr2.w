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

&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
{sys/inc/VAR.i NEW SHARED}
{ap/d-selinv.i NEW}

ASSIGN cocode = g_company
       locode = g_loc.

DEF VAR lv-actnum AS cha NO-UNDO.
DEF VAR act_dscr AS cha FORM "x(30)" LABEL "Account Description" NO-UNDO.
DEF BUFFER bf-payl FOR ap-payl.
DEF BUFFER bf-vend FOR vend.
DEF VAR lv-inv-displayed AS LOG NO-UNDO.
DEF VAR ll-inquiry AS LOG NO-UNDO.
DEF VAR v-vend-act AS cha NO-UNDO.

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES ap-pay
&Scoped-define FIRST-EXTERNAL-TABLE ap-pay


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ap-pay.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ap-payl

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table ap-payl.inv-no ~
ap-payl.due-date ap-payl.amt-due ap-payl.amt-paid ap-payl.amt-disc ~
ap-payl.actnum display-actdscr() @ act_dscr ap-payl.rec_key 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table ap-payl.inv-no ~
ap-payl.due-date ap-payl.amt-due ap-payl.amt-paid ap-payl.amt-disc ~
ap-payl.actnum 
&Scoped-define ENABLED-TABLES-IN-QUERY-Browser-Table ap-payl
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Browser-Table ap-payl
&Scoped-define QUERY-STRING-Browser-Table FOR EACH ap-payl OF ap-pay WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH ap-payl OF ap-pay WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table ap-payl
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table ap-payl


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-Browser-Table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-actdscr B-table-Win 
FUNCTION display-actdscr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE VARIABLE auto_find AS CHARACTER FORMAT "X(256)":U 
     LABEL "Auto Find" 
     VIEW-AS FILL-IN 
     SIZE 60 BY 1 NO-UNDO.

DEFINE VARIABLE browse-order AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "N/A", 1
     SIZE 55 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      ap-payl
    FIELDS(ap-payl.inv-no
      ap-payl.due-date
      ap-payl.amt-due
      ap-payl.amt-paid
      ap-payl.amt-disc
      ap-payl.actnum
      ap-payl.rec_key) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      ap-payl.inv-no FORMAT "x(12)":U WIDTH 18.2
      ap-payl.due-date FORMAT "99/99/9999":U WIDTH 19.2
      ap-payl.amt-due COLUMN-LABEL "Balance Due" FORMAT "->>,>>>,>>9.99":U
            WIDTH 20.2
      ap-payl.amt-paid COLUMN-LABEL "Credit Amount" FORMAT "->>,>>>,>>9.99":U
            WIDTH 23
      ap-payl.amt-disc COLUMN-LABEL "Debit Amount" FORMAT "->>,>>>,>>9.99":U
            WIDTH 23.2
      ap-payl.actnum COLUMN-LABEL "Account Number" FORMAT "x(25)":U
            WIDTH 32.2
      display-actdscr() @ act_dscr
      ap-payl.rec_key FORMAT "X(20)":U
  ENABLE
      ap-payl.inv-no
      ap-payl.due-date
      ap-payl.amt-due
      ap-payl.amt-paid
      ap-payl.amt-disc
      ap-payl.actnum
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 9.05
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 10.29 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 10.29 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 10.29 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 10.29 COL 2
     RECT-4 AT ROW 10.05 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: ASI.ap-pay
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: External-Tables
   Other Settings: PERSISTENT-ONLY
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
         HEIGHT             = 19.38
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

ASSIGN 
       ap-payl.rec_key:VISIBLE IN BROWSE Browser-Table = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "ASI.ap-payl OF ASI.ap-pay"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _FldNameList[1]   > ASI.ap-payl.inv-no
"ap-payl.inv-no" ? ? "character" ? ? ? ? ? ? yes ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.ap-payl.due-date
"ap-payl.due-date" ? ? "date" ? ? ? ? ? ? yes ? no no "19.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.ap-payl.amt-due
"ap-payl.amt-due" "Balance Due" ? "decimal" ? ? ? ? ? ? yes ? no no "20.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > ASI.ap-payl.amt-paid
"ap-payl.amt-paid" "Credit Amount" ? "decimal" ? ? ? ? ? ? yes ? no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > ASI.ap-payl.amt-disc
"ap-payl.amt-disc" "Debit Amount" "->>,>>>,>>9.99" "decimal" ? ? ? ? ? ? yes ? no no "23.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > ASI.ap-payl.actnum
"ap-payl.actnum" "Account Number" ? "character" ? ? ? ? ? ? yes ? no no "32.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > "_<CALC>"
"display-actdscr() @ act_dscr" ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > ASI.ap-payl.rec_key
"ap-payl.rec_key" ? ? "character" ? ? ? ? ? ? no "" no no ? no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
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
ON DEFAULT-ACTION OF Browser-Table IN FRAME F-Main
DO:
  def var phandle as widget-handle no-undo.
  def var char-hdl as cha no-undo.   


  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'TableIO-source':U,OUTPUT char-hdl).
  phandle = WIDGET-HANDLE(char-hdl).
    
  IF NOT ll-inquiry THEN RUN new-state in phandle ('update-begin':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON HELP OF Browser-Table IN FRAME F-Main
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR lk-recid AS RECID NO-UNDO.

    CASE FOCUS:NAME:
        WHEN "inv-no" THEN DO:
             RUN ap/l-apinv.w (ap-pay.company,ap-pay.vend-no,FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT lk-recid).
             IF lk-recid <> ? THEN DO:
                RUN display-apinv (lk-recid).
             END.
             RETURN NO-APPLY.
        END.
        WHEN "actnum" THEN DO:
            RUN windows/l-acct3.w (ap-pay.company,"T",FOCUS:SCREEN-VALUE, OUTPUT char-val).
            IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                          act_dscr:SCREEN-VALUE IN BROWSE {&browse-name} = ENTRY(2,char-val).
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
ON return OF Browser-Table IN FRAME F-Main
ANYWHERE
DO:
   APPLY "tab" TO SELF.
   RETURN NO-APPLY.
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
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
  /* {src/adm/template/brsleave.i}*/
     {brsleave.i}
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


&Scoped-define SELF-NAME ap-payl.inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-payl.inv-no Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ap-payl.inv-no IN BROWSE Browser-Table /* Invoice# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-inv-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-payl.due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-payl.due-date Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ap-payl.due-date IN BROWSE Browser-Table /* Due Date */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-payl.amt-due
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-payl.amt-due Browser-Table _BROWSE-COLUMN B-table-Win
ON ENTRY OF ap-payl.amt-due IN BROWSE Browser-Table /* Balance Due */
DO:
  APPLY "tab" TO {&self-name} IN BROWSE {&browse-name}.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ap-payl.actnum
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ap-payl.actnum Browser-Table _BROWSE-COLUMN B-table-Win
ON LEAVE OF ap-payl.actnum IN BROWSE Browser-Table /* Account Number */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-actnum NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    RUN valid-inv-act NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "ap-pay"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ap-pay"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-appayl B-table-Win 
PROCEDURE create-appayl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR X AS INT NO-UNDO.


  FOR EACH tt-inv WHERE tt-inv.selekt,
      FIRST ap-inv WHERE RECID(ap-inv) EQ tt-inv.rec-id NO-LOCK:
        
    FIND FIRST ap-invl WHERE ap-invl.company = ap-inv.company
                         AND ap-invl.i-no = ap-inv.i-no NO-LOCK NO-ERROR.    

    IF AVAIL ap-invl THEN
        ASSIGN v-vend-act = ap-invl.actnum.

    FIND FIRST vend NO-LOCK
        WHERE vend.company EQ ap-pay.company
          AND vend.vend-no EQ ap-pay.vend-no
        NO-ERROR.

    IF AVAIL vend AND v-vend-act = "" THEN
        ASSIGN v-vend-act = vend.actnum.

    IF v-vend-act EQ "" THEN DO:
        FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
        IF AVAIL ap-ctrl THEN
          ASSIGN v-vend-act = ap-ctrl.purchases.
    END.

    CREATE ap-payl.

    ASSIGN
     ap-payl.c-no = ap-pay.c-no
     ap-payl.check-no = ap-pay.check-no
     ap-payl.LINE = X + 1
     ap-payl.memo = TRUE
     ap-payl.vend-no = ap-pay.vend-no
     ap-payl.actnum = v-vend-act
     ap-payl.inv-no = ap-inv.inv-no
     ap-payl.due-date = ap-inv.due-date
     ap-payl.amt-due  = ap-inv.due
     X = X + 1.

    IF ap-inv.due LT 0 THEN
      ASSIGN
       ap-payl.amt-paid = 0
       ap-payl.amt-disc = ap-inv.due * -1.
    ELSE
      ASSIGN
       ap-payl.amt-paid = ap-inv.due
       ap-payl.amt-disc = 0.

    RELEASE ap-payl.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-apinv B-table-Win 
PROCEDURE display-apinv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.


  FIND FIRST ap-inv WHERE RECID(ap-inv) EQ ip-recid NO-LOCK NO-ERROR.
  
  FIND FIRST ap-invl WHERE ap-invl.company = ap-inv.company
                         AND ap-invl.i-no = ap-inv.i-no NO-LOCK NO-ERROR.    

  IF AVAIL ap-invl THEN
      ASSIGN v-vend-act = ap-invl.actnum.

  FIND FIRST vend NO-LOCK
      WHERE vend.company EQ ap-pay.company
        AND vend.vend-no EQ ap-pay.vend-no
      NO-ERROR.

  IF AVAIL vend AND v-vend-act = "" THEN
      ASSIGN v-vend-act = vend.actnum.

  IF v-vend-act EQ "" THEN DO:
      FIND FIRST ap-ctrl WHERE ap-ctrl.company EQ cocode NO-LOCK NO-ERROR.
      IF AVAIL ap-ctrl THEN
        ASSIGN v-vend-act = ap-ctrl.purchases.
  END.

  IF AVAIL ap-inv THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ap-payl.due-date:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ap-inv.due-date,"99/99/9999")
     ap-payl.amt-due:SCREEN-VALUE IN BROWSE {&browse-name}  = STRING(ap-inv.due)
     ap-payl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(ap-inv.inv-no)
     ap-payl.actnum:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-vend-act) 

     lv-inv-displayed = YES.

    IF adm-adding-record THEN
      IF ap-inv.due LT 0 THEN
        ASSIGN
         ap-payl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name} = ""
         ap-payl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ap-inv.due * -1).
      ELSE
        ASSIGN
         ap-payl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(ap-inv.due)
         ap-payl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name} = "".           
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inquiry-mode B-table-Win 
PROCEDURE inquiry-mode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"inquiry-source", OUTPUT char-hdl).

  ASSIGN
   ll-inquiry       = VALID-HANDLE(WIDGET-HANDLE(char-hdl))
   lv-inv-displayed = ll-inquiry.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record B-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  IF ap-pay.posted THEN do:
     MESSAGE "This Memo has been posted. No addings are allowed!"  VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF NOT CAN-FIND(FIRST bf-payl OF ap-pay) THEN
  DO:
     FIND FIRST bf-vend WHERE
          bf-vend.company EQ ap-pay.company AND
          bf-vend.vend-no EQ ap-pay.vend-no
          NO-LOCK NO-ERROR.

     IF AVAIL bf-vend THEN
     DO:
        RUN ap/d-selinv.w (RECID(bf-vend), YES).

        IF CAN-FIND(FIRST tt-inv WHERE tt-inv.selekt) THEN
        DO:
           RUN create-appayl.
           RELEASE bf-vend.

           RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
           RUN reopen-query IN WIDGET-HANDLE(char-hdl).

           RUN dispatch ("open-query").
           RETURN.
        END.
     END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR X AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FOR EACH bf-payl OF ap-pay NO-LOCK BY LINE DESCENDING:
      X = bf-payl.LINE.
      LEAVE.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  FIND FIRST vend WHERE vend.company = ap-pay.company
                    AND vend.vend-no = ap-pay.vend-no
                    NO-LOCK NO-ERROR.
  ASSIGN ap-payl.c-no = ap-pay.c-no
         ap-payl.check-no = ap-pay.check-no
         ap-payl.LINE = X + 1
         ap-payl.memo = TRUE
         ap-payl.vend-no = ap-pay.vend-no
         ap-payl.actnum = IF AVAIL vend THEN vend.actnum ELSE "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record B-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF ap-pay.posted THEN do:
     MESSAGE "This Cash Receipt has been posted. No deletion allowed!"  VIEW-AS ALERT-BOX ERROR.
     RETURN.
  END.

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reopen-query.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields B-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  IF ap-pay.posted THEN do:
     MESSAGE "This Cash Receipt has been posted. No updates are allowed!"  VIEW-AS ALERT-BOX ERROR.
     RETURN.

  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    APPLY "entry" TO ap-payl.inv-no IN BROWSE {&browse-name}.
  END.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF AVAIL ap-payl THEN
  DO:
     /*set the rec_key so notes work properly*/
     {methods\template\local\setvalue.i}
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record B-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-tot-pay AS DEC NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
 /*=-== validateion ==== */
  RUN valid-inv-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-amt NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-actnum NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-inv-act NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
   /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN redisplay-header.
  ASSIGN adm-adding-record = NO
         adm-updating-record = NO
         adm-new-record = NO
         lv-inv-displayed = NO.
         

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE redisplay-header B-table-Win 
PROCEDURE redisplay-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source", OUTPUT char-hdl).
  RUN reopen-query IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query B-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN dispatch ("open-query").
  RUN redisplay-header.

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
  {src/adm/template/snd-list.i "ap-pay"}
  {src/adm/template/snd-list.i "ap-payl"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-actnum B-table-Win 
PROCEDURE valid-actnum :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:    
    FIND FIRST account
        WHERE account.company EQ g_company
          AND account.actnum  EQ ap-payl.actnum:SCREEN-VALUE IN BROWSE {&browse-name}
          AND account.type    NE "T"
        NO-LOCK NO-ERROR.
    IF NOT AVAIL account THEN DO:
      MESSAGE "Invalid GL Account Number" VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ap-payl.actnum IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.   
    act_dscr:SCREEN-VALUE IN BROWSE {&browse-name} = display-actdscr().                      
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-amt B-table-Win 
PROCEDURE valid-amt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    IF DEC(ap-payl.amt-disc:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 AND
       DEC(ap-payl.amt-paid:SCREEN-VALUE IN BROWSE {&browse-name}) EQ 0 THEN DO:
      MESSAGE "You MUST enter a Credit OR Debit value..."
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ap-payl.amt-disc IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-act B-table-Win 
PROCEDURE valid-inv-act :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR ll AS LOG INIT YES NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-payl
        WHERE bf-payl.c-no   EQ ap-pay.c-no
          AND bf-payl.inv-no EQ ap-payl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}
          AND bf-payl.actnum EQ ap-payl.actnum:SCREEN-VALUE IN BROWSE {&browse-name}
          AND ROWID(bf-payl) NE ROWID(ap-payl)
        NO-LOCK NO-ERROR.

    IF AVAIL bf-payl THEN DO:
      ll = FOCUS:NAME IN BROWSE {&browse-name} EQ "inv-no".
      MESSAGE "Invoice/Acct# already on Cash Receipt,"
              "re-enter Invoice?  (NO to re-enter Acct#)"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll. 
      IF ll THEN
        APPLY "entry" TO ap-payl.inv-no IN BROWSE {&browse-name}. 
      ELSE
        APPLY "entry" TO ap-payl.actnum IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.                       
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-no B-table-Win 
PROCEDURE valid-inv-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST ap-inv
        WHERE ap-inv.company EQ g_company 
          AND ap-inv.posted  EQ YES
          AND ap-inv.vend-no EQ ap-pay.vend-no
          AND ap-inv.inv-no  EQ ap-payl.inv-no:SCREEN-VALUE IN BROWSE {&browse-name}
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ap-inv THEN DO:
      MESSAGE "Invalid Invoice Number..." VIEW-AS ALERT-BOX ERROR.           
      APPLY "entry" TO ap-payl.inv-no IN BROWSE {&browse-name}.
      RETURN ERROR.
    END.

    IF NOT lv-inv-displayed THEN RUN display-apinv (RECID(ap-inv)).

    act_dscr:SCREEN-VALUE = display-actdscr().
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-actdscr B-table-Win 
FUNCTION display-actdscr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  IF AVAIL ap-payl THEN DO:
       FIND FIRST account WHERE account.company = g_company
                            AND account.actnum = (IF lv-actnum = "" THEN ap-payl.actnum ELSE lv-actnum) NO-LOCK NO-ERROR.         
       IF AVAIL account THEN RETURN account.dscr.
       ELSE RETURN "". 
  END.
  RETURN "".


END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

