&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admBrowserUsing.i} /* added by script _admBrowsers.p */



/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-itemfgUpd NO-UNDO LIKE itemfg.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*------------------------------------------------------------------------

  File: util\b-hrms-x.w 

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

&SCOPED-DEFINE dataGridInclude dataGrid\util\b-updfg.i
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i "new shared"}

ASSIGN
 cocode = g_company
 locode = g_loc.
DEF TEMP-TABLE tt-itemfg LIKE itemfg
    FIELD case-count-a AS CHAR
    FIELD weight-100-a AS CHAR.

DEF VAR lv-i-name LIKE item.i-name NO-UNDO.
DEF VAR gvlSkipBadItems AS LOG NO-UNDO.
&SCOPED-DEFINE sortby-phrase BY tt-itemfgupd.i-no

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-itemfgUpd

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table tt-itemfgUpd.i-no ~
tt-itemfgUpd.part-no tt-itemfgUpd.i-name tt-itemfgUpd.part-dscr1 ~
tt-itemfgUpd.part-dscr2 tt-itemfgUpd.part-dscr3 tt-itemfgUpd.spare-char-1 ~
tt-itemfgUpd.die-no tt-itemfgUpd.plate-no tt-itemfgUpd.cad-no ~
tt-itemfgUpd.spc-no tt-itemfgUpd.upc-no tt-itemfgUpd.procat ~
tt-itemfgUpd.def-loc tt-itemfgUpd.def-loc-bin tt-itemfgUpd.case-count ~
tt-itemfgUpd.weight-100 tt-itemfgUpd.frt-class tt-itemfgUpd.spare-char-5 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH tt-itemfgUpd WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH tt-itemfgUpd WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table tt-itemfgUpd
&Scoped-define FIRST-TABLE-IN-QUERY-br_table tt-itemfgUpd


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-br_table}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table RECT-1 btSkipThese btTryLater 
&Scoped-Define DISPLAYED-OBJECTS edErrorNotes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD display-i-name B-table-Win 
FUNCTION display-i-name RETURNS CHARACTER
  ( INPUT ip-type AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btSkipThese 
     LABEL "Skip Lines W/ Errors / Apply Changes" 
     SIZE 41 BY 1.14.

DEFINE BUTTON btTryLater 
     LABEL "Exit and Try Later" 
     SIZE 37 BY 1.14.

DEFINE VARIABLE edErrorNotes AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 147 BY 4.05 NO-UNDO.

DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 157 BY 6.67.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      tt-itemfgUpd SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      tt-itemfgUpd.i-no FORMAT "x(22)":U
      tt-itemfgUpd.part-no FORMAT "x(21)":U
      tt-itemfgUpd.i-name FORMAT "x(30)":U
      tt-itemfgUpd.part-dscr1 COLUMN-LABEL "Desc 1" FORMAT "x(30)":U
      tt-itemfgUpd.part-dscr2 COLUMN-LABEL "Desc 2" FORMAT "x(30)":U
      tt-itemfgUpd.part-dscr3 COLUMN-LABEL "Desc 3" FORMAT "x(30)":U
      tt-itemfgUpd.spare-char-1 COLUMN-LABEL "Group" FORMAT "x(8)":U
      tt-itemfgUpd.die-no FORMAT "x(15)":U
      tt-itemfgUpd.plate-no FORMAT "x(15)":U
      tt-itemfgUpd.cad-no COLUMN-LABEL "Cad #" FORMAT "x(15)":U
      tt-itemfgUpd.spc-no COLUMN-LABEL "QC #" FORMAT "x(15)":U
      tt-itemfgUpd.upc-no COLUMN-LABEL "UPC #" FORMAT "x(15)":U
      tt-itemfgUpd.procat FORMAT "x(5)":U
      tt-itemfgUpd.def-loc FORMAT "x(8)":U
      tt-itemfgUpd.def-loc-bin FORMAT "x(8)":U
      tt-itemfgUpd.case-count COLUMN-LABEL "Count" FORMAT ">>>9":U
      tt-itemfgUpd.weight-100 FORMAT ">,>>9.99":U
      tt-itemfgUpd.frt-class FORMAT "x(8)":U
      tt-itemfgUpd.spare-char-5 COLUMN-LABEL "Errors" FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 234 BY 17.24
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
     fi_sortby AT ROW 16.24 COL 2 COLON-ALIGNED NO-LABEL
     edErrorNotes AT ROW 19.33 COL 43 NO-LABEL WIDGET-ID 4
     btSkipThese AT ROW 23.86 COL 69 WIDGET-ID 6
     btTryLater AT ROW 23.86 COL 122 WIDGET-ID 8
     "Click On Red Color Rows to See Error Details Below" VIEW-AS TEXT
          SIZE 63 BY .95 AT ROW 18.38 COL 49 WIDGET-ID 10
          FGCOLOR 9 FONT 6
     RECT-1 AT ROW 18.86 COL 38 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-itemfgUpd T "?" NO-UNDO asi itemfg
   END-TABLES.
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
         HEIGHT             = 25.19
         WIDTH              = 238.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

{Advantzware/WinKit/dataGridProc.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR EDITOR edErrorNotes IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       edErrorNotes:RETURN-INSERTED IN FRAME F-Main  = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "Temp-Tables.tt-itemfgUpd"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _FldNameList[1]   > Temp-Tables.tt-itemfgUpd.i-no
"tt-itemfgUpd.i-no" ? "x(22)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tt-itemfgUpd.part-no
"tt-itemfgUpd.part-no" ? "x(21)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.tt-itemfgUpd.i-name
     _FldNameList[4]   > Temp-Tables.tt-itemfgUpd.part-dscr1
"tt-itemfgUpd.part-dscr1" "Desc 1" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tt-itemfgUpd.part-dscr2
"tt-itemfgUpd.part-dscr2" "Desc 2" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tt-itemfgUpd.part-dscr3
"tt-itemfgUpd.part-dscr3" "Desc 3" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tt-itemfgUpd.spare-char-1
"tt-itemfgUpd.spare-char-1" "Group" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   = Temp-Tables.tt-itemfgUpd.die-no
     _FldNameList[9]   = Temp-Tables.tt-itemfgUpd.plate-no
     _FldNameList[10]   > Temp-Tables.tt-itemfgUpd.cad-no
"tt-itemfgUpd.cad-no" "Cad #" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.tt-itemfgUpd.spc-no
"tt-itemfgUpd.spc-no" "QC #" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > Temp-Tables.tt-itemfgUpd.upc-no
"tt-itemfgUpd.upc-no" "UPC #" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   = Temp-Tables.tt-itemfgUpd.procat
     _FldNameList[14]   = Temp-Tables.tt-itemfgUpd.def-loc
     _FldNameList[15]   = Temp-Tables.tt-itemfgUpd.def-loc-bin
     _FldNameList[16]   > Temp-Tables.tt-itemfgUpd.case-count
"tt-itemfgUpd.case-count" "Count" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   = Temp-Tables.tt-itemfgUpd.weight-100
     _FldNameList[18]   = Temp-Tables.tt-itemfgUpd.frt-class
     _FldNameList[19]   > Temp-Tables.tt-itemfgUpd.spare-char-5
"tt-itemfgUpd.spare-char-5" "Errors" "x(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON HELP OF br_table IN FRAME F-Main
DO:
  DEF VAR char-val AS cha NO-UNDO.
  DEF VAR help-recid AS RECID NO-UNDO.


  CASE FOCUS:NAME :
    WHEN "code2" THEN DO:
      RUN windows/l-item3.w (cocode,"","",FOCUS:SCREEN-VALUE, OUTPUT char-val).
      help-recid = INT(char-val) NO-ERROR.
      IF ERROR-STATUS:ERROR THEN help-recid = ?.
      FIND item WHERE RECID(item) EQ help-recid NO-LOCK NO-ERROR.
      IF AVAIL item AND item.i-no NE FOCUS:SCREEN-VALUE THEN DO:
        FOCUS:SCREEN-VALUE = item.i-no.
        RUN new-code2.
      END.
    END.
  END CASE.

  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:
  RUN set-row-bgcolor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  /* This code displays initial values for newly added or copied rows. */
  {src/adm/template/brsentry.i}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
   if keyfunction(lastkey) = "page-up" or 
      keyfunction(lastkey) = "page-down" or
      keyfunction(lastkey) = "cursor-up" or
      keyfunction(lastkey) = "cursor-down" 
   then do:  
      return no-apply.
   end.
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {est/brsleave.i}
   RUN check-modified IN THIS-PROCEDURE ('clear':U) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
  RUN startSearch.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}


  DO WITH FRAME {&FRAME-NAME}:

    IF tt-itemfgUpd.spare-char-5 GT "" THEN DO:    

      edErrorNotes:SET-SELECTION( 1 , 200 ).
      edErrorNotes:EDIT-CLEAR( ).
      edErrorNotes:SCREEN-VALUE = "".
      edErrorNotes:INSERT-STRING(tt-itemfgUpd.spare-char-5).

    END. /* Add text */
    ELSE DO:   

       edErrorNotes:SET-SELECTION ( 1 , 200 ).
       edErrorNotes:EDIT-CLEAR ( ).
       edErrorNotes:SCREEN-VALUE = "".
    END. /* Clear all text */

  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSkipThese
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSkipThese B-table-Win
ON CHOOSE OF btSkipThese IN FRAME F-Main /* Skip Lines W/ Errors / Apply Changes */
DO:
  IF SELF:LABEL BEGINS "Skip" THEN
    gvlSkipBadItems = TRUE.
  RUN performUpdate.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btTryLater
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btTryLater B-table-Win
ON CHOOSE OF btTryLater IN FRAME F-Main /* Exit and Try Later */
DO:
  DEF VAR char-hdl AS CHAR NO-UNDO.
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"container-source",OUTPUT char-hdl).

 IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
   RUN local-exit IN WIDGET-HANDLE(char-hdl).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

/* CREATE tt-itemfgupd.                */
/* ASSIGN tt-itemfgupd.i-no = "Test1". */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-open-query-cases B-table-Win  adm/support/_adm-opn.p
PROCEDURE adm-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Opens different cases of the query based on attributes
               such as the 'Key-Name', or 'SortBy-Case'
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* No Foreign keys are accepted by this SmartObject. */

  {&OPEN-QUERY-{&QUERY-NAME}}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-enable-buttons B-table-Win 
PROCEDURE disable-enable-buttons :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipcDisableEnable AS CHARACTER   NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:

  IF ipcDisableEnable EQ "Disable" THEN 
    ASSIGN
    btSkipThese:VISIBLE = FALSE
    btTryLater:VISIBLE = FALSE.
  ELSE DO:

    ASSIGN
    btSkipThese:VISIBLE = TRUE
    btTryLater:VISIBLE = TRUE.
    ENABLE btTryLater btSkipThese.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query-cases B-table-Win 
PROCEDURE local-open-query-cases :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pass-temp-table B-table-Win 
PROCEDURE pass-temp-table :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER TABLE FOR tt-itemfg.

DEF VAR i AS INT NO-UNDO.

EMPTY TEMP-TABLE tt-itemfgUpd.

FOR EACH tt-itemfg.
  CREATE tt-itemfgUpd.
  BUFFER-COPY tt-itemfg TO tt-itemfgUpd.
  i = i + 1.
END.

FIND FIRST tt-itemfgUpd WHERE tt-itemfgUpd.spare-char-5 NE "" 
   NO-LOCK NO-ERROR.
IF NOT AVAIL tt-itemfgUpd THEN DO WITH FRAME {&FRAME-NAME}:

  ASSIGN btSkipThese:LABEL = "Apply These Changes"
         btTryLater:VISIBLE = FALSE.

END.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .
  APPLY 'value-changed' TO br_table.

RUN disable-enable-buttons (INPUT "Enable").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE performUpdate B-table-Win 
PROCEDURE performUpdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-eb FOR eb.

 MESSAGE "Are you sure you want to apply these changes?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
          TITLE "" UPDATE lDoUpdate AS LOGICAL.

IF lDoUpdate THEN DO:

  FOR EACH tt-itemfgUpd:
    IF tt-itemfgUpd.spare-char-5 EQ "" THEN DO:
      FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no EQ tt-itemfgUPd.i-no
        EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL itemfg THEN DO:
        IF tt-itemfgUpd.part-dscr1 GT "" THEN
          itemfg.part-dscr1   = tt-itemfgUpd.part-dscr1.
        IF tt-itemfgUpd.part-dscr2 GT "" THEN
          itemfg.part-dscr2   = tt-itemfgUpd.part-dscr2.
        IF tt-itemfgUpd.part-dscr3 GT "" THEN
          itemfg.part-dscr3   = tt-itemfgUpd.part-dscr3.
        IF tt-itemfgUpd.part-no GT "" THEN
          itemfg.part-no   = tt-itemfgUpd.part-no.
        IF tt-itemfgUpd.i-name GT "" THEN
          itemfg.i-name   = tt-itemfgUpd.i-name.
        IF tt-itemfgUpd.spare-char-1 GT "" THEN
          itemfg.spare-char-1 = tt-itemfgUpd.spare-char-1.
        IF tt-itemfgUpd.die-no GT "" THEN
          itemfg.die-no       = tt-itemfgUpd.die-no.
        IF tt-itemfgUpd.plate-no GT "" THEN
          itemfg.plate-no     = tt-itemfgUpd.plate-no. 
        IF tt-itemfgUpd.cad-no GT "" THEN
          itemfg.cad-no       = tt-itemfgUpd.cad-no. 
        IF tt-itemfgUpd.spc-no GT "" THEN
          itemfg.spc-no       = tt-itemfgUpd.spc-no .
        IF tt-itemfgUpd.upc-no GT "" THEN
          itemfg.upc-no       = tt-itemfgUpd.upc-no.
        IF tt-itemfgUpd.procat GT "" THEN
          itemfg.procat       = tt-itemfgUpd.procat .
        IF tt-itemfgUpd.def-loc GT "" THEN
          itemfg.def-loc      = tt-itemfgUpd.def-loc .
        IF tt-itemfgUpd.def-loc-bin GT "" THEN
          itemfg.def-loc-bin  = tt-itemfgUpd.def-loc-bin.
        IF tt-itemfgUpd.case-count GT 0 THEN
          itemfg.case-count   = tt-itemfgUpd.case-count .
        IF tt-itemfgUpd.weight-100 GT 0 THEN
          itemfg.weight-100   = tt-itemfgUpd.weight-100.
        IF tt-itemfgUpd.frt-class GT "" THEN
          itemfg.frt-class    = tt-itemfgUpd.frt-class.


       FOR EACH eb NO-LOCK
           WHERE eb.company EQ itemfg.company
             AND eb.cust-no EQ itemfg.cust-no
             AND ((eb.part-no EQ itemfg.part-no
             AND   eb.stock-no EQ "")
              OR   eb.stock-no EQ itemfg.i-no):
         FIND b-eb WHERE ROWID(b-eb) EQ ROWID(eb) EXCLUSIVE NO-WAIT NO-ERROR.
         IF AVAIL b-eb THEN DO:
           ASSIGN
            b-eb.part-no = itemfg.part-no
            b-eb.part-dscr1 = itemfg.i-name
            b-eb.part-dscr2 = itemfg.part-dscr1
            b-eb.plate-no = itemfg.plate-no
            b-eb.spc-no = itemfg.spc-no
            b-eb.upc-no = itemfg.upc-no
            b-eb.procat = itemfg.procat.
         END. /* avail b-eb */
         FIND CURRENT b-eb NO-LOCK.
         RELEASE b-eb.
       END. /* each eb */

      END. /* if avail then assign */
    END. /* if no errors */
  END. /* each update tt */
  MESSAGE "Done applying these changes."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
END. /* Answered 'yes' */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo-query B-table-Win 
PROCEDURE repo-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


  RUN dispatch ('open-query').

  REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.

  RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key B-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* There are no foreign keys supplied by this SmartObject. */

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
  {src/adm/template/snd-list.i "tt-itemfgUpd"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-row-bgcolor B-table-Win 
PROCEDURE set-row-bgcolor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF AVAIL(tt-itemfgUpd) AND tt-itemfgUpd.spare-char-5 NE "" THEN DO:
  ASSIGN
    tt-itemfgUpd.cad-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12
    tt-itemfgUpd.case-count:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12
    tt-itemfgUpd.def-loc:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.def-loc-bin:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.die-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.frt-class:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.i-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.i-name:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12  
    tt-itemfgUpd.part-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.part-dscr1:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.part-dscr2:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.part-dscr3:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.plate-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.procat:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.spare-char-1:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.spare-char-5:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.spc-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.upc-no:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12 
    tt-itemfgUpd.weight-100:BGCOLOR IN BROWSE {&BROWSE-NAME}    = 12.

 END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION display-i-name B-table-Win 
FUNCTION display-i-name RETURNS CHARACTER
  ( INPUT ip-type AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/



  RETURN IF AVAIL item THEN item.i-name
         ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

