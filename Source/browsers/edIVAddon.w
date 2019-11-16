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

  Input Parameters: <none>

  Output Parameters: <none>

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

&SCOPED-DEFINE setBrowseFocus
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/gcompany.i}
{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}
{sys/inc/varasgn.i}

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
&Scoped-define EXTERNAL-TABLES EDIVLine
&Scoped-define FIRST-EXTERNAL-TABLE EDIVLine


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDIVLine.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES EDIVAddon

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table EDIVAddon.Partner ~
EDIVAddon.Seq EDIVAddon.Ref-Num EDIVAddon.Uom-code EDIVAddon.Percent ~
EDIVAddon.Rate EDIVAddon.Qty EDIVAddon.Special-svc-code ~
EDIVAddon.Option-code EDIVAddon.rec_key EDIVAddon.Note[9] EDIVAddon.Note[8] ~
EDIVAddon.Note[7] EDIVAddon.Note[6] EDIVAddon.Note[5] EDIVAddon.Note[4] ~
EDIVAddon.Note[3] EDIVAddon.Note[2] EDIVAddon.Note[1] EDIVAddon.Line ~
EDIVAddon.Invoice-no EDIVAddon.Hand-meth EDIVAddon.Description[2] ~
EDIVAddon.Description[1] EDIVAddon.Company EDIVAddon.Basis-qual ~
EDIVAddon.Amount EDIVAddon.Allow-charge EDIVAddon.Agency-qual ~
EDIVAddon.Agency-code EDIVAddon.Addon-line 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH EDIVAddon OF EDIVLine WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH EDIVAddon OF EDIVLine WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table EDIVAddon
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table EDIVAddon


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Browser-Table RECT-4 browse-order auto_find ~
Btn_Clear_Find 
&Scoped-Define DISPLAYED-OBJECTS browse-order auto_find 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
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
     SIZE 154 BY 1.43.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Browser-Table FOR 
      EDIVAddon
    FIELDS(EDIVAddon.Partner
      EDIVAddon.Seq
      EDIVAddon.Ref-Num
      EDIVAddon.Uom-code
      EDIVAddon.Percent
      EDIVAddon.Rate
      EDIVAddon.Qty
      EDIVAddon.Special-svc-code
      EDIVAddon.Option-code
      EDIVAddon.rec_key
      EDIVAddon.Note[9]
      EDIVAddon.Note[8]
      EDIVAddon.Note[7]
      EDIVAddon.Note[6]
      EDIVAddon.Note[5]
      EDIVAddon.Note[4]
      EDIVAddon.Note[3]
      EDIVAddon.Note[2]
      EDIVAddon.Note[1]
      EDIVAddon.Line
      EDIVAddon.Invoice-no
      EDIVAddon.Hand-meth
      EDIVAddon.Description[2]
      EDIVAddon.Description[1]
      EDIVAddon.Company
      EDIVAddon.Basis-qual
      EDIVAddon.Amount
      EDIVAddon.Allow-charge
      EDIVAddon.Agency-qual
      EDIVAddon.Agency-code
      EDIVAddon.Addon-line) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      EDIVAddon.Partner FORMAT "x(05)":U
      EDIVAddon.Seq FORMAT ">>>>>>9":U
      EDIVAddon.Ref-Num COLUMN-LABEL "Ref Num" FORMAT "x(30)":U
      EDIVAddon.Uom-code COLUMN-LABEL "Uom" FORMAT "x(2)":U
      EDIVAddon.Percent FORMAT "->>>>>.999":U
      EDIVAddon.Rate FORMAT "->>>>>.99<<":U
      EDIVAddon.Qty FORMAT "->,>>>,>>>.99":U
      EDIVAddon.Special-svc-code COLUMN-LABEL "Special Svc Code" FORMAT "x(4)":U
      EDIVAddon.Option-code COLUMN-LABEL "Option Code" FORMAT "x(20)":U
      EDIVAddon.rec_key FORMAT "X(20)":U
      EDIVAddon.Note[9] FORMAT "x(60)":U
      EDIVAddon.Note[8] FORMAT "x(60)":U
      EDIVAddon.Note[7] FORMAT "x(60)":U
      EDIVAddon.Note[6] FORMAT "x(60)":U
      EDIVAddon.Note[5] FORMAT "x(60)":U
      EDIVAddon.Note[4] FORMAT "x(60)":U
      EDIVAddon.Note[3] FORMAT "x(60)":U
      EDIVAddon.Note[2] FORMAT "x(60)":U
      EDIVAddon.Note[1] FORMAT "x(60)":U
      EDIVAddon.Line FORMAT ">>9":U
      EDIVAddon.Invoice-no FORMAT "x(22)":U
      EDIVAddon.Hand-meth FORMAT "x(2)":U
      EDIVAddon.Description[2] FORMAT "x(30)":U
      EDIVAddon.Description[1] FORMAT "x(30)":U
      EDIVAddon.Company FORMAT "x(8)":U
      EDIVAddon.Basis-qual FORMAT "x(1)":U
      EDIVAddon.Amount FORMAT "->>>,>>>.99":U
      EDIVAddon.Allow-charge FORMAT "A/C":U
      EDIVAddon.Agency-qual FORMAT "x(2)":U
      EDIVAddon.Agency-code FORMAT "x(10)":U
      EDIVAddon.Addon-line FORMAT ">>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 154 BY 12.62
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 13.86 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 13.86 COL 79 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 13.86 COL 141 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 13.86 COL 1.6
     RECT-4 AT ROW 13.62 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartNavBrowser
   External Tables: asi.EDIVLine
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
         HEIGHT             = 14.05
         WIDTH              = 154.
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.EDIVAddon OF asi.EDIVLine"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED,"
     _FldNameList[1]   = asi.EDIVAddon.Partner
     _FldNameList[2]   = asi.EDIVAddon.Seq
     _FldNameList[3]   > asi.EDIVAddon.Ref-Num
"EDIVAddon.Ref-Num" "Ref Num" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > asi.EDIVAddon.Uom-code
"EDIVAddon.Uom-code" "Uom" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = asi.EDIVAddon.Percent
     _FldNameList[6]   = asi.EDIVAddon.Rate
     _FldNameList[7]   = asi.EDIVAddon.Qty
     _FldNameList[8]   > asi.EDIVAddon.Special-svc-code
"EDIVAddon.Special-svc-code" "Special Svc Code" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > asi.EDIVAddon.Option-code
"EDIVAddon.Option-code" "Option Code" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = asi.EDIVAddon.rec_key
     _FldNameList[11]   = asi.EDIVAddon.Note[9]
     _FldNameList[12]   = asi.EDIVAddon.Note[8]
     _FldNameList[13]   = asi.EDIVAddon.Note[7]
     _FldNameList[14]   = asi.EDIVAddon.Note[6]
     _FldNameList[15]   = asi.EDIVAddon.Note[5]
     _FldNameList[16]   = asi.EDIVAddon.Note[4]
     _FldNameList[17]   = asi.EDIVAddon.Note[3]
     _FldNameList[18]   = asi.EDIVAddon.Note[2]
     _FldNameList[19]   = asi.EDIVAddon.Note[1]
     _FldNameList[20]   = asi.EDIVAddon.Line
     _FldNameList[21]   = asi.EDIVAddon.Invoice-no
     _FldNameList[22]   = asi.EDIVAddon.Hand-meth
     _FldNameList[23]   = asi.EDIVAddon.Description[2]
     _FldNameList[24]   = asi.EDIVAddon.Description[1]
     _FldNameList[25]   = asi.EDIVAddon.Company
     _FldNameList[26]   = asi.EDIVAddon.Basis-qual
     _FldNameList[27]   = asi.EDIVAddon.Amount
     _FldNameList[28]   = asi.EDIVAddon.Allow-charge
     _FldNameList[29]   = asi.EDIVAddon.Agency-qual
     _FldNameList[30]   = asi.EDIVAddon.Agency-code
     _FldNameList[31]   = asi.EDIVAddon.Addon-line
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}

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
  {src/adm/template/row-list.i "EDIVLine"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "EDIVLine"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

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
  {src/adm/template/snd-list.i "EDIVLine"}
  {src/adm/template/snd-list.i "EDIVAddon"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setBrowseFocus B-table-Win 
PROCEDURE setBrowseFocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

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

