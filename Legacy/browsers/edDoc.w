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
DEFINE BUFFER bf-eddoc FOR eddoc.

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
&Scoped-define INTERNAL-TABLES EDDoc

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE Browser-Table                                 */
&Scoped-define FIELDS-IN-QUERY-Browser-Table EDDoc.Partner EDDoc.DocID ~
EDDoc.SetID EDDoc.Version EDDoc.Posted EDDoc.Direction EDDoc.Status-Flag ~
EDDoc.UserRef EDDoc.Unique-SDQ-No EDDoc.Unique-Order-No EDDoc.Stat ~
EDDoc.DocSeq EDDoc.ST-Code EDDoc.ST EDDoc.Set-Test-Prod EDDoc.Seq ~
EDDoc.rec_key EDDoc.Recv-Test-Prod EDDoc.P-FATime EDDoc.P-FADate ~
EDDoc.OpenItem EDDoc.ISA EDDoc.InterpTime EDDoc.InterpDate EDDoc.GS ~
EDDoc.FGTime EDDoc.FGSender EDDoc.FGRecvID EDDoc.FGID EDDoc.FGDate ~
EDDoc.FGAgency EDDoc.C-FADate EDDoc.Error-count EDDoc.C-FATime ~
EDDoc.EDI_Standard EDDoc.EDI_Agency EDDoc.ChgTime EDDoc.ChgOp EDDoc.ChgDate ~
EDDoc.AddTime EDDoc.AddOp EDDoc.AddDate 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Browser-Table 
&Scoped-define QUERY-STRING-Browser-Table FOR EACH EDDoc WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-Browser-Table OPEN QUERY Browser-Table FOR EACH EDDoc WHERE ~{&KEY-PHRASE} NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-Browser-Table EDDoc
&Scoped-define FIRST-TABLE-IN-QUERY-Browser-Table EDDoc


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
     SIZE 59 BY 1 NO-UNDO.

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
      EDDoc
    FIELDS(EDDoc.Partner
      EDDoc.DocID
      EDDoc.SetID
      EDDoc.Version
      EDDoc.Posted
      EDDoc.Direction
      EDDoc.Status-Flag
      EDDoc.UserRef
      EDDoc.Unique-SDQ-No
      EDDoc.Unique-Order-No
      EDDoc.Stat
      EDDoc.DocSeq
      EDDoc.ST-Code
      EDDoc.ST
      EDDoc.Set-Test-Prod
      EDDoc.Seq
      EDDoc.rec_key
      EDDoc.Recv-Test-Prod
      EDDoc.P-FATime
      EDDoc.P-FADate
      EDDoc.OpenItem
      EDDoc.ISA
      EDDoc.InterpTime
      EDDoc.InterpDate
      EDDoc.GS
      EDDoc.FGTime
      EDDoc.FGSender
      EDDoc.FGRecvID
      EDDoc.FGID
      EDDoc.FGDate
      EDDoc.FGAgency
      EDDoc.C-FADate
      EDDoc.Error-count
      EDDoc.C-FATime
      EDDoc.EDI_Standard
      EDDoc.EDI_Agency
      EDDoc.ChgTime
      EDDoc.ChgOp
      EDDoc.ChgDate
      EDDoc.AddTime
      EDDoc.AddOp
      EDDoc.AddDate) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Browser-Table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Browser-Table B-table-Win _STRUCTURED
  QUERY Browser-Table NO-LOCK DISPLAY
      EDDoc.Partner FORMAT "x(15)":U
      EDDoc.DocID FORMAT "x(35)":U
      EDDoc.SetID FORMAT "x(6)":U
      EDDoc.Version FORMAT "x(12)":U
      EDDoc.Posted FORMAT "yes/no":U
      EDDoc.Direction FORMAT "x(01)":U
      EDDoc.Status-Flag FORMAT "x(3)":U
      EDDoc.UserRef FORMAT "x(22)":U
      EDDoc.Unique-SDQ-No FORMAT ">>>>>9":U
      EDDoc.Unique-Order-No FORMAT ">>>>>9":U
      EDDoc.Stat FORMAT "9":U
      EDDoc.DocSeq FORMAT ">>>>9":U
      EDDoc.ST-Code FORMAT "x(12)":U
      EDDoc.ST FORMAT ">>>>>>>>9":U
      EDDoc.Set-Test-Prod FORMAT "x(1)":U
      EDDoc.Seq FORMAT ">>>>>>9":U
      EDDoc.rec_key FORMAT "X(20)":U
      EDDoc.Recv-Test-Prod FORMAT "x(1)":U
      EDDoc.P-FATime FORMAT ">>>>>9":U
      EDDoc.P-FADate FORMAT "99/99/9999":U
      EDDoc.OpenItem FORMAT "Y/N":U
      EDDoc.ISA FORMAT ">>>>>>>>9":U
      EDDoc.InterpTime FORMAT ">>>>>9":U
      EDDoc.InterpDate FORMAT "99/99/9999":U
      EDDoc.GS FORMAT ">>>>>>>>9":U
      EDDoc.FGTime FORMAT ">>>>>9":U
      EDDoc.FGSender FORMAT "x(15)":U
      EDDoc.FGRecvID FORMAT "x(15)":U
      EDDoc.FGID FORMAT "x(2)":U
      EDDoc.FGDate FORMAT "99/99/9999":U
      EDDoc.FGAgency FORMAT "x(2)":U
      EDDoc.C-FADate FORMAT "99/99/9999":U
      EDDoc.Error-count FORMAT ">>>":U
      EDDoc.C-FATime FORMAT ">>>>>9":U
      EDDoc.EDI_Standard FORMAT "x(4)":U
      EDDoc.EDI_Agency FORMAT "x(1)":U
      EDDoc.ChgTime FORMAT ">>>>>":U
      EDDoc.ChgOp FORMAT "x(12)":U
      EDDoc.ChgDate FORMAT "99/99/9999":U
      EDDoc.AddTime FORMAT ">>>>>":U
      EDDoc.AddOp FORMAT "x(12)":U
      EDDoc.AddDate FORMAT "99/99/9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 145 BY 18.1
         FONT 2.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     Browser-Table AT ROW 1 COL 1 HELP
          "Use Home, End, Page-Up, Page-Down, & Arrow Keys to Navigate"
     browse-order AT ROW 19.33 COL 6 HELP
          "Select Browser Sort Order" NO-LABEL
     auto_find AT ROW 19.33 COL 70 COLON-ALIGNED HELP
          "Enter Auto Find Value"
     Btn_Clear_Find AT ROW 19.33 COL 132 HELP
          "CLEAR AUTO FIND Value"
     "By:" VIEW-AS TEXT
          SIZE 4 BY 1 AT ROW 19.33 COL 2
     RECT-4 AT ROW 19.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 FGCOLOR 0  WIDGET-ID 100.


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
         HEIGHT             = 19.67
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Browser-Table
/* Query rebuild information for BROWSE Browser-Table
     _TblList          = "asi.EDDoc"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = "USED"
     _FldNameList[1]   = asi.EDDoc.Partner
     _FldNameList[2]   = asi.EDDoc.DocID
     _FldNameList[3]   = asi.EDDoc.SetID
     _FldNameList[4]   = asi.EDDoc.Version
     _FldNameList[5]   = asi.EDDoc.Posted
     _FldNameList[6]   = asi.EDDoc.Direction
     _FldNameList[7]   = asi.EDDoc.Status-Flag
     _FldNameList[8]   = asi.EDDoc.UserRef
     _FldNameList[9]   = asi.EDDoc.Unique-SDQ-No
     _FldNameList[10]   = asi.EDDoc.Unique-Order-No
     _FldNameList[11]   = asi.EDDoc.Stat
     _FldNameList[12]   = asi.EDDoc.DocSeq
     _FldNameList[13]   = asi.EDDoc.ST-Code
     _FldNameList[14]   = asi.EDDoc.ST
     _FldNameList[15]   = asi.EDDoc.Set-Test-Prod
     _FldNameList[16]   = asi.EDDoc.Seq
     _FldNameList[17]   = asi.EDDoc.rec_key
     _FldNameList[18]   = asi.EDDoc.Recv-Test-Prod
     _FldNameList[19]   = asi.EDDoc.P-FATime
     _FldNameList[20]   = asi.EDDoc.P-FADate
     _FldNameList[21]   = asi.EDDoc.OpenItem
     _FldNameList[22]   = asi.EDDoc.ISA
     _FldNameList[23]   = asi.EDDoc.InterpTime
     _FldNameList[24]   = asi.EDDoc.InterpDate
     _FldNameList[25]   = asi.EDDoc.GS
     _FldNameList[26]   = asi.EDDoc.FGTime
     _FldNameList[27]   = asi.EDDoc.FGSender
     _FldNameList[28]   = asi.EDDoc.FGRecvID
     _FldNameList[29]   = asi.EDDoc.FGID
     _FldNameList[30]   = asi.EDDoc.FGDate
     _FldNameList[31]   = asi.EDDoc.FGAgency
     _FldNameList[32]   = asi.EDDoc.C-FADate
     _FldNameList[33]   = asi.EDDoc.Error-count
     _FldNameList[34]   = asi.EDDoc.C-FATime
     _FldNameList[35]   = asi.EDDoc.EDI_Standard
     _FldNameList[36]   = asi.EDDoc.EDI_Agency
     _FldNameList[37]   = asi.EDDoc.ChgTime
     _FldNameList[38]   = asi.EDDoc.ChgOp
     _FldNameList[39]   = asi.EDDoc.ChgDate
     _FldNameList[40]   = asi.EDDoc.AddTime
     _FldNameList[41]   = asi.EDDoc.AddOp
     _FldNameList[42]   = asi.EDDoc.AddDate
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RepoQuery B-table-Win
PROCEDURE RepoQuery:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


    DO WITH FRAME {&FRAME-NAME}:
        REPOSITION {&browse-name} TO ROWID ip-rowid NO-ERROR.
        IF NOT ERROR-STATUS:ERROR THEN RUN dispatch ('row-changed').
    END.

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resend-doc B-table-Win 
PROCEDURE resend-doc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE rSaveQueryPosition AS ROWID NO-UNDO.
IF AVAILABLE eddoc THEN DO:
    rSaveQueryPosition = ROWID(eddoc).
  FIND bf-eddoc WHERE ROWID(bf-eddoc) EQ ROWID(eddoc) NO-ERROR.
  IF AVAIL bf-eddoc THEN
    bf-eddoc.posted = NOT bf-eddoc.posted.
  run dispatch in this-procedure ("open-query").
  RUN RepoQuery(INPUT rSaveQueryPosition).
  
END.
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
  {src/adm/template/snd-list.i "EDDoc"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE toggleResendDoc B-table-Win 
PROCEDURE toggleResendDoc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF AVAILABLE eddoc THEN DO:
  FIND bf-eddoc EXCLUSIVE-LOCK WHERE ROWID(bf-eddoc) EQ ROWID(eddoc)
    NO-ERROR.
  /* Toggle to resend */
  bf-eddoc.posted = NOT bf-eddoc.posted.
  RELEASE bf-eddoc.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'open-query':U ) .

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

