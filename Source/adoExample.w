&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

DEF VAR CnHandle AS COM-HANDLE NO-UNDO.
DEF VAR RsHandle AS COM-HANDLE NO-UNDO.
DEF VAR cidhdl   AS COM-HANDLE NO-UNDO.
DEF VAR cnmhdl   AS COM-HANDLE NO-UNDO.
DEF VAR cnQtyHdl   AS COM-HANDLE NO-UNDO.


DEF VAR FieldsHandle AS COM-HANDLE NO-UNDO.

DEF VAR tt-handle AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btn_Connection btn_CloseEverything ~
btn_CloseCon btn_ConString btn_OpenCon btn_DisplayFields btn_RecordSet ~
btn_UpdateField btn_ListFields btn_OpenCustItemRecordSet btn_PrevRec ~
btn_LoadCustomerItemList btn_NextRec sel_CustomerItems 
&Scoped-Define DISPLAYED-OBJECTS sel_CustomerItems 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-AddField C-Win 
FUNCTION f-AddField RETURNS LOGICAL(
                INPUT piTTHandle AS HANDLE, 
                INPUT piFldName    AS CHAR,
                INPUT piFldType    AS CHAR,
                INPUT piExtents    AS INT
              )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f-FieldType C-Win 
FUNCTION f-FieldType RETURNS CHARACTER
  ( INPUT piFieldTypeNum AS int )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_CloseCon 
     LABEL "Close Connection" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btn_CloseEverything 
     LABEL "Close Everything" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btn_Connection 
     LABEL "Step 1: Create Connection" 
     SIZE 38 BY 1.14.

DEFINE BUTTON btn_ConString 
     LABEL "Step 2: Set Connection Str" 
     SIZE 38 BY 1.14.

DEFINE BUTTON btn_DisplayFields 
     LABEL "Display Fields" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btn_ListFields 
     LABEL "List Fields" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btn_LoadCustomerItemList 
     LABEL "Step 6: Load Customer Item List Below" 
     SIZE 44 BY 1.14.

DEFINE BUTTON btn_NextRec 
     LABEL "Next Rec" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btn_OpenCon 
     LABEL "Step 3: Open Connection" 
     SIZE 38 BY 1.14.

DEFINE BUTTON btn_OpenCustItemRecordSet 
     LABEL "Step 5: Open Customer Item RecordSet" 
     SIZE 44 BY 1.14.

DEFINE BUTTON btn_PrevRec 
     LABEL "Prev Rec" 
     SIZE 26 BY 1.14.

DEFINE BUTTON btn_RecordSet 
     LABEL "Step 4: Create RecordSet" 
     SIZE 38 BY 1.14.

DEFINE BUTTON btn_UpdateField 
     LABEL "Update Field" 
     SIZE 26 BY 1.14.

DEFINE VARIABLE sel_CustomerItems AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     SIZE 120 BY 11.91 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btn_Connection AT ROW 2.19 COL 7
     btn_CloseEverything AT ROW 2.19 COL 57
     btn_CloseCon AT ROW 3.38 COL 57
     btn_ConString AT ROW 3.62 COL 7
     btn_OpenCon AT ROW 5.05 COL 7
     btn_DisplayFields AT ROW 5.76 COL 92
     btn_RecordSet AT ROW 6.48 COL 7
     btn_UpdateField AT ROW 7 COL 92
     btn_ListFields AT ROW 8.24 COL 92
     btn_OpenCustItemRecordSet AT ROW 9.1 COL 7
     btn_PrevRec AT ROW 9.43 COL 92
     btn_LoadCustomerItemList AT ROW 10.52 COL 7
     btn_NextRec AT ROW 10.62 COL 92
     sel_CustomerItems AT ROW 12.91 COL 2 NO-LABEL
     "Misc. ADO Commands" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 3.86 COL 94
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 122.6 BY 24.19.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "ADO Example Code"
         HEIGHT             = 24.19
         WIDTH              = 122.6
         MAX-HEIGHT         = 24.19
         MAX-WIDTH          = 154.8
         VIRTUAL-HEIGHT     = 24.19
         VIRTUAL-WIDTH      = 154.8
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* ADO Example Code */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* ADO Example Code */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "Choose" TO btn_CloseEverything IN FRAME {&FRAME-NAME}.
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_CloseCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_CloseCon C-Win
ON CHOOSE OF btn_CloseCon IN FRAME DEFAULT-FRAME /* Close Connection */
DO:
  CnHandle:Close.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_CloseEverything
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_CloseEverything C-Win
ON CHOOSE OF btn_CloseEverything IN FRAME DEFAULT-FRAME /* Close Everything */
DO:
  
  IF RsHandle NE 0 AND RsHandle NE ? THEN do:
     RsHandle:CLOSE NO-ERROR.
     RELEASE OBJECT RsHandle NO-ERROR.
  END.
  IF CnHandle NE 0 AND CnHandle NE ? THEN do:
     CnHandle:Close NO-ERROR.
     RELEASE OBJECT CnHandle NO-ERROR.
  END.
  IF cidhdl NE 0 AND cidhdl NE ? THEN do:
     cidhdl:CLOSE NO-ERROR.
     RELEASE OBJECT cidhdl NO-ERROR.
  END.
  IF cnmhdl NE 0 AND cnmhdl NE ? THEN do:
     cnmhdl:Close NO-ERROR.
     RELEASE OBJECT cnmhdl NO-ERROR.
  END.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Connection
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Connection C-Win
ON CHOOSE OF btn_Connection IN FRAME DEFAULT-FRAME /* Step 1: Create Connection */
DO:
    /*
      The M$ ADO libs have to exist on your machine.  Your version may be higher or lower.
      This version is ADO 2.6
    */

    CREATE "ADODB.Connection.2.6" CnHandle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ConString
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ConString C-Win
ON CHOOSE OF btn_ConString IN FRAME DEFAULT-FRAME /* Step 2: Set Connection Str */
DO:

  /*
     Ron:  Your connection string will be a little different.  It will have a reference to a
     DSN (your ODBC DSN)
     Also, I have not included the Access Db to which this code refers.  It's proprietary client
     info
  */
  CnHandle:ConnectionString = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=C:\Impact\WallUnits\PERMDATA for Impact.MDB".
  /*CnHandle:OPEN("C:\Impact\WallUnits\PERMDATA for Impact.MDB",,,).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_DisplayFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_DisplayFields C-Win
ON CHOOSE OF btn_DisplayFields IN FRAME DEFAULT-FRAME /* Display Fields */
DO:
  DEF VAR cid AS CHAR NO-UNDO.
  DEF VAR cnm AS CHAR NO-UNDO.

  

  cidhdl = RsHandle:FIELDS("BDID").
  cnmhdl = RsHandle:FIELDS("BDDesc").
  
  MESSAGE
         cidhdl:Value
         SKIP
         cnmhdl:Value
  VIEW-AS ALERT-BOX.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_ListFields
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_ListFields C-Win
ON CHOOSE OF btn_ListFields IN FRAME DEFAULT-FRAME /* List Fields */
DO:
  DEF VAR i AS INT NO-UNDO.
  DEF VAR x AS INT NO-UNDO.
  DEF VAR FldHandle AS COM-HANDLE NO-UNDO.
  DEF VAR vOk AS LOG NO-UNDO.

  /*CREATE TEMP-TABLE tt-handle.*/
  i = FieldsHandle:COUNT.

  DO X = 0 TO i - 1:
     FldHandle = FieldsHandle:ITEM(X).

     /*
     vLog = dynamic-function('f-AddField',
                                           INPUT TT-Handle,
                                           INPUT fldHandle:NAME,
                                           INPUT piFldType    AS CHAR,
                                           INPUT 0 /*piExtents AS INT*/
                            ).
                            */

     MESSAGE 
             "pos" x              skip
             "name" fldHandle:NAME SKIP
             "type" fldHandle:TYPE SKIP
             "format" fldHandle:DataFormat SKIP
              "OriginalValue" FldHandle:OriginalValue SKIP
              "UnderlyingValue" FldHandle:UnderlyingValue SKIP
              "Value" FldHandle:Value

     VIEW-AS ALERT-BOX.
  END.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_LoadCustomerItemList
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_LoadCustomerItemList C-Win
ON CHOOSE OF btn_LoadCustomerItemList IN FRAME DEFAULT-FRAME /* Step 6: Load Customer Item List Below */
DO:
  DEF VAR i AS INT NO-UNDO.
  /*
    Iterate thru the record set and put the results into the selection list
  */
  SESSION:SET-WAIT-STATE("GENERAL").
  RsHandle:MoveFirst.
  ETIME(YES).
  DO WHILE NOT RsHandle:BOF() AND NOT RsHandle:EOF():
     ASSIGN 
            cidhdl   = RsHandle:FIELDS("BDID")
            cnmhdl   = RsHandle:FIELDS("BDDesc")
            cnQtyHdl = RsHandle:FIELDS("AsOf")
            i = i + 1.
     sel_CustomerItems:ADD-LAST(string(i) + " " + cidhdl:VALUE + " " + cnmhdl:Value + " " + cnQtyHdl:Value).
     RsHandle:MoveNext.
  END.
  SESSION:SET-WAIT-STATE("").
  MESSAGE ETIME i VIEW-AS ALERT-BOX.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_NextRec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_NextRec C-Win
ON CHOOSE OF btn_NextRec IN FRAME DEFAULT-FRAME /* Next Rec */
DO:
  

  RsHandle:MoveNext.
  IF RsHandle:EOF THEN RsHandle:MoveLast.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_OpenCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_OpenCon C-Win
ON CHOOSE OF btn_OpenCon IN FRAME DEFAULT-FRAME /* Step 3: Open Connection */
DO:
    /*
     Connect to Db
    */
    CnHandle:OPEN (,,,) NO-ERROR.

  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_OpenCustItemRecordSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_OpenCustItemRecordSet C-Win
ON CHOOSE OF btn_OpenCustItemRecordSet IN FRAME DEFAULT-FRAME /* Step 5: Open Customer Item RecordSet */
DO:
  
  /*
    Set the lock type, feed it some SQueeL, and let it rip
  */
  RsHandle:LockType = 2.
  RsHandle:OPEN(
                "Select BDID,BDDesc,Qty,AsOf from tblBillingDeals",
                CnHandle,
                1,
                 ,
                ).
  FieldsHandle = RsHandle:FIELDS.

 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_PrevRec
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_PrevRec C-Win
ON CHOOSE OF btn_PrevRec IN FRAME DEFAULT-FRAME /* Prev Rec */
DO:
  
  RsHandle:MovePrevious.
  IF RsHandle:BOF THEN RsHandle:MoveFirst.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_RecordSet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_RecordSet C-Win
ON CHOOSE OF btn_RecordSet IN FRAME DEFAULT-FRAME /* Step 4: Create RecordSet */
DO:
    /*
      Create an ADO recordset
    */
    CREATE "ADODB.Recordset.2.6" RsHandle.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_UpdateField
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_UpdateField C-Win
ON CHOOSE OF btn_UpdateField IN FRAME DEFAULT-FRAME /* Update Field */
DO:
  
  cnmhdl:VALUE = "John's Testing".
  rsHandle:UPDATE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY sel_CustomerItems 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btn_Connection btn_CloseEverything btn_CloseCon btn_ConString 
         btn_OpenCon btn_DisplayFields btn_RecordSet btn_UpdateField 
         btn_ListFields btn_OpenCustItemRecordSet btn_PrevRec 
         btn_LoadCustomerItemList btn_NextRec sel_CustomerItems 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ip-AddFieldsToTT C-Win 
PROCEDURE ip-AddFieldsToTT :
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-AddField C-Win 
FUNCTION f-AddField RETURNS LOGICAL(
                INPUT piTTHandle AS HANDLE, 
                INPUT piFldName    AS CHAR,
                INPUT piFldType    AS CHAR,
                INPUT piExtents    AS INT
              ) :

  DEF VAR vRetVal AS LOG INIT FALSE.

  vRetVal = piTTHandle:ADD-NEW-FIELD(piFldName,piFldType,piExtents,?).


  RETURN vRetVal.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f-FieldType C-Win 
FUNCTION f-FieldType RETURNS CHARACTER
  ( INPUT piFieldTypeNum AS int ) :

  

  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

