&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File: testers/OU1Tester.w

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: Created on 22nd Dec 2020
  
  Author : Rahul Rawat
          
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES oe-ordl oe-ord

/* Definitions for BROWSE BROWSE-2                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-2 oe-ord.ord-no oe-ord.ord-date ~
oe-ordl.req-date oe-ordl.qty oe-ordl.cust-no oe-ordl.i-no oe-ordl.i-name ~
oe-ordl.job-no oe-ordl.job-no2 oe-ordl.opened oe-ord.po-no 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-2 
&Scoped-define QUERY-STRING-BROWSE-2 FOR EACH oe-ordl ~
      WHERE oe-ordl.company EQ "001" ~
AND oe-ordl.ord-no EQ 9999999 NO-LOCK, ~
      EACH oe-ord OF oe-ordl NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-2 OPEN QUERY BROWSE-2 FOR EACH oe-ordl ~
      WHERE oe-ordl.company EQ "001" ~
AND oe-ordl.ord-no EQ 9999999 NO-LOCK, ~
      EACH oe-ord OF oe-ordl NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-2 oe-ordl oe-ord
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-2 oe-ordl
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-2 oe-ord


/* Definitions for FRAME F-Main                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-Main ~
    ~{&OPEN-QUERY-BROWSE-2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 fiNumRecords fiTimeOut fiCustNo btGo ~
btShowAll BROWSE-2 
&Scoped-Define DISPLAYED-OBJECTS fiNumRecords fiTimeOut fiCustNo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btGo 
     LABEL "Go" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btShowAll 
     LABEL "Show All" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE fiCustNo AS CHARACTER FORMAT "X(256)":U 
     LABEL "CustNo" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiNumRecords AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Number of Records" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fiTimeOut AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Query TimeOut" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 145.6 BY 3.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-2 FOR 
      oe-ordl, 
      oe-ord SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-2 W-Win _STRUCTURED
  QUERY BROWSE-2 NO-LOCK DISPLAY
      oe-ord.ord-no FORMAT ">>>>>9":U
      oe-ord.ord-date FORMAT "99/99/9999":U
      oe-ordl.req-date FORMAT "99/99/9999":U
      oe-ordl.qty FORMAT "->>,>>>,>>9.9<<":U
      oe-ordl.cust-no FORMAT "x(8)":U
      oe-ordl.i-no FORMAT "x(15)":U
      oe-ordl.i-name FORMAT "x(30)":U
      oe-ordl.job-no FORMAT "x(6)":U
      oe-ordl.job-no2 COLUMN-LABEL "Job No2" FORMAT ">9":U
      oe-ordl.opened FORMAT "Open/Closed":U
      oe-ord.po-no FORMAT "x(15)":U WIDTH 13.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SEPARATORS SIZE 150 BY 21.67
         FONT 5 ROW-HEIGHT-CHARS .81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiNumRecords AT ROW 3.14 COL 24.8 COLON-ALIGNED WIDGET-ID 2
     fiTimeOut AT ROW 3.14 COL 58.8 COLON-ALIGNED WIDGET-ID 4
     fiCustNo AT ROW 3.14 COL 91.8 COLON-ALIGNED WIDGET-ID 6
     btGo AT ROW 3.14 COL 114.4 WIDGET-ID 8
     btShowAll AT ROW 3.14 COL 130.8 WIDGET-ID 14
     BROWSE-2 AT ROW 5.52 COL 1 WIDGET-ID 200
     "msec" VIEW-AS TEXT
          SIZE 7 BY 1.19 AT ROW 3.19 COL 75 WIDGET-ID 12
          FONT 5
     RECT-1 AT ROW 1.81 COL 1.4 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 150.6 BY 26.19
         BGCOLOR 15 FGCOLOR 1 FONT 5
         DEFAULT-BUTTON btGo WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "OU1 Browse Tester"
         HEIGHT             = 26.19
         WIDTH              = 150.8
         MAX-HEIGHT         = 33.57
         MAX-WIDTH          = 273.2
         VIRTUAL-HEIGHT     = 33.57
         VIRTUAL-WIDTH      = 273.2
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-2 btShowAll F-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-2
/* Query rebuild information for BROWSE BROWSE-2
     _TblList          = "ASI.oe-ordl,ASI.oe-ord OF ASI.oe-ordl"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "oe-ordl.company EQ ""001""
AND oe-ordl.ord-no EQ 9999999"
     _FldNameList[1]   = ASI.oe-ord.ord-no
     _FldNameList[2]   = ASI.oe-ord.ord-date
     _FldNameList[3]   = ASI.oe-ordl.req-date
     _FldNameList[4]   = ASI.oe-ordl.qty
     _FldNameList[5]   = ASI.oe-ordl.cust-no
     _FldNameList[6]   = ASI.oe-ordl.i-no
     _FldNameList[7]   = ASI.oe-ordl.i-name
     _FldNameList[8]   = ASI.oe-ordl.job-no
     _FldNameList[9]   > ASI.oe-ordl.job-no2
"oe-ordl.job-no2" "Job No2" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   = ASI.oe-ordl.opened
     _FldNameList[11]   > ASI.oe-ord.po-no
"oe-ord.po-no" ? ? "character" ? ? ? ? ? ? no ? no no "13.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* OU1 Browse Tester */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* OU1 Browse Tester */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGo W-Win
ON CHOOSE OF btGo IN FRAME F-Main /* Go */
DO:
    DEFINE VARIABLE cLimitingQuery AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cBrowseQuery   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRecKey        AS CHARACTER NO-UNDO.
    
      ASSIGN 
        fiCustNo
        fiNumRecords
        fiTimeOut
        .    
    cLimitingQuery = "FOR EACH oe-ord NO-LOCK"
                    + " WHERE oe-ord.company EQ '001'" 
                    + " AND oe-ord.ord-date  GE " + STRING(TODAY - 365) 
                    + " AND oe-ord.stat NE 'W'"
                    + " AND oe-ord.opened EQ YES"      
                    + ", FIRST oe-ordl of oe-ord "
                    + " WHERE oe-ordl.cust-no BEGINS " + QUOTER(fiCustNo)
                    + " BY oe-ord.rec_key DESC" 
                    .
                    
    SESSION:SET-WAIT-STATE ("general").
    STATUS DEFAULT "Searching...". 
           
    RUN pPrepareAndExecuteQuery(
        INPUT  BROWSE BROWSE-2:HANDLE,
        INPUT  cLimitingQuery,
        INPUT  "oe-ord,oe-ordl",
        INPUT  "oe-ord",
        INPUT  "rec_key",
        INPUT  YES,
        INPUT  NO ,
        OUTPUT cRecKey
        ).  
    IF cRecKey EQ "" THEN DO:
        MESSAGE "No Records Found" 
        VIEW-AS ALERT-BOX ERROR.
        
        SESSION:SET-WAIT-STATE ("").
        STATUS DEFAULT.
         
        RETURN NO-APPLY.    
    END.                      
    cBrowseQuery = " FOR EACH oe-ordl NO-LOCK"
                   + " WHERE oe-ordl.company EQ '001'"
                   + " AND oe-ordl.opened EQ YES"
                   + " AND oe-ordl.stat NE 'C'" 
                   + " AND oe-ordl.cust-no BEGINS " + QUOTER(fiCustNo)
                   + " AND oe-ordl.rec_key GE " + QUOTER(cRecKey)
                   + " USE-INDEX rec_key"
                   + " ,FIRST oe-ord of oe-ordl"
                   + " WHERE oe-ord.stat NE 'W'"
                   + " USE-INDEX ord-no"
                   + " BY oe-ord.ord-date DESC BY oe-ord.ord-no DESC"
                   .     
                   
    RUN pPrepareAndExecuteQuery(
        INPUT  BROWSE BROWSE-2:QUERY,
        INPUT  cBrowseQuery,
        INPUT  "oe-ordl,oe-ord",
        INPUT  "",
        INPUT  "",
        INPUT  NO,
        INPUT  NO ,
        OUTPUT cRecKey
        ).  
        
    SESSION:SET-WAIT-STATE ("").
        
    STATUS DEFAULT "".                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btShowAll
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btShowAll W-Win
ON CHOOSE OF btShowAll IN FRAME F-Main /* Show All */
DO:
    DEFINE VARIABLE cShowAllQuery AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnValue  AS CHARACTER NO-UNDO.
    
      ASSIGN 
        fiCustNo
        fiNumRecords
        fiTimeOut
        .    
        
    SESSION:SET-WAIT-STATE ("general").
    STATUS DEFAULT "Searching...". 
           
                     
    cShowAllQuery  = " FOR EACH oe-ordl NO-LOCK"
                   + " WHERE oe-ordl.company EQ '001'"
                   + " AND oe-ordl.opened EQ YES"
                   + " AND oe-ordl.stat NE 'C'" 
                   + " AND oe-ordl.cust-no BEGINS " + QUOTER(fiCustNo)
                   + " ,FIRST oe-ord of oe-ordl"
                   + " WHERE oe-ord.stat NE 'W'"
                   + " USE-INDEX ord-no"
                   + " BY oe-ord.ord-date DESC BY oe-ord.ord-no DESC"
                   .     
                   
    RUN pPrepareAndExecuteQuery(
        INPUT  BROWSE BROWSE-2:QUERY,
        INPUT  cShowAllQuery ,
        INPUT  "oe-ordl,oe-ord",
        INPUT  "",
        INPUT  "",
        INPUT  NO,
        INPUT  NO ,
        OUTPUT cReturnValue 
        ).  
        
    SESSION:SET-WAIT-STATE ("").
        
    STATUS DEFAULT "". 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY fiNumRecords fiTimeOut fiCustNo 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE RECT-1 fiNumRecords fiTimeOut fiCustNo btGo btShowAll BROWSE-2 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pPrepareAndExecuteQuery W-Win 
PROCEDURE pPrepareAndExecuteQuery PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iphdBrowseQuery    AS HANDLE    NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQueryString     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBufferString    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTableName       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFieldName       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsLimitingQuery AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsBreakByUsed   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReturnValue     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdQuery       AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdBuffer      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.   
    DEFINE VARIABLE hdTableBuffer AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cMessage      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iTimeTaken    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTotalCount   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iIndex        AS INTEGER   NO-UNDO.
    
    /* If is a limiting query then create query and set the buffers */
    IF iplIsLimitingQuery THEN DO:    
        CREATE QUERY hdQuery.       
        DO iCount = 1 TO NUM-ENTRIES(ipcBufferString):
            CREATE BUFFER hdBuffer FOR TABLE ENTRY(iCount,ipcBufferString).
            hdQuery:ADD-BUFFER(hdBuffer).
            IF hdBuffer:NAME EQ ipcTableName THEN
                hdTableBuffer = hdBuffer.                                    
        END.
    END.
    ELSE 
        hdQuery = iphdBrowseQuery.
        
    hdQuery:QUERY-PREPARE(ipcQueryString). 
    hdQuery:QUERY-OPEN().
      
    IF iplIsLimitingQuery THEN DO:
        hdQuery:GET-NEXT().
        
        MainLoop:    
        REPEAT:
            /*Reset the Timer */
            ETIME(YES). 
            iCount = 0.
            DO WHILE ETIME LT fiTimeOut AND NOT hdQuery:QUERY-OFF-END:
                IF iplIsLimitingQuery THEN DO:
                    opcReturnValue = STRING(hdBuffer:BUFFER-FIELD(ipcFieldName):BUFFER-VALUE).
                    ASSIGN 
                        iTotalCount = iTotalCount + 1
                        iCount      = iCount + 1
                        .
                    IF iCount GE fiNumRecords THEN DO:                   
                        iTimeTaken = iTimeTaken + ETIME.
                        LEAVE.
                    END.    
                END. 
             
                hdQuery:GET-NEXT().                          
            END.
    
            /* If Query has ended, leave the mainloop */
            IF hdQuery:QUERY-OFF-END THEN 
                LEAVE MainLoop. 
            ELSE IF iplIsLimitingQuery THEN DO:
                IF iCount GE fiNumRecords THEN 
                    cMessage = "You have reached your record limit of " + STRING(iTotalCount) + ", Time taken = " + STRING(iTimeTaken) + " ms" + ". Do you want to continue searching?".
                ELSE DO:
                    iIndex = iIndex + 1.
                    cMessage = "You have reached timelimit of " + STRING(fiTimeOut * iIndex) + " ms, Records searched= " + STRING(iTotalCount) + ". Do you want to continue searching?".         
                END.  
                MESSAGE cMessage
                VIEW-AS ALERT-BOX BUTTONS YES-NO 
                UPDATE lResponse AS LOGICAL. 
            
                /* Break the query if user doesnot want to proceed */    
                IF NOT lResponse THEN 
                    LEAVE MainLoop. 
                ELSE 
                    NEXT MainLoop.
            END. /* End of Else IF */         
        END.
    END. 
        
    /*Delete Query Handle*/
    IF iplIsLimitingQuery AND VALID-HANDLE(hdQuery) THEN DO: 
        IF hdQuery:IS-OPEN THEN
            hdQuery:QUERY-CLOSE().
        DELETE OBJECT hdQuery.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "oe-ordl"}
  {src/adm/template/snd-list.i "oe-ord"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

