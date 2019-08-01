&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File:  

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES APIOutbound

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table APIOutbound.apiID APIOutbound.clientID APIOutbound.authType APIOutbound.requestDataType APIOutbound.requestVerb APIOutbound.isActive   
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table   
&Scoped-define SELF-NAME br_table
&Scoped-define QUERY-STRING-br_table FOR EACH APIOutbound WHERE     (IF fiAPIID:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN         TRUE      ELSE         APIOutbound.apiID BEGINS fiAPIID:SCREEN-VALUE) AND     (IF fiClientID:SCREEN-VALUE EQ "" THEN         TRUE      ELSE         APIOutbound.clientID BEGINS fiClientID:SCREEN-VALUE) AND     (IF cbRequestDataType:SCREEN-VALUE EQ "All" THEN         TRUE      ELSE         APIOutbound.requestDataType EQ cbRequestDataType:SCREEN-VALUE) AND     (IF cbRequestVerb:SCREEN-VALUE EQ "All" THEN         TRUE      ELSE         APIOutbound.requestVerb EQ cbRequestVerb:SCREEN-VALUE) AND     (IF cbStatus:SCREEN-VALUE EQ "All" THEN         TRUE      ELSE IF cbStatus:SCREEN-VALUE EQ "Active" THEN         APIOutbound.isActive      ELSE         NOT APIOutbound.isActive)     NO-LOCK     ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY {&SELF-NAME} FOR EACH APIOutbound WHERE     (IF fiAPIID:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN         TRUE      ELSE         APIOutbound.apiID BEGINS fiAPIID:SCREEN-VALUE) AND     (IF fiClientID:SCREEN-VALUE EQ "" THEN         TRUE      ELSE         APIOutbound.clientID BEGINS fiClientID:SCREEN-VALUE) AND     (IF cbRequestDataType:SCREEN-VALUE EQ "All" THEN         TRUE      ELSE         APIOutbound.requestDataType EQ cbRequestDataType:SCREEN-VALUE) AND     (IF cbRequestVerb:SCREEN-VALUE EQ "All" THEN         TRUE      ELSE         APIOutbound.requestVerb EQ cbRequestVerb:SCREEN-VALUE) AND     (IF cbStatus:SCREEN-VALUE EQ "All" THEN         TRUE      ELSE IF cbStatus:SCREEN-VALUE EQ "Active" THEN         APIOutbound.isActive      ELSE         NOT APIOutbound.isActive)     NO-LOCK     ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table APIOutbound
&Scoped-define FIRST-TABLE-IN-QUERY-br_table APIOutbound


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiAPIId fiClientID cbRequestDataType ~
cbRequestVerb cbStatus btGo br_table 
&Scoped-Define DISPLAYED-OBJECTS fiAPIIDLabel fiClientIDLabel ~
fiRequestDataTypeLabel fiRequestVerbLabel fiStatusLabel fiAPIId fiClientID ~
cbRequestDataType cbRequestVerb cbStatus 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" B-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Advanced Query Options" B-table-Win _INLINE
/* Actions: ? adm/support/advqedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
&BROWSE-NAME
</KEY-OBJECT>
<SORTBY-OPTIONS>
</SORTBY-OPTIONS> 
<SORTBY-RUN-CODE>
************************
* Set attributes related to SORTBY-OPTIONS */
RUN set-attribute-list (
    'SortBy-Options = ""':U).
/************************
</SORTBY-RUN-CODE> 
<FILTER-ATTRIBUTES>
</FILTER-ATTRIBUTES> */   

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btGo 
     LABEL "Go" 
     SIZE 15 BY 1.14
     FONT 35.

DEFINE VARIABLE cbRequestDataType AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","JSON","XML" 
     DROP-DOWN-LIST
     SIZE 27 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cbRequestVerb AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","Post","Get" 
     DROP-DOWN-LIST
     SIZE 20.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE cbStatus AS CHARACTER FORMAT "X(256)":U INITIAL "All" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "All","Active","Inactive" 
     DROP-DOWN-LIST
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiAPIId AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiAPIIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "API ID" 
     VIEW-AS FILL-IN 
     SIZE 9.4 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiClientID AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fiClientIDLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Client ID" 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestDataTypeLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Request Data Type" 
     VIEW-AS FILL-IN 
     SIZE 27 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiRequestVerbLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Request Verb" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

DEFINE VARIABLE fiStatusLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Status" 
     VIEW-AS FILL-IN 
     SIZE 9.8 BY 1
     FGCOLOR 9 FONT 35 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      APIOutbound SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _FREEFORM
  QUERY br_table NO-LOCK DISPLAY
      APIOutbound.apiID COLUMN-LABEL "API ID" FORMAT "x(32)":U
            WIDTH 33.2
      APIOutbound.clientID COLUMN-LABEL "Client ID" FORMAT "x(16)":U
            WIDTH 23.6
      APIOutbound.authType COLUMN-LABEL "Authentication Type" FORMAT "x(8)":U
            WIDTH 27
      APIOutbound.requestDataType COLUMN-LABEL "Request Data Type" FORMAT "x(8)":U
            WIDTH 25.6
      APIOutbound.requestVerb COLUMN-LABEL "Request Verb" FORMAT "x(8)":U
            WIDTH 26.2
      APIOutbound.isActive COLUMN-LABEL "Status" FORMAT "Active/Inactive":U
            WIDTH 20.8
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 166 BY 18.1
         BGCOLOR 15  ROW-HEIGHT-CHARS .57 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fiAPIIDLabel AT ROW 1.14 COL 10 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     fiClientIDLabel AT ROW 1.14 COL 38.6 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     fiRequestDataTypeLabel AT ROW 1.14 COL 61.6 COLON-ALIGNED NO-LABEL WIDGET-ID 18
     fiRequestVerbLabel AT ROW 1.14 COL 97 COLON-ALIGNED NO-LABEL WIDGET-ID 16
     fiStatusLabel AT ROW 1.14 COL 126.2 COLON-ALIGNED NO-LABEL WIDGET-ID 14
     fiAPIId AT ROW 2.19 COL 4.4 NO-LABEL WIDGET-ID 2
     fiClientID AT ROW 2.19 COL 33.6 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     cbRequestDataType AT ROW 2.19 COL 61.6 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     cbRequestVerb AT ROW 2.19 COL 96.2 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     cbStatus AT ROW 2.19 COL 123 COLON-ALIGNED NO-LABEL WIDGET-ID 22
     btGo AT ROW 3.86 COL 4 WIDGET-ID 24
     br_table AT ROW 5.52 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   Allow: Basic,Browse
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
  CREATE WINDOW B-table-Win ASSIGN
         HEIGHT             = 22.81
         WIDTH              = 166.8.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table btGo F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiAPIId IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiAPIIDLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiClientIDLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRequestDataTypeLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiRequestVerbLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiStatusLabel IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH APIOutbound WHERE
    (IF fiAPIID:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN
        TRUE
     ELSE
        APIOutbound.apiID BEGINS fiAPIID:SCREEN-VALUE) AND
    (IF fiClientID:SCREEN-VALUE EQ "" THEN
        TRUE
     ELSE
        APIOutbound.clientID BEGINS fiClientID:SCREEN-VALUE) AND
    (IF cbRequestDataType:SCREEN-VALUE EQ "All" THEN
        TRUE
     ELSE
        APIOutbound.requestDataType EQ cbRequestDataType:SCREEN-VALUE) AND
    (IF cbRequestVerb:SCREEN-VALUE EQ "All" THEN
        TRUE
     ELSE
        APIOutbound.requestVerb EQ cbRequestVerb:SCREEN-VALUE) AND
    (IF cbStatus:SCREEN-VALUE EQ "All" THEN
        TRUE
     ELSE IF cbStatus:SCREEN-VALUE EQ "Active" THEN
        APIOutbound.isActive
     ELSE
        NOT APIOutbound.isActive)
    NO-LOCK
    ~{&SORTBY-PHRASE}.
     _END_FREEFORM
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Query            is NOT OPENED
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
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
    DEFINE VARIABLE phandle  AS HANDLE    NO-UNDO.
    DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
        
    {methods/run_link.i "CONTAINER-SOURCE" "SELECT-PAGE" "(2)"}.
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
    /* Do not disable this code or no updates will take place except
     by pressing the Save button on an Update SmartPanel. */
   {src/adm/template/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  /* This ADM trigger code must be preserved in order to notify other
     objects when the browser's current row changes. */
  {src/adm/template/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btGo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btGo B-table-Win
ON CHOOSE OF btGo IN FRAME F-Main /* Go */
DO:
    {&OPEN-QUERY-br_table}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable B-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    RUn pInit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInit B-table-Win 
PROCEDURE pInit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN dispatch ('open-query').
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
  {src/adm/template/snd-list.i "APIOutbound"}

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

