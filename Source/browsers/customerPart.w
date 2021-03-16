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

  File: browsers/customerPart.w

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
&SCOPED-DEFINE winReSize
{methods/defines/winReSize.i}
&SCOPED-DEFINE browseOnly
&SCOPED-DEFINE noSortByField
{custom/globdefs.i}
&SCOPED-DEFINE yellowColumnsName customerPart

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustomer     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipTo       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItem         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustomerPart AS CHARACTER NO-UNDO.

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
&Scoped-define INTERNAL-TABLES customerPart

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table customerPart.customerID ~
customerPart.shipToID customerPart.itemID customerPart.customerPart 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH customerPart WHERE ~{&KEY-PHRASE} ~
      AND customerPart.company = cCompany AND ~
ASI.customerPart.customerID BEGINS cCustomer AND ~
ASI.customerPart.shipToID BEGINS cShipTo AND ~
ASI.customerPart.itemID BEGINS cItem AND ~
ASI.customerPart.customerPart BEGINS cCustomerPart NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH customerPart WHERE ~{&KEY-PHRASE} ~
      AND customerPart.company = cCompany AND ~
ASI.customerPart.customerID BEGINS cCustomer AND ~
ASI.customerPart.shipToID BEGINS cShipTo AND ~
ASI.customerPart.itemID BEGINS cItem AND ~
ASI.customerPart.customerPart BEGINS cCustomerPart NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table customerPart
&Scoped-define FIRST-TABLE-IN-QUERY-br_table customerPart


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 btSearch fiCustomer fiShipTo ~
fiItem fiCustomerPart br_table 
&Scoped-Define DISPLAYED-OBJECTS fiCustomer fiShipTo fiItem fiCustomerPart 

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
DEFINE BUTTON btSearch 
     IMAGE-UP FILE "Graphics/32x32/search_new.png":U
     LABEL "" 
     SIZE 6.8 BY 1.62.

DEFINE VARIABLE fiCustomer AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiCustomerPart AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28.4 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiItem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24.4 BY 1
     FONT 22 NO-UNDO.

DEFINE VARIABLE fiShipTo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY 1
     FONT 22 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 108 BY 2.2
     BGCOLOR 23 FGCOLOR 24 .

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE .5 BY 1.8
     BGCOLOR 23 FGCOLOR 24 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      customerPart SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      customerPart.customerID LABEL-BGCOLOR 14 FORMAT "x(8)":U WIDTH 18.2
      customerPart.shipToID LABEL-BGCOLOR 14 FORMAT "x(8)":U WIDTH 25.2
      customerPart.itemID LABEL-BGCOLOR 14 FORMAT "x(15)":U WIDTH 31.2
      customerPart.customerPart LABEL-BGCOLOR 14 FORMAT "x(32)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 120 BY 13.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     btSearch AT ROW 1.43 COL 100.8 WIDGET-ID 18
     fiCustomer AT ROW 2.05 COL 4 NO-LABEL WIDGET-ID 4
     fiShipTo AT ROW 2.05 COL 22.6 NO-LABEL WIDGET-ID 6
     fiItem AT ROW 2.05 COL 41.6 NO-LABEL WIDGET-ID 10
     fiCustomerPart AT ROW 2.05 COL 67.6 NO-LABEL WIDGET-ID 14
     br_table AT ROW 3.57 COL 1
     "Ship To #" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.38 COL 22.6 WIDGET-ID 8
          BGCOLOR 23 FGCOLOR 24 FONT 22
     "Customer #" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.38 COL 4 WIDGET-ID 2
          BGCOLOR 23 FGCOLOR 24 FONT 22
     "Customer Part #" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.38 COL 67.8 WIDGET-ID 16
          BGCOLOR 23 FGCOLOR 24 FONT 22
     "Item #" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 1.38 COL 41.6 WIDGET-ID 12
          BGCOLOR 23 FGCOLOR 24 FONT 22
     RECT-1 AT ROW 1.19 COL 1.8 WIDGET-ID 20
     RECT-2 AT ROW 1.38 COL 98.6 WIDGET-ID 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 15  WIDGET-ID 100.


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
         HEIGHT             = 16.29
         WIDTH              = 120.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}
{src/adm/method/query.i}
{custom/yellowColumns.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW B-table-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table fiCustomerPart F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fiCustomer IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiCustomerPart IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiItem IN FRAME F-Main
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiShipTo IN FRAME F-Main
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.customerPart"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.customerPart.company = cCompany AND
ASI.customerPart.customerID BEGINS cCustomer AND
ASI.customerPart.shipToID BEGINS cShipTo AND
ASI.customerPart.itemID BEGINS cItem AND
ASI.customerPart.customerPart BEGINS cCustomerPart"
     _FldNameList[1]   > ASI.customerPart.customerID
"customerPart.customerID" ? ? "character" ? ? ? ? ? ? no ? no no "18.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.customerPart.shipToID
"customerPart.shipToID" ? ? "character" ? ? ? ? ? ? no ? no no "25.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > ASI.customerPart.itemID
"customerPart.itemID" ? ? "character" ? ? ? ? ? ? no ? no no "31.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   = ASI.customerPart.customerPart
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main B-table-Win
ON HELP OF FRAME F-Main
DO:
    DEFINE VARIABLE cFieldsValue  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFoundValue   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE recFoundRecID AS RECID     NO-UNDO.

    CASE FOCUS:NAME :
        WHEN "fiCustomer"  THEN DO:
            RUN system/openLookup.p (
            INPUT  g_company, 
            INPUT  "",  /* Lookup ID */
            INPUT  23,  /* Subject ID */
            INPUT  "",  /* User ID */
            INPUT  0,   /* Param Value ID */
            OUTPUT cFieldsValue, 
            OUTPUT cFoundValue, 
            OUTPUT recFoundRecID
            ).   
            IF cFoundValue <> "" THEN 
                ASSIGN FOCUS:SCREEN-VALUE = cFoundValue.         
        END.
        WHEN "fiShipTo"  THEN DO:
            RUN system/openLookup.p (
            INPUT  g_company, 
            INPUT  "",  /* Lookup ID */
            INPUT  122,  /* Subject ID */
            INPUT  "",  /* User ID */
            INPUT  0,   /* Param Value ID */
            OUTPUT cFieldsValue, 
            OUTPUT cFoundValue, 
            OUTPUT recFoundRecID
            ).   
            IF cFoundValue <> "" THEN 
                ASSIGN FOCUS:SCREEN-VALUE = cFoundValue.         
        END.
        WHEN "fiItem"  THEN DO:
            RUN system/openLookup.p (
            INPUT  g_company, 
            INPUT  "",  /* Lookup ID */
            INPUT  25,  /* Subject ID */
            INPUT  "",  /* User ID */
            INPUT  0,   /* Param Value ID */
            OUTPUT cFieldsValue, 
            OUTPUT cFoundValue, 
            OUTPUT recFoundRecID
            ).   
            IF cFoundValue <> "" THEN 
                ASSIGN FOCUS:SCREEN-VALUE = cFoundValue.         
        END.

    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Browser-Table B-table-Win
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
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btSearch
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btSearch B-table-Win
ON CHOOSE OF btSearch IN FRAME F-Main
DO:
    ASSIGN
        cCustomer     = fiCustomer:SCREEN-VALUE
        cShipTo       = fiShipTo:SCREEN-VALUE
        cItem         = fiItem:SCREEN-VALUE
        cCustomerPart = fiCustomerPart:SCREEN-VALUE
        .

    RUN dispatch (
        INPUT "open-query"
        ).            
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
    RUN pInit.
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
    RUN spGetSessionParam ("Company", OUTPUT cCompany).
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
  {src/adm/template/snd-list.i "customerPart"}

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

