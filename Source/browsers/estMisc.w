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

  File: browsers/estMisc.w 

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
DEFINE VARIABLE cCompany    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstimateNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcOnDesc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCalcToDesc AS CHARACTER NO-UNDO.

RUN spGetSessionParam("Company", OUTPUT cCompany).

/* Required for run_link.i */
DEFINE VARIABLE char-hdl  AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle   AS HANDLE    NO-UNDO.

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
&Scoped-define INTERNAL-TABLES estMisc

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table estMisc.costDescription ~
estMisc.estCostCalcBy ~
fGetCalcDesc(estMisc.estCostCategoryID, TRUE) @ cCalcToDesc ~
fGetCalcDesc(estMisc.estCostCalcSource, FALSE) @ cCalcOnDesc ~
estMisc.flatFeeCharge estMisc.chargePercent 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH estMisc WHERE ~{&KEY-PHRASE} ~
      AND estMisc.company = cCompany AND estMisc.estimateNo = cEstimateNo NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH estMisc WHERE ~{&KEY-PHRASE} ~
      AND estMisc.company = cCompany AND estMisc.estimateNo = cEstimateNo NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table estMisc
&Scoped-define FIRST-TABLE-IN-QUERY-br_table estMisc


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetCalcDesc B-table-Win 
FUNCTION fGetCalcDesc RETURNS CHARACTER
  ( ipcCalcID AS CHARACTER, iplIsCategory AS LOGICAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      estMisc SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      estMisc.costDescription FORMAT "x(40)":U WIDTH 32.2
      estMisc.estCostCalcBy FORMAT "x(32)":U WIDTH 17.2
      fGetCalcDesc(estMisc.estCostCategoryID, TRUE) @ cCalcToDesc COLUMN-LABEL "Category" FORMAT "X(50)":U
            WIDTH 23.2
      fGetCalcDesc(estMisc.estCostCalcSource, FALSE) @ cCalcOnDesc COLUMN-LABEL "Cost Source" FORMAT "X(50)":U
            WIDTH 26.2
      estMisc.flatFeeCharge FORMAT ">>,>>,>>9.99<<<<":U
      estMisc.chargePercent FORMAT ">>,>>,>>9.99<<<<":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN SEPARATORS SIZE 129 BY 14.29.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


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
         HEIGHT             = 14.43
         WIDTH              = 129.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB B-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/browser.i}

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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "ASI.estMisc"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _Where[1]         = "ASI.estMisc.company = cCompany AND ASI.estMisc.estimateNo = cEstimateNo"
     _FldNameList[1]   > ASI.estMisc.costDescription
"costDescription" ? ? "character" ? ? ? ? ? ? no ? no no "32.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > ASI.estMisc.estCostCalcBy
"estCostCalcBy" ? ? "character" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > "_<CALC>"
"fGetCalcDesc(estMisc.estCostCategoryID, TRUE) @ cCalcToDesc" "Category" "X(50)" ? ? ? ? ? ? ? no ? no no "23.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"fGetCalcDesc(estMisc.estCostCalcSource, FALSE) @ cCalcOnDesc" "Cost Source" "X(50)" ? ? ? ? ? ? ? no ? no no "26.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = ASI.estMisc.flatFeeCharge
     _FldNameList[6]   = ASI.estMisc.chargePercent
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 

&Scoped-define BROWSE-NAME br_table

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-open-query B-table-Win 
PROCEDURE local-open-query :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    {methods/run_link.i "CONTAINER-SOURCE" "GetEstimateNo" "(OUTPUT cEstimateNo)"}
    
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
  {src/adm/template/snd-list.i "estMisc"}

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

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetCalcDesc B-table-Win 
FUNCTION fGetCalcDesc RETURNS CHARACTER
  ( ipcCalcID AS CHARACTER, iplIsCategory AS LOGICAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE BUFFER bf-estCostGroupSystem      FOR estCostGroupSystem.
    DEFINE BUFFER bf-estCostGroupLevelSystem FOR estCostGroupLevelSystem.
    DEFINE BUFFER bf-estCostCategorySystem   FOR estCostCategorySystem.
    
    IF AVAILABLE estMisc THEN DO:
        IF iplIsCategory THEN DO:
            FIND FIRST bf-estCostCategorySystem NO-LOCK
                 WHERE bf-estCostCategorySystem.estCostCategoryID EQ ipcCalcID
                 NO-ERROR.
            IF AVAILABLE bf-estCostCategorySystem THEN
                RETURN bf-estCostCategorySystem.estCostCategoryDesc.                
        END.
        ELSE DO:
            IF estMisc.estCostCalcBy EQ "Group" THEN DO:
                FIND FIRST bf-estCostGroupSystem NO-LOCK
                     WHERE bf-estCostGroupSystem.estCostGroupID EQ ipcCalcID
                     NO-ERROR.
                IF AVAILABLE bf-estCostGroupSystem THEN
                    RETURN bf-estCostGroupSystem.estCostGroupDesc.        
            END.
            ELSE IF estMisc.estCostCalcBy EQ "Level" THEN DO:
                FIND FIRST bf-estCostGroupLevelSystem NO-LOCK
                     WHERE bf-estCostGroupLevelSystem.estCostGroupLevelID EQ INTEGER(ipcCalcID)
                     NO-ERROR.
                IF AVAILABLE bf-estCostGroupLevelSystem THEN
                    RETURN bf-estCostGroupLevelSystem.estCostGroupLevelDesc.        
            END.
            ELSE IF estMisc.estCostCalcBy EQ "Category" THEN DO:
                FIND FIRST bf-estCostCategorySystem NO-LOCK
                     WHERE bf-estCostCategorySystem.estCostCategoryID EQ ipcCalcID
                     NO-ERROR.
                IF AVAILABLE bf-estCostCategorySystem THEN
                    RETURN bf-estCostCategorySystem.estCostCategoryDesc.        
            END.
        END.
    END.
    
    RETURN "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

