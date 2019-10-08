&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS B-table-Win 
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

/* gdm - 09150810 */
&SCOPED-DEFINE yellowColumnsName  smanmtrx
&SCOPED-DEFINE winReSize
&SCOPED-DEFINE sizeOption HEIGHT
&SCOPED-DEFINE browseOnly
{methods/defines/winReSize.i}

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE procatDscr AS CHARACTER NO-UNDO.

DEFINE VARIABLE basisDscr AS CHARACTER NO-UNDO .
DEF BUFFER b-smanmtrx FOR smanmtrx.

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

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES sman
&Scoped-define FIRST-EXTERNAL-TABLE sman


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR sman.
/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES smanmtrx

/* Define KEY-PHRASE in case it is used by any query. */
&Scoped-define KEY-PHRASE TRUE

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table smanmtrx.procat ~
procatDscr(smanmtrx.company,smanmtrx.procat) @ procatDscr ~
smanmtrx.commbasis basisDscr(smanmtrx.commbasis) @ basisDscr ~
smanmtrx.netpct smanmtrx.comm 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table smanmtrx.procat ~
smanmtrx.commbasis smanmtrx.netpct smanmtrx.comm 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table smanmtrx
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table smanmtrx
&Scoped-define QUERY-STRING-br_table FOR EACH smanmtrx OF sman WHERE ~{&KEY-PHRASE} ~
      AND smanmtrx.custype EQ custTypes NO-LOCK ~
    ~{&SORTBY-PHRASE}
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH smanmtrx OF sman WHERE ~{&KEY-PHRASE} ~
      AND smanmtrx.custype EQ custTypes NO-LOCK ~
    ~{&SORTBY-PHRASE}.
&Scoped-define TABLES-IN-QUERY-br_table smanmtrx
&Scoped-define FIRST-TABLE-IN-QUERY-br_table smanmtrx


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table custTypes 
&Scoped-Define DISPLAYED-OBJECTS fi_sortby custTypes 

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
company||y|asi.cust-part.company
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company"':U).

/* Tell the ADM to use the OPEN-QUERY-CASES. */
&Scoped-define OPEN-QUERY-CASES RUN dispatch ('open-query-cases':U).
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD basisDscr B-table-Win 
FUNCTION basisDscr RETURNS CHARACTER
  (ipBasis AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD procatDscr B-table-Win 
FUNCTION procatDscr RETURNS CHARACTER
  (ipCompany AS CHARACTER,ipProCat AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE fi_sortby AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1
     BGCOLOR 14 FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .91.

DEFINE VARIABLE custTypes AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Empty","Empty" 
     SIZE 45 BY 17.14
     FONT 2 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY br_table FOR 
      smanmtrx SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table B-table-Win _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      smanmtrx.procat COLUMN-LABEL "Category" FORMAT "x(5)":U LABEL-BGCOLOR 14
      procatDscr(smanmtrx.company,smanmtrx.procat) @ procatDscr COLUMN-LABEL "Description" FORMAT "X(20)":U
            WIDTH 28 LABEL-BGCOLOR 8
      smanmtrx.commbasis FORMAT "X":U LABEL-BGCOLOR 14
      basisDscr(smanmtrx.commbasis) @ basisDscr COLUMN-LABEL "Description" FORMAT "X(15)":U
            WIDTH 16 LABEL-BGCOLOR 8
      smanmtrx.netpct COLUMN-LABEL "Margin%" FORMAT "->>9.99":U
            WIDTH 9.8 LABEL-BGCOLOR 14
      smanmtrx.comm COLUMN-LABEL "Comm%" FORMAT ">>9.99":U LABEL-BGCOLOR 14
  ENABLE
      smanmtrx.procat
      smanmtrx.commbasis
      smanmtrx.netpct
      smanmtrx.comm
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-ROW-MARKERS SEPARATORS SIZE 89 BY 18.1
         BGCOLOR 8 FONT 2 ROW-HEIGHT-CHARS .57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_sortby AT ROW 1 COL 18 COLON-ALIGNED NO-LABEL
     br_table AT ROW 1 COL 46
     custTypes AT ROW 1.95 COL 1 NO-LABEL
     "Types:" VIEW-AS TEXT
          SIZE 8 BY .67 AT ROW 1.14 COL 2
          FONT 2
     RECT-8 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 8 .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartBrowser
   External Tables: asi.sman
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
         HEIGHT             = 18.1
         WIDTH              = 135.8.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table fi_sortby F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:ALLOW-COLUMN-SEARCHING IN FRAME F-Main = TRUE.

/* SETTINGS FOR FILL-IN fi_sortby IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       fi_sortby:HIDDEN IN FRAME F-Main           = TRUE
       fi_sortby:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-8 IN FRAME F-Main
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "asi.smanmtrx OF asi.sman"
     _Options          = "NO-LOCK KEY-PHRASE SORTBY-PHRASE"
     _TblOptList       = ", FIRST OUTER"
     _Where[1]         = "smanmtrx.custype EQ custTypes"
     _FldNameList[1]   > asi.smanmtrx.procat
"smanmtrx.procat" "Category" ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"procatDscr(smanmtrx.company,smanmtrx.procat) @ procatDscr" "Description" "X(20)" ? ? ? ? 8 ? ? no ? no no "28" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > asi.smanmtrx.commbasis
"smanmtrx.commbasis" ? ? "character" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > "_<CALC>"
"basisDscr(smanmtrx.commbasis) @ basisDscr" "Description" "X(15)" ? ? ? ? 8 ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > asi.smanmtrx.netpct
"smanmtrx.netpct" "Margin%" ? "decimal" ? ? ? 14 ? ? yes ? no no "9.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > asi.smanmtrx.comm
"smanmtrx.comm" "Comm%" ? "decimal" ? ? ? 14 ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON HELP OF br_table IN FRAME F-Main
DO:
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.

  CASE FOCUS:NAME:
    WHEN "procat" THEN DO:
      /*RUN lookups/procat.p.
      IF g_lookup-var NE '' THEN
      ASSIGN
        smanmtrx.procat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = g_lookup-var
        procatDscr:SCREEN-VALUE = procatDscr(smanmtrx.company,smanmtrx.procat:SCREEN-VALUE).
      APPLY 'ENTRY':U TO smanmtrx.procat IN BROWSE {&BROWSE-NAME}.
    END.                     */
      run windows/l-fgcat.w (g_company,smanmtrx.procat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME}, output char-val).
      if char-val <> "" then 
         assign smanmtrx.procat:SCREEN-VALUE = entry(1,char-val)
                procatDscr:SCREEN-VALUE = entry(2,char-val)
          .
    END.
  END.
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
   /*{src/adm/template/brsleave.i}*/
  {brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table B-table-Win
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
    /* gdm - 09150806 */
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


&Scoped-define SELF-NAME smanmtrx.procat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL smanmtrx.procat br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF smanmtrx.procat IN BROWSE br_table /* Category */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-procat NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    ASSIGN
      smanmtrx.procat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = CAPS(smanmtrx.procat:SCREEN-VALUE)
      procatDscr:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = procatDscr(smanmtrx.company,smanmtrx.procat:SCREEN-VALUE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME smanmtrx.commbasis
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL smanmtrx.commbasis br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF smanmtrx.commbasis IN BROWSE br_table /* Basis */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-basis NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    ASSIGN
      smanmtrx.commbasis:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = CAPS(smanmtrx.commbasis:SCREEN-VALUE)
      basisDscr:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = basisDscr(smanmtrx.commbasis:SCREEN-VALUE).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME smanmtrx.netpct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL smanmtrx.netpct br_table _BROWSE-COLUMN B-table-Win
ON ENTRY OF smanmtrx.netpct IN BROWSE br_table /* Margin% */
DO:
   IF LASTKEY NE -1 AND smanmtrx.commbasis NE 'M' THEN
   DO:
      APPLY 'ENTRY' TO smanmtrx.comm IN BROWSE {&BROWSE-NAME}.
      RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL smanmtrx.netpct br_table _BROWSE-COLUMN B-table-Win
ON LEAVE OF smanmtrx.netpct IN BROWSE br_table /* Margin% */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN valid-margin NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME custTypes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL custTypes B-table-Win
ON VALUE-CHANGED OF custTypes IN FRAME F-Main
DO:
    
  ASSIGN {&SELF-NAME}.
  RUN dispatch ('open-query':U).
  RUN valid-procat-margin .

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK B-table-Win 


/* ***************************  Main Block  *************************** */

/* gdm - 09150806 */
{custom/yellowColumns.i}


&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF

{methods/winReSize.i}

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

  {&OPEN-QUERY-{&BROWSE-NAME}}

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

  /* Create a list of all the tables that we need to get.            */
  {src/adm/template/row-list.i "sman"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "sman"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record B-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
/*
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
*/

 RUN windows/d-smtrx.w (RECID(smanmtrx)).
 RUN dispatch ('open-query').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record B-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
    smanmtrx.company = sman.company
    smanmtrx.sman = sman.sman
    smanmtrx.custype = custTypes
    smanmtrx.commbasis = sman.commbasis
    smanmtrx.netpct = sman.netpct
    smanmtrx.comm = sman.scomm.
   

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
  {custom/askdel.i}

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  APPLY 'ENTRY':U TO smanmtrx.procat IN BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize B-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  custTypes:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = ?.
  FOR EACH custype WHERE custype.company = g_company NO-LOCK:
    custTypes:ADD-LAST(custype.custype + ' - ' + custype.dscr,custype.custype).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  custTypes:SCREEN-VALUE = custTypes:ENTRY(1).
  APPLY 'VALUE-CHANGED':U TO custTypes.

  {methods/winReSizeLocInit.i}

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
  RUN valid-procat NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-basis NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  IF smanmtrx.commbasis EQ 'M' THEN
  DO:
     RUN valid-margin NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "company" "cust-part" "company"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "sman"}
  {src/adm/template/snd-list.i "smanmtrx"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-basis B-table-Win 
PROCEDURE valid-basis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    smanmtrx.commbasis:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = CAPS(smanmtrx.commbasis:SCREEN-VALUE).
    IF basisDscr(smanmtrx.commbasis:SCREEN-VALUE) EQ '' OR
       basisDscr(smanmtrx.commbasis:SCREEN-VALUE) BEGINS "*"
    THEN DO:
      MESSAGE 'Invalid ' + TRIM(smanmtrx.commbasis:LABEL) VIEW-AS ALERT-BOX ERROR.
      APPLY 'ENTRY':U TO smanmtrx.commbasis IN BROWSE {&BROWSE-NAME}. 
      RETURN ERROR.
    END.
    ELSE basisDscr:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = basisDscr(smanmtrx.commbasis:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-margin B-table-Win 
PROCEDURE valid-margin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH b-smanmtrx WHERE 
       b-smanmtrx.company EQ sman.company AND
       b-smanmtrx.sman EQ sman.sman AND
       b-smanmtrx.custype EQ custTypes
       NO-LOCK:

       IF ROWID(b-smanmtrx) NE ROWID(smanmtrx) THEN
       DO:
          IF DEC(smanmtrx.netpct:SCREEN-VALUE IN BROWSE {&browse-name}) EQ
             b-smanmtrx.netpct THEN
          DO:
             MESSAGE 'Invalid Duplicate Margin.' VIEW-AS ALERT-BOX ERROR.
             APPLY 'ENTRY':U TO smanmtrx.netpct IN BROWSE {&BROWSE-NAME}. 
             RETURN ERROR.
          END.
       END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat B-table-Win 
PROCEDURE valid-procat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    smanmtrx.procat:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} = CAPS(smanmtrx.procat:SCREEN-VALUE).

    IF smanmtrx.commbasis:SCREEN-VALUE IN BROWSE {&BROWSE-NAME} NE 'M' THEN
    DO:
       IF smanmtrx.procat:SCREEN-VALUE NE '' AND 
          NOT CAN-FIND(fgcat WHERE fgcat.company EQ smanmtrx.company
                                AND fgcat.procat EQ smanmtrx.procat:SCREEN-VALUE) THEN DO:
         MESSAGE 'Invalid ' + TRIM(smanmtrx.procat:LABEL) VIEW-AS ALERT-BOX ERROR.
         APPLY 'ENTRY':U TO smanmtrx.procat IN BROWSE {&BROWSE-NAME}. 
         RETURN ERROR.
       END.
    END.
    ELSE
    DO:
       IF smanmtrx.procat:SCREEN-VALUE NE '' THEN
       DO:
         MESSAGE TRIM(smanmtrx.procat:LABEL) + " for Basis Margin must be Blank." 
            VIEW-AS ALERT-BOX ERROR.
         APPLY 'ENTRY':U TO smanmtrx.procat IN BROWSE {&BROWSE-NAME}. 
         RETURN ERROR.
       END.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION basisDscr B-table-Win 
FUNCTION basisDscr RETURNS CHARACTER
  (ipBasis AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE basisDscr AS CHARACTER NO-UNDO INIT 'Gross Profit,Sell Price,Margin'.
  DEFINE VARIABLE basisValue AS CHARACTER NO-UNDO INIT 'G,S,M'.

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  i = LOOKUP(ipBasis,basisValue).

  RETURN IF i GT 0 THEN ENTRY(i,basisDscr) ELSE '* Invalid *'.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION procatDscr B-table-Win 
FUNCTION procatDscr RETURNS CHARACTER
  (ipCompany AS CHARACTER,ipProCat AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND fgcat NO-LOCK WHERE fgcat.company EQ ipCompany
                       AND fgcat.procat EQ ipProCat NO-ERROR.
  RETURN IF AVAILABLE fgcat THEN fgcat.dscr 
         ELSE IF ipprocat = '' THEN "All Undefined Cat."
         ELSE ''.
    
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE export-xl B-table-Win   /*Task# 11071309*/
PROCEDURE export-xl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE lcSmanFrom AS CHAR NO-UNDO.
DEFINE VARIABLE lcSmanTo   AS CHAR NO-UNDO.

IF sman.sman NE "" THEN
    ASSIGN
        lcSmanFrom = sman.sman
        lcSmanTo = sman.sman .

RUN fg/m-smnexp.w (lcSmanFrom,
                       lcSmanTo).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-procat-margin B-table-Win 
PROCEDURE valid-procat-margin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-smanmtrx  FOR smanmtrx .
    DEFINE  VARIABLE ll-ans AS LOGICAL NO-UNDO.
    FOR EACH bf-smanmtrx OF sman EXCLUSIVE WHERE 
        bf-smanmtrx.custype EQ custTypes AND 
        bf-smanmtrx.commbasis EQ "M" AND 
        bf-smanmtrx.procat NE "" 
        BREAK BY bf-smanmtrx.procat:
        IF FIRST(bf-smanmtrx.procat) THEN do:
            MESSAGE 
                "You cannot pay commission on margin based on categories." SKIP 
                "You must change type or remove this line." SKIP
                "Enter 'YES' to remove the line, or 'NO' to change the type." 
                VIEW-AS ALERT-BOX QUESTION BUTTON yes-no UPDATE ll-ans .
        END.
        IF ll-ans  THEN
            DELETE bf-smanmtrx .
        ELSE ASSIGN 
            bf-smanmtrx.commbasis = "S" .
    END.
    IF ll-ans THEN DO:
        RUN dispatch ('open-query':U).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
