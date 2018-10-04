&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/costtype.w

  Description: from VIEWER.W - Template for SmartViewer Objects

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

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR ll-acct-warned AS LOG EXTENT 4 NO-UNDO.

&SCOPED-DEFINE enable-costtype enable-costtype

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES costtype
&Scoped-define FIRST-EXTERNAL-TABLE costtype


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR costtype.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS costtype.descr costtype.inv-asset ~
costtype.pur-var costtype.cons-exp costtype.ap-accrued 
&Scoped-define ENABLED-TABLES costtype
&Scoped-define FIRST-ENABLED-TABLE costtype
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS costtype.cost-type costtype.descr ~
costtype.inv-asset costtype.pur-var costtype.cons-exp costtype.ap-accrued 
&Scoped-define DISPLAYED-TABLES costtype
&Scoped-define FIRST-DISPLAYED-TABLE costtype


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS costtype.cost-type 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
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


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 80 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     costtype.cost-type AT ROW 1.24 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     costtype.descr AT ROW 1.24 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     costtype.inv-asset AT ROW 2.43 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     costtype.pur-var AT ROW 3.62 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     costtype.cons-exp AT ROW 4.81 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     costtype.ap-accrued AT ROW 6 COL 40 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.costtype
   Allow: Basic,DB-Fields
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
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 7.76
         WIDTH              = 80.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN costtype.cost-type IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME





/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME costtype.ap-accrued
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL costtype.ap-accrued V-table-Win
ON LEAVE OF costtype.ap-accrued IN FRAME F-Main /* A/P Accrued Acct# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-acct (4) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL costtype.ap-accrued V-table-Win
ON VALUE-CHANGED OF costtype.ap-accrued IN FRAME F-Main /* A/P Accrued Acct# */
DO:
  ll-acct-warned[4] = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME costtype.cons-exp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL costtype.cons-exp V-table-Win
ON LEAVE OF costtype.cons-exp IN FRAME F-Main /* Consumption Expense Acct# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-acct (3) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL costtype.cons-exp V-table-Win
ON VALUE-CHANGED OF costtype.cons-exp IN FRAME F-Main /* Consumption Expense Acct# */
DO:
  ll-acct-warned[3] = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME costtype.inv-asset
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL costtype.inv-asset V-table-Win
ON LEAVE OF costtype.inv-asset IN FRAME F-Main /* Inventory Asset Acct# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-acct (1) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL costtype.inv-asset V-table-Win
ON VALUE-CHANGED OF costtype.inv-asset IN FRAME F-Main /* Inventory Asset Acct# */
DO:
  ll-acct-warned[1] = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME costtype.pur-var
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL costtype.pur-var V-table-Win
ON LEAVE OF costtype.pur-var IN FRAME F-Main /* Purchase Price Variance Acct# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-acct (2) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL costtype.pur-var V-table-Win
ON VALUE-CHANGED OF costtype.pur-var IN FRAME F-Main /* Purchase Price Variance Acct# */
DO:
  ll-acct-warned[2] = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN dispatch ('add-record').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win  _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "costtype"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "costtype"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-costtype V-table-Win 
PROCEDURE enable-costtype :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
   ll-acct-warned    = NO
   ll-acct-warned[3] = NOT adm-adding-record.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/costtype.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  DO li = 1 TO 4:
    RUN valid-acct (li) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "costtype"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed V-table-Win 
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
      {src/adm/template/vstates.i}
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-acct V-table-Win 
PROCEDURE valid-acct :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-int AS INT NO-UNDO.

  DEF VAR lv-value AS CHAR NO-UNDO.
  DEF VAR lv-label AS CHAR NO-UNDO.
  DEF VAR lv-type AS CHAR INIT "A,E,E,L" NO-UNDO.
  DEF VAR lv-dscr AS CHAR INIT "Asset,Expense,Expense,Liability" NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    CASE ip-int:
      WHEN 1 THEN
         ASSIGN
          lv-value = costtype.inv-asset:SCREEN-VALUE
          lv-label = costtype.inv-asset:LABEL.
      WHEN 2 THEN
         ASSIGN
          lv-value = costtype.pur-var:SCREEN-VALUE
          lv-label = costtype.pur-var:LABEL.
      WHEN 3 THEN
         ASSIGN
          lv-value = costtype.cons-exp:SCREEN-VALUE
          lv-label = costtype.cons-exp:LABEL.
      WHEN 4 THEN
         ASSIGN
          lv-value = costtype.ap-accrued:SCREEN-VALUE
          lv-label = costtype.ap-accrued:LABEL.
      OTHERWISE RETURN ERROR.
    END CASE.

    IF lv-value NE "" THEN DO:
      ll = NO.

      FIND FIRST account
          WHERE account.company EQ cocode
            AND account.actnum  EQ lv-value
          NO-LOCK NO-ERROR.

      IF NOT AVAIL account                          OR
         (account.type NE ENTRY(ip-int,lv-type) AND
          NOT ll-acct-warned[ip-int])               THEN DO:

        lv-msg = TRIM(lv-label) + " is not a valid " +
                 TRIM(ENTRY(ip-int,lv-dscr)) + " account".

        IF AVAIL account                           AND
           ((ip-int EQ 3 AND account.type EQ "A")) THEN DO:

          li = INDEX(lv-type,account.type).

          MESSAGE lv-msg + " but an " + TRIM(ENTRY(li,lv-dscr)) + ", enter anyway?"
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
              UPDATE ll.
        END.

        ELSE MESSAGE lv-msg + ", try help..." VIEW-AS ALERT-BOX ERROR.
      END.

      ELSE ll = YES.

      IF NOT ll THEN DO:
        CASE ip-int:
          WHEN 1 THEN APPLY "entry" TO costtype.inv-asset.
          WHEN 2 THEN APPLY "entry" TO costtype.pur-var.
          WHEN 3 THEN APPLY "entry" TO costtype.cons-exp.
          WHEN 4 THEN APPLY "entry" TO costtype.ap-accrued.
        END CASE.
        RETURN ERROR.
      END.

      ELSE ll-acct-warned[ip-int] = YES.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

