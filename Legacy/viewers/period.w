&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/period.w

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
{sys/inc/var.i NEW SHARED}

DEF BUFFER b-period FOR period.

DEF VAR op-company AS CHAR NO-UNDO.
DEF VAR ll-secure AS LOG INIT NO NO-UNDO.

&SCOPED-DEFINE enable-period enable-period

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES period company
&Scoped-define FIRST-EXTERNAL-TABLE period


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR period, company.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS period.pst period.pend 
&Scoped-define ENABLED-TABLES period
&Scoped-define FIRST-ENABLED-TABLE period
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS period.yr period.pnum period.pst ~
period.pend period.pstat 
&Scoped-define DISPLAYED-TABLES period
&Scoped-define FIRST-DISPLAYED-TABLE period


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS period.yr period.pstat 
&Scoped-define ADM-ASSIGN-FIELDS period.pstat 

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
     SIZE 48 BY 7.14.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     period.yr AT ROW 1.71 COL 26 COLON-ALIGNED FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 FONT 4
     period.pnum AT ROW 2.91 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 15 FONT 4
     period.pst AT ROW 4.1 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     period.pend AT ROW 5.29 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     period.pstat AT ROW 6.48 COL 19 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Open", yes,
"Closed", no
          SIZE 24 BY 1
     "Status:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 6.48 COL 10
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.period,ASI.company
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
         HEIGHT             = 8.57
         WIDTH              = 48.2.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN period.pnum IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET period.pstat IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN period.yr IN FRAME F-Main
   NO-ENABLE 1 EXP-FORMAT                                               */
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

&Scoped-define SELF-NAME period.pstat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period.pstat V-table-Win
ON VALUE-CHANGED OF period.pstat IN FRAME F-Main /* Period Status */
DO: 
  IF {&self-name}:SCREEN-VALUE EQ "yes" AND NOT ll-secure THEN
    RUN sys/ref/d-passwd.w (1, OUTPUT ll-secure).

  IF NOT ll-secure THEN {&self-name}:SCREEN-VALUE = "no".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME period.yr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL period.yr V-table-Win
ON ENTRY OF period.yr IN FRAME F-Main /* Year */
DO:
  DISABLE {&ENABLED-FIELDS} WITH FRAME {&FRAME-NAME}.
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
  {src/adm/template/row-list.i "period"}
  {src/adm/template/row-list.i "company"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "period"}
  {src/adm/template/row-find.i "company"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-period V-table-Win 
PROCEDURE enable-period :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-all-disabled AS LOG INIT NO NO-UNDO.
  DEF VAR char-hdl AS cha NO-UNDO.
  DEF VAR lv-yr LIKE period.yr NO-UNDO.

  
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL period AND NOT period.pstat THEN DO:
      DISABLE {&ENABLED-FIELDS}.
      ll-all-disabled = YES.
    END.
IF AVAIL period THEN DO:
    FIND FIRST b-period NO-LOCK
        WHERE b-period.company EQ period.company
          AND b-period.pstat   EQ YES
        NO-ERROR.
    lv-yr = (IF AVAIL b-period THEN b-period.yr ELSE YEAR(TODAY)) -
            INT(NOT company.yend-per) - 1.

    IF NOT CAN-FIND(FIRST b-period
                    WHERE b-period.company EQ period.company
                      AND b-period.pstat   EQ NO
                      AND b-period.pst     GT period.pst) AND
       NOT period.pstat AND period.yr GT lv-yr            THEN DO:
      ENABLE period.pstat.
      ll-all-disabled = NO.
    END.
END.

    IF ll-all-disabled THEN RETURN "ADM-ERROR":U.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-cancel-record V-table-Win 
PROCEDURE local-cancel-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&frame-name}:
    DISABLE {&DISPLAYED-FIELDS}.
  END.

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
  {methods/viewers/create/period.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR hld-pstat LIKE period.pstat NO-UNDO.
  DEF VAR hld-new-record AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN hld-pstat = period.pstat
         hld-new-record = adm-new-record.   

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&frame-name}:
    DISABLE {&DISPLAYED-FIELDS}.
  END.

  IF NOT hld-pstat AND period.pstat THEN DO:
    SESSION:SET-WAIT-STATE ("general").

    cocode = period.company.
    
    IF NOT hld-new-record THEN
      RUN gl/reopenpr.p (RECID(period)).

    DO TRANSACTION:
      FIND CURRENT company.    
      IF period.pnum EQ company.num-per THEN company.yend-per = YES.
      FIND CURRENT company NO-LOCK.
    END.

    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl). 

    RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(period)).

    RUN get-link-handle IN adm-broker-hdl(WIDGET-HANDLE(char-hdl),"record-source",OUTPUT char-hdl).

    RUN dispatch IN WIDGET-HANDLE(char-hdl) ("row-changed").

    SESSION:SET-WAIT-STATE ("").
  END.

  ASSIGN
   adm-new-record    = NO
   adm-adding-record = NO.

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
  {src/adm/template/snd-list.i "period"}
  {src/adm/template/snd-list.i "company"}

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

