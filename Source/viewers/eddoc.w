&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*********************************************************************
* Copyright (C) 2000 by Progress Software Corporation. All rights    *
* reserved. Prior versions of this work may contain portions         *
* contributed by participants of Possenet.                           *
*                                                                    *
*********************************************************************/
/*------------------------------------------------------------------------

  File:

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME hF-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES EDDoc
&Scoped-define FIRST-EXTERNAL-TABLE EDDoc


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR EDDoc.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS EDDoc.Partner EDDoc.SetID EDDoc.Direction ~
EDDoc.Set-Test-Prod EDDoc.Posted EDDoc.DocID EDDoc.ST EDDoc.ISA EDDoc.GS ~
EDDoc.DocSeq EDDoc.Version EDDoc.FGAgency EDDoc.FGDate EDDoc.Recv-Test-Prod ~
EDDoc.OpenItem EDDoc.EDI_Agency EDDoc.ST-Code EDDoc.C-FADate EDDoc.Stat ~
EDDoc.InterpDate EDDoc.C-FATime EDDoc.Seq EDDoc.Status-Flag ~
EDDoc.InterpTime EDDoc.Unique-Order-No EDDoc.EDI_Standard EDDoc.Error-count ~
EDDoc.Unique-SDQ-No EDDoc.UserRef EDDoc.AddOp EDDoc.AddDate EDDoc.AddTime ~
EDDoc.FGTime EDDoc.ChgOp EDDoc.ChgDate EDDoc.ChgTime EDDoc.FGSender ~
EDDoc.P-FADate EDDoc.FGID EDDoc.FGRecvID EDDoc.P-FATime 
&Scoped-define ENABLED-TABLES EDDoc
&Scoped-define FIRST-ENABLED-TABLE EDDoc
&Scoped-Define DISPLAYED-FIELDS EDDoc.Partner EDDoc.SetID EDDoc.Direction ~
EDDoc.Set-Test-Prod EDDoc.Posted EDDoc.DocID EDDoc.ST EDDoc.ISA EDDoc.GS ~
EDDoc.DocSeq EDDoc.Version EDDoc.FGAgency EDDoc.FGDate EDDoc.Recv-Test-Prod ~
EDDoc.OpenItem EDDoc.EDI_Agency EDDoc.ST-Code EDDoc.C-FADate EDDoc.Stat ~
EDDoc.InterpDate EDDoc.C-FATime EDDoc.Seq EDDoc.Status-Flag ~
EDDoc.InterpTime EDDoc.Unique-Order-No EDDoc.EDI_Standard EDDoc.Error-count ~
EDDoc.Unique-SDQ-No EDDoc.UserRef EDDoc.AddOp EDDoc.AddDate EDDoc.AddTime ~
EDDoc.FGTime EDDoc.ChgOp EDDoc.ChgDate EDDoc.ChgTime EDDoc.FGSender ~
EDDoc.P-FADate EDDoc.FGID EDDoc.FGRecvID EDDoc.P-FATime 
&Scoped-define DISPLAYED-TABLES EDDoc
&Scoped-define FIRST-DISPLAYED-TABLE EDDoc


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME hF-Main
     EDDoc.Partner AT ROW 1 COL 13.2 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     EDDoc.SetID AT ROW 1 COL 32.2 COLON-ALIGNED WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     EDDoc.Direction AT ROW 1 COL 53 COLON-ALIGNED WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     EDDoc.Set-Test-Prod AT ROW 1 COL 88 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     EDDoc.Posted AT ROW 1.1 COL 62 WIDGET-ID 86
          VIEW-AS TOGGLE-BOX
          SIZE 12.8 BY .81
     EDDoc.DocID AT ROW 2 COL 13.2 COLON-ALIGNED WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     EDDoc.ST AT ROW 2.91 COL 80 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     EDDoc.ISA AT ROW 3 COL 13.2 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     EDDoc.GS AT ROW 3 COL 41.8 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     EDDoc.DocSeq AT ROW 4 COL 13.2 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     EDDoc.Version AT ROW 4 COL 42 COLON-ALIGNED WIDGET-ID 84
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDDoc.FGAgency AT ROW 6.48 COL 13.6 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     EDDoc.FGDate AT ROW 6.48 COL 42 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     EDDoc.Recv-Test-Prod AT ROW 6.71 COL 84 COLON-ALIGNED WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     EDDoc.OpenItem AT ROW 7.43 COL 42 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     EDDoc.EDI_Agency AT ROW 7.48 COL 13.6 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     EDDoc.ST-Code AT ROW 7.71 COL 84 COLON-ALIGNED WIDGET-ID 72
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDDoc.C-FADate AT ROW 8.43 COL 13.6 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     EDDoc.Stat AT ROW 8.48 COL 42 COLON-ALIGNED WIDGET-ID 74
          VIEW-AS FILL-IN 
          SIZE 3.4 BY 1
     EDDoc.InterpDate AT ROW 8.91 COL 84 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     EDDoc.C-FATime AT ROW 9.43 COL 13.6 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDDoc.Seq AT ROW 9.52 COL 42 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 11.8 BY 1
     EDDoc.Status-Flag AT ROW 9.86 COL 84 COLON-ALIGNED WIDGET-ID 76
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     EDDoc.InterpTime AT ROW 10.33 COL 13.6 COLON-ALIGNED WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDDoc.Unique-Order-No AT ROW 10.48 COL 42 COLON-ALIGNED WIDGET-ID 78
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDDoc.EDI_Standard AT ROW 11.33 COL 13.6 COLON-ALIGNED WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     EDDoc.Error-count AT ROW 12.62 COL 13.6 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     EDDoc.Unique-SDQ-No AT ROW 12.67 COL 42 COLON-ALIGNED WIDGET-ID 80
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME hF-Main
     EDDoc.UserRef AT ROW 12.91 COL 84 COLON-ALIGNED WIDGET-ID 82
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     EDDoc.AddOp AT ROW 13.62 COL 13.6 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDDoc.AddDate AT ROW 13.62 COL 42 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     EDDoc.AddTime AT ROW 13.62 COL 62.2 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     EDDoc.FGTime AT ROW 13.86 COL 84 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     EDDoc.ChgOp AT ROW 14.57 COL 13.6 COLON-ALIGNED WIDGET-ID 14
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     EDDoc.ChgDate AT ROW 14.57 COL 42 COLON-ALIGNED WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     EDDoc.ChgTime AT ROW 14.57 COL 62.2 COLON-ALIGNED WIDGET-ID 16
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     EDDoc.FGSender AT ROW 14.81 COL 84 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     EDDoc.P-FADate AT ROW 15.52 COL 13.6 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     EDDoc.FGID AT ROW 15.52 COL 42 COLON-ALIGNED WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 6.4 BY 1
     EDDoc.FGRecvID AT ROW 15.76 COL 84 COLON-ALIGNED WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     EDDoc.P-FATime AT ROW 16.48 COL 13.6 COLON-ALIGNED WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.EDDoc
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
         HEIGHT             = 17.57
         WIDTH              = 116.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME hF-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME hF-Main:SCROLLABLE       = FALSE
       FRAME hF-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME hF-Main
/* Query rebuild information for FRAME hF-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME hF-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME EDDoc.Partner
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL EDDoc.Partner V-table-Win
ON LEAVE OF EDDoc.Partner IN FRAME hF-Main /* Partner */
DO:
    IF LASTKEY NE -1 THEN DO:
      RUN valid-partner NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    END.
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
  {src/adm/template/row-list.i "EDDoc"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "EDDoc"}

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
  HIDE FRAME hF-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
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
  {src/adm/template/snd-list.i "EDDoc"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-partner V-table-Win 
PROCEDURE valid-partner :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}

  DEF VAR v-avail AS LOG INIT YES NO-UNDO.

  IF NOT CAN-FIND(FIRST edmast NO-LOCK 
    WHERE edmast.partner EQ eddoc.partner:SCREEN-VALUE IN FRAME {&FRAME-NAME}) THEN
      v-avail = FALSE.
  
  IF NOT v-avail THEN RETURN ERROR.

  {methods/lValidateError.i NO}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

