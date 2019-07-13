&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rfq              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
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

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES rfqitem
&Scoped-define FIRST-EXTERNAL-TABLE rfqitem


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR rfqitem.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS rfqitem.qty[1] rfqitem.qty[2] rfqitem.qty[3] ~
rfqitem.qty[4] rfqitem.qty[5] rfqitem.qty[6] rfqitem.qty[7] rfqitem.qty[8] ~
rfqitem.qty[9] rfqitem.qty[10] rfqitem.qty[11] rfqitem.qty[12] ~
rfqitem.qty[13] rfqitem.qty[14] rfqitem.qty[15] rfqitem.qty[16] ~
rfqitem.qty[17] rfqitem.qty[18] rfqitem.qty[19] rfqitem.qty[20] ~
rfqitem.qty[21] rfqitem.qty[22] rfqitem.qty[23] rfqitem.qty[24] ~
rfqitem.qty[25] rfqitem.qty[26] rfqitem.qty[27] rfqitem.qty[28] ~
rfqitem.qty[29] rfqitem.qty[30] 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}qty[1] ~{&FP2}qty[1] ~{&FP3}~
 ~{&FP1}qty[2] ~{&FP2}qty[2] ~{&FP3}~
 ~{&FP1}qty[3] ~{&FP2}qty[3] ~{&FP3}~
 ~{&FP1}qty[4] ~{&FP2}qty[4] ~{&FP3}~
 ~{&FP1}qty[5] ~{&FP2}qty[5] ~{&FP3}~
 ~{&FP1}qty[6] ~{&FP2}qty[6] ~{&FP3}~
 ~{&FP1}qty[7] ~{&FP2}qty[7] ~{&FP3}~
 ~{&FP1}qty[8] ~{&FP2}qty[8] ~{&FP3}~
 ~{&FP1}qty[9] ~{&FP2}qty[9] ~{&FP3}~
 ~{&FP1}qty[10] ~{&FP2}qty[10] ~{&FP3}~
 ~{&FP1}qty[11] ~{&FP2}qty[11] ~{&FP3}~
 ~{&FP1}qty[12] ~{&FP2}qty[12] ~{&FP3}~
 ~{&FP1}qty[13] ~{&FP2}qty[13] ~{&FP3}~
 ~{&FP1}qty[14] ~{&FP2}qty[14] ~{&FP3}~
 ~{&FP1}qty[15] ~{&FP2}qty[15] ~{&FP3}~
 ~{&FP1}qty[16] ~{&FP2}qty[16] ~{&FP3}~
 ~{&FP1}qty[17] ~{&FP2}qty[17] ~{&FP3}~
 ~{&FP1}qty[18] ~{&FP2}qty[18] ~{&FP3}~
 ~{&FP1}qty[19] ~{&FP2}qty[19] ~{&FP3}~
 ~{&FP1}qty[20] ~{&FP2}qty[20] ~{&FP3}~
 ~{&FP1}qty[21] ~{&FP2}qty[21] ~{&FP3}~
 ~{&FP1}qty[22] ~{&FP2}qty[22] ~{&FP3}~
 ~{&FP1}qty[23] ~{&FP2}qty[23] ~{&FP3}~
 ~{&FP1}qty[24] ~{&FP2}qty[24] ~{&FP3}~
 ~{&FP1}qty[25] ~{&FP2}qty[25] ~{&FP3}~
 ~{&FP1}qty[26] ~{&FP2}qty[26] ~{&FP3}~
 ~{&FP1}qty[27] ~{&FP2}qty[27] ~{&FP3}~
 ~{&FP1}qty[28] ~{&FP2}qty[28] ~{&FP3}~
 ~{&FP1}qty[29] ~{&FP2}qty[29] ~{&FP3}~
 ~{&FP1}qty[30] ~{&FP2}qty[30] ~{&FP3}
&Scoped-define ENABLED-TABLES rfqitem
&Scoped-define FIRST-ENABLED-TABLE rfqitem
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS rfqitem.qty[1] rfqitem.qty[2] ~
rfqitem.qty[3] rfqitem.qty[4] rfqitem.qty[5] rfqitem.qty[6] rfqitem.qty[7] ~
rfqitem.qty[8] rfqitem.qty[9] rfqitem.qty[10] rfqitem.qty[11] ~
rfqitem.qty[12] rfqitem.qty[13] rfqitem.qty[14] rfqitem.qty[15] ~
rfqitem.qty[16] rfqitem.qty[17] rfqitem.qty[18] rfqitem.qty[19] ~
rfqitem.qty[20] rfqitem.qty[21] rfqitem.qty[22] rfqitem.qty[23] ~
rfqitem.qty[24] rfqitem.qty[25] rfqitem.qty[26] rfqitem.qty[27] ~
rfqitem.qty[28] rfqitem.qty[29] rfqitem.qty[30] 

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 95 BY 6.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     rfqitem.qty[1] AT ROW 1.95 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[2] AT ROW 1.95 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[3] AT ROW 1.95 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[4] AT ROW 1.95 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[5] AT ROW 1.95 COL 78 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[6] AT ROW 2.91 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[7] AT ROW 2.91 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[8] AT ROW 2.91 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[9] AT ROW 2.91 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[10] AT ROW 2.91 COL 78 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[11] AT ROW 3.86 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[12] AT ROW 3.86 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[13] AT ROW 3.86 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[14] AT ROW 3.86 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[15] AT ROW 3.86 COL 78 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[16] AT ROW 4.81 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[17] AT ROW 4.81 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[18] AT ROW 4.81 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[19] AT ROW 4.81 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[20] AT ROW 4.81 COL 78 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[21] AT ROW 5.76 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[22] AT ROW 5.76 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[23] AT ROW 5.76 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[24] AT ROW 5.76 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[25] AT ROW 5.76 COL 78 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[26] AT ROW 6.71 COL 10 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[27] AT ROW 6.71 COL 27 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[28] AT ROW 6.71 COL 44 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     rfqitem.qty[29] AT ROW 6.71 COL 61 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     rfqitem.qty[30] AT ROW 6.71 COL 78 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     "Quantity Detail" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.24 COL 6
     RECT-1 AT ROW 1.48 COL 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: rfq.rfqitem
   Allow: Basic,DB-Fields
   Frames: 1
   Add Fields to: EXTERNAL-TABLES
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT."
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW V-table-Win ASSIGN
         HEIGHT             = 7.05
         WIDTH              = 99.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available V-table-Win _ADM-ROW-AVAILABLE
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
  {src/adm/template/row-list.i "rfqitem"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "rfqitem"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI V-table-Win _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records V-table-Win _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "rfqitem"}

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


