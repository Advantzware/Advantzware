&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/zipcode.w

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
{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}

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
&Scoped-define EXTERNAL-TABLES zipcode
&Scoped-define FIRST-EXTERNAL-TABLE zipcode


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR zipcode.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS zipcode.pref_type zipcode.pref# zipcode.city ~
zipcode.state zipcode.country zipcode.county zipcode.area_code ~
zipcode.time_zone zipcode.fips_code zipcode.carrier zipcode.del-zone ~
zipcode.dst 
&Scoped-define ENABLED-TABLES zipcode
&Scoped-define FIRST-ENABLED-TABLE zipcode
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS zipcode.zipcode zipcode.pref_type ~
zipcode.pref# zipcode.city zipcode.state zipcode.country zipcode.county ~
zipcode.area_code zipcode.time_zone zipcode.fips_code zipcode.carrier ~
zipcode.del-zone zipcode.dst 
&Scoped-define DISPLAYED-TABLES zipcode
&Scoped-define FIRST-DISPLAYED-TABLE zipcode
&Scoped-Define DISPLAYED-OBJECTS statecod_description F1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS zipcode.zipcode 
&Scoped-define DISPLAY-FIELD zipcode.state 
&Scoped-define F1 F1 

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
DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE statecod_description AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 7 FGCOLOR 15 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 83 BY 13.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     zipcode.zipcode AT ROW 1.24 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     zipcode.pref_type AT ROW 1.24 COL 56 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
          BGCOLOR 15 FONT 4
     zipcode.pref# AT ROW 1.24 COL 77 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 FONT 4
     zipcode.city AT ROW 2.67 COL 21 COLON-ALIGNED FORMAT "X(25)"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
          BGCOLOR 15 FONT 4
     zipcode.state AT ROW 3.86 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 15 FONT 4
     statecod_description AT ROW 3.86 COL 30 COLON-ALIGNED HELP
          "Enter Code Description" NO-LABEL
     zipcode.country AT ROW 5.05 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     zipcode.county AT ROW 6.24 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     zipcode.area_code AT ROW 7.43 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.6 BY 1
          BGCOLOR 15 FONT 4
     zipcode.time_zone AT ROW 8.62 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     zipcode.fips_code AT ROW 9.81 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
          BGCOLOR 15 FONT 4
     zipcode.carrier AT ROW 10.81 COL 21 COLON-ALIGNED WIDGET-ID 4
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          FONT 4
     zipcode.del-zone AT ROW 12 COL 21 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          FONT 4
     zipcode.dst AT ROW 13.05 COL 23
          VIEW-AS TOGGLE-BOX
          SIZE 30.4 BY 1
     F1 AT ROW 3.86 COL 29 NO-LABEL
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: NOSWEAT.zipcode
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
         HEIGHT             = 13.38
         WIDTH              = 83.
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

/* SETTINGS FOR FILL-IN zipcode.city IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN zipcode.state IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN statecod_description IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN zipcode.zipcode IN FRAME F-Main
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

&Scoped-define SELF-NAME zipcode.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zipcode.carrier V-table-Win
ON HELP OF zipcode.carrier IN FRAME F-Main /* Carrier */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   run windows/l-carrie.w  (g_company, g_loc, focus:screen-value, output char-val). 
   if char-val <> "" then 
      focus:screen-value in frame {&frame-name} = entry(1,char-val).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zipcode.carrier V-table-Win
ON LEAVE OF zipcode.carrier IN FRAME F-Main /* Carrier */
DO:
   IF LASTKEY NE -1 THEN DO:
      RUN valid-carrier NO-ERROR.
      IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME zipcode.del-zone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zipcode.del-zone V-table-Win
ON HELP OF zipcode.del-zone IN FRAME F-Main /* Delivery Zone */
DO:
   DEF VAR char-val AS CHAR NO-UNDO.

   run windows/l-delzon.w  (g_company, g_loc, zipcode.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME}, focus:screen-value, output char-val).
   if char-val <> "" then 
     focus:screen-value in frame {&frame-name} = entry(1,char-val).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zipcode.del-zone V-table-Win
ON LEAVE OF zipcode.del-zone IN FRAME F-Main /* Delivery Zone */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dest-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME zipcode.state
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL zipcode.state V-table-Win
ON LEAVE OF zipcode.state IN FRAME F-Main /* Province/State */
DO:
  {methods/dispflds.i}
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
  {src/adm/template/row-list.i "zipcode"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "zipcode"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-carrier NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-dest-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

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
  {src/adm/template/snd-list.i "zipcode"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-carrier V-table-Win 
PROCEDURE valid-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
   DO WITH FRAME {&FRAME-NAME}:
      zipcode.carrier:SCREEN-VALUE = CAPS(zipcode.carrier:SCREEN-VALUE).

      IF zipcode.carrier:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST carrier WHERE
         carrier.company EQ g_company AND
         carrier.loc     EQ g_loc AND
         carrier.carrier EQ zipcode.carrier:SCREEN-VALUE) THEN
         DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO zipcode.carrier.
            RETURN ERROR.
         END.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dest-code V-table-Win 
PROCEDURE valid-dest-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
   DO WITH FRAME {&FRAME-NAME}:
      zipcode.del-zone:SCREEN-VALUE = CAPS(zipcode.del-zone:SCREEN-VALUE).

      IF zipcode.del-zone:SCREEN-VALUE NE "" AND
         NOT CAN-FIND(FIRST carr-mtx WHERE
         carr-mtx.company  EQ g_company AND
         carr-mtx.carrier  EQ zipcode.carrier:SCREEN-VALUE AND
         carr-mtx.loc      EQ g_loc AND
         carr-mtx.del-zone EQ zipcode.del-zone:SCREEN-VALUE) THEN
         DO:
            MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO zipcode.del-zone.
            RETURN ERROR.
         END.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

