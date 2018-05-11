&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/carrier.w

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

{custom/gcompany.i}
{methods/defines/loc.i &NEW=NEW}

&SCOPED-DEFINE enable-carrier enable-carrier

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
&Scoped-define EXTERNAL-TABLES carrier
&Scoped-define FIRST-EXTERNAL-TABLE carrier


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR carrier.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS carrier.dscr carrier.loc 
&Scoped-define ENABLED-TABLES carrier
&Scoped-define FIRST-ENABLED-TABLE carrier
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS carrier.carrier carrier.dscr carrier.loc 
&Scoped-define DISPLAYED-TABLES carrier
&Scoped-define FIRST-DISPLAYED-TABLE carrier
&Scoped-Define DISPLAYED-OBJECTS loc_dscr rd_chg-method F1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS carrier.carrier 
&Scoped-define ADM-ASSIGN-FIELDS rd_chg-method 
&Scoped-define DISPLAY-FIELD carrier.loc 
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

DEFINE VARIABLE loc_dscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE VARIABLE rd_chg-method AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "MSF", "M",
"Pallet", "P",
"Weight", "W"
     SIZE 45 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88 BY 3.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     carrier.carrier AT ROW 1.24 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FONT 4
     carrier.dscr AT ROW 1.24 COL 48 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          BGCOLOR 15 FONT 4
     carrier.loc AT ROW 2.43 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FONT 4
     loc_dscr AT ROW 2.43 COL 36 COLON-ALIGNED NO-LABEL
     rd_chg-method AT ROW 3.62 COL 25 NO-LABEL
     F1 AT ROW 2.43 COL 35 NO-LABEL
     "Charge Method:" VIEW-AS TEXT
          SIZE 19 BY .95 AT ROW 3.62 COL 5
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.carrier
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
         HEIGHT             = 3.81
         WIDTH              = 88.6.
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

/* SETTINGS FOR FILL-IN carrier.carrier IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN carrier.loc IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN loc_dscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET rd_chg-method IN FRAME F-Main
   NO-ENABLE 2                                                          */
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

&Scoped-define SELF-NAME carrier.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL carrier.loc V-table-Win
ON ENTRY OF carrier.loc IN FRAME F-Main /* Location */
DO:
  s-company = gcompany.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL carrier.loc V-table-Win
ON LEAVE OF carrier.loc IN FRAME F-Main /* Location */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL carrier.carrier V-table-Win
ON LEAVE OF carrier.carrier IN FRAME F-Main /* Location */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-carrier NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL carrier.loc V-table-Win
ON VALUE-CHANGED OF carrier.loc IN FRAME F-Main /* Location */
DO:
  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
DEF BUFFER b-carrier FOR carrier.

SESSION:DATA-ENTRY-RETURN = YES.

FOR EACH b-carrier
    WHERE b-carrier.company   EQ carrier.company
      AND b-carrier.by-pallet NE ?
    NO-LOCK:

  RUN util/ucarrier.p (RECID(b-carrier)).
END.

{sys/inc/f3help.i}
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
  {src/adm/template/row-list.i "carrier"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "carrier"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-carrier V-table-Win 
PROCEDURE enable-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE rd_chg-method.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE load-fedex V-table-Win 
PROCEDURE load-fedex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{&methods/lValidateError.i YES}
IF carrier.carrier = "FEDX" OR INDEX(carrier.dscr, "federal express") > 0
    OR INDEX(carrier.dscr, "fedex") > 0 THEN
RUN ar/impfedx.w (INPUT carrier.loc, INPUT carrier.carrier).
ELSE DO:
    MESSAGE "This load utility is for Fedex only!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.
{&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def buffer bf-carrier for carrier.
  def buffer bf-carr-mtx for carr-mtx.
  def var ls-prev-carrier like carrier.carrier no-undo.
  def var ls-prev-loc like carrier.loc no-undo.
  DEF VAR vChgMethod AS cha NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  assign ls-prev-carrier = carrier.carrier
         ls-prev-loc = carrier.loc
         vChgMethod = rd_chg-method:SCREEN-VALUE IN FRAME {&FRAME-NAME}
         .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN rd_chg-method = vChgMethod
         carrier.chg-method = vChgMethod.

  IF adm-adding-record THEN DISP rd_chg-method WITH FRAME {&FRAME-NAME}.

  if adm-new-record and not adm-adding-record then do:  /* copy */
     find bf-carrier where bf-carrier.company = carrier.company and
                           bf-carrier.loc = ls-prev-loc and
                           bf-carrier.carrier = ls-prev-carrier
                           no-lock no-error. 
     for each carr-mtx of bf-carrier no-lock:
         create bf-carr-mtx.
         buffer-copy carr-mtx except carr-mtx.carrier carr-mtx.loc to bf-carr-mtx.
         assign bf-carr-mtx.loc = carrier.loc
                bf-carr-mtx.carrier = carrier.carrier.
     end.
  end.



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
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE rd_chg-method.
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
  {methods/viewers/create/carrier.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  rd_chg-method = carrier.chg-method.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

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
  RUN valid-loc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-carrier NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.



  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DO WITH FRAME {&FRAME-NAME}:
    DISABLE rd_chg-method.
  END.

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
  {src/adm/template/snd-list.i "carrier"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc V-table-Win 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    carrier.loc:SCREEN-VALUE = CAPS(carrier.loc:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST loc WHERE loc.company EQ gcompany
                                AND loc.loc     EQ carrier.loc:SCREEN-VALUE)
    THEN DO:
      MESSAGE "Invalid " + TRIM(carrier.loc:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX.
      APPLY "entry" TO carrier.loc.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc V-table-Win 
PROCEDURE valid-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-carrier FOR carrier.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
   IF adm-new-record AND adm-adding-record THEN 
    IF CAN-FIND(FIRST b-carrier WHERE b-carrier.company EQ gcompany
                                 AND b-carrier.carrier    EQ carrier.carrier:SCREEN-VALUE
                                 AND ROWID(b-carrier)  NE ROWID(carrier)) 
    THEN DO:
      MESSAGE "Sorry, " + TRIM(carrier.carrier:LABEL) + " " + "already exists."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO carrier.carrier.
      RETURN ERROR.
    END.
  

  IF adm-new-record AND NOT adm-adding-record THEN 
    IF CAN-FIND(FIRST b-carrier WHERE b-carrier.company EQ gcompany
                                 AND b-carrier.carrier    EQ carrier.carrier:SCREEN-VALUE)
    THEN DO:
      MESSAGE "Sorry, " + TRIM(carrier.carrier:LABEL) + " " + "already exists."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO carrier.carrier.
      RETURN ERROR.
    END.
  END.


  {methods/lValidateError.i NO}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
