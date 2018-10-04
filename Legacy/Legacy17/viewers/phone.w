&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          nosweat          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/phone.w

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

DEFINE VARIABLE     ip-rec_key  AS CHAR     NO-UNDO.
DEFINE SHARED VAR   vrPhone     AS RECID    NO-UNDO.
DEFINE VARIABLE char-hld AS CHARACTER   NO-UNDO.
/* DEFINE VARIABLE     vrPhone     AS RECID    NO-UNDO. */

DEF BUFFER b-shipto FOR shipto.
DEF BUFFER b-phone FOR phone.
&SCOPED-DEFINE create-more methods/viewers/create/phone

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
&Scoped-define EXTERNAL-TABLES phone
&Scoped-define FIRST-EXTERNAL-TABLE phone


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR phone.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS phone.phone_ctry_code phone.phone_city_code ~
phone.phone phone.phone_ext phone.fax_ctry_code phone.fax_city_code ~
phone.fax phone.titlcode phone.attention phone.e_mail 
&Scoped-define ENABLED-TABLES phone
&Scoped-define FIRST-ENABLED-TABLE phone
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS phone.phone_ctry_code phone.phone_city_code ~
phone.phone phone.phone_ext phone.fax_ctry_code phone.fax_city_code ~
phone.fax phone.titlcode phone.attention phone.e_mail 
&Scoped-define DISPLAYED-TABLES phone
&Scoped-define FIRST-DISPLAYED-TABLE phone
&Scoped-Define DISPLAYED-OBJECTS titlcode_description tbNotice F1 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define DISPLAY-FIELD phone.titlcode 
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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdvancedNotice V-table-Win 
FUNCTION AdvancedNotice RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE F1 AS CHARACTER FORMAT "X(256)":U INITIAL "F1" 
      VIEW-AS TEXT 
     SIZE 2.2 BY .52
     BGCOLOR 0 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE titlcode_description AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1
     BGCOLOR 7 FGCOLOR 15 FONT 4.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 11.43.

DEFINE VARIABLE tbNotice AS LOGICAL INITIAL no 
     LABEL "E-Mail Notification" 
     VIEW-AS TOGGLE-BOX
     SIZE 39.4 BY .81
     FGCOLOR 12  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     phone.phone_ctry_code AT ROW 2.67 COL 13 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
          BGCOLOR 15 FONT 4
     phone.phone_city_code AT ROW 2.67 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
          BGCOLOR 15 FONT 4
     phone.phone AT ROW 2.67 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     phone.phone_ext AT ROW 2.67 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
          BGCOLOR 15 FONT 4
     phone.fax_ctry_code AT ROW 3.86 COL 13 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
          BGCOLOR 15 FONT 4
     phone.fax_city_code AT ROW 3.86 COL 26 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6.8 BY 1
          BGCOLOR 15 FONT 4
     phone.fax AT ROW 3.86 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     phone.titlcode AT ROW 6.05 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 FONT 4
     titlcode_description AT ROW 6.05 COL 30 COLON-ALIGNED HELP
          "Enter Code Description" NO-LABEL
     phone.attention AT ROW 7.24 COL 13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
          BGCOLOR 15 FONT 4
     phone.e_mail AT ROW 9.52 COL 13 COLON-ALIGNED NO-LABEL FORMAT "X(60)"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
          BGCOLOR 15 FONT 4
     tbNotice AT ROW 10.81 COL 15.2
     F1 AT ROW 6.05 COL 29 NO-LABEL
     "E-Mail:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 9.52 COL 6
     "Intl Code" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.95 COL 13
          BGCOLOR 3 FGCOLOR 15 
     "Fax:" VIEW-AS TEXT
          SIZE 5 BY 1 AT ROW 3.86 COL 9
     "Area Code" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.95 COL 25
          BGCOLOR 4 FGCOLOR 15 
     "Number" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1.95 COL 41
          BGCOLOR 4 FGCOLOR 15 
     "Ext" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 1.95 COL 54
          BGCOLOR 1 FGCOLOR 15 
     "Phone:" VIEW-AS TEXT
          SIZE 8 BY 1 AT ROW 2.91 COL 6
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: NOSWEAT.phone
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
         HEIGHT             = 11.76
         WIDTH              = 63.4.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer2.i}

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

/* SETTINGS FOR FILL-IN phone.e_mail IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN F1 IN FRAME F-Main
   NO-ENABLE ALIGN-L 6                                                  */
ASSIGN 
       F1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR TOGGLE-BOX tbNotice IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN phone.titlcode IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN titlcode_description IN FRAME F-Main
   NO-ENABLE                                                            */
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

&Scoped-define SELF-NAME phone.attention
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL phone.attention V-table-Win
ON LEAVE OF phone.attention IN FRAME F-Main /* Attention */
DO:
 IF LASTKEY = -1 THEN RETURN.
 {&methods/lValidateError.i YES}
  IF phone.attention:SCREEN-VALUE = "" THEN DO:  /* task 11181301 */
    MESSAGE "Phone Attention field can't be blank.." VIEW-AS ALERT-BOX ERROR.
    return no-apply.  
  END.
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME phone.e_mail
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL phone.e_mail V-table-Win
ON VALUE-CHANGED OF phone.e_mail IN FRAME F-Main /* e-mail Address */
DO:
  RUN SetNotifyMode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbNotice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbNotice V-table-Win
ON VALUE-CHANGED OF tbNotice IN FRAME F-Main /* E-Mail Notification */
DO:
  RUN EmailNotify (NO).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME phone.titlcode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL phone.titlcode V-table-Win
ON HELP OF phone.titlcode IN FRAME F-Main /* Title Code */
DO:
   DEF VAR v-title AS cha NO-UNDO.

   RUN windows/l-ttlcod.w (phone.titlcode:SCREEN-VALUE, OUTPUT v-title).
   IF v-title <> "" THEN
       ASSIGN
       phone.titlcode:SCREEN-VALUE = entry(1,v-title)
       titlcode_description:SCREEN-VALUE = entry(2,v-title) .
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
  {src/adm/template/row-list.i "phone"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "phone"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmailNotify V-table-Win 
PROCEDURE EmailNotify :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAM ilQuietMode  AS LOGICAL NO-UNDO.
   {&methods/lValidateError.i YES}
  IF tbNotice:CHECKED IN FRAME {&FRAME-NAME} THEN DO:

    IF NOT CAN-FIND (FIRST reftable NO-LOCK
                     WHERE reftable.rec_key = phone.table_rec_key
                       AND reftable.CODE    = STRING (phone.rec_key)) 
    THEN DO:

      CREATE reftable.
      ASSIGN reftable.rec_key   = STRING (phone.table_rec_key)
             reftable.CODE      = STRING (phone.rec_key).
    END.
  END.

  ELSE DO:

    IF AVAIL phone AND phone.table_rec_key NE "" THEN
    DO:

    FIND FIRST reftable EXCLUSIVE-LOCK
         WHERE reftable.rec_key = phone.table_rec_key
           AND reftable.CODE    = STRING (phone.rec_key) NO-ERROR.

    IF AVAIL reftable THEN DO:

      IF CAN-FIND (FIRST reftable NO-LOCK 
                   WHERE reftable.rec_key = STRING (phone.rec_key))
      THEN DO:
        IF NOT ilQuietMode THEN DO:

          MESSAGE 'This contact is currently set to receive Advanced Ship Notice(s).'  SKIP
                  'Do you wish to stop sending such notices to this contact?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE vlProceed AS LOGICAL.

          IF NOT vlProceed THEN DO:
            tbNotice:SCREEN-VALUE = 'YES'. 
            MESSAGE 'Aborted.'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
          END.
          ELSE DO:
            DELETE reftable.
            FOR EACH reftable EXCLUSIVE-LOCK
               WHERE reftable.rec_key = STRING (phone.rec_key):
              DELETE reftable.
            END.
            RUN SetEmailNotify.
          END.
        END.
        ELSE DO: 
          DELETE reftable.
          FOR EACH reftable EXCLUSIVE-LOCK
             WHERE reftable.rec_key = STRING (phone.rec_key):
            DELETE reftable.
          END.
          RUN SetEmailNotify.
        END.
      END.
      ELSE DO:
        tbNotice:SCREEN-VALUE = 'NO'.
        DELETE reftable.
      END.
    END.
    END.
  END.
  {&methods/lValidateError.i NO}
  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"CONTAINER",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN AdvancedNotice IN WIDGET-HANDLE(char-hdl).

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
  {methods/viewers/create/phone.i}


  RUN SetEmailNotify.
  vrPhone = ?.
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"CONTAINER",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN AdvancedNotice IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* After Standard Behavior. */
  vrPhone = RECID(phone).
  RUN SetEmailNotify.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"CONTAINER",OUTPUT char-hdl).

  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN AdvancedNotice IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-choice AS LOG NO-UNDO.
  DEF VAR ll-new-record AS LOG NO-UNDO.
  {&methods/lValidateError.i YES}
  ll-new-record = adm-new-record.
  /* task 11181301 */
    IF phone.attention:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
         MESSAGE "Phone Attention field can't be blank.." VIEW-AS ALERT-BOX ERROR.
         APPLY "entry" TO phone.attention .
         RETURN.
    END.
  {&methods/lValidateError.i NO}
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* After Standard Behavior. */
  vrPhone = RECID(phone).
  RUN SetEmailNotify.

  IF ll-new-record AND CAN-FIND(FIRST shipto WHERE
     shipto.rec_key EQ ip-rec_key) THEN
     DO:
        MESSAGE "Copy to All Shiptos for this Customer?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-choice.

        IF ll-choice THEN
        DO:
           FIND FIRST shipto WHERE
                shipto.rec_key EQ ip-rec_key
                NO-LOCK NO-ERROR.

           IF AVAIL shipto THEN
           DO:
              FOR EACH b-shipto WHERE
                  b-shipto.company EQ shipto.company AND
                  b-shipto.cust    EQ shipto.cust AND
                  b-shipto.rec_key NE ip-rec_key
                  NO-LOCK:

                  CREATE b-phone.
                  BUFFER-COPY phone EXCEPT rec_key table_rec_key TO b-phone
                      ASSIGN b-phone.TABLE_rec_key = b-shipto.rec_key.
                  RELEASE b-phone.
              END.
           END.
        END.
     END.
     {methods/run_link.i "CONTAINER-SOURCE" "setAddStatus" "(INPUT NO)"}
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
  {src/adm/template/snd-list.i "phone"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEmailNotify V-table-Win 
PROCEDURE SetEmailNotify :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    RUN SetNotifyMode.

    IF CAN-FIND (FIRST reftable NO-LOCK
                 WHERE reftable.rec_key = phone.table_rec_key
                   AND reftable.CODE    = STRING (phone.rec_key)) 
      THEN tbNotice:CHECKED = YES.
      ELSE tbNotice:CHECKED = NO.
  END.

  vrPhone = RECID (phone).

/*   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"CONTAINER",OUTPUT char-hdl). */
/*                                                                                       */
/*   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN                                       */
/*       RUN AdvancedNotice IN WIDGET-HANDLE(char-hdl).                                  */
/*                                                                                       */
/*   MESSAGE 'RECID: ' vrPhone     SKIP     */
/*           'Email: ' phone.e_mail         */
/*       VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetNotifyMode V-table-Win 
PROCEDURE SetNotifyMode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:

    IF phone.e_mail:SENSITIVE THEN tbNotice:SENSITIVE = TRUE.
                              ELSE tbNotice:SENSITIVE = FALSE.
  END.

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

  RUN EmailNotify (YES).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdvancedNotice V-table-Win 
FUNCTION AdvancedNotice RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  IF adm-new-record THEN
   vrPhone = ?.
  ELSE
    IF AVAIL(phone) THEN
      vrPhone = recid(phone).
  IF CAN-FIND (FIRST reftable NO-LOCK
               WHERE reftable.rec_key = phone.table_rec_key
                 AND reftable.CODE    = STRING (phone.rec_key)) 
                 AND tbNotice:CHECKED IN FRAME {&FRAME-NAME}
      THEN RETURN TRUE.
      ELSE RETURN FALSE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




