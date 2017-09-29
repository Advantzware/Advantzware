&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/users.w

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
{sys/inc/VAR.i NEW SHARED}
ASSIGN cocode = g_company
       locode = g_loc.

&SCOPED-DEFINE proc-enable proc-enable
&SCOPED-DEFINE users-rowavail proc-rowavail

DEFINE NEW GLOBAL SHARED VAR cIniLoc AS CHAR NO-UNDO.
DEFINE NEW GLOBAL SHARED VAR cUsrLoc AS CHAR NO-UNDO.

DEF VAR createLabelPath AS LOG NO-UNDO.
DEF VAR cOldUserID AS CHAR NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES users usr
&Scoped-define FIRST-EXTERNAL-TABLE users


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR users, usr.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS users.securityLevel users.userAlias ~
users.user_name users.track_usage users.use_colors users.use_fonts ~
users.use_ctrl_keys users.developer users.user_program[2] ~
users.user_program[1] users.user_program[3] 
&Scoped-define ENABLED-TABLES users
&Scoped-define FIRST-ENABLED-TABLE users
&Scoped-Define ENABLED-OBJECTS cbUserType SELECT-4 SELECT-5 SELECT-6 
&Scoped-Define DISPLAYED-FIELDS users.securityLevel users.userAlias ~
users.user_id users.user_name users.track_usage users.use_colors ~
users.use_fonts users.use_ctrl_keys users.developer users.user_program[2] ~
users.user_program[1] users.user_program[3] 
&Scoped-define DISPLAYED-TABLES users
&Scoped-define FIRST-DISPLAYED-TABLE users
&Scoped-Define DISPLAYED-OBJECTS cbUserType SELECT-4 SELECT-5 SELECT-6 ~
tg_po tg_bol tg_invoice tg_ack tg_quote fi_phone-area lv-phone-num ~
fi_phone-country fi_fax-area lv-fax-num fi_fax-country fi_email 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS users.user_id 
&Scoped-define ADM-ASSIGN-FIELDS tg_po tg_bol tg_invoice tg_ack tg_quote ~
fi_phone-area lv-phone-num fi_phone-country fi_fax-area lv-fax-num ~
fi_fax-country fi_email 
&Scoped-define DISPLAY-FIELD tg_po tg_bol tg_invoice tg_ack tg_quote ~
fi_phone-area lv-phone-num fi_phone-country fi_fax-area lv-fax-num ~
fi_fax-country fi_email 

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
DEFINE VARIABLE cbUserType AS CHARACTER FORMAT "X(256)":U 
     LABEL "User Type" 
     VIEW-AS COMBO-BOX INNER-LINES 4
     LIST-ITEMS "Full User","Production Floor","Administrator","Portal User" 
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fi_email AS CHARACTER FORMAT "X(60)":U 
     LABEL "Email" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1 NO-UNDO.

DEFINE VARIABLE fi_fax-area AS CHARACTER FORMAT "(xxx)":U 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_fax-country AS CHARACTER FORMAT "X(8)":U 
     LABEL "FAX +" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_phone-area AS CHARACTER FORMAT "(xxx)":U 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE fi_phone-country AS CHARACTER FORMAT "X(8)":U 
     LABEL "Phone +" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE lv-fax-num AS CHARACTER FORMAT "xxx-xxxx":U 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE lv-phone-num AS CHARACTER FORMAT "xxx-xxxx":U 
     LABEL "-" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE SELECT-4 AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Production","Test" 
     SIZE 35 BY 2 NO-UNDO.

DEFINE VARIABLE SELECT-5 AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Production","Test" 
     SIZE 36 BY 2 NO-UNDO.

DEFINE VARIABLE SELECT-6 AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "Advantzware","Addon","Case Labels","Loadtags","Sharpshooter","Touchscreen" 
     SIZE 36 BY 4.76 NO-UNDO.

DEFINE VARIABLE tg_ack AS LOGICAL INITIAL no 
     LABEL "Acknowledgments" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .81 NO-UNDO.

DEFINE VARIABLE tg_bol AS LOGICAL INITIAL no 
     LABEL "BOLs" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE tg_invoice AS LOGICAL INITIAL no 
     LABEL "Invoices" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE tg_po AS LOGICAL INITIAL no 
     LABEL "POs" 
     VIEW-AS TOGGLE-BOX
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE tg_quote AS LOGICAL INITIAL no 
     LABEL "Quotes" 
     VIEW-AS TOGGLE-BOX
     SIZE 11 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     cbUserType AT ROW 4.1 COL 99 COLON-ALIGNED WIDGET-ID 48
     users.securityLevel AT ROW 2.91 COL 99 COLON-ALIGNED WIDGET-ID 44
          LABEL "Security Level" FORMAT ">999"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     SELECT-4 AT ROW 6.48 COL 101 NO-LABEL WIDGET-ID 50
     SELECT-5 AT ROW 8.86 COL 101 NO-LABEL WIDGET-ID 52
     SELECT-6 AT ROW 11.24 COL 101 NO-LABEL WIDGET-ID 54
     users.userAlias AT ROW 1.24 COL 99 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     users.user_id AT ROW 1.24 COL 15 COLON-ALIGNED
          LABEL "User ID"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 FONT 4
     users.user_name AT ROW 1.24 COL 40 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 43 BY 1
          BGCOLOR 15 FONT 4
     users.track_usage AT ROW 11.95 COL 16
          VIEW-AS TOGGLE-BOX
          SIZE 19.8 BY 1
     tg_po AT ROW 15.76 COL 59 WIDGET-ID 32
     users.use_colors AT ROW 12.91 COL 16
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY 1
     tg_bol AT ROW 13.86 COL 59 WIDGET-ID 28
     users.use_fonts AT ROW 13.86 COL 16
          VIEW-AS TOGGLE-BOX
          SIZE 26.2 BY 1
     tg_invoice AT ROW 14.81 COL 59 WIDGET-ID 30
     tg_ack AT ROW 12.91 COL 59 WIDGET-ID 26
     users.use_ctrl_keys AT ROW 14.81 COL 16
          VIEW-AS TOGGLE-BOX
          SIZE 38.4 BY 1
     tg_quote AT ROW 11.95 COL 59 WIDGET-ID 34
     users.developer AT ROW 15.76 COL 16
          VIEW-AS TOGGLE-BOX
          SIZE 16.8 BY 1
     fi_phone-area AT ROW 2.91 COL 29 COLON-ALIGNED WIDGET-ID 10
     lv-phone-num AT ROW 2.91 COL 39 COLON-ALIGNED WIDGET-ID 14
     fi_phone-country AT ROW 2.91 COL 19 COLON-ALIGNED WIDGET-ID 12
     fi_fax-area AT ROW 4.1 COL 29 COLON-ALIGNED WIDGET-ID 16
     lv-fax-num AT ROW 4.1 COL 39 COLON-ALIGNED WIDGET-ID 20
     fi_fax-country AT ROW 4.1 COL 19 COLON-ALIGNED WIDGET-ID 18
     users.user_program[2] AT ROW 8.38 COL 19 COLON-ALIGNED HELP
          "" WIDGET-ID 8
          LABEL "Report Path" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     users.user_program[1] AT ROW 7.19 COL 19 COLON-ALIGNED
          LABEL "Image Viewer" FORMAT "x(80)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     users.user_program[3] AT ROW 9.57 COL 2 WIDGET-ID 36
          LABEL "Document Path" FORMAT "x(100)"
          VIEW-AS FILL-IN 
          SIZE 60 BY 1
     fi_email AT ROW 5.52 COL 19 COLON-ALIGNED WIDGET-ID 38
     "Phone/Fax Appear on:" VIEW-AS TEXT
          SIZE 27 BY .62 AT ROW 11.24 COL 58 WIDGET-ID 24
     "Databases:" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 8.86 COL 87 WIDGET-ID 60
     "Modes:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 11.24 COL 92 WIDGET-ID 62
     "Options:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 11.24 COL 14 WIDGET-ID 42
     "User Can Select:" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 5.52 COL 101 WIDGET-ID 56
     "Environments:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 6.48 COL 84 WIDGET-ID 58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: NOSWEAT.users,asi.usr
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
         HEIGHT             = 16.43
         WIDTH              = 142.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi_email IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN fi_fax-area IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN fi_fax-country IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN fi_phone-area IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN fi_phone-country IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN lv-fax-num IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN lv-phone-num IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN users.securityLevel IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX tg_ack IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR TOGGLE-BOX tg_bol IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR TOGGLE-BOX tg_invoice IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR TOGGLE-BOX tg_po IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR TOGGLE-BOX tg_quote IN FRAME F-Main
   NO-ENABLE 2 4                                                        */
/* SETTINGS FOR FILL-IN users.user_id IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN users.user_name IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN users.user_program[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN users.user_program[2] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN users.user_program[3] IN FRAME F-Main
   ALIGN-L EXP-LABEL EXP-FORMAT                                         */
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

&Scoped-define SELF-NAME users.user_program[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.user_program[1] V-table-Win
ON HELP OF users.user_program[1] IN FRAME F-Main /* Image Viewer */
DO:
    DEF VAR ls-filename AS CHAR NO-UNDO.
    DEF VAR ll-ok AS LOG NO-UNDO.

    SYSTEM-DIALOG GET-FILE ls-filename 
        TITLE "Select Image Viewer File to insert"
        FILTERS "Application Files    (*.exe)" "*.exe",
                "All Files    (*.*) " "*.*"
        INITIAL-DIR '"c:\program files\"'
        MUST-EXIST
        USE-FILENAME
        UPDATE ll-ok.

     IF INDEX(ls-filename,"/") > 0 THEN ASSIGN
        SELF:SCREEN-VALUE = SUBSTRING(ls-filename,R-INDEX(ls-filename,"/") + 1).
     ELSE IF INDEX(ls-filename,"\") > 0 THEN ASSIGN
        SELF:SCREEN-VALUE = SUBSTRING(ls-filename,R-INDEX(ls-filename,"\") + 1).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.user_program[1] V-table-Win
ON LEAVE OF users.user_program[1] IN FRAME F-Main /* Image Viewer */
DO:

    IF INDEX(SELF:SCREEN-VALUE,"/") > 0 THEN ASSIGN
        SELF:SCREEN-VALUE = SUBSTRING(SELF:SCREEN-VALUE,R-INDEX(SELF:SCREEN-VALUE,"/") + 1).
     ELSE IF INDEX(SELF:SCREEN-VALUE,"\") > 0 THEN ASSIGN
        SELF:SCREEN-VALUE = SUBSTRING(SELF:SCREEN-VALUE,R-INDEX(SELF:SCREEN-VALUE,"\") + 1).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME users.user_program[3]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL users.user_program[3] V-table-Win
ON HELP OF users.user_program[3] IN FRAME F-Main /* Document Path */
DO:
    DEF VAR ls-filename AS CHAR NO-UNDO.
    DEF VAR ll-ok AS LOG NO-UNDO.

    SYSTEM-DIALOG GET-DIR ls-filename 
        TITLE "Select Path to save"
        INITIAL-DIR users.USER_program[3]
        UPDATE ll-ok.

    IF ll-ok THEN ASSIGN
        SELF:SCREEN-VALUE = ls-filename.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  {custom/gcompany.i}
  {custom/getcmpny.i}

  DO TRANSACTION:
     {sys/inc/webroot.i}
  END.

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
  {src/adm/template/row-list.i "users"}
  {src/adm/template/row-list.i "usr"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "users"}
  {src/adm/template/row-find.i "usr"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEF BUFFER bf-usercust FOR usercust .
    DEF BUFFER bf-usrx FOR usrx .
    DEF BUFFER bf-usercomp FOR usercomp.
    
    DEF VAR lv-default-comp AS cha NO-UNDO.
    DEF VAR lv-default-loc AS cha NO-UNDO.
    DEF VAR ll-ans AS LOG NO-UNDO.
    DEF VAR ll-dummy AS LOG NO-UNDO.
    DEF VAR v-old-pass AS cha FORM "x(30)" NO-UNDO.
    DEF VAR v-new-pass AS cha FORM "x(30)" NO-UNDO.
    DEF VAR cOldUserID AS CHARACTER FORM "x(30)" NO-UNDO.
  
    ASSIGN 
        cOldUserID = users.user_id.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  /* gdm - 05180924 */
  ASSIGN users.image_filename = TRIM(fi_email).

  {methods/viewers/assign/{&FIRST-EXTERNAL-TABLE}.i}

  IF adm-new-record THEN DO:
     FIND FIRST bf-usercomp WHERE bf-usercomp.USER_id = "ASI" AND
                                  bf-usercomp.company_default NO-LOCK NO-ERROR.
     lv-default-comp = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001".

     FIND FIRST usercomp WHERE usercomp.USER_id = users.USER_id 
                           AND usercomp.company = lv-default-comp AND
                               usercomp.loc = ""
         NO-LOCK NO-ERROR.
     IF NOT AVAIL usercomp THEN DO:
        CREATE usercomp.
        ASSIGN usercomp.user_id = users.USER_id
            usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
            usercomp.loc = ""
            usercomp.company_default = YES.
     END.
     FIND FIRST bf-usercomp WHERE bf-usercomp.USER_id = "ASI" AND
                                  bf-usercomp.loc_default NO-LOCK NO-ERROR.
     lv-default-loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN".

     FIND FIRST usercomp WHERE usercomp.USER_id = users.USER_id 
                           AND usercomp.company = lv-default-comp AND
                               usercomp.loc = lv-default-loc NO-LOCK NO-ERROR.

     IF NOT AVAIL usercomp THEN DO:
        CREATE usercomp.
        ASSIGN usercomp.user_id = users.USER_id
            usercomp.company = IF AVAIL bf-usercomp THEN bf-usercomp.company ELSE "001"
            usercomp.loc = IF AVAIL bf-usercomp THEN bf-usercomp.loc ELSE "MAIN"
            usercomp.loc_DEFAULT = YES.
     END.
  END.

  if adm-new-record and not adm-adding-record then do:  /* copy */
   FOR EACH usercust NO-LOCK
           WHERE usercust.user_id EQ cOldUserID 
             AND usercust.company EQ cocode  , 
           FIRST cust WHERE 
              cust.company EQ usercust.company AND 
              cust.cust-no EQ usercust.cust-no NO-LOCK  :

          CREATE bf-usercust .
          BUFFER-COPY usercust EXCEPT rec_key user_id TO bf-usercust.
          ASSIGN
              bf-usercust.user_id = users.USER_id .

      END.
      /*FOR EACH uservend WHERE      uservend.user_id EQ cOldUserID AND
                uservend.company EQ cocode NO-LOCK, 
          FIRST vend WHERE             vend.company EQ uservend.company AND
             vend.vend-no EQ uservend.vend-no  NO-LOCK    :

          CREATE bf-uservend .
          BUFFER-COPY uservend EXCEPT rec_key user_id TO bf-uservend.
          ASSIGN
              bf-uservend.user_id = users.USER_id .

      END.
      FOR EACH usersman WHERE      usersman.user_id EQ cOldUserID AND
                usersman.company EQ cocode NO-LOCK, 
          FIRST sman WHERE             sman.company EQ usersman.company AND  
              sman.sman EQ usersman.sman NO-LOCK :

          CREATE bf-usersman .
          BUFFER-COPY usersman EXCEPT rec_key user_id TO bf-usersman.
          ASSIGN
              bf-usersman.user_id = users.USER_id .
      END.*/ /*Ticket - 22100*/

      FOR EACH usrx 
          WHERE usrx.uid = cOldUserID AND usrx.company = cocode AND 
          usrx.loc NE "" NO-LOCK, 
          EACH loc OF usrx  NO-LOCK :

          CREATE bf-usrx .
          BUFFER-COPY usrx EXCEPT rec_key uid TO bf-usrx.
          ASSIGN
              bf-usrx.uid = users.USER_id .
      END.
  END.

  SESSION:SET-WAIT-STATE("general").
     FOR EACH prgrms :
         IF LOOKUP(cOldUserID,prgrms.can_run) > 0 
            AND LOOKUP(users.user_id:SCREEN-VALUE IN FRAME {&FRAME-NAME},prgrms.can_run) <= 0 
            THEN prgrms.can_run = prgrms.can_run + "," + users.user_id:SCREEN-VALUE.
         IF LOOKUP(cOldUserID,prgrms.can_create) > 0 
             AND LOOKUP(users.user_id:SCREEN-VALUE,prgrms.can_create) <= 0 
            THEN prgrms.can_create = prgrms.can_create + "," + users.user_id:SCREEN-VALUE.
         IF LOOKUP(cOldUserID,prgrms.can_update) > 0 
             AND LOOKUP(users.user_id:SCREEN-VALUE,prgrms.can_update) <= 0 
            THEN prgrms.can_update = prgrms.can_update + "," + users.user_id:SCREEN-VALUE.
         IF LOOKUP(cOldUserID,prgrms.can_delete) > 0 
             AND LOOKUP(users.user_id:SCREEN-VALUE,prgrms.can_delete) <= 0 
            THEN prgrms.can_delete = prgrms.can_delete + "," + users.user_id:SCREEN-VALUE.
     END.
  RUN reftable-values(NO).

  SESSION:SET-WAIT-STATE("").

  IF NOT adm-new-record THEN
     MESSAGE "Do you want to change " users.USER_id "'s password?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans .
  IF ll-ans THEN DO:

     FIND NOSWEAT._user
          WHERE NOSWEAT._user._userid = users.user_id EXCLUSIVE-LOCK NO-ERROR.
     IF AVAILABLE NOSWEAT._user THEN         
     DO:
         RUN windows/d-passwd.w (RECID(_user)).
         /*
         UPDATE SKIP(1) "Enter User's Current Password" 
                SPACE(5) v-old-pass VALIDATE(ENCODE(v-old-pass) = _user._password,"Invalid Current Password")
             SPACE(5) 
                "                        New Password" space(5) v-new-pass 
         SKIP(1)
         WITH FRAME f-password2
              CENTERED NO-LABEL ROW 10 SIDE-LABELS TITLE "User Password" OVERLAY.
         HIDE FRAME f-password2 NO-PAUSE.
         /*
         ASSIGN
             NOSWEAT._user._password = ENCODE(v-new-pass).
         */
         /*ll-dummy = SETUSERID(_user._USERid,encode(v-new-pass),ldbname(1)) .
         */
         _user._password = ENCODE(v-new-pass).
         */
      END.

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  DISABLE tg_po tg_bol tg_invoice tg_ack tg_quote
          fi_phone-area lv-phone-num fi_phone-country
          fi_fax-area lv-fax-num fi_fax-country fi_email
          WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        fi_email = users.image_filename
        fi_email:SCREEN-VALUE IN FRAME {&FRAME-NAME} = users.image_filename.

    RUN reftable-values(INPUT YES).

    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    

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
  RUN validate-userid NO-ERROR.
  IF error-status:error THEN RETURN.

  IF users.user_program[2]:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN
  DO:
     IF SUBSTRING(users.user_program[2]:SCREEN-VALUE,LENGTH(users.user_program[2]:SCREEN-VALUE),1) EQ "\" OR
        SUBSTRING(users.user_program[2]:SCREEN-VALUE,LENGTH(users.user_program[2]:SCREEN-VALUE),1) EQ "/" THEN
        DO:
           MESSAGE "Document/Report Temp Path cannot end in / or \." 
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY "ENTRY" TO users.user_program[2] IN FRAME {&FRAME-NAME}.
           RETURN.
        END.
  END.

  IF users.user_program[3]:SCREEN-VALUE NE "" THEN
  DO:
     {&methods/lValidateError.i YES}
     IF SUBSTRING(users.user_program[3]:SCREEN-VALUE,LENGTH(users.user_program[3]:SCREEN-VALUE),1) EQ "\" OR
        SUBSTRING(users.user_program[3]:SCREEN-VALUE,LENGTH(users.user_program[3]:SCREEN-VALUE),1) EQ "/" THEN
     DO:
           MESSAGE "FG/RM Pallet Load Tag / Case Label Text File Path cannot end in / or \." 
               VIEW-AS ALERT-BOX ERROR BUTTONS OK.
           APPLY "ENTRY" TO users.user_program[3] IN FRAME {&FRAME-NAME}.
           RETURN.
     END.
     FILE-INFO:FILE-NAME = users.USER_program[3].
     IF FILE-INFO:FILE-type eq ? then do:
        message "FG/RM Pallet Load Tag / Case Label Text File Path does not exist. Do you want to create it?" 
             view-as alert-box ERROR BUTTON YES-NO UPDATE v-ans AS LOG.
        IF v-ans THEN OS-CREATE-DIR VALUE(file-info:file-name).
     END.
    {&methods/lValidateError.i NO}
  END.  

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DISABLE tg_po tg_bol tg_invoice tg_ack tg_quote
          fi_phone-area lv-phone-num fi_phone-country
          fi_fax-area lv-fax-num fi_fax-country fi_email
          WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-enable V-table-Win 
PROCEDURE proc-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* task 04101303*/
    IF USERID(LDBNAME(1)) EQ "asi" THEN
         ASSIGN users.track_usage:SENSITIVE IN FRAME {&FRAME-NAME} = YES 
                users.use_colors:SENSITIVE = YES
                users.use_fonts:SENSITIVE = YES
                users.use_ctrl_keys:SENSITIVE = YES
                users.developer:SENSITIVE = YES.
    ELSE
        ASSIGN users.track_usage:SENSITIVE = NO 
               users.use_colors:SENSITIVE = NO
               users.use_fonts:SENSITIVE = NO
               users.use_ctrl_keys:SENSITIVE = NO
               users.developer:SENSITIVE = NO.

  ENABLE tg_po tg_bol tg_invoice tg_ack tg_quote
         fi_phone-area lv-phone-num fi_phone-country
         fi_fax-area lv-fax-num fi_fax-country fi_email
         WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-rowavail V-table-Win 
PROCEDURE proc-rowavail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values V-table-Win 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT PARAM ip-display AS LOG NO-UNDO.

  DEF VAR v-phone-num AS CHAR NO-UNDO.
  DEF VAR v-fax-num AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:

    IF AVAIL users THEN DO:
       FIND FIRST reftable WHERE
            reftable.reftable EQ "users.user-docs" AND
            reftable.company EQ users.user_id
            NO-ERROR.

       IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
            reftable.reftable = "users.user-docs"
            reftable.company  = users.user_id.
       END.

       IF ip-display THEN
          ASSIGN
             tg_po = LOGICAL(reftable.val[1])
             tg_bol = LOGICAL(reftable.val[2])
             tg_invoice = LOGICAL(reftable.val[3])
             tg_ack = LOGICAL(reftable.val[4])
             tg_quote = LOGICAL(reftable.val[5]).
       ELSE
          ASSIGN
             reftable.val[1] = IF LOGICAL(tg_po:SCREEN-VALUE) THEN 1 ELSE 0
             reftable.val[2] = IF LOGICAL(tg_bol:SCREEN-VALUE) THEN 1 ELSE 0
             reftable.val[3] = IF LOGICAL(tg_invoice:SCREEN-VALUE) THEN 1 ELSE 0
             reftable.val[4] = IF LOGICAL(tg_ack:SCREEN-VALUE) THEN 1 ELSE 0
             reftable.val[5] = IF LOGICAL(tg_quote:SCREEN-VALUE) THEN 1 ELSE 0.

       RELEASE reftable.

       FIND FIRST reftable WHERE
            reftable.reftable EQ "users.phone-no" AND
            reftable.company EQ users.user_id
            NO-ERROR.

       IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
            reftable.reftable = "users.phone-no"
            reftable.company  = users.user_id.
       END.

       IF ip-display THEN
          ASSIGN
             fi_phone-area = SUBSTRING(reftable.CODE,1,3)
             lv-phone-num = SUBSTRING(reftable.CODE,4).
       ELSE
          reftable.CODE = REPLACE(REPLACE(fi_phone-area:SCREEN-VALUE,'(',''),')','')
                        + REPLACE(lv-phone-num:SCREEN-VALUE,'-','').

       RELEASE reftable.

       FIND FIRST reftable WHERE
            reftable.reftable EQ "users.fax-no" AND
            reftable.company EQ users.user_id
            NO-ERROR.

       IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
            reftable.reftable = "users.fax-no"
            reftable.company  = users.user_id.
       END.

       IF ip-display THEN
          ASSIGN
             fi_fax-area = SUBSTRING(reftable.CODE,1,3)
             lv-fax-num = SUBSTRING(reftable.CODE,4).
       ELSE
          reftable.CODE = REPLACE(REPLACE(fi_fax-area:SCREEN-VALUE,'(',''),')','')
                        + REPLACE(lv-fax-num:SCREEN-VALUE,'-','').

       RELEASE reftable.

       FIND FIRST reftable WHERE
            reftable.reftable EQ "users.phone-cnty" AND
            reftable.company EQ users.user_id
            NO-ERROR.

       IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
            reftable.reftable = "users.phone-cnty"
            reftable.company  = users.user_id.
       END.

       IF ip-display THEN
          ASSIGN
             fi_phone-country = reftable.CODE.
       ELSE
          reftable.CODE = fi_phone-country:SCREEN-VALUE.

       RELEASE reftable.

       FIND FIRST reftable WHERE
            reftable.reftable EQ "users.fax-cnty" AND
            reftable.company EQ users.user_id
            NO-ERROR.

       IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
            reftable.reftable = "users.fax-cnty"
            reftable.company  = users.user_id.
       END.

       IF ip-display THEN
          ASSIGN
             fi_fax-country = reftable.CODE.
       ELSE
          reftable.CODE = fi_fax-country:SCREEN-VALUE.

       RELEASE reftable.
    END.
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
  {src/adm/template/snd-list.i "users"}
  {src/adm/template/snd-list.i "usr"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-user-id V-table-Win 
PROCEDURE valid-user-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-user-id AS cha NO-UNDO.
  DEF BUFFER bf-users FOR users.

  {methods/lValidateError.i YES}
  FIND FIRST bf-users WHERE bf-users.USER_id = ip-user-id NO-LOCK NO-ERROR.
  IF NOT AVAIL bf-users AND NOT adm-new-record THEN DO:
     MESSAGE "Invalid User ID. " VIEW-AS ALERT-BOX ERROR.     
     RETURN ERROR.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validate-userid V-table-Win 
PROCEDURE validate-userid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  IF users.USER_id:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN DO:
     MESSAGE "User Id must be entered. " VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO users.USER_id.
     RETURN ERROR.
  END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

