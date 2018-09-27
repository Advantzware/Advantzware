&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/loc.w

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
&Scoped-define EXTERNAL-TABLES loc location
&Scoped-define FIRST-EXTERNAL-TABLE loc


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR loc, location.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS loc.whs-chg loc.wc-uom loc.dscr ~
location.lActive location.streetAddr[1] location.streetAddr[2] ~
location.streetAddr[3] location.subCode3 location.subCode1 ~
location.subCode4 location.countryCode location.subCode2 location.geoLat ~
location.geoLong location.phone location.fax location.email ~
location.defaultBin location.externalID[1] location.notes 
&Scoped-define ENABLED-TABLES loc location
&Scoped-define FIRST-ENABLED-TABLE loc
&Scoped-define SECOND-ENABLED-TABLE location
&Scoped-Define DISPLAYED-FIELDS loc.loc loc.whs-chg loc.wc-uom loc.dscr ~
location.lActive location.streetAddr[1] location.streetAddr[2] ~
location.streetAddr[3] location.streetAddr[4] location.subCode3 ~
location.streetAddr[5] location.subCode1 location.streetAddr[6] ~
location.subCode4 location.countryCode location.subCode2 location.geoLat ~
location.geoLong location.phone location.fax location.email ~
location.defaultBin location.externalID[1] location.notes 
&Scoped-define DISPLAYED-TABLES loc location
&Scoped-define FIRST-DISPLAYED-TABLE loc
&Scoped-define SECOND-DISPLAYED-TABLE location
&Scoped-Define DISPLAYED-OBJECTS fsStDesc fsCtyDesc 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS loc.loc 

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
DEFINE VARIABLE fsCtyDesc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE fsStDesc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     loc.loc AT ROW 1.24 COL 12 COLON-ALIGNED
          LABEL "Location"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     loc.whs-chg AT ROW 1.24 COL 49 COLON-ALIGNED
          LABEL "Whse Chg"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     loc.wc-uom AT ROW 1.24 COL 77 COLON-ALIGNED
          LABEL "Chg Unit"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     loc.dscr AT ROW 2.67 COL 16 COLON-ALIGNED
          LABEL "Name"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     location.lActive AT ROW 2.67 COL 68
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     location.streetAddr[1] AT ROW 3.62 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     location.streetAddr[2] AT ROW 4.57 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     location.streetAddr[3] AT ROW 5.52 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     location.streetAddr[4] AT ROW 6 COL 95 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
     location.subCode3 AT ROW 6.48 COL 16 COLON-ALIGNED
          LABEL "City"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     location.streetAddr[5] AT ROW 6.95 COL 95 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
     location.subCode1 AT ROW 7.43 COL 16 COLON-ALIGNED
          LABEL "St/Prov"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     fsStDesc AT ROW 7.43 COL 27 COLON-ALIGNED NO-LABEL
     location.streetAddr[6] AT ROW 7.91 COL 95 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
     location.subCode4 AT ROW 8.38 COL 16 COLON-ALIGNED
          LABEL "Zip/Post"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     location.countryCode AT ROW 8.38 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     fsCtyDesc AT ROW 8.38 COL 52 COLON-ALIGNED NO-LABEL
     location.subCode2 AT ROW 9.57 COL 16 COLON-ALIGNED
          LABEL "County"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     location.geoLat AT ROW 9.57 COL 43 COLON-ALIGNED
          LABEL "Lat"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     location.geoLong AT ROW 9.57 COL 69 COLON-ALIGNED
          LABEL "Long"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     location.phone AT ROW 10.76 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     location.fax AT ROW 11.71 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     location.email AT ROW 12.67 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 74 BY 1
     location.defaultBin AT ROW 13.86 COL 16 COLON-ALIGNED
          LABEL "Default Bin"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     location.externalID[1] AT ROW 13.86 COL 42 COLON-ALIGNED
          LABEL "Ext.Code"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     location.notes AT ROW 15.05 COL 18 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 73 BY 2.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.2 BY 16.81
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.loc,asi.location
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
         HEIGHT             = 16.81
         WIDTH              = 102.2.
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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN location.defaultBin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.dscr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN location.externalID[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fsCtyDesc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fsStDesc IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN location.geoLat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN location.geoLong IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.loc IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN location.streetAddr[4] IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       location.streetAddr[4]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN location.streetAddr[5] IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       location.streetAddr[5]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN location.streetAddr[6] IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       location.streetAddr[6]:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN location.subCode1 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN location.subCode2 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN location.subCode3 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN location.subCode4 IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.wc-uom IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.whs-chg IN FRAME F-Main
   EXP-LABEL                                                            */
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

&Scoped-define SELF-NAME loc.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL loc.loc V-table-Win
ON LEAVE OF loc.loc IN FRAME F-Main /* Location */
DO:
   IF LASTKEY = -1 THEN  RETURN.
   {&methods/lValidateError.i YES}
   IF adm-new-record AND loc.loc:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ""
       THEN DO:
       MESSAGE "Locations must be entered." VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
   END.
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
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
  {src/adm/template/row-list.i "loc"}
  {src/adm/template/row-list.i "location"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "loc"}
  {src/adm/template/row-find.i "location"}

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
  {methods/viewers/create/loc.i}
    ASSIGN 
        location.locationCode = loc.loc:SCREEN-VALUE IN FRAME {&frame-name}
        loc.rec_key           = DYNAMIC-FUNCTION("sfGetNextRecKey") 
        location.rec_key      = DYNAMIC-FUNCTION("sfGetNextRecKey") 
        loc.addrRecKey        = location.rec_key.

        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/


    /* Code placed here will execute PRIOR to standard behavior. */
 

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
  {&methods/lValidateError.i YES}
  do with frame {&frame-name}:
     IF adm-new-record THEN DO:
        IF loc.loc:SCREEN-VALUE = "" THEN DO:
           MESSAGE "Locations must be entered." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO loc.loc.
           RETURN.
        END.
     END.
  END.
  {&methods/lValidateError.i NO}
    /* ============== end of validations ==================*/
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
  {src/adm/template/snd-list.i "loc"}
  {src/adm/template/snd-list.i "location"}

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

