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
{custom/globdefs.i}
{sys/inc/var.i new shared}
{sys/inc/varasgn.i}

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
&Scoped-Define ENABLED-FIELDS location.defaultBin loc.dscr ~
location.streetAddr[1] location.streetAddr[2] location.streetAddr[3] ~
location.subCode3 location.subCode1 location.subCode4 location.countryCode ~
location.subCode2 location.geoLat location.geoLong location.phone ~
location.externalID[1] location.fax location.email location.notes 
&Scoped-define ENABLED-TABLES location loc
&Scoped-define FIRST-ENABLED-TABLE location
&Scoped-define SECOND-ENABLED-TABLE loc
&Scoped-Define ENABLED-OBJECTS rsBinType 
&Scoped-Define DISPLAYED-FIELDS loc.loc location.defaultBin loc.company ~
location.streetAddr[4] loc.dscr location.streetAddr[5] ~
location.streetAddr[1] location.streetAddr[6] location.streetAddr[2] ~
location.streetAddr[3] location.subCode3 location.subCode1 ~
location.subCode4 location.countryCode location.subCode2 location.geoLat ~
location.geoLong location.phone location.externalID[1] location.fax ~
location.email location.notes 
&Scoped-define DISPLAYED-TABLES loc location
&Scoped-define FIRST-DISPLAYED-TABLE loc
&Scoped-define SECOND-DISPLAYED-TABLE location
&Scoped-Define DISPLAYED-OBJECTS rsBinType fsStDesc fsCtyDesc 

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
     SIZE 37 BY .95 NO-UNDO.

DEFINE VARIABLE fsStDesc AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .95 NO-UNDO.

DEFINE VARIABLE rsBinType AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "FG", "FG",
"RM", "RM",
"WP", "WP"
     SIZE 23 BY .95 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     loc.loc AT ROW 1.24 COL 12 COLON-ALIGNED
          LABEL "Location"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 FONT 4
     location.defaultBin AT ROW 1.24 COL 49 COLON-ALIGNED
          LABEL "Default Bin"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     rsBinType AT ROW 1.24 COL 64 NO-LABEL
     loc.company AT ROW 1.24 COL 97 COLON-ALIGNED NO-LABEL BLANK 
          VIEW-AS FILL-IN 
          SIZE 2 BY 1
     location.streetAddr[4] AT ROW 2.19 COL 97 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2 BY 1
     loc.dscr AT ROW 2.67 COL 16 COLON-ALIGNED
          LABEL "Name" FORMAT "x(60)"
          VIEW-AS FILL-IN 
          SIZE 73 BY 1
     location.streetAddr[5] AT ROW 3.14 COL 97 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2 BY 1
     location.streetAddr[1] AT ROW 3.62 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 73 BY 1
     location.streetAddr[6] AT ROW 4.1 COL 97 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2 BY 1
     location.streetAddr[2] AT ROW 4.57 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 73 BY 1
     location.streetAddr[3] AT ROW 5.52 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 73 BY 1
     location.subCode3 AT ROW 6.48 COL 16 COLON-ALIGNED
          LABEL "City"
          VIEW-AS FILL-IN 
          SIZE 53 BY 1
     location.subCode1 AT ROW 7.43 COL 16 COLON-ALIGNED
          LABEL "St/Prov"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     fsStDesc AT ROW 7.48 COL 27 COLON-ALIGNED NO-LABEL
     location.subCode4 AT ROW 8.38 COL 16 COLON-ALIGNED
          LABEL "Zip/Post"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     location.countryCode AT ROW 8.43 COL 43 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .95
     fsCtyDesc AT ROW 8.43 COL 52 COLON-ALIGNED NO-LABEL
     location.subCode2 AT ROW 9.57 COL 16 COLON-ALIGNED
          LABEL "County"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     location.geoLat AT ROW 9.57 COL 43 COLON-ALIGNED
          LABEL "Lat"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     location.geoLong AT ROW 9.57 COL 74 COLON-ALIGNED
          LABEL "Long"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     location.phone AT ROW 10.76 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     location.externalID[1] AT ROW 10.76 COL 71 COLON-ALIGNED
          LABEL "Ext.Code"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     location.fax AT ROW 11.71 COL 16 COLON-ALIGNED
          LABEL "Fax"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     location.email AT ROW 12.67 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 73 BY 1
     location.notes AT ROW 14.33 COL 18 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 73 BY 3.1
     "Notes:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 14.33 COL 9
     "Address:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 4.1 COL 7
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

/* SETTINGS FOR FILL-IN loc.company IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN location.defaultBin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.dscr IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN location.externalID[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN location.fax IN FRAME F-Main
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

&Scoped-define SELF-NAME F-Main
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
    DEFINE VARIABLE char-val  AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE hlp-recid AS RECID         NO-UNDO.
    DEFINE VARIABLE lw-focus  AS WIDGET-HANDLE NO-UNDO.
    DEFINE VARIABLE ll        AS LOGICAL       INITIAL YES NO-UNDO.
    DEFINE VARIABLE op-rowid  AS ROWID         NO-UNDO.
    DEFINE VARIABLE op-recid  AS RECID         NO-UNDO.

    lw-focus = FOCUS.

    CASE lw-focus:NAME :
        WHEN "defaultBin" THEN 
            DO:
                IF rsBinType:SCREEN-VALUE EQ "FG" THEN 
                    RUN windows/l-fgbin.w (loc.company:SCREEN-VALUE,loc.loc:SCREEN-VALUE, lw-focus:SCREEN-VALUE, OUTPUT char-val). 
                ELSE IF rsBinType:SCREEN-VALUE EQ "RM" THEN 
                    RUN windows/l-rmbin.w (loc.company:SCREEN-VALUE,loc.loc:SCREEN-VALUE, lw-focus:SCREEN-VALUE, OUTPUT char-val). 
                ELSE IF rsBinType:SCREEN-VALUE EQ "WP" THEN 
                    RUN windows/l-wipbin.w (loc.company:SCREEN-VALUE,loc.loc:SCREEN-VALUE, OUTPUT char-val, OUTPUT op-recid). 
                IF char-val NE "" THEN 
                DO :
                    ASSIGN 
                        lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
                END.   
            END.
    END CASE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME location.countryCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL location.countryCode V-table-Win
ON LEAVE OF location.countryCode IN FRAME F-Main /* Country */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME location.defaultBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL location.defaultBin V-table-Win
ON LEAVE OF location.defaultBin IN FRAME F-Main /* Default Bin */
DO:
    DEF VAR lFound AS LOG NO-UNDO.
    
    IF LASTKEY = -1 THEN  RETURN.

    FIND FIRST fg-bin NO-LOCK WHERE 
        fg-bin.company EQ g_company AND 
        fg-bin.loc EQ loc.loc:SCREEN-VALUE AND 
        fg-bin.loc-bin EQ SELF:SCREEN-VALUE 
        NO-ERROR.
    IF AVAIL fg-bin THEN ASSIGN 
        lFound = TRUE.
        
    FIND FIRST rm-bin NO-LOCK WHERE 
        rm-bin.company EQ g_company AND 
        rm-bin.loc EQ loc.loc:SCREEN-VALUE AND 
        rm-bin.loc-bin EQ SELF:SCREEN-VALUE 
        NO-ERROR.
    IF AVAIL rm-bin THEN ASSIGN 
        lFound = TRUE.

    FIND FIRST wip-bin NO-LOCK WHERE 
        wip-bin.company EQ g_company AND 
        wip-bin.loc EQ loc.loc:SCREEN-VALUE AND 
        wip-bin.loc-bin EQ SELF:SCREEN-VALUE 
        NO-ERROR.
    IF AVAIL wip-bin THEN ASSIGN 
        lFound = TRUE.
        
    IF NOT lFound THEN DO:
        MESSAGE 
            "Unable to locate this bin in the fg-bin, rm-bin, or wip-bin tables."
            VIEW-AS ALERT-BOX.
        APPLY 'entry' TO SELF.
        RETURN NO-APPLY.
    END. 
               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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


&Scoped-define SELF-NAME location.subCode1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL location.subCode1 V-table-Win
ON LEAVE OF location.subCode1 IN FRAME F-Main /* St/Prov */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME location.subCode4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL location.subCode4 V-table-Win
ON LEAVE OF location.subCode4 IN FRAME F-Main /* Zip/Post */
DO:
    IF LASTKEY = -1 THEN  RETURN.

    FIND FIRST fg-bin NO-LOCK WHERE 
        fg-bin.company EQ g_company AND 
        fg-bin.loc EQ loc.loc:SCREEN-VALUE AND 
        fg-bin.loc-bin EQ SELF:SCREEN-VALUE 
        NO-ERROR.
    IF AVAIL fg-bin THEN ASSIGN 
            lFound = TRUE.
  
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
    DEF VAR cBinType AS CHAR NO-UNDO.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

    
    IF LASTKEY = -1 THEN  RETURN.

    FIND FIRST fg-bin NO-LOCK WHERE 
        fg-bin.company EQ g_company AND 
        fg-bin.loc EQ loc.loc:SCREEN-VALUE IN FRAME {&frame-name} AND 
        fg-bin.loc-bin EQ SELF:SCREEN-VALUE 
        NO-ERROR.
    IF AVAIL fg-bin THEN ASSIGN
        cBinType = "FG".
        
    FIND FIRST rm-bin NO-LOCK WHERE 
        rm-bin.company EQ g_company AND 
        rm-bin.loc EQ loc.loc:SCREEN-VALUE AND 
        rm-bin.loc-bin EQ SELF:SCREEN-VALUE 
        NO-ERROR.
    IF AVAIL rm-bin THEN ASSIGN 
        cBinType = "RM".

    FIND FIRST wip-bin NO-LOCK WHERE 
        wip-bin.company EQ g_company AND 
        wip-bin.loc EQ loc.loc:SCREEN-VALUE AND 
        wip-bin.loc-bin EQ SELF:SCREEN-VALUE 
        NO-ERROR.
    IF AVAIL wip-bin THEN ASSIGN 
        cBinType = "WP".
        
    IF cBinType EQ ? THEN ASSIGN 
        cBinType = "FG".
    ASSIGN 
        rsBinType:SCREEN-VALUE = cBinType.    

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

