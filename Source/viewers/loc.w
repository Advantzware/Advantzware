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

 DEFINE VARIABLE lCheckBinMessage AS LOGICAL NO-UNDO .
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
&Scoped-Define ENABLED-FIELDS location.defaultBin loc.dscr loc.handlingCost ~
location.streetAddr[1] loc.storageCost[1] location.streetAddr[2] ~
loc.storageCost[2] location.streetAddr[3] loc.storageCost[3] ~
location.subCode3 loc.storageCost[4] location.subCode1 loc.owner ~
location.subCode4 location.countryCode loc.locationSquareFeet ~
loc.palletCapacity location.subCode2 location.geoLat location.geoLong ~
location.phone location.externalID[1] location.fax loc.division ~
location.email loc.glCode location.notes loc.active loc.isAPIEnabled ~
location.lActive 
&Scoped-define ENABLED-TABLES location loc
&Scoped-define FIRST-ENABLED-TABLE location
&Scoped-define SECOND-ENABLED-TABLE loc
&Scoped-Define ENABLED-OBJECTS rsBinType 
&Scoped-Define DISPLAYED-FIELDS loc.loc location.defaultBin loc.company ~
location.streetAddr[4] loc.dscr loc.handlingCost location.streetAddr[5] ~
location.streetAddr[1] loc.storageCost[1] location.streetAddr[6] ~
location.streetAddr[2] loc.storageCost[2] location.streetAddr[3] ~
loc.storageCost[3] location.subCode3 loc.storageCost[4] location.subCode1 ~
loc.owner location.subCode4 location.countryCode loc.locationSquareFeet ~
loc.palletCapacity location.subCode2 location.geoLat location.geoLong ~
location.phone location.externalID[1] location.fax loc.division ~
location.email loc.glCode location.notes loc.active loc.isAPIEnabled ~
location.lActive 
&Scoped-define DISPLAYED-TABLES loc location
&Scoped-define FIRST-DISPLAYED-TABLE loc
&Scoped-define SECOND-DISPLAYED-TABLE location
&Scoped-Define DISPLAYED-OBJECTS rsBinType 

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
     loc.dscr AT ROW 2.43 COL 12 COLON-ALIGNED
          LABEL "Name" FORMAT "x(60)"
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     loc.handlingCost AT ROW 2.43 COL 78 COLON-ALIGNED
          LABEL "Handling" FORMAT "->,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     location.streetAddr[5] AT ROW 3.14 COL 97 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2 BY 1
     location.streetAddr[1] AT ROW 3.38 COL 12 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     loc.storageCost[1] AT ROW 3.38 COL 78 COLON-ALIGNED
          LABEL "Storage 1" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     location.streetAddr[6] AT ROW 4.1 COL 97 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 2 BY 1
     location.streetAddr[2] AT ROW 4.33 COL 12 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     loc.storageCost[2] AT ROW 4.33 COL 78 COLON-ALIGNED
          LABEL "Storage 2" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     location.streetAddr[3] AT ROW 5.29 COL 12 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 50 BY 1
     loc.storageCost[3] AT ROW 5.29 COL 78 COLON-ALIGNED
          LABEL "Storage 3" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     location.subCode3 AT ROW 6.24 COL 12 COLON-ALIGNED
          LABEL "City"
          VIEW-AS FILL-IN 
          SIZE 40 BY 1
     loc.storageCost[4] AT ROW 6.24 COL 78 COLON-ALIGNED
          LABEL "Storage 4" FORMAT "->>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     location.subCode1 AT ROW 7.19 COL 12 COLON-ALIGNED
          LABEL "St/Prov"
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     loc.owner AT ROW 7.19 COL 78 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     location.subCode4 AT ROW 7.24 COL 34.4 COLON-ALIGNED
          LABEL "Zip/Post"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     location.countryCode AT ROW 7.24 COL 60.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY .95
     loc.locationSquareFeet AT ROW 8.43 COL 39 COLON-ALIGNED
          LABEL "Location Gross"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     loc.palletCapacity AT ROW 8.43 COL 70 COLON-ALIGNED
          LABEL "Pallets"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     location.subCode2 AT ROW 9.57 COL 12 COLON-ALIGNED
          LABEL "County"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     location.geoLat AT ROW 9.57 COL 39 COLON-ALIGNED
          LABEL "Lat"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.2 BY 17.38
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     location.geoLong AT ROW 9.57 COL 70 COLON-ALIGNED
          LABEL "Long"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     location.phone AT ROW 10.52 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     location.externalID[1] AT ROW 10.52 COL 67 COLON-ALIGNED
          LABEL "Ext.Code"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     location.fax AT ROW 11.48 COL 12 COLON-ALIGNED
          LABEL "Fax"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     loc.division AT ROW 11.48 COL 67 COLON-ALIGNED
          LABEL "Division"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     location.email AT ROW 12.43 COL 12 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     loc.glCode AT ROW 12.43 COL 67 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 29 BY 1
     location.notes AT ROW 14.1 COL 14 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 73 BY 3.1
     loc.active AT ROW 17.33 COL 14 WIDGET-ID 2
          LABEL "Active?"
          VIEW-AS TOGGLE-BOX
          SIZE 13.4 BY .81
     loc.isAPIEnabled AT ROW 17.33 COL 34 WIDGET-ID 4
          LABEL "API Enabled"
          VIEW-AS TOGGLE-BOX
          SIZE 19.6 BY .81
     location.lActive AT ROW 17.33 COL 55 HELP
          "Flag to indicate that location supports consignment materials."
          LABEL "Consignment"
          VIEW-AS TOGGLE-BOX
          SIZE 18.4 BY .81
     "Notes:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 14.1 COL 5
     "Address:" VIEW-AS TEXT
          SIZE 10 BY .62 AT ROW 3.86 COL 3
     "Capacity:" VIEW-AS TEXT
          SIZE 11.4 BY .62 AT ROW 8.62 COL 2.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.2 BY 17.38
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
         HEIGHT             = 17.38
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

/* SETTINGS FOR TOGGLE-BOX loc.active IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.company IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       loc.company:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN location.defaultBin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.division IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.dscr IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN location.externalID[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN location.fax IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN location.geoLat IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN location.geoLong IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.handlingCost IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX loc.isAPIEnabled IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX location.lActive IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN loc.loc IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN loc.locationSquareFeet IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.palletCapacity IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN loc.storageCost[1] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN loc.storageCost[2] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN loc.storageCost[3] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN loc.storageCost[4] IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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
        WHEN "owner" THEN
        DO:
            RUN windows/l-vendno.w (g_company, "A", FOCUS:SCREEN-VALUE, OUTPUT char-val).
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
        
    IF NOT lFound AND location.defaultBin:SCREEN-VALUE NE "" AND NOT lCheckBinMessage THEN DO:
        MESSAGE 
            "Do you want to add the new bin?"
            VIEW-AS ALERT-BOX QUESTION 
            BUTTONS YES-NO UPDATE lcheckflg as logical .
        IF NOT lcheckflg THEN do:
            APPLY 'entry' TO SELF.
            RETURN NO-APPLY.
        END.
        ELSE lCheckBinMessage = YES .
    END. 
               
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME location.defaultBin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL location.defaultBin V-table-Win
ON VALUE-CHANGED OF location.defaultBin IN FRAME F-Main /* Default Bin */
DO: 
  lCheckBinMessage = NO .
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
    IF lCheckBinMessage THEN DO:
        IF rsBinType:SCREEN-VALUE EQ "FG" THEN DO:
            FIND FIRST fg-bin NO-LOCK WHERE 
                fg-bin.company EQ g_company AND 
                fg-bin.loc EQ loc.loc:SCREEN-VALUE IN FRAME {&frame-name} AND 
                fg-bin.loc-bin EQ location.defaultBin 
                NO-ERROR.
            IF NOT AVAIL fg-bin THEN do: 
                CREATE fg-bin .
                ASSIGN
                    fg-bin.company = g_company
                    fg-bin.loc = loc.loc
                    fg-bin.loc-bin = location.defaultBin  .
            END.
        END. /* rsBinType:SCREEN-VALUE EQ "FG"*/
        ELSE IF rsBinType:SCREEN-VALUE EQ "RM" THEN DO:
            FIND FIRST rm-bin NO-LOCK WHERE 
                rm-bin.company EQ g_company AND 
                rm-bin.loc EQ loc.loc:SCREEN-VALUE IN FRAME {&frame-name} AND 
                rm-bin.loc-bin EQ location.defaultBin
                NO-ERROR.
            IF NOT AVAIL rm-bin THEN do: 
                CREATE rm-bin .
                ASSIGN
                    rm-bin.company = g_company
                    rm-bin.loc = loc.loc
                    rm-bin.loc-bin = location.defaultBin  .
            END.
        END. /* rsBinType:SCREEN-VALUE EQ "rm"*/
        ELSE IF rsBinType:SCREEN-VALUE EQ "wp" THEN DO:
            FIND FIRST wip-bin NO-LOCK WHERE 
                wip-bin.company EQ g_company AND 
                wip-bin.loc EQ loc.loc:SCREEN-VALUE IN FRAME {&frame-name} AND 
                wip-bin.loc-bin EQ location.defaultBin
                NO-ERROR.
            IF NOT AVAIL wip-bin THEN do: 
                CREATE wip-bin .
                ASSIGN
                    wip-bin.company = g_company
                    wip-bin.loc = loc.loc
                    wip-bin.loc-bin = location.defaultBin  .
            END.
        END. /* rsBinType:SCREEN-VALUE EQ "wp"*/
    END.
    
    lCheckBinMessage = NO .     
    adm-new-record = NO .    

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
  lCheckBinMessage = NO .
  adm-new-record = NO .

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

