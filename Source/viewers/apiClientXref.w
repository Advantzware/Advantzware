&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/apiClientXref.w

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

DEFINE VARIABLE lCheckCustProgram AS LOGICAL NO-UNDO.

IF  PROGRAM-NAME(3) MATCHES "*/cust.w*"  THEN
   lCheckCustProgram = YES .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* External Tables                                                      */
&Scoped-define EXTERNAL-TABLES apiClientXref
&Scoped-define FIRST-EXTERNAL-TABLE apiClientXref


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR apiClientXref.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS  ~
apiClientXref.company apiClientXref.scopeID apiClientXref.scopeType ~
apiClientXref.clientID
&Scoped-define ENABLED-TABLES apiClientXref
&Scoped-define FIRST-ENABLED-TABLE apiClientXref
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS apiClientXref.rec_key ~
apiClientXref.company apiClientXref.scopeID apiClientXref.scopeType ~
apiClientXref.clientID
&Scoped-define DISPLAYED-TABLES apiClientXref
&Scoped-define FIRST-DISPLAYED-TABLE apiClientXref
&Scoped-Define DISPLAYED-OBJECTS  

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5 */
&Scoped-define ADM-CREATE-FIELDS apiClientXref.rec_key 
&Scoped-define DISPLAY-FIELD apiClientXref.rec_key 


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
     SIZE 71 BY 5.67.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     apiClientXref.rec_key AT ROW 1.24 COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 FONT 4
     apiClientXref.company AT ROW 1.24  COL 14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
          BGCOLOR 15 FONT 4
     apiClientXref.scopeType AT ROW 2.43 COL 14 COLON-ALIGNED  FORMAT "x(8)"
          VIEW-AS COMBO-BOX INNER-LINES 4 
          LIST-ITEM-PAIRS "Customer","Customer",
                     "ShipTo","ShipTo",
                     "Vendor","Vendor"
          DROP-DOWN-LIST
          SIZE 18 BY 1
          BGCOLOR 15 FONT 4     
     apiClientXref.scopeID AT ROW  3.68  COL 14 COLON-ALIGNED FORMAT "x(12)"           
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FONT 4        
     apiClientXref.clientID AT ROW 4.83 COL 14 COLON-ALIGNED  FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
          BGCOLOR 15 FONT 4        
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: apiClientXref
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
         HEIGHT             = 6.81
         WIDTH              = 71.
/* END WINDOW DEFINITION */        */
                                                                        
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
{methods/template/viewer4.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW V-table-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE Size-to-Fit                                              */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.
       apiClientXref.rec_key:HIDDEN IN FRAME {&FRAME-NAME} = YES .

/* SETTINGS FOR FILL-IN apiClientXref.rec_key IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN apiClientXref.company IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN apiClientXref.scopeID IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN apiClientXref.scopeType IN FRAME F-Main
   EXP-FORMAT 1                                                          */
/* SETTINGS FOR FILL-IN apiClientXref.clientID IN FRAME F-Main
   4                                                                    */ 
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

&Scoped-define SELF-NAME apiClientXref.scopeType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL apiClientXref.scopeType V-table-Win
ON ENTRY OF apiClientXref.scopeType IN FRAME F-Main /* scopeType */
DO:
    APPLY "tab" TO SELF.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME apiClientXref.scopeID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL apiClientXref.scopeID V-table-Win
ON LEAVE OF apiClientXref.scopeID IN FRAME F-Main /* scopeType */
DO:
    DEFINE VARIABLE lCheckReturnError AS LOGICAL NO-UNDO.
    IF LASTKEY NE -1 THEN DO:
     RUN valid-scopeID(OUTPUT lCheckReturnError) NO-ERROR.
     IF lCheckReturnError THEN RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME apiClientXref.scopeID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL apiClientXref.scopeID V-table-Win
ON HELP OF apiClientXref.scopeID IN FRAME F-Main /* scopeID */
DO:
   DEFINE VARIABLE returnFields AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lookupField  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE recVal       AS RECID     NO-UNDO.
   
   CASE apiClientXref.scopeType:SCREEN-VALUE:
        WHEN "Customer" THEN DO:
            /* Customer lookup */
            RUN system/openlookup.p (
                apiClientXref.company:SCREEN-VALUE, 
                "cust-no", /* lookup field */
                0,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
        END.
        WHEN "Vendor" THEN DO:
            /* Vendor lookup */
            RUN system/openlookup.p (
                apiClientXref.company:SCREEN-VALUE, 
                "vend-no", /* lookup field */
                0,   /* Subject ID */
                "",  /* User ID */
                0,   /* Param value ID */
                OUTPUT returnFields, 
                OUTPUT lookupField, 
                OUTPUT recVal
                ). 
        END.
          
    END CASE.
   
   apiClientXref.scopeID:SCREEN-VALUE = ENTRY(1,lookupField).
  
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
  {src/adm/template/row-list.i "apiClientXref"}
  
  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "apiClientXref"}
 
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
  
  {custom/rec_key.i apiClientXref}
  apiClientXref.company = g_company .
  if lCheckCustProgram then 
   apiClientXref.scopeType = "Customer" .
  else apiClientXref.scopeType = "Vendor" .   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR thisOne AS CHAR NO-UNDO.
  DEFINE BUFFER buff-soldto FOR soldto .
  DEF BUFFER b-soldto FOR soldto. 
  {&methods/lValidateError.i YES}
  /* Code placed here will execute PRIOR to standard behavior. */    

  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
  {&methods/lValidateError.i NO}
    /* Code placed here will execute BEFORE standard behavior.    */ 
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

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
   DEFINE VARIABLE lCheckReturnError AS LOGICAL NO-UNDO.
     
    RUN valid-scopeID(OUTPUT lCheckReturnError) NO-ERROR.
    IF lCheckReturnError THEN RETURN NO-APPLY.

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
  {src/adm/template/snd-list.i "apiClientXref"}
  
  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-scopeID V-table-Win 
PROCEDURE valid-scopeID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .   
  {&methods/lValidateError.i YES}

  DO WITH FRAME {&FRAME-NAME}:
    IF apiClientXref.scopeType:SCREEN-VALUE EQ "Customer" THEN DO:
     FIND FIRST cust NO-LOCK
          WHERE cust.cust-no EQ apiClientXref.scopeID:SCREEN-VALUE NO-ERROR .
      IF NOT AVAIL cust THEN
      DO:
           MESSAGE "Please enter a vaild customer " VIEW-AS ALERT-BOX INFO .
           opReturnError = YES .           
      END.
    
    END.
    ELSE IF apiClientXref.scopeType:SCREEN-VALUE EQ "ShipTo" THEN DO:
      FIND FIRST shipto NO-LOCK
          WHERE shipto.ship-id EQ apiClientXref.scopeID:SCREEN-VALUE NO-ERROR .
      IF NOT AVAIL shipto THEN
      DO:
           MESSAGE "Please enter a vaild Shipto " VIEW-AS ALERT-BOX INFO .
           opReturnError = YES .           
      END.
    
    END.
    ELSE IF apiClientXref.scopeType:SCREEN-VALUE EQ "Vendor" THEN DO:
      FIND FIRST vend NO-LOCK
          WHERE vend.vend-no EQ apiClientXref.scopeID:SCREEN-VALUE NO-ERROR .
      IF NOT AVAIL vend THEN
      DO:
           MESSAGE "Please enter a vaild Vendor " VIEW-AS ALERT-BOX INFO .
           opReturnError = YES .           
      END.
    
    END.     
  
  END.
  {&methods/lValidateError.i NO}
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

