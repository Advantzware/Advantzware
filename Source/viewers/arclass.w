&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/arclass.w

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
DEF VAR lAddRecord AS LOG NO-UNDO.
DEF VAR iBaseLevel AS INT NO-UNDO.

{custom/gcompany.i}
{custom/globdefs.i}
{system/fSuperRunning.i}

/* The below variables are used in run_link.i */
DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
DEFINE VARIABLE pHandle  AS HANDLE    NO-UNDO.


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
&Scoped-define EXTERNAL-TABLES arclass
&Scoped-define FIRST-EXTERNAL-TABLE arclass


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR arclass.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS arClass.classID arClass.inActive ~
arClass.description arClass.receivablesAcct arClass.currencyCode 
&Scoped-define ENABLED-TABLES arClass
&Scoped-define FIRST-ENABLED-TABLE arClass
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS arClass.classID arClass.inActive ~
arClass.description arClass.receivablesAcct arClass.currencyCode ~
arClass.createTime arClass.createUser arClass.updateTime arClass.updateUser ~
arClass.createDate arClass.updateDate 
&Scoped-define DISPLAYED-TABLES arClass
&Scoped-define FIRST-DISPLAYED-TABLE arClass
&Scoped-Define DISPLAYED-OBJECTS cCurrDscr cCreateTime cUpdateTime 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */

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
DEFINE VARIABLE cCreateTime AS CHARACTER FORMAT "x(11)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE cCurrDscr AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 47 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE cUpdateTime AS CHARACTER FORMAT "x(11)" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1
     BGCOLOR 15 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 106 BY 9.05.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     arClass.classID AT ROW 1.24 COL 27.4 COLON-ALIGNED FORMAT ">>"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
          BGCOLOR 15 
     arClass.inActive AT ROW 1.38 COL 75
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY 1
          BGCOLOR 15 
     arClass.description AT ROW 2.43 COL 27.4 COLON-ALIGNED
          LABEL "Class Description" FORMAT "x(60)"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
          BGCOLOR 15 
     arClass.receivablesAcct AT ROW 3.62 COL 27.4 COLON-ALIGNED HELP
          ""
          LABEL "Receivables Account" FORMAT "x(25)"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
          BGCOLOR 15 
     arClass.currencyCode AT ROW 4.81 COL 27.4 COLON-ALIGNED
          LABEL "Currency Code" FORMAT "x(3)"
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
          BGCOLOR 15 
     cCurrDscr AT ROW 4.86 COL 38 COLON-ALIGNED NO-LABEL
     
     arClass.createTime AT ROW 6.67 COL 23 COLON-ALIGNED NO-LABEL FORMAT "99999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15
     cCreateTime AT ROW 6.67 COL 23 COLON-ALIGNED NO-LABEL     
     arClass.createUser AT ROW 6.67 COL 40 COLON-ALIGNED
          LABEL "By" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 9.5 BY 1
          BGCOLOR 15     
     arClass.updateTime AT ROW 6.67 COL 76.5 COLON-ALIGNED NO-LABEL FORMAT "99999"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15
     cUpdateTime AT ROW 6.67 COL 76.5 COLON-ALIGNED NO-LABEL        
     arClass.updateUser AT ROW 6.67 COL 93.5 COLON-ALIGNED
          LABEL "By" FORMAT "x(12)"
          VIEW-AS FILL-IN 
          SIZE 9.5 BY 1
          BGCOLOR 15 
     arClass.createDate AT ROW 6.71 COL 10.6 COLON-ALIGNED
          LABEL "Created" FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     arClass.updateDate AT ROW 6.71 COL 64 COLON-ALIGNED
          LABEL "Updated" FORMAT "99/99/99"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.arclass
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
         HEIGHT             = 9.14
         WIDTH              = 106.2.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}
/*{methods/template/viewer.i}*/

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

/* SETTINGS FOR FILL-IN cCreateTime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cCurrDscr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN arClass.classID IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN arClass.createDate IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN arClass.createTime IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN arClass.createUser IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN cUpdateTime IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN arClass.currencyCode IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN arClass.description IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN arClass.receivablesAcct IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN arClass.updateDate IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN arClass.updateTime IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN arClass.updateUser IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
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
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
  DEFINE VARIABLE riLookup AS RECID NO-UNDO.
  DEFINE VARIABLE lv-handle AS HANDLE NO-UNDO.
  DEFINE VARIABLE cFieldsValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cFoundValue AS CHARACTER NO-UNDO.
  DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.
  DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

  {&methods/lValidateError.i YES}
  CASE FOCUS:NAME :
    WHEN "receivablesAcct" THEN DO:        
       RUN system/openLookup.p (
       INPUT  g_company, 
       INPUT  "", /* Lookup ID */
       INPUT  107,  /* Subject ID */
       INPUT  "", /* User ID */
       INPUT  0,  /* Param Value ID */
       OUTPUT cFieldsValue, 
       OUTPUT cFoundValue, 
       OUTPUT recRecordID ).  
       
       IF cFoundValue NE "" THEN do:    
         arclass.receivablesAcct:SCREEN-VALUE IN FRAME {&frame-name} = cFoundValue.
         APPLY 'entry' TO arclass.receivablesAcct.
       END.
    END.      
    WHEN "currencyCode" THEN DO:        
       RUN system/openLookup.p (
       INPUT  g_company, 
       INPUT  "", /* Lookup ID */
       INPUT  118,  /* Subject ID */
       INPUT  "", /* User ID */
       INPUT  0,  /* Param Value ID */
       OUTPUT cFieldsValue, 
       OUTPUT cFoundValue, 
       OUTPUT recRecordID ).  
       
       IF cFoundValue NE "" THEN do: 
         arclass.currencyCode:SCREEN-VALUE IN FRAME {&frame-name} = cFoundValue.
         APPLY "value-changed" TO arclass.currencyCode .
         APPLY 'entry' TO arclass.currencyCode.
       END.
    END.    
  END CASE.  
  {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arClass.classID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arClass.classID V-table-Win
ON LEAVE OF arClass.classID IN FRAME F-Main /* AR Class ID */
DO:
    DEFINE VARIABLE lCheckReturnError AS LOGICAL NO-UNDO.
    IF LASTKEY NE -1 THEN DO:
      RUN valid-classID(OUTPUT lCheckReturnError) NO-ERROR.
      IF lCheckReturnError THEN RETURN NO-APPLY.
    END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arClass.receivablesAcct
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arClass.receivablesAcct V-table-Win
ON LEAVE OF arClass.receivablesAcct IN FRAME F-Main /* Receivables Account */
DO:
    DEFINE VARIABLE lCheckReturnError AS LOGICAL NO-UNDO.
    IF LASTKEY NE -1 THEN DO:
      RUN valid-glAccount(OUTPUT lCheckReturnError) NO-ERROR.
      IF lCheckReturnError THEN DO:
          APPLY "ENTRY" TO arClass.receivablesAcct .
          RETURN NO-APPLY.
      END.
    END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&Scoped-define SELF-NAME arClass.currencyCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arClass.currencyCode V-table-Win
ON LEAVE OF arClass.currencyCode IN FRAME F-Main /* currencyCode */
DO:
    DEFINE VARIABLE lCheckReturnError AS LOGICAL NO-UNDO.
    IF LASTKEY NE -1 THEN DO:
      RUN valid-currency(OUTPUT lCheckReturnError) NO-ERROR.
      IF lCheckReturnError THEN RETURN NO-APPLY.
    END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arClass.currencyCode V-table-Win
ON VALUE-CHANGED OF arClass.currencyCode IN FRAME F-Main /* currencyCode */
DO:
   FIND FIRST currency  NO-LOCK
          WHERE currency.company = g_company
          AND currency.c-code = arclass.currencyCode:SCREEN-VALUE
          NO-ERROR. 
      IF avail currency THEN
       cCurrDscr:SCREEN-VALUE =  currency.c-desc. 
     
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
  {src/adm/template/row-list.i "arclass"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "arclass"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/             
    ASSIGN
        lAddRecord = TRUE.
           
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

    DO WITH FRAME {&FRAME-NAME}:
        /*ASSIGN
            arclass.securityLevel:SCREEN-VALUE in frame {&frame-name} = "900". */
        APPLY 'entry' TO arclass.classID.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .
  DO WITH FRAME {&FRAME-NAME}:
   arclass.updateDate = TODAY.
   arclass.updateTime = TIME.
   arclass.updateUser = USERID(LDBNAME(1)).
   arclass.updateDate:SCREEN-VALUE = string(TODAY).
   arclass.updateTime:SCREEN-VALUE = string(TIME).
   arclass.updateUser:SCREEN-VALUE = USERID(LDBNAME(1)).
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
    
    IF arclass.receivablesAcct:BGCOLOR IN FRAME {&FRAME-NAME} EQ 16 THEN             
        ASSIGN 
            arclass.receivablesAcct:BGCOLOR IN FRAME {&FRAME-NAME} = ?
            arclass.receivablesAcct:FGCOLOR IN FRAME {&FRAME-NAME} = ?
            .  

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ).

    ASSIGN
        lAddRecord = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-copy-record V-table-Win 
PROCEDURE local-copy-record :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   /* IF iSecurityLevel LT 1000 THEN DO:
        MESSAGE 
            "You do not have sufficient permissions to add a utility." SKIP 
            "Please contact Advantzware Support for assistance."
            VIEW-AS ALERT-BOX INFO.
        RETURN.
    END. */

    ASSIGN
        lAddRecord = TRUE.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

    DO WITH FRAME {&FRAME-NAME}:
        APPLY 'entry' TO arclass.classID.
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

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
    
    {custom/rec_key.i arclass}
     arclass.createDate = TODAY.
     arclass.createTime = TIME.
     arclass.createUser = USERID(LDBNAME(1)).
    DO WITH FRAME {&FRAME-NAME}:
      arclass.createDate:SCREEN-VALUE = string(TODAY).
      arclass.createTime:SCREEN-VALUE = string(TIME).
      arclass.createUser:SCREEN-VALUE = USERID(LDBNAME(1)).
      DISPLAY  arclass.createDate arclass.createTime arclass.createUser .
      
       cCreateTime:SCREEN-VALUE =  STRING(arclass.createTime,'HH:MM am') .
       
    END.
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
    DEFINE BUFFER buff-cust FOR cust .
    
    IF NOT adm-new-record THEN DO:
        {custom/askdel.i}
    END.
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win
PROCEDURE local-disable-fields:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateEnd"}
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAIL arclass THEN do:
     ASSIGN
       cCreateTime =  STRING(arclass.createTime,'HH:MM am')
       cUpdateTime =  STRING(arclass.updatetime,'HH:MM am').
       
     FIND FIRST currency  NO-LOCK
          WHERE currency.company = g_company
          AND currency.c-code = arclass.currencyCode
          NO-ERROR. 
      IF avail currency THEN
       cCurrDscr =  currency.c-desc.       
    END.
       
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ).
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

    IF NOT lAddRecord THEN ASSIGN 
        arclass.classID:SENSITIVE IN FRAME f-main = FALSE.
        
  {methods/run_link.i "CONTAINER-SOURCE" "SetUpdateBegin"}       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-reset-record V-table-Win
PROCEDURE local-reset-record:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

    /* Code placed here will execute PRIOR to standard behavior. */
    IF arclass.receivablesAcct:BGCOLOR IN FRAME {&FRAME-NAME} EQ 16 THEN             
        ASSIGN 
            arclass.receivablesAcct:BGCOLOR IN FRAME {&FRAME-NAME} = ?
            arclass.receivablesAcct:FGCOLOR IN FRAME {&FRAME-NAME} = ?
            .  
    /* Dispatch standard ADM method.                             */
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'reset-record':U ) .        

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lCheckReturnError AS LOGICAL NO-UNDO.    
    
    RUN valid-classID(OUTPUT lCheckReturnError) NO-ERROR.
    IF lCheckReturnError THEN RETURN NO-APPLY.
    
    RUN valid-glAccount(OUTPUT lCheckReturnError) NO-ERROR.
    IF lCheckReturnError THEN DO:
        APPLY "ENTRY" TO arClass.receivablesAcct IN FRAME {&FRAME-NAME} .
        RETURN NO-APPLY.
    END.     
    
     RUN valid-currency(OUTPUT lCheckReturnError) NO-ERROR.
      IF lCheckReturnError THEN RETURN NO-APPLY.
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
    
    ASSIGN
        lAddRecord = FALSE.
    DO WITH FRAME {&FRAME-NAME}:    
    ASSIGN
       cCreateTime:SCREEN-VALUE =  STRING(arclass.createTime,'HH:MM am')
       cUpdateTime:SCREEN-VALUE =  STRING(arclass.updatetime,'HH:MM am'). 
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
  {src/adm/template/snd-list.i "arclass"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-classID V-table-Win 
PROCEDURE valid-classID :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO . 
  DEFINE BUFFER bf-arclass FOR arclass .
  {&methods/lValidateError.i YES}

  DO WITH FRAME {&FRAME-NAME}:
    IF arclass.classID:SCREEN-VALUE EQ "" OR arclass.classID:SCREEN-VALUE EQ "0" THEN DO:
        MESSAGE "Enter valid Ar Class id = Must enter a unique number from 1-99" VIEW-AS ALERT-BOX INFO .
        opReturnError = YES .                 
    END.
    ELSE DO:
      FIND FIRST bf-arclass NO-LOCK
          WHERE bf-arclass.classID EQ integer(arclass.classID:SCREEN-VALUE)
          AND rowid(bf-arclass) NE rowid(arclass) NO-ERROR .
      IF AVAIL bf-arclass THEN
      DO:
           MESSAGE "AR Class ID is already exist... " VIEW-AS ALERT-BOX INFO .
           opReturnError = YES .           
      END.     
    END.    
    
  END.
  {&methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-glAccount V-table-Win 
PROCEDURE valid-glAccount :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO . 
  DEFINE VARIABLE oplValid AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
  DEFINE BUFFER bf-arclass FOR arclass.
  DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
  RUN util/Validate.p PERSISTENT SET hdValidator.
  {&methods/lValidateError.i YES}

  DO WITH FRAME {&FRAME-NAME}:
    IF arclass.receivablesAcct:SCREEN-VALUE NE "" THEN DO:
        RUN pIsValidGLAccount IN hdValidator  (arclass.receivablesAcct:SCREEN-VALUE, NO, g_company, OUTPUT oplValid, OUTPUT cValidNote).        
        IF NOT oplValid THEN do:
            IF arclass.receivablesAcct:BGCOLOR EQ 16 THEN             
                ASSIGN 
                    arclass.receivablesAcct:BGCOLOR = ?
                    arclass.receivablesAcct:FGCOLOR = ?
                   .
            IF INDEX(cValidNote, "Inactive") GT 0 THEN 
                ASSIGN 
                    arclass.receivablesAcct:BGCOLOR = 16
                    arclass.receivablesAcct:FGCOLOR = 15
                    . 
            MESSAGE cValidNote VIEW-AS ALERT-BOX INFO .
            opReturnError = YES .   
        END.               
    END.   
    IF arclass.receivablesAcct:SCREEN-VALUE EQ "" OR oplValid THEN 
        IF arclass.receivablesAcct:BGCOLOR EQ 16 THEN             
            ASSIGN 
                arclass.receivablesAcct:BGCOLOR = ?
                arclass.receivablesAcct:FGCOLOR = ?
                .       
  END.
  {&methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-currency V-table-Win 
PROCEDURE valid-currency :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opReturnError AS LOGICAL NO-UNDO .
  {methods/lValidateError.i YES}
 FIND FIRST currency WHERE currency.company = g_company
                       AND currency.c-code = arclass.currencyCode:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                       NO-LOCK NO-ERROR.
 IF NOT AVAIL currency THEN DO:
    MESSAGE "Please enter a vaild Currency Code. Try help" VIEW-AS ALERT-BOX ERROR.
    APPLY "entry" TO arclass.currencyCode.
    opReturnError = TRUE.
 END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

