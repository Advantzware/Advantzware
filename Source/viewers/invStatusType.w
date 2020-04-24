&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/invStatusType.w

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
DEF VAR iSecurityLevel AS INT NO-UNDO.
DEF VAR iBaseLevel AS INT NO-UNDO.

{custom/globdefs.i} 
{custom/gcompany.i}

{methods/defines/hndlset.i}  

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
&Scoped-define EXTERNAL-TABLES inventoryStatusType
&Scoped-define FIRST-EXTERNAL-TABLE inventoryStatusType


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inventoryStatusType.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS inventoryStatusType.statusID ~
inventoryStatusType.inActive inventoryStatusType.description ~
inventoryStatusType.onHold 
&Scoped-define ENABLED-TABLES inventoryStatusType
&Scoped-define FIRST-ENABLED-TABLE inventoryStatusType
&Scoped-Define ENABLED-OBJECTS RECT-1 
&Scoped-Define DISPLAYED-FIELDS inventoryStatusType.statusID ~
inventoryStatusType.inActive inventoryStatusType.description ~
inventoryStatusType.onHold inventoryStatusType.createDate ~
inventoryStatusType.updateDate  ~
inventoryStatusType.createUser ~
inventoryStatusType.updateUser 
&Scoped-define DISPLAYED-TABLES inventoryStatusType
&Scoped-define FIRST-DISPLAYED-TABLE inventoryStatusType
&Scoped-Define DISPLAYED-OBJECTS cUpdateTime cCreateTime RECT-1

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 100 BY 9.05.
     
DEFINE VARIABLE cCreateTime AS CHARACTER FORMAT "x(11)" 
     LABEL "Create Time" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 
     BGCOLOR 15.  
     
DEFINE VARIABLE cUpdateTime AS CHARACTER FORMAT "x(11)" 
     LABEL "Update Time" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 
     BGCOLOR 15.      


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     inventoryStatusType.statusID AT ROW 1.38 COL 25 COLON-ALIGNED FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     inventoryStatusType.inActive AT ROW 1.48 COL 65
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1
          BGCOLOR 15 
     inventoryStatusType.description AT ROW 2.81 COL 25 COLON-ALIGNED
          LABEL "Description" FORMAT "x(32)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
          BGCOLOR 15 
     inventoryStatusType.onHold AT ROW 2.91 COL 65
          VIEW-AS TOGGLE-BOX
          SIZE 20 BY 1
          BGCOLOR 15 
     inventoryStatusType.createDate AT ROW 4.48 COL 25 COLON-ALIGNED
          LABEL "Create Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     inventoryStatusType.updateDate AT ROW 4.48 COL 63 COLON-ALIGNED
          LABEL "Update Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     
     cCreateTime  AT ROW 5.76 COL 25 COLON-ALIGNED     
     
     cUpdateTime  AT ROW 5.76 COL 63 COLON-ALIGNED       
     inventoryStatusType.createUser AT ROW 7.1 COL 25 COLON-ALIGNED
          LABEL "Create User" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     inventoryStatusType.updateUser AT ROW 7.1 COL 63 COLON-ALIGNED
          LABEL "Update User" FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.inventoryStatusType
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
         WIDTH              = 100.2.
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

/* SETTINGS FOR FILL-IN inventoryStatusType.createDate IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */ 
/* SETTINGS FOR FILL-IN inventoryStatusType.createUser IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN inventoryStatusType.description IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN inventoryStatusType.statusID IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN inventoryStatusType.updateDate IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */ 
/* SETTINGS FOR FILL-IN inventoryStatusType.updateUser IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN cCreateTime IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                      */  
/* SETTINGS FOR FILL-IN cUpdateTime IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT                                      */    
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


&Scoped-define SELF-NAME inventoryStatusType.statusID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inventoryStatusType.statusID V-table-Win
ON LEAVE OF inventoryStatusType.statusID IN FRAME F-Main /* Customer */
DO:
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
   
   IF LASTKEY <> -1 AND inventoryStatusType.statusID:SCREEN-VALUE <> "" THEN DO:
      RUN valid-statusId ( OUTPUT lCheckError) NO-ERROR.
      IF lCheckError THEN RETURN NO-APPLY.
   END.
 
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
  {src/adm/template/row-list.i "inventoryStatusType"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inventoryStatusType"}

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
      ASSIGN 
          inventoryStatusType.updateDate =  TODAY
          inventoryStatusType.updateTime =  TIME
          inventoryStatusType.updateUser =  USERID(LDBNAME(1)) 
          inventoryStatusType.updateDate:SCREEN-VALUE =  string(TODAY)            
          inventoryStatusType.updateUser:SCREEN-VALUE =  USERID(LDBNAME(1)).
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
    
    ASSIGN
        lAddRecord = TRUE.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'copy-record':U ) .

   
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
    
    {custom/rec_key.i inventoryStatusType}
    ASSIGN 
      inventoryStatusType.createDate =  TODAY
      inventoryStatusType.createTime =  TIME
      inventoryStatusType.createUser =  USERID(LDBNAME(1)) .
      
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN 
      inventoryStatusType.createDate:SCREEN-VALUE =  string(TODAY)        
      inventoryStatusType.createUser:SCREEN-VALUE =  string(USERID(LDBNAME(1))) .
      ASSIGN
       cCreateTime:SCREEN-VALUE =  STRING(inventoryStatusType.createTime,'HH:MM:SS am')
       cUpdateTime:SCREEN-VALUE =  STRING(inventoryStatusType.updatetime,'HH:MM:SS am').
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAIL inventoryStatusType THEN
    ASSIGN
       cCreateTime =  STRING(inventoryStatusType.createTime,'HH:MM:SS am')
       cUpdateTime =  STRING(inventoryStatusType.updatetime,'HH:MM:SS am').
  
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
  
  DO WITH FRAME {&FRAME-NAME}:
      IF NOT adm-new-record THEN
      DISABLE inventoryStatusType.statusID .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
        
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
   
    
   RUN valid-statusId ( OUTPUT lCheckError) NO-ERROR.
   IF lCheckError THEN RETURN NO-APPLY.   
    
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .
    
    ASSIGN
        lAddRecord = FALSE.
        adm-new-record = NO .
    DO WITH FRAME {&FRAME-NAME}:    
    ASSIGN
       cCreateTime:SCREEN-VALUE =  STRING(inventoryStatusType.createTime,'HH:MM:SS am')
       cUpdateTime:SCREEN-VALUE =  STRING(inventoryStatusType.updatetime,'HH:MM:SS am'). 
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
  {src/adm/template/snd-list.i "inventoryStatusType"}

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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-statusId V-table-Win 
PROCEDURE valid-statusId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO .
    DEFINE BUFFER bf-inventoryStatusType FOR inventoryStatusType .
    IF adm-new-record THEN do:
        IF inventoryStatusType.statusID:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "" THEN DO:
          MESSAGE "Please enter Status ID.." VIEW-AS ALERT-BOX INFO.
          APPLY "entry" TO inventoryStatusType.statusID.
          oplReturnError = YES .
        END.
        FIND FIRST bf-inventoryStatusType NO-LOCK 
          WHERE bf-inventoryStatusType.statusID EQ inventoryStatusType.statusID:SCREEN-VALUE IN FRAME {&FRAME-NAME}
            AND ROWID(bf-inventoryStatusType) NE ROWID(inventoryStatusType)        
            NO-ERROR .

        IF AVAILABLE bf-inventoryStatusType THEN DO:
          MESSAGE "This record is a duplicate of a previous entry; please adjust." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO inventoryStatusType.statusID.
          oplReturnError = YES .
        END.
    END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
