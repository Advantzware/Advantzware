&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/sys-ctrl.w

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

&SCOPED-DEFINE tableName sys-ctrl
&SCOPED-DEFINE nameField {&tableName}.name:SCREEN-VALUE
&SCOPED-DEFINE post-enable post-enable

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
/* {methods/defines/globdefs.i &NEW="NEW GLOBAL"} */
/* {methods/defines/hndldefs.i &NEW="NEW"}        */
{custom/globdefs.i}

{custom/gcompany.i}

{sys/ref/sys-ctrl.i}

DEF VAR v-secure AS LOG INIT NO NO-UNDO.
DEF VAR v-secur  AS LOG INIT NO NO-UNDO.
DEF VAR v-valid  AS LOG INIT NO NO-UNDO.
DEF VAR v-msg    AS CHAR FORMAT  "x(100)" NO-UNDO.
DEF VAR gvhToggleHandles AS HANDLE EXTENT 10.
DEFINE VAR cNK1Code AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNK1ValidValues AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cNK1CurrentValues AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS Record-Source,Record-Target,TableIO-Target

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 tgValue1 tgValue2 tgValue3 tgValue4 ~
tgValue5 tgValue6 tgValue7 tgValue8 tgValue9 tgValue10 btn_go 
&Scoped-Define DISPLAYED-OBJECTS tgValue1 tgValue2 tgValue3 tgValue4 ~
tgValue5 tgValue6 tgValue7 tgValue8 tgValue9 tgValue10 

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
DEFINE BUTTON btn_go 
     LABEL "&Ok" 
     SIZE 25 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 87 BY 12.62.

DEFINE VARIABLE tgValue1 AS LOGICAL INITIAL no 
     LABEL "Toggle 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 84 BY .81 NO-UNDO.

DEFINE VARIABLE tgValue10 AS LOGICAL INITIAL no 
     LABEL "Toggle 10" 
     VIEW-AS TOGGLE-BOX
     SIZE 84 BY .81 NO-UNDO.

DEFINE VARIABLE tgValue2 AS LOGICAL INITIAL no 
     LABEL "Toggle 2" 
     VIEW-AS TOGGLE-BOX
     SIZE 84 BY .81 NO-UNDO.

DEFINE VARIABLE tgValue3 AS LOGICAL INITIAL no 
     LABEL "Toggle 3" 
     VIEW-AS TOGGLE-BOX
     SIZE 84 BY .81 NO-UNDO.

DEFINE VARIABLE tgValue4 AS LOGICAL INITIAL no 
     LABEL "Toggle 4" 
     VIEW-AS TOGGLE-BOX
     SIZE 84 BY .81 NO-UNDO.

DEFINE VARIABLE tgValue5 AS LOGICAL INITIAL no 
     LABEL "Toggle 5" 
     VIEW-AS TOGGLE-BOX
     SIZE 84 BY .81 NO-UNDO.

DEFINE VARIABLE tgValue6 AS LOGICAL INITIAL no 
     LABEL "Toggle 6" 
     VIEW-AS TOGGLE-BOX
     SIZE 84 BY .81 NO-UNDO.

DEFINE VARIABLE tgValue7 AS LOGICAL INITIAL no 
     LABEL "Toggle 7" 
     VIEW-AS TOGGLE-BOX
     SIZE 84 BY .81 NO-UNDO.

DEFINE VARIABLE tgValue8 AS LOGICAL INITIAL no 
     LABEL "Toggle 8" 
     VIEW-AS TOGGLE-BOX
     SIZE 84 BY .81 NO-UNDO.

DEFINE VARIABLE tgValue9 AS LOGICAL INITIAL no 
     LABEL "Toggle 9" 
     VIEW-AS TOGGLE-BOX
     SIZE 84 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tgValue1 AT ROW 1.33 COL 2 WIDGET-ID 22
     tgValue2 AT ROW 2.48 COL 2 WIDGET-ID 4
     tgValue3 AT ROW 3.62 COL 2 WIDGET-ID 6
     tgValue4 AT ROW 4.76 COL 2 WIDGET-ID 8
     tgValue5 AT ROW 5.91 COL 2 WIDGET-ID 10
     tgValue6 AT ROW 7.05 COL 2 WIDGET-ID 12
     tgValue7 AT ROW 8.19 COL 2 WIDGET-ID 14
     tgValue8 AT ROW 9.33 COL 2 WIDGET-ID 16
     tgValue9 AT ROW 10.48 COL 2 WIDGET-ID 18
     tgValue10 AT ROW 11.57 COL 2 WIDGET-ID 20
     btn_go AT ROW 14.91 COL 32 WIDGET-ID 24
     RECT-1 AT ROW 1.1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
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
         HEIGHT             = 16.14
         WIDTH              = 87.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

 {src/adm/method/viewer.i} 
/* {methods/template/viewer.i} */

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

&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go V-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Ok */
DO:
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR h AS HANDLE NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEFINE VARIABLE cValueList AS CHARACTER   NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN {&DISPLAYED-OBJECTS}.
  END.

  run get-link-handle in adm-broker-hdl (this-procedure, "container-source", output char-hdl).  

  DO i = 1 TO 10:
    h = gvhToggleHandles[i].
    IF h:SCREEN-VALUE = "YES" THEN
      cValueList = cValueList + "," + h:LABEL.    
  END.   
  cValueList = TRIM(cValueList, ",").

  RUN setOutputList IN widget-handle(char-hdl) (INPUT cValueList). 
  RUN local-exit IN widget-handle(char-hdl).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
/* {custom/getcmpny.i} */


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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE assign-initial-values V-table-Win 
PROCEDURE assign-initial-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR i AS INT NO-UNDO.
DEF VAR h AS HANDLE NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:

  gvhToggleHandles[1] = tgValue1:HANDLE.
  gvhToggleHandles[2] = tgValue2:HANDLE.
  gvhToggleHandles[3] = tgValue3:HANDLE.
  gvhToggleHandles[4] = tgValue4:HANDLE.
  gvhToggleHandles[5] = tgValue5:HANDLE.
  gvhToggleHandles[6] = tgValue6:HANDLE.
  gvhToggleHandles[7] = tgValue7:HANDLE.
  gvhToggleHandles[8] = tgValue8:HANDLE.
  gvhToggleHandles[9] = tgValue9:HANDLE.
  gvhToggleHandles[10] = tgValue10:HANDLE.



  cNk1CurrentValues = TRIM(cNK1CurrentValues).
  cNk1CurrentValues = TRIM(cNK1CurrentValues,",").
  DO i = 1 TO 10:
    h = gvhToggleHandles[i].
    IF i LE NUM-ENTRIES(cNK1CurrentValues) THEN
      ASSIGN h:LABEL = ENTRY(i, cNK1CurrentValues).
    IF CAN-DO(cNK1ValidValues, h:LABEL) THEN
       h:SCREEN-VALUE = "YES".
    IF i GT NUM-ENTRIES(cNK1CurrentValues) THEN 
      h:HIDDEN = TRUE.
  END.   

END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-flg V-table-Win 
PROCEDURE check-flg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-flg AS LOG INIT YES NO-UNDO.

/*   v-valid = YES.                                                              */
/*                                                                               */
/*   DO WITH FRAME {&frame-name}:                                                */
/*     IF NOT v-secure                                                       AND */
/*        NOT adm-new-record                                                 AND */
/*        STRING(sys-ctrl.log-fld,"yes/no") NE sys-ctrl.log-fld:SCREEN-VALUE     */
/*       THEN DO:                                                                */
/*                                                                               */
/*       IF sys-ctrl.name EQ "RELCREDT" AND                                      */
/*          sys-ctrl.log-fld = YES THEN                                          */
/*          ASSIGN v-secure = YES.                                               */
/*       ELSE                                                                    */
/*          RUN sys/ref/d-ASIpwd.w (OUTPUT v-secure).                            */
/*                                                                               */
/*       IF NOT v-secure THEN                                                    */
/*         ASSIGN                                                                */
/*          v-valid                   = NO                                       */
/*          sys-ctrl.log-fld:SCREEN-VALUE = STRING(sys-ctrl.log-fld,"yes/no").   */
/*                                                                               */
/*     END.                                                                      */
/*                                                                               */
/* /*     IF NOT v-valid THEN APPLY "entry" TO sys-ctrl.log-fld. */              */
/*                                                                               */
/*   END.                                                                        */
/*                                                                               */
/*   ASSIGN                                                                      */
/*       v-secure = NO                                                           */
/*       v-secur  = NO.                                                          */


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
  DEF VAR li AS INT NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

/*   /* Code placed here will execute AFTER standard behavior.    */                       */
/*   IF sys-ctrl.name EQ "FGRECPT" THEN DO:                                                */
/*     li = LOOKUP(sys-ctrl.char-fld,str-init[66]).                                        */
/*                                                                                         */
/*     RUN update-other-record ("AUTOPOST", li EQ 1).                                      */
/*     RUN update-other-record ("TSPOSTFG", li EQ 2 OR li EQ 5). /* TSPOSTFG or TSPARTS */ */
/*   END.                                                                                  */
/*                                                                                         */
/*   IF sys-ctrl.name EQ "OEFGUPDT" THEN SUBSTRING(sys-ctrl.char-fld,8,1) = "N".           */
/*                                                                                         */
/*     IF (sys-ctrl.name EQ "RELCREDT" OR                                                  */
/*       /*sys-ctrl.name EQ "SalesMgmt" OR */                                              */
/*       sys-ctrl.name EQ "SalesBudget") THEN DO:                                          */
/*                                                                                         */
/*        RUN check-flg.                                                                   */
/*        IF NOT v-valid THEN DO:                                                          */
/*            MESSAGE                                                                      */
/*               "Please call ASI, to purchase this functionality."                        */
/*                VIEW-AS ALERT-BOX INFO BUTTONS OK.                                       */
/*            RETURN NO-APPLY.                                                             */
/*        END.                                                                             */
/*    END.                                                                                 */
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

/*   /* Code placed here will execute AFTER standard behavior.    */                                          */
/*   sys-ctrl.company = gcompany.                                                                             */
/*   /* same as {methods/triggers/create.i}   */                                                              */
/*                                                                                                            */
/*   assign sys-ctrl.rec_key = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999"). */
/*                                                                                                            */
/*   CREATE rec_key.                                                                                          */
/*   ASSIGN rec_key.rec_key = sys-ctrl.rec_key                                                                */
/*          rec_key.table_name = "sys-ctrl".                                                                  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize V-table-Win 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR char-hdl AS CHAR NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */


  IF cNK1Code EQ "" THEN DO:   
     run get-link-handle in adm-broker-hdl (this-procedure, "container-source", output char-hdl).  
     IF VALID-HANDLE(widget-handle(char-hdl)) THEN
     RUN setInitValues IN widget-handle(char-hdl) (OUTPUT cNK1Code, OUTPUT cNK1CurrentValues, OUTPUT cNK1ValidValues).     
  END.

  IF cNK1Code NE "" THEN 
      RUN assign-initial-Values.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-prev-char-fld LIKE sys-ctrl.char-fld NO-UNDO.


/*   /* Code placed here will execute PRIOR to standard behavior. */                                     */
/*   RUN valid-char-fld NO-ERROR.                                                                        */
/*   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.                                                         */
/*                                                                                                       */
/*   RUN valid-log-fld NO-ERROR.                                                                         */
/*   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.                                                         */
/*                                                                                                       */
/*   lv-prev-char-fld = sys-ctrl.char-fld.                                                               */
/*   IF sys-ctrl.name EQ "RELCREDT" AND sys-ctrl.log-fld:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "NO" THEN */
/*      ASSIGN sys-ctrl.char-fld:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".                               */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

/*   /* Code placed here will execute AFTER standard behavior.    */                         */
/*   IF sys-ctrl.name EQ "APTAX"              AND                                            */
/*      sys-ctrl.char-fld EQ "NoTax"          AND                                            */
/*      sys-ctrl.char-fld NE lv-prev-char-fld THEN                                           */
/*   FOR EACH po-ord NO-LOCK                                                                 */
/*       WHERE po-ord.company EQ sys-ctrl.company                                            */
/*         AND po-ord.opened  EQ YES,                                                        */
/*       EACH po-ordl                                                                        */
/*       WHERE po-ordl.company EQ po-ord.company                                             */
/*         AND po-ordl.po-no   EQ po-ord.po-no                                               */
/*       TRANSACTION:                                                                        */
/*     po-ordl.tax = NO.                                                                     */
/*   END.                                                                                    */
/*                                                                                           */
/*   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl). */
/*                                                                                           */
/*   IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN                                           */
/*     RUN repo-query IN WIDGET-HANDLE(char-hdl) (ROWID(sys-ctrl)).                          */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE post-enable V-table-Win 
PROCEDURE post-enable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   IF sys-ctrl.NAME:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Autopost" OR */
/*      sys-ctrl.NAME:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "TSPOSTFG"    */
/*   THEN DO:                                                             */
/*       DISABLE sys-ctrl.log-fld WITH FRAME {&FRAME-NAME}.               */
/*                                                                        */
/*   END.                                                                 */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ScanTagOnly-Logic V-table-Win 
PROCEDURE ScanTagOnly-Logic :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*   DO WITH FRAME {&FRAME-NAME}:                                            */
/*                                                                           */
/*       IF sys-ctrl.char-fld:SCREEN-VALUE = "ScanTagOnly" THEN              */
/*       ASSIGN sys-ctrl.log-fld:SCREEN-VALUE = "NO"                         */
/*              sys-ctrl.log-fld:SENSITIVE = NO.                             */
/*       ELSE                                                                */
/*           ASSIGN sys-ctrl.log-fld:SCREEN-VALUE = string(sys-ctrl.log-fld) */
/*                  sys-ctrl.log-fld:SENSITIVE = YES.                        */
/*                                                                           */
/*   END.                                                                    */


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

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartViewer, and there are no
     tables specified in any contained Browse, Query, or Frame. */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-other-record V-table-Win 
PROCEDURE update-other-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEF INPUT PARAM ip-name    LIKE sys-ctrl.name NO-UNDO.    */
/*   DEF INPUT PARAM ip-log-fld LIKE sys-ctrl.log-fld NO-UNDO. */
/*                                                             */
/*   DEF BUFFER b-sys-ctrl FOR sys-ctrl.                       */
/*                                                             */
/*                                                             */
/*   FIND FIRST b-sys-ctrl                                     */
/*       WHERE b-sys-ctrl.company EQ sys-ctrl.company          */
/*         AND b-sys-ctrl.name    EQ ip-name                   */
/*       NO-ERROR.                                             */
/*   IF AVAIL b-sys-ctrl THEN b-sys-ctrl.log-fld = ip-log-fld. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-char-fld V-table-Win 
PROCEDURE valid-char-fld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


  {methods/lValidateError.i YES}
/*   DEF VAR thisOne AS CHAR NO-UNDO.                                        */
/*   DEF VAR comp-char-val AS CHAR NO-UNDO.                                  */
/*   DEF VAR cEntryTo AS CHAR NO-UNDO.                                       */
/*   DEF VAR lValid AS LOG NO-UNDO.                                          */
/*   DEF VAR j AS INT NO-UNDO.                                               */
/*                                                                           */
/*   DO WITH FRAME {&FRAME-NAME}:                                            */
/*     RUN sys/ref/validSysCtrlChar.p                                        */
/*       (INPUT g_company,                                                   */
/*        INPUT g_loc,                                                       */
/*        INPUT sys-ctrl.NAME:SCREEN-VALUE,                                  */
/*        INPUT sys-ctrl.char-fld:LABEL,                                     */
/*        INPUT sys-ctrl.log-fld:SCREEN-VALUE,                               */
/*        INPUT sys-ctrl.char-fld:SCREEN-VALUE,                              */
/*        INPUT name-fld-list,                                               */
/*        INPUT str-init[LOOKUP(sys-ctrl.NAME:SCREEN-VALUE, name-fld-list)], */
/*        OUTPUT cEntryTo,                                                   */
/*        OUTPUT lValid).                                                    */
/*     IF NOT lValid THEN DO:                                                */
/*       CASE cEntryTo:                                                      */
/*         WHEN "Char" THEN                                                  */
/*           APPLY 'ENTRY':U TO {&tableName}.char-fld.                       */
/*         WHEN "Log" THEN                                                   */
/*           APPLY 'ENTRY':U TO {&tableName}.log-fld.                        */
/*       END CASE.                                                           */
/*     END.                                                                  */
/*                                                                           */
/*   END.                                                                    */
/*
  IF NOT (sys-ctrl.NAME:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "VendXfer" OR
     sys-ctrl.NAME:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "CustXfer" OR  sys-ctrl.NAME:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "ORDERXFER")
  THEN DO: 
      {sys/ref/valid-char-fld.i}
  END.
      ELSE DO:
          DO WITH FRAME {&FRAME-NAME}:
          ASSIGN comp-char-val = sys-ctrl.char-fld:SCREEN-VALUE.          

          DO J = 1 TO NUM-ENTRIES(comp-char-val):
              ASSIGN thisOne = ENTRY(j,comp-char-val).          

              FIND FIRST company WHERE company.company = thisOne NO-LOCK NO-ERROR.
              IF NOT AVAIL company THEN DO:         
                  MESSAGE   thisOne  "is not a valid company" VIEW-AS ALERT-BOX ERROR.
                  APPLY 'ENTRY':U TO {&tableName}.char-fld.
                  RETURN ERROR.
              END.
          END.   
      END.
  END.


  IF sys-ctrl.name EQ "SSRMISSUE" THEN DO:
      IF sys-ctrl.char-fld:SCREEN-VALUE = "ScanTagOnly" AND
          sys-ctrl.log-fld:SCREEN-VALUE = "yes" THEN DO:
          MESSAGE "Value 'ScanTagOnly' cannot be used with auto-post functionality (logical value YES)."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY 'ENTRY':U TO {&tableName}.log-fld.
          RETURN ERROR.
      END.
  END.
  */

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-log-fld V-table-Win 
PROCEDURE valid-log-fld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
 /* {sys/ref/valid-log-fld.i} */

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

