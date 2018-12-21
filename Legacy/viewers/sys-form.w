&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/sys-form.w

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

&SCOPED-DEFINE tableName sys-ctrl-shipto
&SCOPED-DEFINE nameField opName

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}

{sys/ref/sys-ctrl.i}

DEFINE NEW SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.

DEFINE VARIABLE opName AS CHARACTER NO-UNDO.
DEFINE VARIABLE opModule AS CHARACTER NO-UNDO.
DEFINE VARIABLE gvcMultiSelect AS CHARACTER NO-UNDO INIT "OEDATECHANGE,SSBOLEMAIL".
DEFINE VARIABLE cValidateList AS CHARACTER   NO-UNDO.
  cValidateList = 'QUOPRINT,BOLFMT,ACKHEAD,RELPRINT,POPRINT,'
                  + 'INVPRINT,BOLCERT,JOBCARDF,JOBCARDC,QUOPRICE'
                  + 'SSBOLEMAIL,OEDATECHANGE,RELPOST'.
&SCOPED-DEFINE Enhance NO

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
&Scoped-define EXTERNAL-TABLES sys-ctrl-shipto
&Scoped-define FIRST-EXTERNAL-TABLE sys-ctrl-shipto


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR sys-ctrl-shipto.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS sys-ctrl-shipto.ship-id ~
sys-ctrl-shipto.char-fld sys-ctrl-shipto.date-fld sys-ctrl-shipto.dec-fld ~
sys-ctrl-shipto.int-fld sys-ctrl-shipto.log-fld 
&Scoped-define ENABLED-TABLES sys-ctrl-shipto
&Scoped-define FIRST-ENABLED-TABLE sys-ctrl-shipto
&Scoped-Define DISPLAYED-FIELDS sys-ctrl-shipto.cust-vend ~
sys-ctrl-shipto.cust-vend-no sys-ctrl-shipto.ship-id ~
sys-ctrl-shipto.char-fld sys-ctrl-shipto.date-fld sys-ctrl-shipto.dec-fld ~
sys-ctrl-shipto.int-fld sys-ctrl-shipto.log-fld 
&Scoped-define DISPLAYED-TABLES sys-ctrl-shipto
&Scoped-define FIRST-DISPLAYED-TABLE sys-ctrl-shipto
&Scoped-Define DISPLAYED-OBJECTS type_name ship_name 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS sys-ctrl-shipto.cust-vend ~
sys-ctrl-shipto.cust-vend-no 
&Scoped-define DISPLAY-FIELD sys-ctrl-shipto.cust-vend-no ~
sys-ctrl-shipto.ship-id 

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
DEFINE VARIABLE ship_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE type_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   
     SIZE 102 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     sys-ctrl-shipto.cust-vend AT ROW 1.24 COL 27 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "&Customer", yes,
"&Vendor", no
          SIZE 34 BY 1
     sys-ctrl-shipto.cust-vend-no AT ROW 2.43 COL 25 COLON-ALIGNED
          LABEL "Customer/Vendor ID"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     type_name AT ROW 2.43 COL 40.4 COLON-ALIGNED NO-LABEL
     sys-ctrl-shipto.ship-id AT ROW 3.62 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 15 
     ship_name AT ROW 3.62 COL 40.4 COLON-ALIGNED NO-LABEL
     sys-ctrl-shipto.char-fld AT ROW 4.81 COL 25 COLON-ALIGNED
          LABEL "Business Form Name" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 75 BY 1
          BGCOLOR 15 
     sys-ctrl-shipto.date-fld AT ROW 6 COL 25 COLON-ALIGNED
          LABEL "Date Value"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     sys-ctrl-shipto.dec-fld AT ROW 7.19 COL 25 COLON-ALIGNED
          LABEL "Decimal Value"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     sys-ctrl-shipto.int-fld AT ROW 8.38 COL 25 COLON-ALIGNED
          LABEL "Integer Value"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 15 
     sys-ctrl-shipto.log-fld AT ROW 9.57 COL 27
          LABEL "Logical Value"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .81
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: asi.sys-ctrl-shipto
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
         HEIGHT             = 9.57
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN sys-ctrl-shipto.char-fld IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR RADIO-SET sys-ctrl-shipto.cust-vend IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.cust-vend-no IN FRAME F-Main
   NO-ENABLE 1 4 EXP-LABEL                                              */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.date-fld IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.dec-fld IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.int-fld IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX sys-ctrl-shipto.log-fld IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sys-ctrl-shipto.ship-id IN FRAME F-Main
   4                                                                    */
/* SETTINGS FOR FILL-IN ship_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN type_name IN FRAME F-Main
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

&Scoped-define SELF-NAME sys-ctrl-shipto.char-fld
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.char-fld V-table-Win
ON ENTRY OF sys-ctrl-shipto.char-fld IN FRAME F-Main /* Business Form Name */
DO:
  DEF VAR ls-name-value AS CHAR FORMAT "x(100)" NO-UNDO.

  {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT opName,OUTPUT opModule)"}
  STATUS INPUT ''.
  IF CAN-DO(name-fld-list,opName) THEN DO:
     ls-name-value = str-init[LOOKUP(opName,name-fld-list)].
     STATUS INPUT ls-name-value.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.char-fld V-table-Win
ON HELP OF sys-ctrl-shipto.char-fld IN FRAME F-Main /* Business Form Name */
DO:
   DEF VAR char-val1 AS CHARACTER NO-UNDO.
   DEF VAR i-chrfld AS CHAR NO-UNDO.
   DEF VAR v_chrfld1  AS CHAR NO-UNDO INIT 'c:\'.

   DO WITH FRAME {&FRAME-NAME}:

     {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT opName,OUTPUT opModule)"}   

      IF opName EQ "RELMERGE" THEN
      DO:
         RUN windows/l-sysfchr.w (gcompany,opName,FOCUS:SCREEN-VALUE,OUTPUT char-val1).
         IF char-val1 NE '' THEN
            sys-ctrl-shipto.char-fld:SCREEN-VALUE = STRING(char-val1).
         RETURN NO-APPLY.
      END.

      IF opName EQ "CINVOICE" THEN
      DO:
         RUN windows/l-sysfchr.w (gcompany,opName,FOCUS:SCREEN-VALUE,OUTPUT char-val1).
         IF char-val1 NE '' THEN
            sys-ctrl-shipto.char-fld:SCREEN-VALUE = ENTRY(1,char-val1).

         RETURN NO-APPLY.
      END.
      ELSE IF opName EQ 'BOLPrint' THEN DO: 
         RUN windows/l-fgbin2.w (gcompany,"",FOCUS:SCREEN-VALUE,OUTPUT char-val1).
         IF char-val1 NE '' THEN
            sys-ctrl-shipto.char-fld:SCREEN-VALUE = ENTRY(1,char-val1).

         RETURN NO-APPLY.
      END.
      /* gdm - 11050804 */
      ELSE IF opName EQ 'CASLABEL' THEN DO:          

          ASSIGN i-chrfld = ""
                 i-chrfld = sys-ctrl-shipto.char-fld:SCREEN-VALUE.

          IF i-chrfld NE "" THEN
             RUN sys\ref\char-fld-help.w(INPUT gcompany,
                                          INPUT i-chrfld,
                                          OUTPUT v_chrfld1).
          ELSE DO:

              FIND FIRST sys-ctrl NO-LOCK
                  WHERE sys-ctrl.company EQ gcompany
                    AND sys-ctrl.name    EQ 'CASLABEL'
                    NO-ERROR.

              IF AVAIL sys-ctrl THEN
                 ASSIGN i-chrfld = TRIM(sys-ctrl.char-fld).

              RUN sys\ref\char-fld-help.w(INPUT gcompany,
                                          INPUT i-chrfld,
                                          OUTPUT v_chrfld1).
          END.


          IF TRIM(v_chrfld1) NE "" 
            THEN ASSIGN sys-ctrl-shipto.char-fld:SCREEN-VALUE = v_chrfld1.

      END. /* gdm - 11050804 end */

      /* gdm - 12170903 */
      ELSE IF (opName EQ 'BARDIR' OR opName EQ 'RMBARDIR') THEN DO:

          MESSAGE "Do you want to display Xprint Values.... "
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
              TITLE "" UPDATE lChoiceXprnt AS LOGICAL.

        IF NOT lChoiceXprnt THEN do:
          ASSIGN i-chrfld = ""
                 i-chrfld = TRIM(sys-ctrl-shipto.char-fld:SCREEN-VALUE).

          RELEASE sys-ctrl.

          IF i-chrfld EQ "" THEN
             FIND FIRST sys-ctrl NO-LOCK 
               WHERE sys-ctrl.company EQ gcompany
                 AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.

          IF AVAIL sys-ctrl THEN
             ASSIGN i-chrfld = TRIM(sys-ctrl.char-fld).

          RUN sys\ref\char-fld-help.w(INPUT gcompany,
                                      INPUT i-chrfld,
                                      OUTPUT v_chrfld1).

          IF TRIM(v_chrfld1) NE "" THEN
             ASSIGN sys-ctrl-shipto.char-fld:SCREEN-VALUE = v_chrfld1.
        END.
        ELSE DO:
            RUN windows/l-typxpr.w (INPUT opName, OUTPUT char-val1).
            IF char-val1 NE '' THEN
                sys-ctrl-shipto.char-fld:SCREEN-VALUE = char-val1.
            RETURN NO-APPLY.
        END.

      END. /* gdm - 12170903 end */

      ELSE IF
          CAN-DO(cValidateList,opName)   /*Task 11011323  */ /* Task# 01211409 */ 
      THEN DO:
          {sys/ref/char-fld-help.i}
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.char-fld V-table-Win
ON LEAVE OF sys-ctrl-shipto.char-fld IN FRAME F-Main /* Business Form Name */
DO:
  STATUS INPUT ''.
  IF LASTKEY NE -1 THEN DO:
    RUN valid-char-fld NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sys-ctrl-shipto.cust-vend
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.cust-vend V-table-Win
ON VALUE-CHANGED OF sys-ctrl-shipto.cust-vend IN FRAME F-Main /* Cust/Vend */
DO:
  ASSIGN
    sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE = ''
    sys-ctrl-shipto.ship-id:SCREEN-VALUE = ''
    type_name:SCREEN-VALUE = ''
    ship_name:SCREEN-VALUE = ''.
  IF SELF:SCREEN-VALUE EQ 'YES' THEN
     ENABLE sys-ctrl-shipto.ship-id WITH FRAME {&FRAME-NAME}.
  ELSE
     DISABLE sys-ctrl-shipto.ship-id WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sys-ctrl-shipto.cust-vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.cust-vend-no V-table-Win
ON HELP OF sys-ctrl-shipto.cust-vend-no IN FRAME F-Main /* Customer/Vendor ID */
DO:
  IF sys-ctrl-shipto.cust-vend:SCREEN-VALUE EQ 'YES' THEN
  RUN lookups/cust.p.
  ELSE
  RUN lookups/vend.p.
  SELF:SCREEN-VALUE = g_lookup-var.
  APPLY 'ENTRY':U TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.cust-vend-no V-table-Win
ON LEAVE OF sys-ctrl-shipto.cust-vend-no IN FRAME F-Main /* Customer/Vendor ID */
DO:
  {&methods/lValidateError.i YES}
  IF SELF:SCREEN-VALUE NE '' THEN DO:
    IF {&FIRST-EXTERNAL-TABLE}.cust-vend:SCREEN-VALUE EQ 'Yes' THEN DO:
      {methods/entryerr.i
        &can-find="FIRST cust 
           WHERE cust.company EQ gcompany
             AND cust.cust-no EQ {&FIRST-EXTERNAL-TABLE}.cust-vend-no:SCREEN-VALUE"
        &error-message="Invalid Customer"}
    END.
    ELSE DO:
      {methods/entryerr.i
        &can-find="FIRST vend 
           WHERE vend.company EQ gcompany
             AND vend.vend-no EQ {&FIRST-EXTERNAL-TABLE}.cust-vend-no:SCREEN-VALUE"
        &error-message="Invalid Vendor"}
    END.
  END.
  {methods/dispflds.i}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sys-ctrl-shipto.log-fld
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.log-fld V-table-Win
ON VALUE-CHANGED OF sys-ctrl-shipto.log-fld IN FRAME F-Main /* Logical Value */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-log-fld NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sys-ctrl-shipto.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.ship-id V-table-Win
ON HELP OF sys-ctrl-shipto.ship-id IN FRAME F-Main /* Ship To ID */
DO:
  IF sys-ctrl-shipto.cust-vend:SCREEN-VALUE EQ 'YES' THEN DO:
    RUN windows/l-shipto.w (gcompany,gloc,sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE,
                            SELF:SCREEN-VALUE,OUTPUT g_lookup-var).
    g_lookup-var = ENTRY(1,g_lookup-var).
  END.
  ELSE
  RUN lookups/vend.p.
  SELF:SCREEN-VALUE = g_lookup-var.
  APPLY 'ENTRY':U TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sys-ctrl-shipto.ship-id V-table-Win
ON LEAVE OF sys-ctrl-shipto.ship-id IN FRAME F-Main /* Ship To ID */
DO:
  {&methods/lValidateError.i YES}
  IF {&FIRST-EXTERNAL-TABLE}.cust-vend:SCREEN-VALUE EQ 'Yes' AND
     SELF:SCREEN-VALUE NE '' THEN DO:
    {methods/entryerr.i
      &can-find="FIRST shipto NO-LOCK
           WHERE shipto.company EQ gcompany
             AND shipto.cust-no EQ {&FIRST-EXTERNAL-TABLE}.cust-vend-no:SCREEN-VALUE
             AND shipto.ship-id EQ {&FIRST-EXTERNAL-TABLE}.ship-id:SCREEN-VALUE"
      &error-message="Invalid Ship To"}
  END.
  {methods/dispflds.i}
  {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

  {custom/getcmpny.i}
  {custom/getloc.i}
  cocode = gcompany.

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
  {src/adm/template/row-list.i "sys-ctrl-shipto"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "sys-ctrl-shipto"}

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

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/assign/sys-ctrl-shipto.i}

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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
    {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT opName,OUTPUT opModule)"} 

    IF opName EQ "CustomerList" THEN
        ASSIGN sys-ctrl-shipto.log-fld:LABEL IN FRAME {&FRAME-NAME} = "Limit Customers?" .
    ELSE
        ASSIGN sys-ctrl-shipto.log-fld:LABEL IN FRAME {&FRAME-NAME}  = "Logical Value" . 

    IF opName  EQ "CASLABEL" OR opName  EQ "BarDir" OR opName  EQ "RMTags" THEN
         ASSIGN sys-ctrl-shipto.char-fld:LABEL IN FRAME {&FRAME-NAME} = "Label Location" .
    ELSE IF opName  EQ "PushPin" THEN
         ASSIGN sys-ctrl-shipto.char-fld:LABEL IN FRAME {&FRAME-NAME} = "File Directory" .
    ELSE IF opName  EQ "CustomerList" THEN
         ASSIGN sys-ctrl-shipto.char-fld:LABEL IN FRAME {&FRAME-NAME} = "Menu Hot Key" .
    ELSE IF opName  EQ "RELMERGE" THEN
         ASSIGN sys-ctrl-shipto.char-fld:LABEL IN FRAME {&FRAME-NAME} = "Character Value" .
    ELSE
        ASSIGN sys-ctrl-shipto.char-fld:LABEL IN FRAME {&FRAME-NAME}  = "Business Form Name" . 

    END PROCEDURE.

    /* _UIB-CODE-BLOCK-END */
    &ANALYZE-RESUME

    &ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
    PROCEDURE local-update-record :
    /*------------------------------------------------------------------------------
      Purpose:     Override standard ADM method
      Notes:       
    ------------------------------------------------------------------------------*/
      DEF VAR check-all-update AS LOG NO-UNDO.
      DEF BUFFER bf-sys-ctrl-shipto FOR sys-ctrl-shipto .
      /* Code placed here will execute PRIOR to standard behavior. */
      DO WITH FRAME {&FRAME-NAME}:
        IF sys-ctrl-shipto.cust-vend-no NE '' THEN
        APPLY 'LEAVE':U TO sys-ctrl-shipto.cust-vend-no.
        IF correct-error THEN RETURN.
        IF sys-ctrl-shipto.ship-id NE '' THEN
        APPLY 'LEAVE':U TO sys-ctrl-shipto.ship-id.
        IF correct-error THEN RETURN.
      END.

  RUN valid-char-fld NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  RUN valid-log-fld NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  RUN valid-cust NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT opName,OUTPUT opModule)"}  

  IF opName = "Reports" AND sys-ctrl-shipto.log-fld  THEN do:

    MESSAGE " Would you like to set all reports to yes? "  
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO 
        UPDATE check-all-update . 

    IF check-all-update  THEN do:
        FOR EACH bf-sys-ctrl-shipto WHERE bf-sys-ctrl-shipto.company = cocode AND 
             bf-sys-ctrl-shipto.NAME = "Reports" EXCLUSIVE-LOCK:

            ASSIGN bf-sys-ctrl-shipto.log-fld = YES .
        END. /* for each */
    END.  /* check-all-update */
  END. /* opName = "Reports" */

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
  {src/adm/template/snd-list.i "sys-ctrl-shipto"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-char-fld V-table-Win 
PROCEDURE valid-char-fld :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR thisOne AS CHAR NO-UNDO.
  DEF VAR comp-char-val AS CHAR NO-UNDO.
  DEF VAR cEntryTo AS CHAR NO-UNDO.
  DEF VAR cSingleValue AS CHAR NO-UNDO.
  DEF VAR lValid AS LOG NO-UNDO.
  DEF VAR i AS INT NO-UNDO.
  DEF VAR j AS INT NO-UNDO. 

  {methods/lValidateError.i YES}
  {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT opName,OUTPUT opModule)"}




  DO WITH FRAME {&FRAME-NAME}:
 /* Task 11011321 */
    IF sys-ctrl-shipto.char-fld:SCREEN-VALUE NE "" AND
        CAN-DO(cValidateList,opName) THEN DO: 

        /*  {sys/ref/valid-char-fld.i} */

      lValid = TRUE.     

      /* Process NK1 options where user can select more than one */
      /* option - validate each option individually              */
      IF LOOKUP(opName, gvcMultiSelect) GT 0 
        AND INDEX(sys-ctrl-shipto.char-fld:SCREEN-VALUE, ",") GT 0 THEN DO:

          DO i = 1 TO NUM-ENTRIES(sys-ctrl-shipto.char-fld:SCREEN-VALUE):

            cSingleValue = ENTRY(i, sys-ctrl-shipto.char-fld:SCREEN-VALUE).


            RUN sys/ref/validSysCtrlChar.p 
              (INPUT g_company,
               INPUT g_loc,
               INPUT opName,
               INPUT sys-ctrl-shipto.char-fld:LABEL,
               INPUT sys-ctrl-shipto.log-fld:SCREEN-VALUE,
               INPUT cSingleValue,
               INPUT name-fld-list,
               INPUT str-init[LOOKUP(opName, name-fld-list)],
               OUTPUT cEntryTo,
               OUTPUT lValid).


            IF NOT lValid THEN DO:   
              CASE cEntryTo:
                WHEN "Char" THEN
                  APPLY 'ENTRY':U TO {&tableName}.char-fld.
                WHEN "Log" THEN
                  APPLY 'ENTRY':U TO {&tableName}.log-fld.
              END CASE.
              LEAVE.
            END. /* if not lvalid */

          END. /* do i = ... */


      END. /* if multiple values to validate */
      ELSE DO:

          RUN sys/ref/validSysCtrlChar.p 
            (INPUT g_company,
             INPUT g_loc,
             INPUT opName,
             INPUT sys-ctrl-shipto.char-fld:LABEL,
             INPUT sys-ctrl-shipto.log-fld:SCREEN-VALUE,
             INPUT sys-ctrl-shipto.char-fld:SCREEN-VALUE,
             INPUT name-fld-list,
             INPUT str-init[LOOKUP(opName, name-fld-list)],
             OUTPUT cEntryTo,
             OUTPUT lValid). 
          IF NOT lValid THEN DO:   
            CASE cEntryTo:
              WHEN "Char" THEN
                APPLY 'ENTRY':U TO {&tableName}.char-fld.
              WHEN "Log" THEN
                APPLY 'ENTRY':U TO {&tableName}.log-fld.
            END CASE.
          END. /* Not lvalid */

      END. /* Single value to validate */

      IF NOT lValid THEN
        RETURN ERROR.
    END.  /* End if non-blank value */   /* Task 11011321 */
    ELSE DO:         
        CASE opName:
           WHEN "CINVOICE" THEN
           DO:
              IF sys-ctrl-shipto.char-fld:SCREEN-VALUE NE "FIBREMEXICO" THEN
              DO:
                 MESSAGE "Invalid Business Form Name."
                     VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                 APPLY "ENTRY":U TO sys-ctrl-shipto.char-fld IN FRAME {&FRAME-NAME}.
                 RETURN ERROR.
              END.
           END. /* end when cinvoice */
        END CASE.
     END. /* else do */
  END. /* do with frame */


  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust V-table-Win 
PROCEDURE valid-cust :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT opName,OUTPUT opModule)"}

  DO WITH FRAME {&FRAME-NAME}:

     IF sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE NE "" THEN
     DO:         
        CASE opName:
           WHEN "RelMerge" THEN
           DO:
/* wfk - 05161408 - Looking at the actrelmerg logic, sys-ctrl-shipto was used at one time   */
/*                  in relation to samePO# with a "with prompt" and "withoutPrompt" options */
/*                  which are no longer in use                                              */
/*               IF                                                                                */
/*                   (asi.sys-ctrl-shipto.char-fld:SCREEN-VALUE NE "SamePo#Only"                   */
/*                    AND asi.sys-ctrl-shipto.char-fld:SCREEN-VALUE NE "SamePo#OnlyWithPrompt"     */
/*                    AND asi.sys-ctrl-shipto.char-fld:SCREEN-VALUE NE "SamePo#OnlyWithoutPrompt") */
/*                    AND sys-ctrl-shipto.cust-vend-no:SCREEN-VALUE NE "" THEN                     */
/*               DO:                                                                               */
/*                  MESSAGE "Cannot save a customer for 'Relmerge' if not SamePo#Only."            */
/*                      VIEW-AS ALERT-BOX ERROR BUTTONS OK.                                        */
/*                  APPLY "ENTRY":U TO sys-ctrl-shipto.cust-vend-no IN FRAME {&FRAME-NAME}.        */
/*                  RETURN ERROR.                                                                  */
/*               END.                                                                              */
           END.
        END CASE.
     END.
  END.
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
  {methods/run_link.i "RECORD-SOURCE" "Get-Values" "(OUTPUT opName,OUTPUT opModule)"}
  {sys/ref/valid-log-fld.i}

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

