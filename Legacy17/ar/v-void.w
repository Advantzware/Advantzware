&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DECLARATIONS B-table-Win
{Advantzware\WinKit\admViewersUsing.i} /* added by script _admViewers.p */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: ar\v-void.w

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

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR v-c-no AS INT NO-UNDO.

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
&Scoped-define EXTERNAL-TABLES ar-cash
&Scoped-define FIRST-EXTERNAL-TABLE ar-cash


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ar-cash.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ar-cash.check-no ar-cash.check-amt ~
ar-cash.check-date ar-cash.bank-code 
&Scoped-define ENABLED-TABLES ar-cash
&Scoped-define FIRST-ENABLED-TABLE ar-cash
&Scoped-Define ENABLED-OBJECTS RECT-1 fi_fchk fi_cust btn_go 
&Scoped-Define DISPLAYED-FIELDS ar-cash.check-no ar-cash.check-amt ~
ar-cash.check-date ar-cash.cust-no ar-cash.bank-code 
&Scoped-define DISPLAYED-TABLES ar-cash
&Scoped-define FIRST-DISPLAYED-TABLE ar-cash
&Scoped-Define DISPLAYED-OBJECTS fi_fchk fi_cust fi_name cust_name ~
bank_name v-voided 

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
DEFINE BUTTON btn_can-2 
     LABEL "&Cancel" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_go 
     LABEL "&Update" 
     SIZE 12 BY 1.

DEFINE BUTTON btn_save 
     LABEL "&Save" 
     SIZE 12 BY 1.

DEFINE VARIABLE bank_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE cust_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE fi_cust AS CHARACTER FORMAT "X(8)":U 
     LABEL "Customer#" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_fchk AS INTEGER FORMAT ">>>>>>>>>>" INITIAL 0 
     LABEL "Check#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE fi_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE v-voided AS LOGICAL FORMAT "Yes/No" INITIAL NO 
     LABEL "Voided?" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 123 BY 6.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_fchk AT ROW 1.48 COL 11 COLON-ALIGNED
     fi_cust AT ROW 1.48 COL 48 COLON-ALIGNED
     fi_name AT ROW 1.48 COL 68.8 COLON-ALIGNED HELP
          "Enter Finished Goods Name used for Alpha Numeric Searches." NO-LABEL
     btn_go AT ROW 2.91 COL 13
     ar-cash.check-no AT ROW 4.1 COL 19 COLON-ALIGNED FORMAT "9999999999"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     ar-cash.check-amt AT ROW 4.1 COL 56.8 COLON-ALIGNED
          LABEL "Check Amount"
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     ar-cash.check-date AT ROW 4.1 COL 99.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     ar-cash.cust-no AT ROW 5.33 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     cust_name AT ROW 5.33 COL 39 COLON-ALIGNED NO-LABEL
     ar-cash.bank-code AT ROW 6.48 COL 19 COLON-ALIGNED
          LABEL "Bank Code"
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     bank_name AT ROW 6.52 COL 39 COLON-ALIGNED NO-LABEL
     v-voided AT ROW 6.52 COL 99 COLON-ALIGNED
     btn_save AT ROW 8.62 COL 90
     btn_can-2 AT ROW 8.62 COL 103
     RECT-1 AT ROW 4 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.ar-cash
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
         HEIGHT             = 17.14
         WIDTH              = 144.8.
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

/* SETTINGS FOR FILL-IN ar-cash.bank-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bank_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_can-2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_save IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-cash.check-amt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-cash.check-no IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN ar-cash.cust-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN v-voided IN FRAME F-Main
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

&Scoped-define SELF-NAME btn_can-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_can-2 V-table-Win
ON CHOOSE OF btn_can-2 IN FRAME F-Main /* Cancel */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      FIND FIRST ar-cash WHERE
           ar-cash.c-no EQ v-c-no
           NO-LOCK NO-ERROR.

      IF AVAIL ar-cash THEN
      DO:
         v-voided = IF ar-cash.cleared = ? THEN YES ELSE NO.
         RELEASE ar-cash.
      END.

      ASSIGN
        btn_save:SENSITIVE = NO
        btn_can-2:SENSITIVE = NO
        btn_go:SENSITIVE = YES
        v-voided:SENSITIVE = NO
        fi_fchk:SENSITIVE = YES
        fi_cust:SENSITIVE = YES
        cust_name = ""
        bank_name = ""
        ar-cash.check-amt:SCREEN-VALUE = "0"
        ar-cash.cust-no:SCREEN-VALUE = ""
        ar-cash.bank-code:SCREEN-VALUE = ""
        ar-cash.check-no:SCREEN-VALUE = "0"
        ar-cash.check-date:SCREEN-VALUE = ?.

      DISPLAY cust_name bank_name v-voided WITH FRAME {&FRAME-NAME}.

      APPLY "ENTRY":U TO fi_fchk IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_go
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_go V-table-Win
ON CHOOSE OF btn_go IN FRAME F-Main /* Update */
DO:
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN
        fi_fchk
        fi_cust
        v-c-no = 0.

     FIND FIRST ar-cash WHERE
          ar-cash.company EQ cocode AND
          ar-cash.check-no EQ fi_fchk AND
          ar-cash.cust-no EQ fi_cust AND
          ar-cash.posted EQ YES /*AND
          ar-cash.reconciled EQ NO*/
          NO-LOCK NO-ERROR.

     IF AVAIL ar-cash THEN
     DO:
        FIND FIRST cust WHERE
             cust.company EQ g_company AND
             cust.cust-no EQ ar-cash.cust-no
             NO-LOCK NO-ERROR.

        IF AVAIL cust THEN cust_name = cust.name.

        FIND FIRST bank WHERE
             bank.company   EQ g_company AND
             bank.bank-code EQ ar-cash.bank-code
             NO-LOCK NO-ERROR.

        IF AVAIL bank THEN bank_name = bank.bank-name.

        v-voided = IF ar-cash.cleared = ? THEN YES ELSE NO.

        DISPLAY ar-cash.check-amt
                ar-cash.cust-no ar-cash.bank-code
                ar-cash.check-no ar-cash.check-date
                cust_name bank_name v-voided
                WITH FRAME {&FRAME-NAME}.

        RELEASE cust.
        RELEASE bank.

        ASSIGN
           v-voided:SENSITIVE = YES
           btn_go:SENSITIVE = NO
           fi_fchk:SENSITIVE = NO
           fi_cust:SENSITIVE = NO
           btn_can-2:SENSITIVE = YES
           btn_save:SENSITIVE = YES
           v-c-no = ar-cash.c-no.
     END.
     ELSE
     DO:
        MESSAGE "Invalid Check."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.

        APPLY "ENTRY":U TO fi_fchk IN FRAME {&FRAME-NAME}.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_save
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_save V-table-Win
ON CHOOSE OF btn_save IN FRAME F-Main /* Save */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      ASSIGN v-voided.

      FIND FIRST ar-cash WHERE
           ar-cash.c-no EQ v-c-no
           EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL ar-cash THEN DO:

         IF v-voided THEN DO:
            FOR EACH ar-cashl WHERE ar-cashl.company = ar-cash.company
                                AND ar-cashl.c-no = ar-cash.c-no:
               IF ar-cashl.inv-no = 0 THEN DO:
                  MESSAGE "Must be applied before voiding receipt.      "
                     VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  v-voided = NO.
                  DISPLAY v-voided WITH FRAME {&FRAME-NAME}.
                  LEAVE.
               END.
            END.
         END.

         ar-cash.cleared = IF v-voided THEN ? ELSE NO. 
         RELEASE ar-cash.
      END.
      ELSE
         MESSAGE "Check not available for updating."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.

      ASSIGN
        btn_save:SENSITIVE = NO
        btn_can-2:SENSITIVE = NO
        btn_go:SENSITIVE = YES
        v-voided:SENSITIVE = NO
        fi_fchk:SENSITIVE = YES
        fi_cust:SENSITIVE = YES.

      APPLY "ENTRY":U TO fi_fchk IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_cust
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_cust V-table-Win
ON LEAVE OF fi_cust IN FRAME F-Main /* Customer# */
DO:
  IF LASTKEY NE -1 THEN
  DO:
     FIND FIRST cust WHERE
          cust.company EQ cocode AND
          cust.cust-no EQ fi_cust:SCREEN-VALUE
          NO-LOCK NO-ERROR.

     IF AVAIL cust THEN fi_name:SCREEN-VALUE = cust.name.
     ELSE fi_name:SCREEN-VALUE = "".

     {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fi_fchk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fi_fchk V-table-Win
ON LEAVE OF fi_fchk IN FRAME F-Main /* Check# */
DO:
   DO WITH FRAME {&FRAME-NAME}:

      IF LASTKEY = -1 THEN RETURN.
     {&methods/lValidateError.i YES}

      IF int(fi_fchk:SCREEN-VALUE) = 0 THEN DO:
         MESSAGE "Check number must be entered..." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.

      IF INT(fi_fchk:SCREEN-VALUE) >= 90000000 AND
         INT(fi_fchk:SCREEN-VALUE) <= 99999999 THEN
          DO:
             MESSAGE "This number reserved for CR/DB memos." VIEW-AS ALERT-BOX ERROR.
             RETURN NO-APPLY.
          END.
      {&methods/lValidateError.i NO}
   END.
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
SESSION:DATA-ENTRY-RETURN = YES.
  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-navigation V-table-Win 
PROCEDURE enable-navigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*leave here*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

