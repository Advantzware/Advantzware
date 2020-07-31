&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/<table>.w

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
&SCOPED-DEFINE enable-arinv proc-enable
&SCOPED-DEFINE create-more methods/viewers/create/ar-inv
&SCOPED-DEFINE CommonFile_is_Running
&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF

DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}
DEF BUFFER bf-inv FOR ar-inv.
DEF VAR ll-got-cust-info AS LOG NO-UNDO.  /* assigned from cust leave trigger */
DEF VAR lv-due-calckt AS LOG NO-UNDO.
DEF VAR ls-add-what AS CHARACTER NO-UNDO.
DEFINE VARIABLE Is-add-dup-inv AS CHARACTER NO-UNDO .
DEFINE VARIABLE oeInvAddDate-Int AS INTEGER NO-UNDO .
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO .
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO .
DEFINE VARIABLE lEDI810Visible AS LOGICAL NO-UNDO.
DEFINE VARIABLE lEDI810NewValue AS LOGICAL NO-UNDO.
DEFINE VARIABLE hdCustomerProcs AS HANDLE NO-UNDO.

{sys/inc/VAR.i "new shared"}
ASSIGN cocode = g_company
       locode = g_loc.

RUN sys/ref/nk1look.p (INPUT cocode, "InvAddDate", "I" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeInvAddDate-Int = INTEGER(cRtnChar) NO-ERROR.

RUN util/checkModule.p (INPUT "", INPUT "EdIvTran.", INPUT NO, OUTPUT lEDI810Visible).

RUN system/CustomerProcs.p PERSISTENT SET hdCustomerProcs.

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
&Scoped-define EXTERNAL-TABLES ar-inv
&Scoped-define FIRST-EXTERNAL-TABLE ar-inv


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR ar-inv.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ar-inv.cust-no ar-inv.ship-id ar-inv.inv-no ~
ar-inv.po-no ar-inv.inv-date ar-inv.due-date ar-inv.printed ar-inv.period ~
ar-inv.tax-code ar-inv.terms ar-inv.cust-name ar-inv.disc-% ~
ar-inv.disc-days ar-inv.carrier ar-inv.f-bill ar-inv.freight 
&Scoped-define ENABLED-TABLES ar-inv
&Scoped-define FIRST-ENABLED-TABLE ar-inv
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 btnCalendar-2 RECT-1 RECT-5 
&Scoped-Define DISPLAYED-FIELDS ar-inv.cust-no ar-inv.ship-id ar-inv.inv-no ~
ar-inv.po-no ar-inv.inv-date ar-inv.due-date ar-inv.printed ar-inv.period ~
ar-inv.tax-code ar-inv.terms ar-inv.terms-d ar-inv.cust-name ar-inv.disc-% ~
ar-inv.disc-days ar-inv.carrier ar-inv.f-bill ar-inv.freight ar-inv.tax-amt ~
ar-inv.gross ar-inv.disc-taken ar-inv.paid ar-inv.due ar-inv.curr-code[1] ~
ar-inv.ex-rate 
&Scoped-define DISPLAYED-TABLES ar-inv
&Scoped-define FIRST-DISPLAYED-TABLE ar-inv
&Scoped-Define DISPLAYED-OBJECTS ship_name tbEdiInvoice 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS ar-inv.cust-no 
&Scoped-define ADM-ASSIGN-FIELDS ar-inv.cust-no ar-inv.terms-d ~
ar-inv.cust-name ar-inv.curr-code[1] 
&Scoped-define ROW-AVAILABLE btnCalendar-1 btnCalendar-2 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
x-no|y|y|ASI.ar-inv.x-no
check-no||y|ASI.ar-inv.check-no
company||y|ASI.ar-inv.company
Carrier||y|ASI.ar-inv.Carrier
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "x-no",
     Keys-Supplied = "x-no,check-no,company,Carrier"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE ship_name AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 130 BY 9.52.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 42 BY 3.81.

DEFINE VARIABLE tbEdiInvoice AS LOGICAL INITIAL no 
     LABEL "EDI Invoice?" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     ar-inv.cust-no AT ROW 1.48 COL 16 COLON-ALIGNED
          LABEL "Customer#"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ar-inv.ship-id AT ROW 2.43 COL 16 COLON-ALIGNED
          LABEL "Ship-To#"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ship_name AT ROW 2.43 COL 37 COLON-ALIGNED NO-LABEL
     ar-inv.inv-no AT ROW 3.38 COL 16 COLON-ALIGNED
          LABEL "Invoice#" FORMAT ">>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ar-inv.po-no AT ROW 4.33 COL 16 COLON-ALIGNED
          LABEL "PO#"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ar-inv.inv-date AT ROW 5.29 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ar-inv.due-date AT ROW 6.24 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ar-inv.printed AT ROW 6.43 COL 56 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ar-inv.period AT ROW 7.29 COL 56 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ar-inv.tax-code AT ROW 7.19 COL 16 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     ar-inv.terms AT ROW 8.14 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     ar-inv.terms-d AT ROW 8.14 COL 29 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     ar-inv.cust-name AT ROW 1.48 COL 37 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     ar-inv.disc-% AT ROW 3.62 COL 65 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     ar-inv.disc-days AT ROW 4.52 COL 65 COLON-ALIGNED
          LABEL "Disc Days"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     ar-inv.carrier AT ROW 5.52 COL 56 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     ar-inv.f-bill AT ROW 3.62 COL 80
          VIEW-AS TOGGLE-BOX
          SIZE 15 BY 1
     ar-inv.freight AT ROW 2.91 COL 105 COLON-ALIGNED
          LABEL "Freight"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-inv.tax-amt AT ROW 3.86 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-inv.gross AT ROW 1.95 COL 105 COLON-ALIGNED
          LABEL "Invoice Amt"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-inv.disc-taken AT ROW 5.91 COL 105 COLON-ALIGNED
          LABEL "Discount"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-inv.paid AT ROW 6.86 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-inv.due AT ROW 7.81 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ar-inv.curr-code[1] AT ROW 9.33 COL 20.4 COLON-ALIGNED
          LABEL "Currency Code"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ar-inv.ex-rate AT ROW 9.33 COL 55 COLON-ALIGNED
          LABEL "Exchange Rate"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     tbEdiInvoice AT ROW 9.43 COL 89 WIDGET-ID 2
     btnCalendar-1 AT ROW 5.29 COL 39.2
     btnCalendar-2 AT ROW 6.24 COL 39.2
     RECT-1 AT ROW 1 COL 1
     RECT-5 AT ROW 5.43 COL 88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.ar-inv
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
         WIDTH              = 147.6.
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

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN ar-inv.curr-code[1] IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN ar-inv.cust-name IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN ar-inv.cust-no IN FRAME F-Main
   1 2 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN ar-inv.disc-days IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.disc-taken IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ar-inv.due IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-inv.ex-rate IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ar-inv.freight IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.gross IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ar-inv.inv-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ar-inv.paid IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-inv.po-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ar-inv.ship-id IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN ship_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-inv.tax-amt IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-inv.tax-code IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX tbEdiInvoice IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ar-inv.terms-d IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
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
    def var char-val as cha no-undo.
    def var look-recid as recid no-undo. 
    DEF VAR lv-handle AS HANDLE NO-UNDO.

    g_lookup-var = "".
    case focus:name :
         when "cust-no" then do:
              run windows/l-custact.w (g_company, focus:screen-value, output char-val, output look-recid).
              if FOCUS:SCREEN-VALUE NE entry(1,char-val) then do:
                 FOCUS:SCREEN-VALUE = entry(1,char-val).
                 run new-cust-no.
              end.       
         end.         
         /*
         when "terms" then do:
              run windows/l-terms.w (g_company,focus:screen-value, output char-val).
              if char-val <> "" then assign focus:screen-value = entry(1,char-val)
                                            oe-ord.terms-d:screen-value = entry(1,char-val).
         end.
         */
         when "tax-code" then do:
              run windows/l-stax.w (g_company,focus:screen-value, output char-val).
              if char-val <> "" then assign focus:screen-value = entry(1,char-val).
         end.
        WHEN "ship-id" THEN DO:
            RUN windows/l-shipt2.w (g_company,g_loc,ar-inv.cust-no:SCREEN-VALUE,FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT look-recid).
            IF char-val <> "" THEN ASSIGN FOCUS:SCREEN-VALUE = ENTRY(1,char-val)
                                          ship_name:SCREEN-VALUE = ENTRY(2,char-val).

        END.
         OTHERWISE DO:
            lv-handle = focus:handle.
            run applhelp.p.

            if g_lookup-var <> "" then do:
               lv-handle:screen-value = g_lookup-var.
            end.  
            APPLY "entry" TO lv-handle.
         END.
    end case.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 V-table-Win
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i ar-inv.inv-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 V-table-Win
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i ar-inv.due-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-inv.curr-code[1]
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.curr-code[1] V-table-Win
ON LEAVE OF ar-inv.curr-code[1] IN FRAME F-Main /* Currency Code */
DO:
   IF LASTKEY = -1 THEN RETURN.
   {&methods/lValidateError.i YES}
   FIND FIRST currency WHERE currency.company = g_company 
                          AND currency.c-code = SELF:SCREEN-VALUE 
                          NO-LOCK NO-ERROR.
   IF AVAIL currency THEN ar-inv.ex-rate:SCREEN-VALUE = string(currency.ex-rate).
   ELSE IF NOT AVAIL currency AND ar-inv.curr-code[1]:SCREEN-VALUE <> ""
   THEN DO:
       MESSAGE "Invalid Currency Code. Try Help. " VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-inv.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.cust-no V-table-Win
ON LEAVE OF ar-inv.cust-no IN FRAME F-Main /* Customer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.cust-no V-table-Win
ON VALUE-CHANGED OF ar-inv.cust-no IN FRAME F-Main /* Customer# */
DO:
  RUN new-cust-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-inv.due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.due-date V-table-Win
ON HELP OF ar-inv.due-date IN FRAME F-Main /* Due Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.due-date V-table-Win
ON LEAVE OF ar-inv.due-date IN FRAME F-Main /* Due Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-due-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-inv.inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.inv-date V-table-Win
ON HELP OF ar-inv.inv-date IN FRAME F-Main /* Invoice Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.inv-date V-table-Win
ON LEAVE OF ar-inv.inv-date IN FRAME F-Main /* Invoice Date */
DO:
    {&methods/lValidateError.i YES}
    IF DATE(ar-inv.inv-date:SCREEN-VALUE) > TODAY THEN DO:
        MESSAGE "Invoice Date is Past Today, Continue?" VIEW-AS ALERT-BOX WARNING BUTTON YES-NO
                      UPDATE ll-ans AS LOG.
        IF ll-ans THEN .
        ELSE RETURN NO-APPLY.
    END.
    RUN pInvDueDate .  /* recalc due-date */
    lv-due-calckt = YES.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-inv.inv-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.inv-no V-table-Win
ON LEAVE OF ar-inv.inv-no IN FRAME F-Main /* Invoice# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-inv-no NO-ERROR.
    IF ERROR-STATUS:ERROR 
        THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-inv.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.ship-id V-table-Win
ON LEAVE OF ar-inv.ship-id IN FRAME F-Main /* Ship-To# */
DO:
   IF LASTKEY = -1 THEN RETURN.
   {&methods/lValidateError.i YES}
   FIND FIRST shipto WHERE shipto.company = g_company 
                       AND shipto.cust-no = ar-inv.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                       AND shipto.ship-id = ar-inv.ship-id:SCREEN-VALUE                
                       NO-LOCK NO-ERROR.
   IF NOT AVAIL shipto THEN DO:
      MESSAGE "Invalid Ship To. Try Help." VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.
   ship_name:SCREEN-VALUE = shipto.ship-name.
   {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-inv.tax-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.tax-code V-table-Win
ON LEAVE OF ar-inv.tax-code IN FRAME F-Main /* Tax Code */
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}
    {VALIDATE/stax.i ar-inv.tax-code}
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbEdiInvoice
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbEdiInvoice V-table-Win
ON VALUE-CHANGED OF tbEdiInvoice IN FRAME F-Main /* EDI Invoice? */
DO:
  ASSIGN tbEdiInvoice.
  
  lEDI810NewValue = tbEdiInvoice.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ar-inv.terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.terms V-table-Win
ON ENTRY OF ar-inv.terms IN FRAME F-Main /* Terms Code */
DO:
  RUN new-terms.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.terms V-table-Win
ON LEAVE OF ar-inv.terms IN FRAME F-Main /* Terms Code */
DO:
    {&methods/lValidateError.i YES}
    FIND FIRST terms WHERE terms.t-code = ar-inv.terms:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
    IF NOT AVAIL terms THEN DO:
       MESSAGE "Invalid Terms. Try Help. " VIEW-AS ALERT-BOX ERROR.
       RETURN NO-APPLY.
    END.

    RUN pInvDueDate .  /* recalc due-date */
    lv-due-calckt = YES.
    {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ar-inv.terms V-table-Win
ON VALUE-CHANGED OF ar-inv.terms IN FRAME F-Main /* Terms Code */
DO:
  DEF VAR li AS INT NO-UNDO.


  RUN new-terms.

  {&self-name}:SCREEN-VALUE = CAPS({&self-name}:SCREEN-VALUE).
  IF LASTKEY EQ 32 THEN {&SELF-NAME}:CURSOR-OFFSET = LENGTH({&SELF-NAME}:SCREEN-VALUE) + 2. /* res */

  DO li = 1 TO LENGTH(TRIM({&self-name}:SCREEN-VALUE)):
    APPLY "cursor-right" TO {&self-name}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-inv V-table-Win 
PROCEDURE add-inv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 RUN dispatch ('add-record').
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-find-using-key V-table-Win  adm/support/_key-fnd.p
PROCEDURE adm-find-using-key :
/*------------------------------------------------------------------------------
  Purpose:     Finds the current record using the contents of
               the 'Key-Name' and 'Key-Value' attributes.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEF VAR key-value AS CHAR NO-UNDO.
  DEF VAR row-avail-enabled AS LOGICAL NO-UNDO.

  /* LOCK status on the find depends on FIELDS-ENABLED. */
  RUN get-attribute ('FIELDS-ENABLED':U).
  row-avail-enabled = (RETURN-VALUE eq 'yes':U).
  /* Look up the current key-value. */
  RUN get-attribute ('Key-Value':U).
  key-value = RETURN-VALUE.

  /* Find the current record using the current Key-Name. */
  RUN get-attribute ('Key-Name':U).
  CASE RETURN-VALUE:
    WHEN 'x-no':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = ar-inv
           &WHERE = "WHERE ar-inv.x-no eq INTEGER(key-value)"
       }
  END CASE.

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
  {src/adm/template/row-list.i "ar-inv"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "ar-inv"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CheckCreate V-table-Win 
PROCEDURE CheckCreate :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

DEFINE OUTPUT PARAMETER op-error AS LOGICAL NO-UNDO .
    ASSIGN 
        ls-add-what = "" 
        Is-add-dup-inv = "" .
    RUN oe/d-addfol.w (INPUT YES, OUTPUT ls-add-what,OUTPUT Is-add-dup-inv).
    
    IF ls-add-what = "" THEN do:  /* cancel from dialog box */
       ASSIGN op-error = YES .
        RETURN NO-APPLY.
    END.

    IF ls-add-what NE "add" AND Is-add-dup-inv EQ "" THEN DO:
        ASSIGN op-error = YES .
        RETURN NO-APPLY.
    END.

    IF ls-add-what EQ "duplicate" THEN DO:
       ASSIGN op-error = YES .
       RUN pCreateDuplicate("duplicate") . 
    END.
    ELSE IF ls-add-what EQ "credit" THEN DO:
        ASSIGN op-error = YES .
        RUN pCreateCredit("credit") .
    END.
    ELSE IF ls-add-what EQ "rebill" THEN DO:
        ASSIGN op-error = YES .
         RUN pCreateCredit("Rebill") .
        RUN  pCreateDuplicate("Rebill") .
    END.
    
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
   DEFINE VARIABLE dOverPer AS DECIMAL NO-UNDO.
   DEFINE VARIABLE dUnderPer AS DECIMAL NO-UNDO.
   DEFINE VARIABLE cOldShipto AS CHARACTER NO-UNDO.
   
  /* Code placed here will execute PRIOR to standard behavior. */
   cOldShipto = IF AVAIL ar-inv THEN ar-inv.ship-id ELSE "" . 

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /*ar-inv.f-bill = ar-inv.freight GT 0.*/
  
  DO WITH FRAME {&FRAME-NAME}:     
     ar-inv.ediInvoice = tbEdiInvoice:SCREEN-VALUE EQ "YES".  
  END. 
  
  FIND FIRST cust WHERE cust.company = g_company
                    AND cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.  
    IF cOldShipto NE ar-inv.ship-id then do:                    
     RUN oe/GetOverUnderPct.p(g_company,
                           ar-inv.cust-no,
                           ar-inv.ship-id,
                           OUTPUT dOverPer , OUTPUT dUnderPer ) .
                           ar-inv.over-pct = dOverPer.
                           ar-inv.Under-pct = dUnderPer. 
    END.
                    
  IF adm-adding-record THEN DO:    
    IF AVAIL cust THEN ASSIGN ar-inv.addr[1] = cust.addr[1]
                              ar-inv.addr[2] = cust.addr[2]
                              ar-inv.city = cust.city
                              ar-inv.state = cust.state
                              ar-inv.zip = cust.zip 
                              ar-inv.fob-code = cust.fob-code
                              .          
    FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
    FIND FIRST soldto NO-LOCK WHERE soldto.company EQ g_company
                             AND soldto.cust-no EQ ar-inv.cust-no
                             AND soldto.sold-id EQ ar-inv.cust-no
                             USE-INDEX sold-id NO-ERROR.
             IF AVAIL soldto THEN
                ar-inv.sold-id =  soldto.sold-id .

    IF NOT ll-got-cust-info THEN DO: /* don't override */
       FIND FIRST cust WHERE cust.company = g_company
                    AND cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.
       IF AVAIL cust THEN ASSIGN ar-inv.terms = cust.terms
                                 ar-inv.carrier = cust.carrier
                                 ar-inv.tax-code = cust.tax-gr
                                 ar-inv.curr-code[1] = cust.curr-code.
       IF cust.curr-code = "" THEN DO:
          FIND company WHERE company.company = g_company NO-LOCK NO-ERROR.
          IF AVAIL company THEN ar-inv.curr-code[1] = company.curr-code.
       END.     
       FIND FIRST shipto WHERE shipto.company = g_company 
                       AND shipto.cust-no = ar-inv.cust-no
                       NO-LOCK NO-ERROR.
       IF AVAIL shipto THEN ASSIGN ar-inv.ship-id = shipto.ship-id
                               ship_name:SCREEN-VALUE IN FRAME {&FRAME-NAME} = shipto.ship-name.
       FIND currency WHERE currency.company = g_company
                   AND currency.c-code = ar-inv.curr-code[1] NO-LOCK NO-ERROR.
       IF AVAIL currency THEN ar-inv.ex-rate = currency.ex-rate .
    END.
    IF adm-new-record THEN DO:
        loop:
        REPEAT:
        FIND FIRST ar-ctrl WHERE
            ar-ctrl.company EQ g_company
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

            IF AVAILABLE ar-ctrl THEN
            DO:
                ar-ctrl.last-inv = ar-ctrl.last-inv + 1.
                RELEASE ar-ctrl.
                LEAVE loop.
            END.

        END.
    END. /*adm-new-record*/

  END. /* adm-adding-record */

  /* gdm - 02270909 */
  FIND FIRST ar-invl EXCLUSIVE-LOCK
      WHERE ar-invl.x-no EQ ar-inv.x-no NO-ERROR.
  IF AVAIL ar-invl THEN DO:

      ASSIGN ar-invl.inv-no = ar-inv.inv-no.         

  END.
  RELEASE ar-invl.
  /* gdm - 02270909 end */

  {ar/ar-invk.i ar-inv}
  RUN dispatch ('display-fields').

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
          
  DO WITH FRAME F-Main:
    DISABLE tbEdiInvoice.
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

  DEF VAR X AS INT NO-UNDO.
  DEF VAR Y AS INT NO-UNDO.

  DEF BUFFER b-ar-inv FOR ar-inv.


  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
     x = 0
     y = 0.

  FIND LAST bf-inv USE-INDEX x-no NO-LOCK NO-ERROR.
  X = IF AVAIL bf-inv THEN bf-inv.x-no + 1 ELSE 1.
  FIND FIRST ar-ctrl WHERE ar-ctrl.company = g_company NO-LOCK NO-ERROR.
  Y = IF AVAIL ar-ctrl THEN ar-ctrl.last-inv + 1 ELSE 1.

  DO WHILE TRUE:
    FIND FIRST b-ar-inv
        WHERE b-ar-inv.company EQ g_company
          AND b-ar-inv.inv-no  EQ y
        NO-LOCK NO-ERROR.
    FIND FIRST inv-head
        WHERE inv-head.company EQ g_company
          AND inv-head.inv-no  EQ y
        NO-LOCK NO-ERROR.
    IF NOT AVAIL b-ar-inv AND NOT AVAIL inv-head THEN LEAVE.

    y = y + 1.
  END.

  assign
   ar-inv.company  = g_company
   ar-inv.inv-date = today
   ar-inv.x-no     = x 
   ar-inv.inv-no   = y 
   .

   RUN dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable V-table-Win 
PROCEDURE local-disable :
/* Code placed here will execute PRIOR to standard behavior. */
 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME F-Main:
    DISABLE tbEdiInvoice.
  END.
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
  DO WITH FRAME {&FRAME-NAME}:
          
    IF AVAILABLE ar-inv THEN 
    FIND FIRST shipto NO-LOCK
        WHERE shipto.company EQ ar-inv.company
          AND shipto.cust-no EQ ar-inv.cust-no:SCREEN-VALUE
          AND shipto.ship-id EQ ar-inv.ship-id:SCREEN-VALUE
        NO-ERROR.
    IF AVAIL shipto THEN ship_name:SCREEN-VALUE = shipto.ship-name.
    IF AVAILABLE ar-inv THEN 
    FIND FIRST currency NO-LOCK
        WHERE currency.company EQ ar-inv.company 
          AND currency.c-code  EQ ar-inv.curr-code[1]:SCREEN-VALUE
        NO-ERROR.
    IF AVAIL currency THEN
      ar-inv.ex-rate:SCREEN-VALUE = STRING(currency.ex-rate).
 
    If NOT adm-new-record THEN 
      tbEdiInvoice:SCREEN-VALUE = (IF AVAILABLE(ar-inv) AND ar-inv.ediInvoice = YES THEN "YES" ELSE "NO").
    ELSE
      tbEdiInvoice:SCREEN-VALUE = (IF lEdi810NewValue = YES THEN "YES" ELSE "NO").

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable V-table-Win 
PROCEDURE local-enable :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
           
  DO WITH FRAME F-Main:
    DISABLE tbEdiInvoice.
      IF NOT  lEDI810Visible THEN 
          tbEdiInvoice:VISIBLE = FALSE.
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
  DEF VAR ll-new-record AS LOG NO-UNDO.
  DEF VAR v-oldinv LIKE ar-inv.inv-no NO-UNDO.
  DEFINE VARIABLE lIsAnEDI AS LOGICAL NO-UNDO.
  DEFINE BUFFER bf-ar-invl FOR ar-invl.

  /* Code placed here will execute PRIOR to standard behavior. */
  /*========= validation=========*/
  ASSIGN v-oldinv = ar-inv.inv-no.

  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
 
  DO WITH FRAME  {&FRAME-NAME}:
  DISABLE tbEdiInvoice.
  END.
  DO WITH FRAME {&FRAME-NAME}:
     {&methods/lValidateError.i YES}
     {VALIDATE/stax.i ar-inv.tax-code}
     FIND FIRST shipto WHERE shipto.company = g_company 
                       AND shipto.cust-no = ar-inv.cust-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                       AND shipto.ship-id = ar-inv.ship-id:SCREEN-VALUE
                       NO-LOCK NO-ERROR.
     IF NOT AVAIL shipto THEN DO:
        MESSAGE "Invalid Ship To. Try Help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ar-inv.ship-id.
        RETURN NO-APPLY.
     END.
     ship_name:SCREEN-VALUE = shipto.ship-name.
     FIND FIRST terms WHERE terms.t-code = ar-inv.terms:SCREEN-VALUE IN FRAME {&FRAME-NAME} NO-LOCK NO-ERROR.
     IF NOT AVAIL terms THEN DO:
        MESSAGE "Invalid Terms. Try Help. " VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO ar-inv.terms.
        RETURN NO-APPLY.
     END.

     
     IF DATE(ar-inv.inv-date:SCREEN-VALUE) > TODAY THEN DO:
        MESSAGE "Invoice Date is Past Today, Continue?" VIEW-AS ALERT-BOX WARNING BUTTON YES-NO
                      UPDATE ll-ans AS LOG.
        IF ll-ans THEN .
        ELSE do:
            APPLY "entry" TO ar-inv.inv-date.
            RETURN NO-APPLY.
        END.
      {&methods/lValidateError.i NO}
    END.

    RUN valid-due-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    {&methods/lValidateError.i YES}
    IF NOT lv-due-calckt THEN DO: RUN pInvDueDate.  /* recalc due-date */
    END.
     FIND FIRST currency WHERE currency.company = g_company 
                          AND currency.c-code = ar-inv.curr-code[1]:SCREEN-VALUE 
                           NO-LOCK NO-ERROR.
     IF AVAIL currency AND ar-inv.ex-rate:SCREEN-VALUE = "0"
         THEN ar-inv.ex-rate:SCREEN-VALUE = string(currency.ex-rate).
     ELSE IF NOT AVAIL currency AND ar-inv.curr-code[1]:SCREEN-VALUE <> ""
     THEN DO:
       MESSAGE "Invalid Currency Code. Try Help. " VIEW-AS ALERT-BOX.
       APPLY "entry" TO ar-inv.curr-code[1].
       RETURN NO-APPLY.
     END.
  END.
  {&methods/lValidateError.i YES}
  /* gdm - 02270909*/
  IF STRING(v-oldinv) NE ar-inv.inv-no:SCREEN-VALUE THEN
  DO:
     RUN valid-inv-no NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
  /* ======= end validation========*/

  ll-new-record = adm-new-record.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF ll-new-record THEN DO:
     DEF VAR char-hdl AS cha NO-UNDO.
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"add-line-target",OUTPUT char-hdl).
     RUN add-line IN WIDGET-HANDLE(char-hdl).
     adm-new-record = NO .
     adm-adding-record = NO .
  END.
  lv-due-calckt = NO.

  /* task 02150601 */
  FOR EACH bf-ar-invl EXCLUSIVE-LOCK 
      WHERE bf-ar-invl.x-no EQ ar-inv.x-no:
      IF bf-ar-invl.po-no NE ar-inv.po-no THEN
          bf-ar-invl.po-no = ar-inv.po-no.
  END. /* each bf-ar-invl */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-cust-no V-table-Win 
PROCEDURE new-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE riShipTo AS ROWID NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST cust
        WHERE cust.company EQ g_company
          AND cust.cust-no EQ ar-inv.cust-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.    
    IF AVAIL cust THEN DO:
      ll-got-cust-info = YES. 
      ASSIGN ar-inv.cust-name:SCREEN-VALUE = cust.NAME
          ar-inv.terms:SCREEN-VALUE = cust.terms
          ar-inv.carrier:SCREEN-VALUE = cust.carrier
          ar-inv.tax-code:SCREEN-VALUE = cust.tax-gr
          ar-inv.curr-code[1]:SCREEN-VALUE = cust.curr-code.
      IF cust.curr-code = "" THEN DO:
         FIND company WHERE company.company = g_company NO-LOCK NO-ERROR.
         IF AVAIL company THEN ar-inv.curr-code[1]:SCREEN-VALUE = company.curr-code.
      END.
      
      RUN Customer_GetDefaultShipTo IN hdCustomerProcs(
          INPUT  g_company,
          INPUT  cust.cust-no,
          OUTPUT riShipTo
          ).
          
      FIND FIRST shipto NO-LOCK 
           WHERE ROWID(shipto) EQ riShipTo
           NO-ERROR.
        
        IF AVAILABLE shipto THEN 
            ASSIGN  
                ar-inv.ship-id:SCREEN-VALUE = shipto.ship-id
                ship_name:SCREEN-VALUE      = shipto.ship-name
                .   
            
      FIND currency WHERE currency.company = g_company
                   AND currency.c-code = cust.curr-code NO-LOCK NO-ERROR.
      IF AVAIL currency THEN DISPLAY currency.ex-rate @ ar-inv.ex-rate WITH FRAME {&FRAME-NAME}.
      FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
      IF AVAIL terms THEN ASSIGN ar-inv.terms-d:SCREEN-VALUE = terms.dscr
                              ar-inv.disc-%:SCREEN-VALUE = string(terms.disc-rate)
                              ar-inv.disc-days:SCREEN-VALUE = STRING(terms.disc-days)
                              .
       RUN pInvDueDate .
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-terms V-table-Win 
PROCEDURE new-terms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST terms WHERE terms.t-code EQ ar-inv.terms:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL terms THEN ar-inv.terms-d:SCREEN-VALUE = terms.dscr.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateCredit V-table-Win 
PROCEDURE pCreateCredit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER cType AS CHARACTER NO-UNDO .  

DEFINE VARIABLE lDummy AS LOGICAL NO-UNDO.
DEFINE VARIABLE X AS INTEGER NO-UNDO.
DEFINE VARIABLE Y AS INTEGER NO-UNDO.

  DEF BUFFER b-ar-inv FOR ar-inv.
  DEFINE BUFFER bff-ar-inv FOR ar-inv .
  DEFINE BUFFER bff-ar-invl FOR ar-invl .
  DEFINE BUFFER bf-invl FOR ar-invl .

  ASSIGN
     x = 0
     y = 0.

  FIND LAST bf-inv USE-INDEX x-no NO-LOCK NO-ERROR.
  X = IF AVAIL bf-inv THEN bf-inv.x-no + 1 ELSE 1.
  FIND FIRST ar-ctrl WHERE ar-ctrl.company = g_company NO-LOCK NO-ERROR.
  Y = IF AVAIL ar-ctrl THEN ar-ctrl.last-inv + 1 ELSE 1.

  DO WHILE TRUE:
    FIND FIRST b-ar-inv
        WHERE b-ar-inv.company EQ g_company
          AND b-ar-inv.inv-no  EQ y
        NO-LOCK NO-ERROR.
    FIND FIRST inv-head
        WHERE inv-head.company EQ g_company
          AND inv-head.inv-no  EQ y
        NO-LOCK NO-ERROR.
    IF NOT AVAIL b-ar-inv AND NOT AVAIL inv-head THEN LEAVE.

    y = y + 1.
  END.
 CREATE ar-inv .
  assign
   ar-inv.company  = g_company
   ar-inv.inv-date = today
   ar-inv.x-no     = x 
   ar-inv.inv-no   = y 
   ar-inv.posted   = FALSE 
   ar-inv.USER-ID  = USERID(LDBNAME(1)) 
   ar-inv.upd-date = TODAY 
   ar-inv.upd-time = TIME 
   .

  FIND FIRST bff-ar-inv NO-LOCK 
      WHERE bff-ar-inv.company EQ g_company 
        AND bff-ar-inv.inv-no EQ integer(Is-add-dup-inv) NO-ERROR .
  IF AVAIL bff-ar-inv THEN DO:
      BUFFER-COPY bff-ar-inv EXCEPT company inv-date x-no inv-no USER-ID upd-date upd-time posted rec_key TO ar-inv .
      ASSIGN 
          ar-inv.t-comm        = ar-inv.t-comm * -1 
          ar-inv.t-cost        = ar-inv.t-cost * -1 
          ar-inv.freight       = ar-inv.freight * -1 
          ar-inv.fuel          = ar-inv.fuel * -1 
          ar-inv.gross         = ar-inv.gross * -1 
          ar-inv.tax-amt       = ar-inv.tax-amt * -1  
          ar-inv.t-weight      = ar-inv.t-weight * -1
          ar-inv.net           = ar-inv.net * -1
          ar-inv.due           = ar-inv.due * -1
          ar-inv.paid          = 0
          ar-inv.printed       = NO 
          ar-inv.inv-date      = IF oeInvAddDate-Int EQ 0 THEN TODAY ELSE ar-inv.inv-date 
          ar-inv.spare-char-1  = IF cType EQ "Credit" THEN "Credit" ELSE "Rebill" 
          ar-inv.spare-int-2   = INTEGER(Is-add-dup-inv)
          ar-inv.t-sales       = ar-inv.t-sales * -1      .
          
          FIND FIRST cust NO-LOCK 
              WHERE cust.company EQ g_company
              AND cust.cust-no EQ ar-inv.cust-no
              NO-ERROR.  
          IF AVAIL cust THEN
          FIND FIRST terms NO-LOCK 
              WHERE terms.t-code EQ cust.terms
             NO-ERROR.
            IF AVAIL terms THEN
                ASSIGN  ar-inv.due-date = IF oeInvAddDate-Int EQ 0 THEN TODAY + terms.net-days ELSE ar-inv.due-date .
            ELSE ar-inv.due-date = IF oeInvAddDate-Int EQ 0 THEN TODAY ELSE ar-inv.due-date .

   FOR EACH bff-ar-invl NO-LOCK 
       WHERE bff-ar-invl.company EQ bff-ar-inv.company
         AND bff-ar-invl.inv-no  EQ bff-ar-inv.inv-no
         AND bff-ar-invl.x-no    EQ bff-ar-inv.x-no :
       
       CREATE bf-invl .
       ASSIGN bf-invl.company = g_company
              bf-invl.x-no    = X
              bf-invl.inv-no  = Y 
              bf-invl.b-no    = 0
              bf-invl.upd-date = TODAY 
              bf-invl.upd-time = TIME
              bf-invl.posted = FALSE
           .
        BUFFER-COPY bff-ar-invl EXCEPT company x-no inv-no b-no upd-date upd-time posted rec_key TO bf-invl .
        ASSIGN
            bf-invl.amt-msf    = bf-invl.amt-msf * -1 
            bf-invl.cost       = bf-invl.cost * -1 
            //bf-invl.disc       = bf-invl.disc * -1 
            bf-invl.inv-qty    = bf-invl.inv-qty * -1 
            bf-invl.qty        = bf-invl.qty * -1
            bf-invl.t-freight  = bf-invl.t-freight * -1 
            bf-invl.t-fuel     = bf-invl.t-fuel * -1 
            bf-invl.amt        = bf-invl.amt * -1
            bf-invl.t-cost     = bf-invl.t-cost * -1  
            bf-invl.prep-amt   = bf-invl.prep-amt * -1
            bf-invl.std-fix-cost = bf-invl.std-fix-cost * -1 
            bf-invl.std-lab-cost = bf-invl.std-lab-cost * -1 
            bf-invl.std-mat-cost = bf-invl.std-mat-cost * -1
            bf-invl.std-tot-cost = bf-invl.std-tot-cost * -1  
            bf-invl.std-var-cost = bf-invl.std-var-cost * -1 

            bf-invl.prep-cost    = bf-invl.prep-cost * -1
            bf-invl.ship-qty     = bf-invl.ship-qty * -1
            bf-invl.sf-sht       = bf-invl.sf-sht * -1
            bf-invl.amt-msf      = bf-invl.amt-msf * -1
              .

        
   END. /* for each bff-ar-invl */
  END. /* if avail bff-ar-inv*/

  FIND CURRENT ar-inv NO-LOCK NO-ERROR .
 
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
  RUN New_record IN WIDGET-HANDLE(char-hdl) (ROWID(ar-inv)) NO-ERROR.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCreateDuplicate V-table-Win 
PROCEDURE pCreateDuplicate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER cType AS CHARACTER NO-UNDO . 


 DEF VAR X AS INT NO-UNDO.
  DEF VAR Y AS INT NO-UNDO.

  DEF BUFFER b-ar-inv FOR ar-inv.
  DEFINE BUFFER bff-ar-inv FOR ar-inv .
  DEFINE BUFFER bff-ar-invl FOR ar-invl .
  DEFINE BUFFER bf-invl FOR ar-invl .

  ASSIGN
     x = 0
     y = 0.

  FIND LAST bf-inv USE-INDEX x-no NO-LOCK NO-ERROR.
  X = IF AVAIL bf-inv THEN bf-inv.x-no + 1 ELSE 1.
  FIND FIRST ar-ctrl WHERE ar-ctrl.company = g_company NO-LOCK NO-ERROR.
  Y = IF AVAIL ar-ctrl THEN ar-ctrl.last-inv + 1 ELSE 1.

  DO WHILE TRUE:
    FIND FIRST b-ar-inv
        WHERE b-ar-inv.company EQ g_company
          AND b-ar-inv.inv-no  EQ y
        NO-LOCK NO-ERROR.
    FIND FIRST inv-head
        WHERE inv-head.company EQ g_company
          AND inv-head.inv-no  EQ y
        NO-LOCK NO-ERROR.
    IF NOT AVAIL b-ar-inv AND NOT AVAIL inv-head THEN LEAVE.

    y = y + 1.
  END.
 CREATE ar-inv .
  assign
   ar-inv.company  = g_company
   ar-inv.inv-date = today
   ar-inv.x-no     = x 
   ar-inv.inv-no   = y 
   ar-inv.posted   = FALSE 
   ar-inv.USER-ID  = USERID(LDBNAME(1)) 
   ar-inv.upd-date = TODAY 
   ar-inv.upd-time = TIME 
   .

  FIND FIRST bff-ar-inv NO-LOCK 
      WHERE bff-ar-inv.company EQ g_company 
        AND bff-ar-inv.inv-no EQ integer(Is-add-dup-inv) NO-ERROR .
  IF AVAIL bff-ar-inv THEN DO:
      BUFFER-COPY bff-ar-inv EXCEPT company inv-date x-no inv-no USER-ID upd-date upd-time posted rec_key TO ar-inv .
       ar-inv.inv-date = IF oeInvAddDate-Int EQ 0 THEN TODAY ELSE ar-inv.inv-date .
       ar-inv.printed  = NO .
       ar-inv.paid     = 0 .
       ar-inv.spare-char-1 = IF cType EQ "duplicate" THEN "Copy" ELSE "Rebill" .
       ar-inv.spare-int-2  = INTEGER(Is-add-dup-inv) .
       FIND FIRST cust NO-LOCK 
           WHERE cust.company EQ g_company
           AND cust.cust-no EQ ar-inv.cust-no
           NO-ERROR.  
       IF AVAIL cust THEN
           FIND FIRST terms NO-LOCK 
           WHERE terms.t-code EQ cust.terms
           NO-ERROR.
       IF AVAIL terms THEN
           ASSIGN  ar-inv.due-date = IF oeInvAddDate-Int EQ 0 THEN TODAY + terms.net-days ELSE ar-inv.due-date .
       ELSE ar-inv.due-date = IF oeInvAddDate-Int EQ 0 THEN TODAY ELSE ar-inv.due-date .

   FOR EACH bff-ar-invl NO-LOCK 
       WHERE bff-ar-invl.company EQ bff-ar-inv.company
         AND bff-ar-invl.inv-no  EQ bff-ar-inv.inv-no
         AND bff-ar-invl.x-no    EQ bff-ar-inv.x-no :
       
       CREATE bf-invl .
       ASSIGN bf-invl.company = g_company
              bf-invl.x-no    = X
              bf-invl.inv-no  = Y 
              bf-invl.b-no    = 0
              bf-invl.upd-date = TODAY 
              bf-invl.upd-time = TIME
              bf-invl.posted = FALSE
           .
        BUFFER-COPY bff-ar-invl EXCEPT company x-no inv-no b-no upd-date upd-time posted rec_key TO bf-invl .
        
   END. /* for each bff-ar-invl */
  END. /* if avail bff-ar-inv*/

  FIND CURRENT ar-inv NO-LOCK NO-ERROR .
  
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
  RUN New_record IN WIDGET-HANDLE(char-hdl) (ROWID(ar-inv)) NO-ERROR. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInvDueDate V-table-Win 
PROCEDURE pInvDueDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


DO WITH FRAME {&FRAME-NAME}:
    ASSIGN ar-inv.due-date:SCREEN-VALUE = STRING( DYNAMIC-FUNCTION("GetInvDueDate", date(ar-inv.inv-date:SCREEN-VALUE IN FRAME {&FRAME-NAME}),cocode ,ar-inv.terms:SCREEN-VALUE IN FRAME {&FRAME-NAME} )).
END.


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

   DO WITH FRAME  {&FRAME-NAME}:
     IF lEDI810Visible THEN 
       ENABLE tbEdiInvoice.
     ELSE 
       tbEDIInvoice:VISIBLE = FALSE.
   END.
   IF NOT adm-new-record THEN
     IF ar-inv.posted THEN DO:
        DEF VAR char-hdl AS cha NO-UNDO.
        MESSAGE "This invoice has been posted. No changes are allowed!" 
                VIEW-AS ALERT-BOX ERROR.
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"tableio-source", OUTPUT char-hdl).
        RUN apply-cancel IN WIDGET-HANDLE(char-hdl).
     END.

     ELSE
     DO WITH FRAME {&FRAME-NAME}:
       ar-inv.cust-no:SENSITIVE = NO.
       APPLY "entry" TO ar-inv.ship-id.
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query V-table-Win 
PROCEDURE reopen-query :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND CURRENT ar-inv NO-LOCK.
  RUN dispatch ('display-fields').


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-key V-table-Win  adm/support/_key-snd.p
PROCEDURE send-key :
/*------------------------------------------------------------------------------
  Purpose:     Sends a requested KEY value back to the calling
               SmartObject.
  Parameters:  <see adm/template/sndkytop.i>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/sndkytop.i}

  /* Return the key value associated with each key case.             */
  {src/adm/template/sndkycas.i "x-no" "ar-inv" "x-no"}
  {src/adm/template/sndkycas.i "check-no" "ar-inv" "check-no"}
  {src/adm/template/sndkycas.i "company" "ar-inv" "company"}
  {src/adm/template/sndkycas.i "Carrier" "ar-inv" "Carrier"}

  /* Close the CASE statement and end the procedure.                 */
  {src/adm/template/sndkyend.i}

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
  {src/adm/template/snd-list.i "ar-inv"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no V-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
     ar-inv.cust-no:SCREEN-VALUE = CAPS(ar-inv.cust-no:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ g_company
                      AND cust.cust-no EQ ar-inv.cust-no:SCREEN-VALUE
                      AND lookup(cust.active,"A,E") > 0 )        
    THEN DO:
      MESSAGE "Invalid Customer, try help..." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-due-date V-table-Win 
PROCEDURE valid-due-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF DATE(ar-inv.due-date:SCREEN-VALUE) LT DATE(ar-inv.inv-date:SCREEN-VALUE) THEN DO:
      MESSAGE TRIM(ar-inv.due-date:LABEL) + " may not be before " +
              TRIM(ar-inv.inv-date:LABEL) + "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ar-inv.due-date.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-inv-no V-table-Win 
PROCEDURE valid-inv-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.

  DEF BUFFER b-ar-inv FOR ar-inv.
  DEF BUFFER b-ar-invl FOR ar-invl.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF INT(ar-inv.inv-no:SCREEN-VALUE) LE 0 THEN
      lv-msg = "may not be zero".

    ELSE
    IF CAN-FIND(FIRST b-ar-inv
                WHERE b-ar-inv.company EQ cocode
                  AND b-ar-inv.inv-no  EQ INT(ar-inv.inv-no:SCREEN-VALUE)
                  AND ROWID(b-ar-inv)  NE ROWID(ar-inv)) THEN
      lv-msg = "already exists".

    /*IF lv-msg EQ "" AND 
       CAN-FIND(FIRST b-ar-invl
                WHERE b-ar-invl.company EQ cocode
                  AND b-ar-invl.inv-no  EQ INT(ar-inv.inv-no:SCREEN-VALUE))
      THEN  lv-msg = "already exists ".*/

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(ar-inv.inv-no:LABEL) +
              " " + TRIM(lv-msg) + " ..."
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

