&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: oe\v-oeinv.w

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
&scoped-define other-ENABLE enable-other
{sys/inc/VAR.i "new shared"}
{custom/globdefs.i}
ASSIGN cocode = g_company
       locode = g_loc.
{oe/oe-sysct1.i NEW}

DEF VAR li-sold-no AS INT NO-UNDO.
DEF VAR ll-cust-displayed AS LOG NO-UNDO.
DEF BUFFER bf-head FOR inv-head.
DEF BUFFER bf-line FOR inv-line.
DEF BUFFER bf-line-2 FOR inv-line.
def VAR ll-cred-lim AS LOG INIT NO NO-UNDO.
DEF VAR ll-over-limit AS LOG NO-UNDO.
DEF VAR ll-use-soldto AS LOG FORMAT "SoldTo/ShipTo" NO-UNDO.
DEF VAR ll-soldto-ans AS LOG NO-UNDO.
DEF VAR v-msg AS CHAR NO-UNDO.
DEFINE VARIABLE ll-new-shipto AS LOGICAL  INIT NO  NO-UNDO.
DEF NEW SHARED VAR v-ship-no LIKE shipto.ship-no.
DEF VAR v-cash-sale AS LOG NO-UNDO.

&SCOPED-DEFINE other-enable enable-other

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
&Scoped-define EXTERNAL-TABLES inv-head
&Scoped-define FIRST-EXTERNAL-TABLE inv-head


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR inv-head.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS inv-head.inv-date inv-head.cust-no ~
inv-head.sold-no inv-head.contact inv-head.tax-gr inv-head.terms ~
inv-head.carrier inv-head.frt-pay inv-head.fob-code inv-head.t-inv-weight ~
inv-head.t-inv-freight inv-head.t-comm 
&Scoped-define ENABLED-TABLES inv-head
&Scoped-define FIRST-ENABLED-TABLE inv-head
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-41 btnCalendar-1 
&Scoped-Define DISPLAYED-FIELDS inv-head.printed inv-head.inv-no ~
inv-head.inv-date inv-head.bol-no inv-head.r-no inv-head.cust-no ~
inv-head.sold-no inv-head.cust-name inv-head.sold-name inv-head.addr[1] ~
inv-head.sold-addr[1] inv-head.addr[2] inv-head.sold-addr[2] inv-head.city ~
inv-head.state inv-head.zip inv-head.sold-city inv-head.sold-state ~
inv-head.sold-zip inv-head.contact inv-head.tax-gr inv-head.terms ~
inv-head.terms-d inv-head.carrier inv-head.frt-pay inv-head.fob-code ~
inv-head.t-inv-weight inv-head.t-inv-tax inv-head.t-inv-freight ~
inv-head.t-inv-rev inv-head.t-comm inv-head.t-inv-cost 
&Scoped-define DISPLAYED-TABLES inv-head
&Scoped-define FIRST-DISPLAYED-TABLE inv-head
&Scoped-Define DISPLAYED-OBJECTS inv-status fi_PO 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS inv-head.inv-no inv-status inv-head.r-no ~
inv-head.cust-name inv-head.sold-name inv-head.addr[1] ~
inv-head.sold-addr[1] inv-head.addr[2] inv-head.sold-addr[2] inv-head.city ~
inv-head.state inv-head.zip inv-head.sold-city inv-head.sold-state ~
inv-head.sold-zip inv-head.terms-d 
&Scoped-define ROW-AVAILABLE btnCalendar-1 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
r-no|y|y|ASI.inv-head.r-no
company||y|ASI.inv-head.company
Carrier||y|ASI.inv-head.Carrier
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "r-no",
     Keys-Supplied = "r-no,company,Carrier"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-stat-dscr V-table-Win 
FUNCTION get-stat-dscr RETURNS CHARACTER
  ( ipcStat AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getCashSaleLog V-table-Win 
FUNCTION getCashSaleLog RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE fi_PO AS CHARACTER FORMAT "X(256)":U 
     LABEL "Cust PO#" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE inv-status AS CHARACTER FORMAT "X(8)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 16.43.

DEFINE RECTANGLE RECT-41
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 131 BY 4.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     inv-head.printed AT ROW 1.43 COL 120 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     inv-head.inv-no AT ROW 1.48 COL 16 COLON-ALIGNED
          LABEL "Invoice#"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     inv-head.inv-date AT ROW 1.48 COL 45 COLON-ALIGNED
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     btnCalendar-1 AT ROW 1.48 COL 63
     inv-head.bol-no AT ROW 1.48 COL 81 COLON-ALIGNED
          LABEL "BOL#"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     inv-status AT ROW 2.38 COL 120 COLON-ALIGNED
     inv-head.r-no AT ROW 3.33 COL 120 COLON-ALIGNED
          LABEL "Seq"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY .95
     inv-head.cust-no AT ROW 3.38 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     inv-head.sold-no AT ROW 3.38 COL 83 COLON-ALIGNED
          LABEL "Ship to"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     inv-head.cust-name AT ROW 4.33 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 49 BY 1
     inv-head.sold-name AT ROW 4.33 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 49 BY 1
     inv-head.addr[1] AT ROW 5.29 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 49 BY 1
     inv-head.sold-addr[1] AT ROW 5.29 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 49 BY 1
     inv-head.addr[2] AT ROW 6.24 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 49 BY 1
     inv-head.sold-addr[2] AT ROW 6.24 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 49 BY 1
     inv-head.city AT ROW 7.19 COL 16 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     inv-head.state AT ROW 7.19 COL 41 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     inv-head.zip AT ROW 7.19 COL 47 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     inv-head.sold-city AT ROW 7.19 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25 BY 1
     inv-head.sold-state AT ROW 7.19 COL 108 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     inv-head.sold-zip AT ROW 7.19 COL 114 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     inv-head.contact AT ROW 8.14 COL 16 COLON-ALIGNED WIDGET-ID 2
          LABEL "Contact"
          VIEW-AS FILL-IN 
          SIZE 49 BY 1
     inv-head.tax-gr AT ROW 9.33 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     fi_PO AT ROW 9.33 COL 113 COLON-ALIGNED WIDGET-ID 4
     inv-head.terms AT ROW 10.29 COL 27 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     inv-head.terms-d AT ROW 10.29 COL 37 COLON-ALIGNED NO-LABEL FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 44 BY 1
     inv-head.carrier AT ROW 10.29 COL 113 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     inv-head.frt-pay AT ROW 11.24 COL 27 COLON-ALIGNED
          LABEL "Freight Terms"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     inv-head.fob-code AT ROW 11.24 COL 113 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY 1
     inv-head.t-inv-weight AT ROW 13.38 COL 41 COLON-ALIGNED
          LABEL "Total Weight"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     inv-head.t-inv-tax AT ROW 13.38 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY 1
     inv-head.t-inv-freight AT ROW 14.33 COL 41 COLON-ALIGNED
          LABEL "Total Freight"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     inv-head.t-inv-rev AT ROW 14.38 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     inv-head.t-comm AT ROW 15.29 COL 41 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.2 BY 1
     inv-head.t-inv-cost AT ROW 15.29 COL 105 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18.8 BY 1
     "I N V O I C E  T O T A L S" VIEW-AS TEXT
          SIZE 32 BY 1.19 AT ROW 11.95 COL 84 RIGHT-ALIGNED
          FGCOLOR 9 
     RECT-1 AT ROW 1.19 COL 1
     RECT-41 AT ROW 12.43 COL 9
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.inv-head
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
         WIDTH              = 144.
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

/* SETTINGS FOR FILL-IN inv-head.addr[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.addr[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.bol-no IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN inv-head.city IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.contact IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.cust-name IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_PO IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN inv-head.inv-date IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.inv-no IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN inv-status IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.printed IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN inv-head.r-no IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN inv-head.sold-addr[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.sold-addr[2] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.sold-city IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.sold-name IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.sold-no IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.sold-state IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.sold-zip IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.state IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN inv-head.t-inv-cost IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN inv-head.t-inv-freight IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.t-inv-rev IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN inv-head.t-inv-tax IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN inv-head.t-inv-weight IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN inv-head.terms-d IN FRAME F-Main
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN inv-head.zip IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR TEXT-LITERAL "I N V O I C E  T O T A L S"
          SIZE 32 BY 1.19 AT ROW 11.95 COL 84 RIGHT-ALIGNED             */

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

    DEF VAR lw-focus AS WIDGET NO-UNDO.


    lw-focus = FOCUS.

    case lw-focus:name :
         when "cust-no" then do:
              run windows/l-custact.w (g_company, lw-focus:screen-value, output char-val, output look-recid).
              FIND cust WHERE recid(cust) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL cust THEN DO:
                inv-head.cust-no:SCREEN-VALUE = CAPS(cust.cust-no).
                APPLY "value-changed" TO lw-focus.
              END.
         end.  
         WHEN "sold-no" THEN DO:
              IF ll-use-soldto THEN DO:
                RUN windows/l-soldto.w (g_company, inv-head.cust-no:SCREEN-VALUE, lw-focus:SCREEN-VALUE, OUTPUT char-val).
                IF char-val NE "" THEN inv-head.sold-no:SCREEN-VALUE = ENTRY(2,char-val).
              END.
              ELSE DO:
                RUN windows/l-shipto.w (g_company, "", inv-head.cust-no:SCREEN-VALUE, lw-focus:SCREEN-VALUE, OUTPUT char-val).
                IF char-val NE "" THEN inv-head.sold-no:SCREEN-VALUE = ENTRY(1,char-val).
              END.
              IF char-val NE "" THEN APPLY "value-changed" TO lw-focus.
         END.
         /*when "sman" then do:
              run windows/l-sman.w (g_company, output char-val).
              if char-val <> "" then do:
                 case lw-focus:index:
                      when 1 then assign inv-head.sman[1]:screen-value = entry(1,char-val)
                                         inv-head.sname[1]:screen-value = entry(2,char-val)
                                         inv-head.s-comm[1]:screen-value = entry(3,char-val)
                                         .
                      when 2 then assign inv-head.sman[2]:screen-value = entry(1,char-val)
                                         inv-head.sname[2]:screen-value = entry(2,char-val)
                                         inv-head.s-comm[2]:screen-value = entry(3,char-val)
                                         .
                      when 3 then assign inv-head.sman[3]:screen-value = entry(1,char-val)
                                         inv-head.sname[3]:screen-value = entry(2,char-val)
                                         inv-head.s-comm[3]:screen-value = entry(3,char-val)
                                         .
                 end.
              end.
         end.  
         */
         when "tax-gr" then do:
              run windows/l-stax.w (g_company, lw-focus:screen-value, output char-val).
              if char-val <> "" then assign lw-focus:screen-value = entry(1,char-val).
         end.         
         when "carrier" then do:
             FIND FIRST inv-line NO-LOCK 
                WHERE inv-line.company EQ inv-head.company
                  AND inv-line.r-no    EQ inv-head.r-no NO-ERROR.
             IF AVAIL inv-line THEN 
                 FIND FIRST oe-boll NO-LOCK WHERE oe-boll.company EQ inv-line.company
                        AND oe-boll.b-no    EQ inv-line.b-no
                        AND oe-boll.ord-no  EQ inv-line.ord-no
                        AND oe-boll.i-no    EQ inv-line.i-no
                        AND oe-boll.line    EQ inv-line.line
                        AND oe-boll.po-no   EQ inv-line.po-no NO-ERROR.
              run windows/l-carrie.w (g_company,oe-boll.loc, lw-focus:screen-value, output char-val).
              if char-val <> "" then assign lw-focus:screen-value = entry(1,char-val).
         end.
         when "terms" then do:
              run windows/l-terms.w (g_company, lw-focus:screen-value, output char-val).
              if char-val <> "" AND entry(1,char-val) NE lw-focus:SCREEN-VALUE THEN DO:
                lw-focus:screen-value = entry(1,char-val).
                APPLY "value-changed" TO lw-focus.
              END.
         end.
    end case.

    APPLY "entry" TO lw-focus.
    return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 V-table-Win
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i inv-head.inv-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-head.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.carrier V-table-Win
ON LEAVE OF inv-head.carrier IN FRAME F-Main /* Carrier */
DO:
   IF LASTKEY = -1  THEN RETURN.
   RUN valid-carrier NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-head.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.cust-no V-table-Win
ON LEAVE OF inv-head.cust-no IN FRAME F-Main /* Cust. # */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.cust-no V-table-Win
ON VALUE-CHANGED OF inv-head.cust-no IN FRAME F-Main /* Cust. # */
DO:
  FIND cust
      WHERE cust.company EQ g_company
        AND cust.cust-no EQ {&self-name}:SCREEN-VALUE
      NO-LOCK NO-ERROR.
  IF AVAIL cust THEN RUN display-cust-detail (RECID(cust)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-head.fob-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.fob-code V-table-Win
ON LEAVE OF inv-head.fob-code IN FRAME F-Main /* FOB Code */
DO:
   IF LASTKEY = -1  THEN RETURN.
   RUN valid-fob NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-head.inv-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.inv-date V-table-Win
ON HELP OF inv-head.inv-date IN FRAME F-Main /* Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-head.sold-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.sold-no V-table-Win
ON ENTRY OF inv-head.sold-no IN FRAME F-Main /* Ship to */
DO:
  IF inv-head.bol-no <> 0 THEN DO:
     APPLY "tab" TO SELF.
     RETURN NO-APPLY.
  END.
  RUN soldto-question.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.sold-no V-table-Win
ON LEAVE OF inv-head.sold-no IN FRAME F-Main /* Ship to */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN soldto-question.
    RUN valid-sold-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.sold-no V-table-Win
ON VALUE-CHANGED OF inv-head.sold-no IN FRAME F-Main /* Ship to */
DO:
  IF ll-use-soldto THEN DO:
    FIND soldto
        WHERE soldto.company EQ g_company
          AND soldto.cust-no EQ inv-head.cust-no:SCREEN-VALUE
          AND soldto.sold-id EQ inv-head.sold-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL soldto THEN
      ASSIGN
       inv-head.sold-no:SCREEN-VALUE      = soldto.sold-id
       inv-head.sold-name:SCREEN-VALUE    = soldto.sold-name
       inv-head.sold-addr[1]:SCREEN-VALUE = soldto.sold-addr[1]
       inv-head.sold-addr[2]:SCREEN-VALUE = soldto.sold-addr[2]
       inv-head.sold-city:SCREEN-VALUE    = soldto.sold-city
       inv-head.sold-state:SCREEN-VALUE   = soldto.sold-state
       inv-head.sold-zip:SCREEN-VALUE     = soldto.sold-zip.
  END.

  ELSE DO:
    FIND shipto
        WHERE shipto.company EQ g_company
          AND shipto.cust-no EQ inv-head.cust-no:SCREEN-VALUE
          AND shipto.ship-id EQ inv-head.sold-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL shipto THEN
      ASSIGN
       inv-head.sold-no:SCREEN-VALUE      = shipto.ship-id
       inv-head.sold-name:SCREEN-VALUE    = shipto.ship-name
       inv-head.sold-addr[1]:SCREEN-VALUE = shipto.ship-addr[1]
       inv-head.sold-addr[2]:SCREEN-VALUE = shipto.ship-addr[2]
       inv-head.sold-city:SCREEN-VALUE    = shipto.ship-city
       inv-head.sold-state:SCREEN-VALUE   = shipto.ship-state
       inv-head.sold-zip:SCREEN-VALUE     = shipto.ship-zip.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-head.t-inv-freight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.t-inv-freight V-table-Win
ON LEAVE OF inv-head.t-inv-freight IN FRAME F-Main /* Total Freight */
DO:
   IF LASTKEY = -1  THEN RETURN.
   RUN valid-freight NO-ERROR.
   IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-head.tax-gr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.tax-gr V-table-Win
ON LEAVE OF inv-head.tax-gr IN FRAME F-Main /* Sales Tax Group */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-gr NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inv-head.terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.terms V-table-Win
ON ENTRY OF inv-head.terms IN FRAME F-Main /* Terms Code */
DO:
/* BV - 06031301 - CASH terms is only not editable if NK CASHSALES Log = YES.  
Assuming this was added to allow CASH term to non existent term file*/
 IF inv-head.terms:SCREEN-VALUE EQ "CASH" AND getCashSaleLog() THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.terms V-table-Win
ON LEAVE OF inv-head.terms IN FRAME F-Main /* Terms Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-terms NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inv-head.terms V-table-Win
ON VALUE-CHANGED OF inv-head.terms IN FRAME F-Main /* Terms Code */
DO:
  DEF VAR li AS INT NO-UNDO.


  FIND FIRST terms NO-LOCK
       WHERE terms.company EQ g_company
         AND terms.t-code EQ inv-head.terms:SCREEN-VALUE NO-ERROR.
  IF AVAIL terms THEN
    ASSIGN
     inv-head.terms:SCREEN-VALUE   = terms.t-code
     inv-head.terms-d:SCREEN-VALUE = terms.dscr.

  inv-head.terms:SCREEN-VALUE = CAPS(inv-head.terms:SCREEN-VALUE).
  DO li = 1 TO LENGTH(inv-head.terms:SCREEN-VALUE):
    APPLY "cursor-right" TO inv-head.terms.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
  SESSION:DATA-ENTRY-RETURN = yes.
  RUN oe/oe-sysct.p.
  SUBSCRIBE "eventInvoicePrinted" ANYWHERE.

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
  &ENDIF         

  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addPlusButton V-table-Win 
PROCEDURE addPlusButton :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN dispatch('add-record').

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
    WHEN 'r-no':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = inv-head
           &WHERE = "WHERE inv-head.r-no eq INTEGER(key-value)"
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
  {src/adm/template/row-list.i "inv-head"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "inv-head"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-cust-detail V-table-Win 
PROCEDURE display-cust-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
  DEF VAR v-custype LIKE cust.TYPE NO-UNDO.
  DEF VAR v-ord-limit AS DEC NO-UNDO.
  DEF VAR v-crd-limit AS DEC NO-UNDO.


  find cust where recid(cust) = ip-recid no-lock no-error.
  if avail cust then do with frame {&frame-name} :
    ll-cust-displayed = yes.
    IF inv-head.terms:SCREEN-VALUE NE "CASH" THEN DO:
      inv-head.terms:SCREEN-VALUE = cust.terms.
      find first terms where terms.company eq cocode
                        and terms.t-code  eq cust.terms
               no-lock no-error.
      if avail terms then  inv-head.terms-d:screen-value = terms.dscr.
      else inv-head.terms-d:screen-value = "".
    END.

    assign  inv-head.fob-code:screen-value  = cust.fob-code
            inv-head.frt-pay:screen-value   = if inv-head.frt-pay eq "" then
                                            cust.frt-pay else inv-head.frt-pay
            inv-head.tax-gr:screen-value    = cust.tax-gr
            /*inv-head.f-bill:screen-value    = inv-head.frt-pay:screen-value eq "B"*/ 
            v-custype         = cust.type
            v-ord-limit       = cust.ord-lim
            v-crd-limit       = cust.cr-lim - (cust.acc-bal + cust.ord-bal).


    if inv-head.carrier eq "" then inv-head.carrier:screen-value = cust.carrier.

    assign inv-head.cust-no:screen-value   = cust.cust-no
              inv-head.cust-name:screen-value = cust.name
              inv-head.addr[1]:screen-value   = cust.addr[1]
              inv-head.addr[2]:screen-value   = cust.addr[2]
              inv-head.city:screen-value      = cust.city
              inv-head.state:screen-value     = cust.state
              inv-head.zip:screen-value       = cust.zip
              inv-head.contact:SCREEN-VALUE   = cust.contact.

    /*ASSIGN
       inv-head.sold-no:SCREEN-VALUE      = inv-head.cust-no:SCREEN-VALUE
       inv-head.sold-name:SCREEN-VALUE    = inv-head.cust-name:SCREEN-VALUE
       inv-head.sold-addr[1]:SCREEN-VALUE = inv-head.addr[1]:SCREEN-VALUE
       inv-head.sold-addr[2]:SCREEN-VALUE = inv-head.addr[2]:SCREEN-VALUE
       inv-head.sold-city:SCREEN-VALUE    = inv-head.city:SCREEN-VALUE
       inv-head.sold-state:SCREEN-VALUE   = inv-head.state:SCREEN-VALUE
       inv-head.sold-zip:SCREEN-VALUE     = inv-head.zip:SCREEN-VALUE.*/

    ASSIGN
     inv-head.sold-no:SCREEN-VALUE = cust.cust-no
     ll-use-soldto                 = YES
     ll-soldto-ans                 = NO.

    APPLY "value-changed" TO inv-head.sold-no.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-other V-table-Win 
PROCEDURE enable-other :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ll-soldto-ans = NO.

    IF INT(inv-head.bol-no:SCREEN-VALUE) NE 0 AND NOT adm-new-record THEN DO:
      DISABLE inv-head.sold-no.
      ASSIGN
       ll-use-soldto = NO
       ll-soldto-ans = YES.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eventInvoicePrinted V-table-Win
PROCEDURE eventInvoicePrinted:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    RUN local-display-fields.
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

    IF char-hdl NE "" THEN
        RUN refreshBrowse IN WIDGET-HANDLE(char-hdl) NO-ERROR.
        
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-status V-table-Win 
PROCEDURE get-status :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAMETER op-status AS CHAR NO-UNDO.

   op-status = inv-head.stat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hold-invoice V-table-Win 
PROCEDURE hold-invoice :
/*------------------------------------------------------------------------------
  Purpose:  from oe-inv.x and oe-invh.p    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-choice AS CHAR NO-UNDO.
  DEF VAR v-date   AS DATE NO-UNDO INIT ?.
  DEF VAR v-rowid-list AS CHAR NO-UNDO.
  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR li       AS INT   NO-UNDO.
  DEF BUFFER bf1-head FOR inv-head . 
/*   DEF VAR v-ThisInv-Date AS DATE NO-UNDO INIT ?.   */
/*   DEF VAR v-FirstFG-Date AS DATE NO-UNDO INIT ?.   */
/*   DEF VAR v-ThisCust-Date AS DATE NO-UNDO INIT ?.  */
  DEF VAR v-stat-multi AS CHAR NO-UNDO.                                      

 IF INDEX("HX",inv-head.stat) > 0 OR inv-head.stat = "" THEN DO:
    MESSAGE "Are you sure you wish to"
           ( IF inv-head.stat = "H" THEN "release" ELSE "hold" )
           "this invoice?"  VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
    IF ll-ans THEN DO:

       FIND FIRST cust WHERE
            cust.company EQ inv-head.company AND
            cust.cust-no EQ inv-head.cust-no
            NO-LOCK NO-ERROR.

       /*Group by date*/ 
       IF AVAIL cust AND cust.inv-meth EQ ? THEN
       DO:
          IF inv-head.stat = "H" THEN
          DO:
             /* This Invoice, FG, or Customer */
             ASSIGN v-Date = inv-head.inv-date.

             RUN oe\invhold.w(OUTPUT v-choice,INPUT-OUTPUT v-date, INPUT ROWID(inv-head), OUTPUT v-rowid-list).

             IF v-choice EQ "FG" THEN
             DO:
                FOR FIRST bf-line WHERE
                    bf-line.r-no    EQ inv-head.r-no AND
                    bf-line.i-no    NE ""
                    NO-LOCK,
                    EACH bf-line-2 WHERE
                         bf-line-2.company EQ inv-head.company AND
                         bf-line-2.i-no    EQ bf-line.i-no
                         NO-LOCK,
                    EACH bf-head WHERE
                         bf-head.r-no    EQ bf-line-2.r-no AND
                         bf-head.stat    EQ "H":
                         ASSIGN bf-head.stat = ""
                                bf-head.inv-date = v-date.
                END.

                FIND bf-head WHERE RECID(bf-head) = RECID(inv-head).

                IF bf-head.stat = "H" THEN
                   bf-head.stat = "".

                RELEASE bf-head.

                inv-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "RELEASED".
             END. /*  IF v-choice EQ "FG" */
             ELSE IF v-choice EQ "Customer" THEN
             DO:
                IF v-rowid-list = "" THEN DO:
                    FOR EACH bf-head WHERE
                        bf-head.company EQ inv-head.company AND
                        bf-head.cust-no EQ inv-head.cust-no AND
                        bf-head.stat EQ "H":

                        ASSIGN bf-head.stat = ""
                               bf-head.inv-date = v-date.
                    END.

                    RELEASE bf-head.
                    inv-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "RELEASED".
                END.
                ELSE DO:
                    EACH-SELECTED:
                    DO li = 1 TO NUM-ENTRIES(v-rowid-list):
                      IF v-rowid-list = "" THEN
                          LEAVE.
                      lv-rowid = TO-ROWID(ENTRY(li, v-rowid-list)).
                      FIND bf-head WHERE ROWID(bf-head) = lv-rowid EXCLUSIVE-LOCK NO-ERROR.
                      IF AVAIL bf-head THEN
                        ASSIGN bf-head.stat = ""
                               bf-head.inv-date = v-date.
                        IF AVAIL(bf-head) AND bf-head.inv-no EQ inv-head.inv-no THEN
                                 inv-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                                 IF bf-head.stat = "H" THEN "ON HOLD" ELSE "RELEASED".
                      RELEASE bf-head.

                    END.
                END.

             END.
             ELSE IF v-choice EQ "This Invoice" THEN
             DO:
                FIND bf-head WHERE RECID(bf-head) = RECID(inv-head).
                ASSIGN bf-head.stat = ""
                       bf-head.inv-date = v-date
                       inv-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                                IF bf-head.stat = "H" THEN "ON HOLD" ELSE "RELEASED".

                FIND FIRST bf1-head WHERE bf1-head.company EQ bf-head.company
                     AND bf1-head.inv-no EQ bf-head.inv-no
                     AND bf1-head.cust-no EQ bf-head.cust-no
                     AND bf1-head.multi-invoice = YES
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAIL bf1-head 
                    AND   (bf1-head.bol-no NE bf-head.bol-no
                        OR bf1-head.sold-no NE bf-head.sold-no) THEN
                    ASSIGN bf1-head.bol-no = bf-head.bol-no
                           bf1-head.sold-no = bf-head.sold-no.
                RELEASE bf1-head.
                RELEASE bf-head.

             END.
          END. /* IF inv-head.stat = "H" */
          ELSE /* ELSE IF inv-head.stat NOT "H" */
          DO:
             FIND bf-head WHERE RECID(bf-head) = RECID(inv-head).
             ASSIGN bf-head.stat = "H"
                    inv-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "ON HOLD".
             RELEASE bf-head.
          END.

          IF CAN-FIND(FIRST bf-head WHERE
             bf-head.company EQ inv-head.company AND
             bf-head.inv-no  EQ inv-head.inv-no AND
             bf-head.cust-no EQ inv-head.cust-no AND
             bf-head.multi-invoice EQ NO AND
             bf-head.stat EQ "") THEN
             v-stat-multi = "".
          ELSE
             v-stat-multi = "H".

          IF NOT CAN-FIND(FIRST bf-head WHERE
             bf-head.company EQ inv-head.company AND
             bf-head.inv-no  EQ inv-head.inv-no AND
             bf-head.cust-no EQ inv-head.cust-no AND
             bf-head.multi-invoice EQ YES AND
             bf-head.stat EQ v-stat-multi) THEN
             DO:
                FIND FIRST bf-head WHERE
                     bf-head.company EQ inv-head.company AND
                     bf-head.inv-no  EQ inv-head.inv-no AND
                     bf-head.cust-no EQ inv-head.cust-no AND
                     bf-head.multi-invoice EQ YES
                     NO-ERROR.
                /* Also change date on multi-invoice header record. */
                IF AVAIL bf-head THEN
                DO:
                   ASSIGN bf-head.stat = v-stat-multi
                          bf-head.inv-date = v-date.
                   FIND CURRENT bf-head NO-LOCK NO-ERROR.
                   RELEASE bf-head.
                END.
             END.
       END. /* If group by date */
       ELSE
       DO:
          FIND bf-head WHERE RECID(bf-head) = RECID(inv-head).
          ASSIGN bf-head.stat = IF bf-head.stat = "H" THEN "" ELSE "H"
                 inv-status:SCREEN-VALUE IN FRAME {&FRAME-NAME} = 
                         IF bf-head.stat = "H" THEN "ON HOLD" ELSE "RELEASED".

          RELEASE bf-head.
       END.
    END.
 END.
 ELSE IF INDEX("W",inv-head.stat) > 0 /*OR inv-head.stat = ""*/ THEN DO:
     MESSAGE "Are you sure you wish to"
            ( "release" )
            " this invoice?"  VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans.
     IF ll-ans THEN DO:
/*         FOR EACH bf-head WHERE                      */
/*             bf-head.company EQ inv-head.company AND */
/*             bf-head.cust-no EQ inv-head.cust-no AND */
/*             bf-head.stat EQ "W":                    */
/*                                                     */
         FIND bf-head WHERE RECID(bf-head) = RECID(inv-head) EXCLUSIVE-LOCK.
         ASSIGN bf-head.stat = "".
         RELEASE bf-head.
/*                    bf-head.inv-date = v-date */

/*         END. */
     END.
 END.
 RUN dispatch IN THIS-PROCEDURE ('row-changed':U).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-inv-line FOR inv-line.
  DEF BUFFER bf-inv-misc FOR inv-misc.

  DEF VAR ld-tax-tmp AS DEC NO-UNDO.
  DEF VAR ld-prev-frt-tot AS DEC NO-UNDO.
  DEF VAR ld-frt-tax AS DEC NO-UNDO.
  DEF VAR ld-tax     AS DEC NO-UNDO.
  DEF VAR ld-tax-tot AS DEC NO-UNDO.
  DEF VAR ld-tax-amt AS DEC NO-UNDO.
  DEF VAR ld-inv-accum AS DEC NO-UNDO.


  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN ld-prev-frt-tot = IF inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0 .


  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  /* don't know why ??? - so commented out by YSK 05/25/04 TASK 05250416 
  IF /* inv-head.frt-pay = "" AND */
      inv-head.t-inv-freight > 0 THEN  inv-head.frt-pay = "B".
  */
  /* WFK - took out frt-pay eq 'P', should only be added to total if a 'b' */
  IF inv-status EQ "ON HOLD" AND inv-head.stat NE "H" THEN inv-head.stat = "H".
  inv-head.f-bill = inv-head.frt-pay eq "B" /* OR inv-head.frt-pay eq "P" */.


  /* recalc tax */
  /*if inv-head.tax-gr <> "" THEN DO: */
      FIND FIRST stax WHERE stax.company = cocode AND
                      stax.tax-group = inv-head.tax-gr NO-LOCK NO-ERROR.

      ASSIGN ld-tax-tmp = 0
             ld-tax-tot = 0
             ld-tax-amt = 0
             ld-inv-accum = 0.

      FOR EACH bf-inv-line no-lock where bf-inv-line.r-no = inv-head.r-no:
          ASSIGN ld-inv-accum = ld-inv-accum + bf-inv-line.t-price.
          ASSIGN ld-tax-amt = IF bf-inv-line.tax THEN bf-inv-line.t-price ELSE 0.
          IF inv-head.tax-gr <> "" AND avail stax AND bf-inv-line.tax THEN 
          DO:
              RUN ar/calctax2.p (inv-head.tax-gr,NO,ld-tax-amt,inv-head.company,bf-inv-line.i-no,OUTPUT ld-tax-tmp).
              ASSIGN ld-tax-tot = ld-tax-tot + ld-tax-tmp.
/*               /* Find itemfg. */                                                                    */
/*               FIND FIRST itemfg NO-LOCK WHERE                                                       */
/*                          itemfg.company = bf-inv-line.company AND                                   */
/*                          itemfg.i-no = bf-inv-line.i-no NO-ERROR.                                   */
/*               /* Determine which tax program to run (standard or varied tax calculation). */        */
/*               /* If the itemfg varied tax flag is set... */                                         */
/*               IF AVAIL itemfg AND itemfg.spare-char-2 = "YES" AND                                   */
/*                   /* And the dollar limit is setup in tax code 5... */                              */
/*                   AVAIL stax AND stax.tax-code1[5] = "" AND stax.tax-dscr1[5] = "Dollar Limit" AND  */
/*                   stax.tax-rate1[5] > 0 AND                                                         */
/*                   /* and the invoice price exceeds the dollar limit... */                           */
/*                    bf-inv-line.t-price > stax.tax-rate1[5] THEN                                     */
/*                   /* then run the varied tax rate calculation program. */                           */
/*                   RUN ar/calcvtax.p (inv-head.tax-gr, no,ld-tax-amt, output ld-tax-tot).            */
/*               /* Else run the standard tax rate calculation program. */                             */
/*               ELSE                                                                                  */
/*               /* original calculation */                                                            */
/*               DO i = 1 to 5: /* gdm - 07160902*/                                                    */
/*                     if stax.tax-code1[i] ne "" then do:                                             */
/*                        ld-tax-tmp  = round((if stax.accum-tax then ld-tax-amt                       */
/*                                         ELSE bf-inv-line.t-price) * stax.tax-rate1[i] / 100,2).     */
/*                        ld-tax-amt = ld-tax-amt + ld-tax-tmp.                                        */
/*                        ld-tax-tot = ld-tax-tot + ld-tax-tmp.                                        */
/*                                                                                                     */
/*                     end.                                                                            */
/*               END. /* DO i = 1 to 5 */                                                              */

          END. /* IF inv-head.tax-gr <> "" AND avail stax AND bf-inv-line.tax */
      END. /* FOR EACH bf-inv-line */

      FOR each bf-inv-misc no-lock where bf-inv-misc.company = inv-head.company and
                                          bf-inv-misc.r-no = inv-head.r-no and
                                          bf-inv-misc.bill = "Y" :     
           ASSIGN ld-inv-accum = ld-inv-accum + bf-inv-misc.amt.
           ASSIGN ld-tax-amt = bf-inv-misc.amt.
           IF inv-head.tax-gr <> "" AND bf-inv-misc.tax and avail stax THEN DO:
               RUN ar/calctax2.p (inv-head.tax-gr,NO,ld-tax-amt,inv-head.company,bf-inv-misc.inv-i-no,OUTPUT ld-tax-tmp).
               ASSIGN ld-tax-tot = ld-tax-tot + ld-tax-tmp.
/*               /* Find itemfg. */                                                                    */
/*               FIND FIRST itemfg NO-LOCK WHERE                                                       */
/*                          itemfg.company = bf-inv-misc.company AND                                   */
/*                          itemfg.i-no = bf-inv-misc.inv-i-no NO-ERROR.                               */
/*               /* Determine which tax program to run (standard or varied tax calculation). */        */
/*               /* If the itemfg varied tax flag is set... */                                         */
/*               IF AVAIL itemfg AND itemfg.spare-char-2 = "YES" AND                                   */
/*                   /* And the dollar limit is setup in tax code 5... */                              */
/*                   AVAIL stax AND stax.tax-code1[5] = "" AND stax.tax-dscr1[5] = "Dollar Limit" AND  */
/*                   stax.tax-rate1[5] > 0 AND                                                         */
/*                   /* and the invoice price exceeds the dollar limit... */                           */
/*                    bf-inv-misc.amt > stax.tax-rate1[5] THEN                                         */
/*                   /* then run the varied tax rate calculation program. */                           */
/*                   RUN ar/calcvtax.p (inv-head.tax-gr, no,ld-tax-amt, output ld-tax-tot).            */
/*               /* Else run the standard tax rate calculation program. */                             */
/*               ELSE                                                                                  */
/*                do i = 1 to 5: /* gdm - 07160902*/                                                   */
/*                   if stax.tax-code1[i] ne "" then do:                                               */
/*                        ld-tax-tmp  = round((if stax.accum-tax then ld-tax-amt                       */
/*                                         ELSE bf-inv-misc.amt) * stax.tax-rate1[i] / 100,2).         */
/*                        ld-tax-amt = ld-tax-amt + ld-tax-tmp.                                        */
/*                        ld-tax-tot = ld-tax-tot + ld-tax-tmp.                                        */
/*                   end.                                                                              */
/*                end.                                                                                 */
           END. /* IF inv-head.tax-gr <> "" AND bf-inv-misc.tax and avail stax */
      END.
      ld-inv-accum = ld-inv-accum + 
                     IF inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.
      ld-tax-amt = inv-head.t-inv-freight.
      IF inv-head.tax-gr <> "" and
         inv-head.f-bill AND inv-head.t-inv-freight <> 0 AND AVAIL stax THEN
      do i = 1 to 5: /* gdm - 07160902 */
              if stax.tax-code1[i] ne "" AND stax.tax-frt1[i] then do:
                   ld-tax-tmp  = round((if stax.accum-tax then ld-tax-amt
                                                         ELSE inv-head.t-inv-freight) *
                                        stax.tax-rate1[i] / 100,2).
                   ld-tax-amt = ld-tax-amt + ld-tax-tmp.
                   ld-tax-tot = ld-tax-tot + ld-tax-tmp.

              end.
      end.      
  IF inv-head.cust-no NE '' AND inv-head.sman[1] EQ '' THEN DO:
      FIND FIRST cust 
          WHERE cust.company EQ inv-head.company
            AND cust.cust-no EQ inv-head.cust-no
          NO-LOCK NO-ERROR.
      IF AVAIL cust THEN
          ASSIGN 
            inv-head.sman[1] = cust.sman
            inv-head.s-pct[1] = 100.
  END.


  IF inv-head.tax-gr = "" THEN ld-tax-tot = 0.

  ASSIGN inv-head.t-inv-tax = ld-tax-tot.

  ASSIGN inv-head.t-inv-rev = ld-inv-accum + inv-head.t-inv-tax.

  RUN dispatch ('display-fields').

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
  DEF VAR choice AS LOG NO-UNDO.
  DEF BUFFER bf-inv-head FOR inv-head.


  /* Code placed here will execute PRIOR to standard behavior. */
    x = next-value(inv_r_no_seq).
    
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .
  
  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN ll-cred-lim = NO
         inv-head.r-no = x
         inv-head.company = cocode
         inv-head.sold-no = ""
         inv-head.bol-no  = 0
         inv-head.deleted = no
         inv-head.posted  = no
         inv-head.inv-no  = 0
         inv-head.tot-ord = 0
         inv-head.inv-date = TODAY.

  RUN dispatch ("display-fields").

/*   find first sys-ctrl where sys-ctrl.company eq cocode                    */
/*                         and sys-ctrl.name    eq "CASHSALE"                */
/*                         no-lock no-error.                                 */
/*   if not avail sys-ctrl then do:                                          */
/*      create sys-ctrl.                                                     */
/*      ASSIGN sys-ctrl.company = cocode                                     */
/*             sys-ctrl.name    = "CASHSALE"                                 */
/*             sys-ctrl.descrip = "Allow cash sale to hit G/L cash account?" */
/*             sys-ctrl.log-fld = no.                                        */
/*      MESSAGE sys-ctrl.descrip                                             */
/*          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO                         */
/*          UPDATE sys-ctrl.log-fld.                                         */
/*   end.                                                                    */
/*   choice = sys-ctrl.log-fld.                                              */
  choice = getCashSaleLog().
  IF choice THEN
     MESSAGE "Is this a CASH SALE? (No to Update AR / Yes to Update Cash Account)"
             VIEW-AS ALERT-BOX BUTTON YES-NO UPDATE choice.
  if choice then
    assign
     inv-head.terms   = "CASH"
     inv-head.terms-d = "Cash Sale"
     v-cash-sale = YES.

  ELSE
     ASSIGN
        ll-cred-lim = YES
        v-cash-sale = NO.

  inv-status:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        = get-stat-dscr(inv-head.stat).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR lv-ord-no LIKE inv-line.ord-no.
  DEF BUFFER b-inv-line FOR inv-line.
  DEF BUFFER b-inv-misc FOR inv-misc.
  DEF BUFFER b-oe-ord FOR oe-ord.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT v-oecomm-log  THEN RUN hide-comm (YES).

  IF AVAIL inv-head THEN
  DO:
     inv-status:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        = get-stat-dscr(inv-head.stat).
     FIND FIRST b-inv-line OF inv-head NO-LOCK
        WHERE b-inv-line.ord-no NE 0
        NO-ERROR.
     IF NOT AVAIL b-inv-line THEN
        FIND FIRST b-inv-misc OF inv-head NO-LOCK
            WHERE b-inv-misc.ord-no NE 0
            NO-ERROR.
     lv-ord-no = IF AVAIL b-inv-line THEN b-inv-line.ord-no ELSE
                IF AVAIL b-inv-misc THEN b-inv-misc.ord-no ELSE 0.

/*      FIND FIRST b-oe-bolh WHERE b-oe-bolh.company EQ inv-head.company    */
/*          AND b-oe-bolh.bol-no EQ inv-head.bol-no                         */
/*          AND b-oe-bolh.cust-no EQ inv-head.cust-no NO-LOCK NO-ERROR.     */
/*      IF AVAIL b-oe-bolh THEN DO:                                         */
     IF lv-ord-no NE 0 THEN DO:
        FIND FIRST b-oe-ord WHERE b-oe-ord.company EQ inv-head.company
            AND b-oe-ord.ord-no EQ lv-ord-no NO-LOCK NO-ERROR.
        IF AVAIL b-oe-ord THEN
            fi_PO:SCREEN-VALUE IN FRAME {&FRAME-NAME} = b-oe-ord.po-no.
     END.
     RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"hold-source", OUTPUT char-hdl).

     IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN set-status-btn-lbl IN WIDGET-HANDLE(char-hdl) (INPUT inv-head.stat).
  END.

  DISABLE inv-status WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-add-record AS LOG NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-freight NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN soldto-question.

  RUN valid-sold-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-tax-gr NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-terms NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-carrier NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-fob NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  lv-add-record = adm-adding-record.

/*
  IF INDEX("BCP",inv-head.frt-pay:SCREEN-VALUE IN FRAME {&FRAME-NAME} ) = 0 THEN DO:
      MESSAGE "Invalid freight pay code. Try help." VIEW-AS ALERT-BOX.
      APPLY "entry" TO inv-head.frt-pay.
      RETURN NO-APPLY.
  END.
*/

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ll-cred-lim = NO.
  IF lv-add-record THEN DO:
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"container-source",OUTPUT char-hdl).
     RUN select-page IN WIDGET-HANDLE(char-hdl) (3).
     RUN notify ('row-available').
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"auto-add-target",OUTPUT char-hdl).
     RUN auto-add IN WIDGET-HANDLE(char-hdl).
  END.

  ASSIGN
   lv-add-record     = NO
   adm-adding-record = NO
   adm-new-record    = NO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE refresh-value V-table-Win 
PROCEDURE refresh-value :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND CURRENT inv-head NO-LOCK NO-ERROR.
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
  {src/adm/template/sndkycas.i "r-no" "inv-head" "r-no"}
  {src/adm/template/sndkycas.i "company" "inv-head" "company"}
  {src/adm/template/sndkycas.i "Carrier" "inv-head" "Carrier"}

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
  {src/adm/template/snd-list.i "inv-head"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE soldto-question V-table-Win 
PROCEDURE soldto-question :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF NOT ll-soldto-ans THEN DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
     ll-use-soldto = NO
     ll-soldto-ans = YES.

    MESSAGE "Would you like to use Sold To? (Leave NO for Ship To)"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-use-soldto.

    /*IF NOT ll-use-soldto THEN APPLY "value-changed" TO inv-head.sold-no.*/
  END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-carrier V-table-Win 
PROCEDURE valid-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
     FIND FIRST carrier WHERE carrier.company = g_company
                          AND carrier.carrier = inv-head.carrier:SCREEN-VALUE
                          NO-LOCK NO-ERROR.
     IF NOT AVAIL carrier THEN DO:
        MESSAGE "Invalid Carrier. Try Help." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO inv-head.carrier.
        RETURN ERROR.
     END.
  END.

  {methods/lValidateError.i NO}
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
    v-msg = "".

    FIND FIRST cust
        WHERE cust.company EQ g_company
          AND cust.cust-no EQ inv-head.cust-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF NOT AVAIL cust OR inv-head.cust-no:SCREEN-VALUE EQ "" THEN v-msg = "Invalid entry, try help...".

    IF v-msg EQ "" THEN
      IF cust.active eq "I" THEN v-msg = "Orders may not be processed for inactive customers...".

     IF v-msg EQ "" THEN
      IF cust.active eq "X" THEN v-msg = "Orders may not be processed for Inhouse customers...".

    IF v-msg EQ "" THEN
      IF ll-cred-lim AND inv-status:SCREEN-VALUE NE "ON HOLD" THEN DO:
        RUN oe/creditck.p (ROWID(cust), NO).
        FIND CURRENT cust NO-ERROR.
        IF AVAIL cust AND cust.cr-hold THEN
          ASSIGN
           ll-over-limit           = YES
           inv-status:SCREEN-VALUE = "ON HOLD".
      END.
      /*IF ll-cred-lim                          AND
         inv-status:SCREEN-VALUE NE "ON HOLD" AND
             ((cust.cr-lim - (cust.acc-bal + cust.ord-bal) LE 0) OR
              cust.cr-hold)                       THEN DO:
            MESSAGE "WARNING: Customer has exceeded credit limit, "
                        "Invoice will be put on HOLD..."
                VIEW-AS ALERT-BOX.
            ASSIGN
             ll-over-limit           = YES
             inv-status:SCREEN-VALUE = "ON HOLD".
          END.*/

    IF v-msg NE "" THEN DO:
      MESSAGE v-msg VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO inv-head.cust-no.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-fob V-table-Win 
PROCEDURE valid-fob :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
 DO WITH FRAME {&FRAME-NAME}:
    IF INDEX("ORIG,DEST",inv-head.fob-code:SCREEN-VALUE) <= 0 THEN DO:
       MESSAGE "Invalid FOB Code. Enter ORIG or DEST ." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO inv-head.fob-code.
       RETURN ERROR.
    END.
 END.
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-freight V-table-Win 
PROCEDURE valid-freight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF (inv-head.t-inv-freight:SCREEN-VALUE NE "0.00" ) AND
       NOT CAN-FIND(FIRST inv-line
                    WHERE inv-line.company EQ cocode 
                      AND inv-line.r-no EQ inv-head.r-no)

    THEN DO:
     MESSAGE " **must enter line item before entering freight**"
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO inv-head.t-inv-freight.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sold-no V-table-Win 
PROCEDURE valid-sold-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF VAR v-ship-id     AS CHAR NO-UNDO.
  DEF VAR lv-ship-name AS CHAR NO-UNDO.
  DEF VAR lv-ship-id    LIKE shipto.ship-id    NO-UNDO.
  DEF VAR lv-ship-state LIKE shipto.ship-state NO-UNDO.

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
     IF (ll-use-soldto AND 
         NOT CAN-FIND(FIRST soldto WHERE
             soldto.company EQ g_company AND
             soldto.cust-no EQ inv-head.cust-no:SCREEN-VALUE AND
             soldto.sold-id EQ inv-head.sold-no:SCREEN-VALUE)) THEN
     DO:
        MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
        APPLY "entry" TO inv-head.sold-no.
        RETURN ERROR.
     END.

     IF NOT ll-use-soldto THEN
     DO:
        FIND FIRST shipto WHERE
             shipto.company EQ g_company AND
             shipto.cust-no EQ inv-head.cust-no:SCREEN-VALUE AND
             shipto.ship-id EQ inv-head.sold-no:SCREEN-VALUE
             NO-LOCK NO-ERROR.

        IF AVAIL shipto THEN
           assign inv-head.sold-no:SCREEN-VALUE  = shipto.ship-id
                  inv-head.sold-name:SCREEN-VALUE = shipto.ship-name
                  inv-head.sold-addr[1]:SCREEN-VALUE  = shipto.ship-addr[1]
                  inv-head.sold-addr[2]:SCREEN-VALUE  = shipto.ship-addr[2]
                  inv-head.sold-city:SCREEN-VALUE = shipto.ship-city
                  inv-head.sold-state:SCREEN-VALUE = shipto.ship-state
                  inv-head.sold-zip:SCREEN-VALUE = shipto.ship-zip.
        ELSE 
        DO:
           IF NOT v-cash-sale THEN
           DO:
              MESSAGE "Invalid entry, try help..."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
              APPLY "entry" TO inv-head.sold-no.
              RETURN ERROR.
           END.
           ELSE
           DO:
              MESSAGE "            Invalid entry, try help...             " SKIP(1)
                      "                        OR                         " SKIP(1)
                      "Do you wish to add this Shipto ID to this Customer?"
                      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                      UPDATE ll-new-shipto.
              IF NOT ll-new-shipto THEN DO:
                 APPLY "entry" TO inv-head.sold-no.
                 RETURN ERROR .
              END.
              ELSE
              DO: 
                 ASSIGN 
                    lv-ship-name = inv-head.sold-name:SCREEN-VALUE IN FRAME {&FRAME-NAME}
                    lv-ship-id = inv-head.sold-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}    
                    lv-ship-state = inv-head.sold-state:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

                 RUN viewers/nship.w (ROWID(cust),
                                      INPUT lv-ship-id,
                                      INPUT lv-ship-name,
                                      INPUT lv-ship-state).
              END.
           END.
        END.
     END.

     ll-soldto-ans = YES.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tax-gr V-table-Win 
PROCEDURE valid-tax-gr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF inv-head.tax-gr:SCREEN-VALUE NE "" AND
       NOT CAN-FIND(FIRST stax
                    WHERE stax.company   EQ cocode
                      AND stax.tax-group EQ inv-head.tax-gr:SCREEN-VALUE)
    THEN DO:
      MESSAGE TRIM(inv-head.tax-gr:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO inv-head.tax-gr.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-terms V-table-Win 
PROCEDURE valid-terms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF inv-head.terms:SCREEN-VALUE NE "CASH" AND
       NOT CAN-FIND(FIRST terms WHERE terms.company EQ g_company
                                  AND terms.t-code EQ inv-head.terms:SCREEN-VALUE)
    THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO inv-head.terms.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hide-comm V-table-Win 
PROCEDURE hide-comm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAM ip-hidden AS LOG NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    inv-head.t-comm:HIDDEN = ip-hidden.

    IF NOT ip-hidden THEN DISPLAY inv-head.t-comm.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-stat-dscr V-table-Win 
FUNCTION get-stat-dscr RETURNS CHARACTER
  ( ipcStat AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR opStatDscr AS CHAR NO-UNDO.

opStatDscr = IF inv-head.stat = "H" THEN "ON HOLD" ELSE "RELEASED".

CASE ipcStat:
    WHEN "H" THEN opStatDscr = "ON HOLD".
    WHEN "W" THEN opStatDscr = "WAIT/APVL".
    OTHERWISE opSTatDscr = "RELEASED".
END CASE.
  RETURN opStatDscr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getCashSaleLog V-table-Win 
FUNCTION getCashSaleLog RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "CASHSALE"
                        no-lock no-error.
  if not avail sys-ctrl then do:
     create sys-ctrl.
     ASSIGN sys-ctrl.company = cocode
            sys-ctrl.name    = "CASHSALE"
            sys-ctrl.descrip = "Allow cash sale to hit G/L cash account?"
            sys-ctrl.log-fld = no.
     MESSAGE sys-ctrl.descrip
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE sys-ctrl.log-fld.
  end.
  RETURN sys-ctrl.log-fld.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

