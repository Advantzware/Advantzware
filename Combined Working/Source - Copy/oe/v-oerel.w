&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: oe\v-oerel.w

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
def var lv-ship-no like shipto.ship-no no-undo.
def var ll-got-ship-id as log no-undo.
DEF VAR lv-cust-x LIKE oe-bolh.cust-no NO-UNDO.
DEF VAR l-update-reason-perms AS LOG NO-UNDO.
DEF VAR oeDateChange-log AS LOG NO-UNDO.
DEFINE VARIABLE oeDateChange-char AS CHARACTER   NO-UNDO.
DEF VAR v-rtn-char AS CHAR NO-UNDO.
DEF VAR v-rec-found AS LOG NO-UNDO.
{sys/inc/var.i new shared}

DEF VAR v-chkflg AS LOG NO-UNDO.

/* gdm - 05070905 - CREDIT HOLD */
DEF BUFFER bf-cust1 FOR cust.

DEF VAR v-access-close AS LOG.
DEF VAR v-access-list AS CHAR.
DEFINE VARIABLE  ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

RUN methods/prgsecur.p
    (INPUT "OEDateMod",
     INPUT "UPDATE", /* based on run, create, update, delete or all */
     INPUT NO,    /* use the directory in addition to the program */
     INPUT NO,    /* Show a message if not authorized */
     INPUT NO,    /* Group overrides user security? */
     OUTPUT l-update-reason-perms, /* Allowed? Yes/NO */
     OUTPUT v-access-close, /* used in template/windows.i  */
     OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */


RUN sys/ref/nk1look.p (g_company, "oeDateChange", "L", no, no, "", "", 
                          OUTPUT v-rtn-char, OUTPUT v-rec-found).                      
IF v-rec-found THEN
    oeDateChange-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (g_company, "oeDateChange", "C", no, no, "", "", 
                          OUTPUT v-rtn-char, OUTPUT v-rec-found).                      
IF v-rec-found THEN
    oeDateChange-char = v-rtn-char NO-ERROR.

DO TRANSACTION:
     {sys/ref/CustList.i NEW}
END.

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
&Scoped-define EXTERNAL-TABLES oe-relh
&Scoped-define FIRST-EXTERNAL-TABLE oe-relh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-relh.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-relh.ship-id oe-relh.carrier ~
oe-relh.rel-date oe-relh.spare-char-1 oe-relh.trailer 
&Scoped-define ENABLED-TABLES oe-relh
&Scoped-define FIRST-ENABLED-TABLE oe-relh
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-38 btnCalendar-1 
&Scoped-Define DISPLAYED-FIELDS oe-relh.printed oe-relh.spare-char-3 ~
oe-relh.cust-no oe-relh.ship-id oe-relh.release# oe-relh.carrier ~
oe-relh.rel-date oe-relh.spare-char-1 oe-relh.spare-char-2 oe-relh.trailer 
&Scoped-define DISPLAYED-TABLES oe-relh
&Scoped-define FIRST-DISPLAYED-TABLE oe-relh
&Scoped-Define DISPLAYED-OBJECTS fi_hold cust_name ship_name cust_addr1 ~
ship_addr1 cust_addr2 ship_addr2 cust_city cust_state cust_zip ship_city ~
ship_state ship_zip line_i-no freight_term qty-ordered qty-rel qty-ship qty-oh 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS oe-relh.cust-no 
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
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE cust_addr1 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE cust_addr2 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE cust_city AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE cust_name AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE cust_state AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1.

DEFINE VARIABLE cust_zip AS CHARACTER FORMAT "x(10)" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fi_hold AS CHARACTER FORMAT "X(10)":U 
     LABEL "Status" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE line_i-no AS CHARACTER FORMAT "x(15)" 
     LABEL "FG Item #" 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1.

DEFINE VARIABLE qty-oh AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Qty On Hand" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE qty-ordered AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Qty Ordered" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE qty-rel AS DECIMAL FORMAT ">>,>>>,>>9" INITIAL 0 
     LABEL "Qty Released" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE qty-ship AS INTEGER FORMAT "->>,>>>,>>9":U INITIAL 0 
     LABEL "Qty Shipped" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE ship_addr1 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE ship_addr2 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE ship_city AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE ship_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1.

DEFINE VARIABLE ship_state AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1.

DEFINE VARIABLE ship_zip AS CHARACTER FORMAT "x(10)" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.
DEFINE VARIABLE freight_term AS CHARACTER FORMAT "x(15)" 
     LABEL "Freight Terms"
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 144 BY 8.57.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141 BY 1.43.

DEFINE RECTANGLE RECT-38
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 45 BY 1.43.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     oe-relh.printed AT ROW 1.19 COL 120.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     oe-relh.spare-char-3 AT ROW 1.19 COL 130.4 COLON-ALIGNED HELP
          "" WIDGET-ID 10
          LABEL "Usr" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-relh.cust-no AT ROW 1.24 COL 13 COLON-ALIGNED
          LABEL "Customer" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-relh.ship-id AT ROW 1.24 COL 56 COLON-ALIGNED
          LABEL "Ship To" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     fi_hold AT ROW 1.24 COL 94.2 COLON-ALIGNED
     cust_name AT ROW 2.19 COL 13 COLON-ALIGNED NO-LABEL
     ship_name AT ROW 2.19 COL 56 COLON-ALIGNED NO-LABEL
     oe-relh.release# AT ROW 2.19 COL 120 COLON-ALIGNED FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     cust_addr1 AT ROW 3.14 COL 13 COLON-ALIGNED NO-LABEL
     oe-relh.carrier AT ROW 3.14 COL 120 COLON-ALIGNED FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     ship_addr1 AT ROW 3.19 COL 56 COLON-ALIGNED NO-LABEL
     cust_addr2 AT ROW 4.1 COL 13 COLON-ALIGNED NO-LABEL
     ship_addr2 AT ROW 4.1 COL 56 COLON-ALIGNED NO-LABEL
     oe-relh.rel-date AT ROW 4.1 COL 120 COLON-ALIGNED
          LABEL "Release Date" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     btnCalendar-1 AT ROW 4.1 COL 137
     cust_city AT ROW 5.05 COL 13 COLON-ALIGNED NO-LABEL
     cust_state AT ROW 5.05 COL 33 COLON-ALIGNED NO-LABEL
     cust_zip AT ROW 5.05 COL 37 COLON-ALIGNED NO-LABEL
     ship_city AT ROW 5.05 COL 56 COLON-ALIGNED NO-LABEL
     ship_state AT ROW 5.05 COL 76 COLON-ALIGNED NO-LABEL
     ship_zip AT ROW 5.05 COL 80 COLON-ALIGNED NO-LABEL
     oe-relh.spare-char-1 AT ROW 5.29 COL 111 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          LABEL "Dt Chg Rsn" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-relh.spare-char-2 AT ROW 5.29 COL 130 COLON-ALIGNED HELP
          "" WIDGET-ID 4
          LABEL "Usr" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     oe-relh.trailer AT ROW 6.48 COL 110 COLON-ALIGNED
          LABEL "Trailer #" FORMAT "x(20)"
          VIEW-AS FILL-IN 
          SIZE 30 BY 1
     line_i-no AT ROW 6.71 COL 16 COLON-ALIGNED
     freight_term AT ROW 6.71 COL 72 COLON-ALIGNED
     qty-ordered AT ROW 8.14 COL 17 COLON-ALIGNED
     qty-rel AT ROW 8.14 COL 53 COLON-ALIGNED
     qty-ship AT ROW 8.14 COL 87 COLON-ALIGNED
     qty-oh AT ROW 8.14 COL 122 COLON-ALIGNED
     RECT-1 AT ROW 1 COL 1
     RECT-2 AT ROW 7.91 COL 2
     RECT-38 AT ROW 6.48 COL 2 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-relh
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
         HEIGHT             = 17.05
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN oe-relh.carrier IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN oe-relh.cust-no IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT                                     */
/* SETTINGS FOR FILL-IN cust_addr1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_addr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_city IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_state IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN cust_zip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi_hold IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN line_i-no IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN freight_term IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-relh.printed IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN qty-oh IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN qty-ordered IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN qty-rel IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN qty-ship IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-relh.rel-date IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-relh.release# IN FRAME F-Main
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-relh.ship-id IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN ship_addr1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_addr2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_city IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_state IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ship_zip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-relh.spare-char-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN oe-relh.spare-char-2 IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN oe-relh.spare-char-3 IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN oe-relh.trailer IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
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
    DEF VAR lv-focus AS WIDGET-HANDLE NO-UNDO.
    case focus:name :
         when "cust-no" then do:
              run windows/l-cstord.w (g_company, focus:screen-value in frame {&frame-name}, output char-val, output look-recid).
              IF ENTRY(1,char-val) NE FOCUS:SCREEN-VALUE AND look-recid NE ? THEN
                run display-cust-detail (look-recid).     
         end.  
         when "ship-id" then do:
              run windows/l-shipt2.w (g_company,g_loc, oe-relh.cust-no:screen-value in frame {&frame-name}, focus:screen-value, output char-val, output look-recid) NO-ERROR.
              FIND shipto WHERE RECID(shipto) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL shipto AND shipto.ship-id NE FOCUS:SCREEN-VALUE THEN DO:
                 oe-relh.ship-id:screen-value in frame {&frame-name} = shipto.ship-id.
                 APPLY "entry" TO  oe-relh.ship-id  .  
                 RUN new-ship-id.
              END.       
          end.  
          when "carrier" then do:
              run windows/l-carrie.w (g_company,g_loc, focus:screen-value, output char-val).
              if char-val <> "" then do:
                 focus:screen-value = entry(1,char-val).
              end.       
          end.  
          /* gdm - */
          WHEN "trailer" THEN DO:              
             RUN browsers/l-truck2.w (g_company,
                                     g_loc,
                                     oe-relh.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                     oe-relh.trailer:SCREEN-VALUE IN FRAME {&FRAME-NAME}, 
                                     OUTPUT char-val).
             IF char-val <> "" THEN 
               ASSIGN oe-relh.trailer:SCREEN-VALUE = ENTRY(1,char-val).

          END.

          WHEN "spare-char-1" THEN DO:
            RUN windows/l-rejpo.w  (g_company,FOCUS:SCREEN-VALUE, OUTPUT char-val). 
            IF char-val <> "" THEN 
              FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME}  = ENTRY(1,char-val).
          END.
         /* gdm - */
    end case.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 V-table-Win
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i oe-relh.rel-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-relh.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-relh.cust-no V-table-Win
ON LEAVE OF oe-relh.cust-no IN FRAME F-Main /* Customer */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    RUN valid-cust-user NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-relh.cust-no V-table-Win
ON VALUE-CHANGED OF oe-relh.cust-no IN FRAME F-Main /* Customer */
DO:
  RUN new-cust-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-relh.rel-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-relh.rel-date V-table-Win
ON HELP OF oe-relh.rel-date IN FRAME F-Main /* Release Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-relh.rel-date V-table-Win
ON LEAVE OF oe-relh.rel-date IN FRAME F-Main /* Release Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-rel-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&Scoped-define SELF-NAME oe-relh.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-relh.ship-id V-table-Win
ON LEAVE OF oe-relh.ship-id IN FRAME F-Main /* Ship To */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ship-id NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-relh.ship-id V-table-Win
ON VALUE-CHANGED OF oe-relh.ship-id IN FRAME F-Main /* Ship To */
DO:
  RUN new-ship-id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-relh.spare-char-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-relh.spare-char-1 V-table-Win
ON LEAVE OF oe-relh.spare-char-1 IN FRAME F-Main /* Dt Chg Rsn */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-date-change NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */

SESSION:SUPPRESS-WARNINGS = YES . 
assign cocode = g_company
       locode = g_loc
       lv-cust-x = "".

FOR EACH cust fields(cust-no) NO-LOCK
    WHERE cust.company EQ cocode
      AND cust.active  EQ "X":
  lv-cust-x = cust.cust-no.
  LEAVE.
END.
 RUN sys/inc/custlistform.p (INPUT "OT1" , INPUT cocode , OUTPUT ou-log , OUTPUT ou-cust-int) .
{sys/inc/relcrhold.i}

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN              
    RUN dispatch IN THIS-PROCEDURE ('initialize':U).        
&ENDIF   






/************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-rel-date V-table-Win
PROCEDURE valid-rel-date:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValid    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE ldDate    AS DATE    NO-UNDO.
  {methods/lValidateError.i YES}
    DO WITH FRAME {&FRAME-NAME}:
        ldDate = DATE(oe-relh.rel-date:SCREEN-VALUE).
        RUN oe/dateFuture.p (INPUT cocode, INPUT ldDate, INPUT YES /* prompt */, OUTPUT lValid, OUTPUT lContinue).
        IF NOT lValid AND  NOT lContinue THEN 
        DO:      
            RETURN ERROR.
        END. 
    END.
  {methods/lValidateError.i NO}
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



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
  {src/adm/template/row-list.i "oe-relh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-relh"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changed-shipid V-table-Win 
PROCEDURE changed-shipid :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bf-oe-rell FOR oe-rell.
  DEF BUFFER bf-oe-rel FOR oe-rel.
  DEF BUFFER bf-itemfg-loc FOR itemfg-loc.
  DEF VAR v-row AS ROWID NO-UNDO.    
  DEF VAR v-q-back LIKE itemfg.q-back NO-UNDO.

    /* 10021210 - Per Joe, if they change the shipto must update the 
       locations on oe-rell and oe-rel                                 */

    RUN oe/custxship.p (oe-relh.company,
                        oe-relh.cust-no,
                        oe-relh.ship-id,
                        BUFFER shipto).

    FOR EACH bf-oe-rell WHERE  bf-oe-rell.r-no EQ oe-relh.r-no EXCLUSIVE-LOCK:

        bf-oe-rell.loc = shipto.loc.
        RUN get-oe-rel (INPUT ROWID(bf-oe-rell), OUTPUT v-row).
        FIND bf-oe-rel 
            WHERE ROWID(bf-oe-rel) EQ v-row
            EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL bf-oe-rel THEN DO: 
            /* Back out inventory for this location */
           FIND FIRST bf-itemfg-loc
               WHERE bf-itemfg-loc.company EQ bf-oe-rel.company
                 AND bf-itemfg-loc.i-no    EQ bf-oe-rel.i-no
                 AND bf-itemfg-loc.loc     EQ bf-oe-rel.spare-char-1
               EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL bf-itemfg-loc THEN DO:

               FIND itemfg WHERE itemfg.company EQ bf-oe-rel.company
                    AND itemfg.i-no EQ bf-oe-rel.i-no
                   NO-LOCK NO-ERROR.
               IF AVAIL itemfg AND AVAIL(bf-itemfg-loc) THEN
                 RUN fg/calcqabl.p (ROWID(itemfg), bf-oe-rel.spare-char-1, OUTPUT bf-itemfg-loc.q-alloc, OUTPUT v-q-back).

               bf-itemfg-loc.q-avail = bf-itemfg-loc.q-onh + bf-itemfg-loc.q-ono - bf-itemfg-loc.q-alloc.

               ASSIGN  bf-oe-rel.spare-char-1   = shipto.loc
                       bf-oe-rel.ship-zip       = shipto.ship-zip
                       bf-oe-rel.ship-state     = shipto.ship-state           
                       bf-oe-rel.ship-city      = shipto.ship-city
                       bf-oe-rel.ship-addr[1]   = shipto.ship-addr[1]
                       bf-oe-rel.ship-addr[2]   = shipto.ship-addr[2]
                       bf-oe-rel.ship-id        = shipto.ship-id.


                RUN fg/fgitmloc.p (INPUT bf-oe-rel.i-no, INPUT ROWID(bf-oe-rel)).  
           END. /* avail itemfg-loc */
        END. /* each bf-oe-rell */

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-cust-holdflg V-table-Win 
PROCEDURE check-cust-holdflg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
         WHEN A RELEASE TICKET IS ON A CREDIT HOLD, THE cr-hold FLAG OF 
  THE CUSTOMER RECORD SHOULD BE FLAGGED AS WELL (cust.cr-hold = true), AND 
  UNFLAGGED WHEN TAKEN OUT OF HOLD.
------------------------------------------------------------------------------*/
DEF INPUT PARAM ip-flag AS LOG NO-UNDO.

FIND FIRST bf-cust1
  WHERE bf-cust1.company EQ oe-relh.company
    AND bf-cust1.cust-no EQ oe-relh.cust-no NO-ERROR.
IF AVAIL bf-cust1 THEN DO:
  ASSIGN bf-cust1.cr-hold = ip-flag.
END.

RELEASE bf-cust1.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-for-record V-table-Win 
PROCEDURE check-for-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT-OUTPUT PARAM op-avail AS LOG NO-UNDO.

op-avail = AVAIL {&FIRST-ENABLED-TABLE}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-hold V-table-Win 
PROCEDURE check-hold :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR char-hdl AS CHAR NO-UNDO.

   IF AVAIL oe-relh THEN
   DO:
      IF oe-relh.w-ord THEN
         ASSIGN 
            fi_hold = "On Hold".
      ELSE
         ASSIGN
            fi_hold = "Approved".

      ASSIGN
          fi_hold:SCREEN-VALUE IN FRAME {&FRAME-NAME}= fi_hold.

      IF fi_hold = "On Hold"  THEN DO:

         /* LABEL CONTROL */
         RUN get-link-handle IN adm-broker-hdl
             (THIS-PROCEDURE,"hold-rel-source",OUTPUT char-hdl).

         IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
            RUN release-label IN WIDGET-HANDLE(char-hdl).
         /* LABEL CONTROL end*/

         /* gdm - 05070905 
         RUN check-cust-holdflg(TRUE).        
         */        

      END.
      ELSE DO:

         RUN get-link-handle IN adm-broker-hdl
             (THIS-PROCEDURE,"hold-rel-source",OUTPUT char-hdl).

         IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
            RUN hold-label IN WIDGET-HANDLE(char-hdl).

         /* gdm - 05070905 
         RUN check-cust-holdflg(NO).
         */        
      END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-relhold V-table-Win 
PROCEDURE create-relhold :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* credit check is not done here, this logic is defaulting the flg to RELCRDT parameter log value */
/*   IF v-chkflg THEN
      oe-relh.w-ord = YES.*/

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
  def input parameter ip-recid as recid no-undo.
  find cust where recid(cust) = ip-recid no-lock no-error.
  if avail cust then do with frame {&frame-name} :

       assign oe-relh.cust-no:screen-value   = cust.cust-no
              cust_name:screen-value = cust.name
              cust_addr1:screen-value   = cust.addr[1]
              cust_addr2:screen-value   = cust.addr[2]
              cust_city:screen-value      = cust.city
              cust_state:screen-value     = cust.state
              cust_zip:screen-value       = cust.zip
              oe-relh.carrier:screen-value = cust.carrier
              freight_term:SCREEN-VALUE   = IF cust.frt-pay EQ "P" THEN "Prepaid"
                                            ELSE IF cust.frt-pay EQ "B" THEN "Bill"
                                            ELSE IF cust.frt-pay EQ "T" THEN "3rd Party"
                                            ELSE  "Collect"  .

       FIND FIRST shipto
           WHERE shipto.company EQ cust.company
             AND shipto.cust-no EQ cust.cust-no
             AND shipto.ship-id EQ oe-relh.ship-id:SCREEN-VALUE
           NO-LOCK NO-ERROR.
       IF NOT AVAIL shipto THEN
       FOR EACH shipto
           WHERE shipto.company EQ cocode
             AND shipto.cust-no EQ oe-ord.cust-no
           NO-LOCK
           BREAK BY shipto.ship-no DESC:
         IF shipto.ship-id EQ oe-ord.cust-no OR LAST(shipto.ship-no) THEN LEAVE.
       END.

       IF AVAIL shipto THEN DO:
         oe-relh.ship-id:SCREEN-VALUE = shipto.ship-id.
         RUN new-ship-id.
       END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-items V-table-Win 
PROCEDURE display-items :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input param ip-recid as recid no-undo.

  DEF BUFFER b-oe-ordl-2 FOR oe-ordl.
  DEF BUFFER b-oe-rell-2 FOR oe-rell.

  assign qty-ordered = 0
         qty-rel = 0
         qty-ship = 0
         qty-oh = 0.

  if ip-recid = ? then return.

  find b-oe-rell-2 where recid(b-oe-rell-2) = ip-recid no-lock .
  line_i-no = if avail b-oe-rell-2 then b-oe-rell-2.i-no else "".
  disp line_i-no with frame {&frame-name}.
  find itemfg where itemfg.company = g_company and
                    itemfg.i-no = line_i-no no-lock no-error.
  if not avail itemfg then return.

  find first b-oe-ordl-2 where
       b-oe-ordl-2.company = g_company AND
       b-oe-ordl-2.ord-no = b-oe-rell-2.ord-no AND
       b-oe-ordl-2.i-no = b-oe-rell-2.i-no
       no-lock no-error.

  if avail b-oe-ordl-2 then
     assign
        qty-ordered = b-oe-ordl-2.qty
        qty-rel = b-oe-ordl-2.t-rel-qty
        qty-ship = b-oe-ordl-2.ship-qty
        qty-oh = itemfg.q-onh.

  else assign qty-ordered = 0
              qty-rel = 0
              qty-ship = 0
              qty-oh = itemfg.q-onh.
  IF QTY-rel < 0  THEN qty-rel = 0.

  display qty-ordered qty-rel qty-ship qty-oh with frame {&frame-name}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-oe-rel V-table-Win 
PROCEDURE get-oe-rel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ip-rell-row AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER op-row     AS ROWID NO-UNDO.

DEF BUFFER bf-oe-rell FOR oe-rell.
DEF BUFFER bf-oe-rel FOR oe-rel.
    FIND FIRST bf-oe-rell WHERE ROWID(bf-oe-rell) EQ ip-rell-row 
        NO-LOCK NO-ERROR.
    IF AVAIL bf-oe-rell  THEN DO:
      IF bf-oe-rell.link-no LE 0 THEN
          FIND FIRST bf-oe-rel WHERE bf-oe-rel.r-no = bf-oe-rell.link-no
           NO-LOCK NO-ERROR.

      IF NOT AVAIL bf-oe-rel THEN

      FIND FIRST bf-oe-rel
        WHERE bf-oe-rel.company  EQ bf-oe-rell.company
          AND bf-oe-rel.link-no  EQ bf-oe-rell.r-no
          AND bf-oe-rel.ord-no   EQ bf-oe-rell.ord-no
          AND bf-oe-rel.rel-no   EQ bf-oe-rell.rel-no
          AND bf-oe-rel.b-ord-no EQ bf-oe-rell.b-ord-no
          AND bf-oe-rel.i-no     EQ bf-oe-rell.i-no
          AND bf-oe-rel.line     EQ bf-oe-rell.line
          AND bf-oe-rel.po-no    EQ bf-oe-rell.po-no        
          USE-INDEX link-ord NO-LOCK NO-ERROR.

      IF NOT AVAIL bf-oe-rel THEN

        FIND FIRST bf-oe-rel
          WHERE bf-oe-rel.company  EQ bf-oe-rell.company
            AND bf-oe-rel.ord-no   EQ bf-oe-rell.ord-no
            AND bf-oe-rel.i-no     EQ bf-oe-rell.i-no
            AND bf-oe-rel.line     EQ bf-oe-rell.line
            AND bf-oe-rel.rel-no   EQ bf-oe-rell.rel-no
            AND bf-oe-rel.b-ord-no EQ bf-oe-rell.b-ord-no
            AND bf-oe-rel.po-no    EQ bf-oe-rell.po-no
            AND INDEX("SIL", bf-oe-rel.stat) EQ 0
          USE-INDEX ord-item NO-LOCK NO-ERROR.        

    END.
    IF AVAIL bf-oe-rel THEN
        op-row = ROWID(bf-oe-rel).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hold-release V-table-Win 
PROCEDURE hold-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR char-hdl AS CHAR NO-UNDO.

   FIND CURRENT oe-relh.

   /* APPROVED */
   IF oe-relh.w-ord THEN DO:
       ASSIGN
           oe-relh.w-ord = NO
           fi_hold = "Approved".

       RUN get-link-handle IN adm-broker-hdl
           (THIS-PROCEDURE,"hold-rel-source",OUTPUT char-hdl).
       IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
          RUN hold-label IN WIDGET-HANDLE(char-hdl).

       /* gdm - 05070905 
       RUN check-cust-holdflg(NO). 
       */
   END.
   ELSE DO: /* HOLD */
       ASSIGN
           oe-relh.w-ord = YES
           fi_hold = "On Hold".                    

       RUN get-link-handle IN adm-broker-hdl
           (THIS-PROCEDURE,"hold-rel-source",OUTPUT char-hdl).
       IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
          RUN release-label IN WIDGET-HANDLE(char-hdl).

       /* gdm - 05070905 
       RUN check-cust-holdflg(TRUE).        
       */

   END.

   FIND CURRENT oe-relh NO-LOCK.

   DISP fi_hold WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR v-date-change-reason  AS CHAR      NO-UNDO.
  DEF VAR v-date-reason-note-id AS ROWID     NO-UNDO.
  DEF VAR v-save-rec-key LIKE oe-rel.rec_key NO-UNDO.
  DEF VAR sdate AS CHAR NO-UNDO.
  DEF VAR v-orig-shipid AS CHAR NO-UNDO.
  DEF BUFFER bfNotes FOR notes.

  /* Code placed here will execute PRIOR to standard behavior. */
  {&methods/lValidateError.i YES}
  IF oe-relh.posted THEN DO:
      MESSAGE "This release has already been posted, no updates allowed."
          VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
  {&methods/lValidateError.i NO}
  sdate = oe-relh.rel-date:SCREEN-VALUE IN FRAME {&FRAME-NAME}.

  IF oe-relh.rel-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE 
      string(oe-relh.rel-date, "99/99/9999")
      AND oeDateChange-log
      AND (oeDateChange-char EQ "" OR LOOKUP("release date", oeDateChange-char) GT 0)
      AND NOT adm-new-record THEN DO:
    FOR EACH oe-rell WHERE oe-rell.r-no EQ oe-relh.r-no NO-LOCK,
      FIRST oe-rel
      WHERE oe-rel.company  EQ oe-rell.company
        AND oe-rel.link-no  EQ oe-rell.r-no
        AND oe-rel.ord-no   EQ oe-rell.ord-no
        AND oe-rel.rel-no   EQ oe-rell.rel-no
        AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
        AND oe-rel.i-no     EQ oe-rell.i-no
        AND oe-rel.line     EQ oe-rell.line
        AND oe-rel.po-no    EQ oe-rell.po-no        
        USE-INDEX link-ord NO-LOCK .
        LEAVE.
    END.
    IF NOT AVAIL oe-rel THEN DO:
      FOR EACH oe-rell WHERE oe-rell.r-no EQ oe-relh.r-no NO-LOCK,
        FIRST oe-rel
        WHERE oe-rel.company  EQ oe-rell.company
          AND oe-rel.ord-no   EQ oe-rell.ord-no
          AND oe-rel.i-no     EQ oe-rell.i-no
          AND oe-rel.line     EQ oe-rell.line
          AND oe-rel.rel-no   EQ oe-rell.rel-no
          AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
          AND oe-rel.po-no    EQ oe-rell.po-no
          AND INDEX("SIL", oe-rel.stat) EQ 0
        USE-INDEX ord-item NO-LOCK .
        LEAVE.
      END.
    END.

    /* prompt user for reason for date change */
    IF AVAIL(oe-rel) THEN DO:
      v-save-rec-key = oe-rel.rec_key.
      RUN oe/d-rsnnot.w
      (INPUT oe-rel.rec_key, INPUT "R", INPUT "", INPUT "", INPUT 0, INPUT "RDC", INPUT "",
       OUTPUT v-date-change-reason, OUTPUT v-date-reason-note-id)  .

    END.
    IF AVAIL oe-relh AND v-date-change-reason GT "" THEN DO:

        FIND CURRENT oe-relh EXCLUSIVE-LOCK.
        IF v-date-change-reason GT "" THEN
            ASSIGN oe-relh.spare-char-1:SCREEN-VALUE = v-date-change-reason
                   oe-relh.spare-char-2:SCREEN-VALUE = USERID("NOSWEAT")
                   oe-relh.spare-char-2 = USERID("NOSWEAT").
        FIND CURRENT oe-relh NO-LOCK.

        FOR EACH oe-rell WHERE oe-rell.r-no EQ oe-relh.r-no NO-LOCK:
          FIND FIRST oe-rel
              WHERE oe-rel.company  EQ oe-rell.company
                AND oe-rel.link-no  EQ oe-rell.r-no
                AND oe-rel.ord-no   EQ oe-rell.ord-no
                AND oe-rel.rel-no   EQ oe-rell.rel-no
                AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
                AND oe-rel.i-no     EQ oe-rell.i-no
                AND oe-rel.line     EQ oe-rell.line
                AND oe-rel.po-no    EQ oe-rell.po-no        
                USE-INDEX link-ord EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAIL oe-rel THEN
            FIND FIRST oe-rel
              WHERE oe-rel.company  EQ oe-rell.company
                AND oe-rel.ord-no   EQ oe-rell.ord-no
                AND oe-rel.i-no     EQ oe-rell.i-no
                AND oe-rel.line     EQ oe-rell.line
                AND oe-rel.rel-no   EQ oe-rell.rel-no
                AND oe-rel.b-ord-no EQ oe-rell.b-ord-no
                AND oe-rel.po-no    EQ oe-rell.po-no
                AND INDEX("SIL", oe-rel.stat) EQ 0
              USE-INDEX ord-item EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL(oe-rel) THEN DO:
             /* Update all related oe-rel records with new update reason
                and copy the note to that rec_key */
             ASSIGN oe-rel.spare-char-2 = v-date-change-reason
                    oe-rel.spare-char-3 = USERID("NOSWEAT").

             /* Was already created */
             IF oe-rel.rec_key = v-save-rec-key THEN
                 NEXT.
             FIND notes WHERE ROWID(notes) EQ v-date-reason-note-id NO-LOCK NO-ERROR.
             IF AVAIL notes THEN DO:
                 CREATE bfNotes.
                 BUFFER-COPY notes EXCEPT rec_key TO bfNotes.
                 bfNotes.rec_key = oe-rel.rec_key.
             END.
           END.
        END.
    END.
  END.

  v-orig-shipid = oe-relh.ship-id.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if lv-ship-no <> 0 then do:
     oe-relh.ship-no = lv-ship-no.

     find shipto where shipto.company = oe-relh.company 
                   and shipto.cust-no = oe-relh.cust-no
                   and shipto.ship-no = lv-ship-no
                   no-lock no-error.
     if avail shipto then 
        assign oe-relh.ship-i[1] = shipto.notes[1]
               oe-relh.ship-i[2] = shipto.notes[2]
               oe-relh.ship-i[3] = shipto.notes[3]
               oe-relh.ship-i[4] = shipto.notes[4].
  END.

  IF oe-relh.ship-id NE v-orig-shipid THEN
      RUN changed-shipid.

  RUN create-relhold.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var li-next-r-no as int no-undo.
  def var char-hdl as cha no-undo.
  DEF VAR li-next-release AS INT NO-UNDO.  

  /* Code placed here will execute PRIOR to standard behavior. */
/*   10051225 */
/*   find last oe-relh use-index r-no no-lock no-error.            */
/*   li-next-r-no = if avail oe-relh then oe-relh.r-no + 1 else 1. */
  RUN oe/getNextRelNo.p (INPUT "oe-relh", OUTPUT li-next-r-no).
  RUN oe/release#.p (g_company, OUTPUT li-next-release).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  assign oe-relh.company = g_company
         oe-relh.r-no = li-next-r-no
         oe-relh.rel-date = today
         oe-relh.release# = li-next-release
         .
  assign ship_name = ""
         ship_addr1 = ""
         ship_addr2 = ""
         ship_city  = ""
         ship_state = ""
         ship_zip = ""
         cust_name = ""
         cust_addr1 = ""
         cust_addr2 = ""
         cust_city = ""
         freight_term = ""
         cust_state = ""
         cust_zip = "" 
         line_i-no = "" 
         qty-oh    = 0 
         qty-ordered = 0
         qty-rel = 0
         qty-ship = 0 .

  display oe-relh.rel-date cust_name cust_addr1 cust_addr2 cust_city cust_state cust_zip
          ship_name ship_addr1 ship_addr2 ship_city ship_state ship_zip line_i-no freight_term qty-oh qty-ordered qty-rel qty-ship with frame {&frame-name}. 
  run dispatch ('row-changed').

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
ASSIGN adm-new-record = NO   .
  {methods/template/local/delete.i}

&IF "{&FIRST-ENABLED-TABLE}" EQ "notes" &THEN
    RUN custom/notewtrg.p (ROWID({&FIRST-ENABLED-TABLE})).
  &ENDIF

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/template/local/deleteAfter.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  def var lv-recid2 as recid no-undo.

  DEF BUFFER b-oe-rell-2 FOR oe-rell.

  /* Code placed here will execute PRIOR to standard behavior. */

 /* gdm - 02020902 */
 ASSIGN fi_hold = "Approved".

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  if avail oe-relh then do:
     find cust where cust.company = oe-relh.company and
                     cust.cust-no = oe-relh.cust-no no-lock no-error.
     if avail cust then assign cust_name = cust.name
                               cust_addr1 = cust.addr[1]
                               cust_addr2 = cust.addr[2]
                               cust_city = cust.city
                               cust_state = cust.state
                               cust_zip = cust.zip
                               freight_term = IF cust.frt-pay EQ "P" THEN "Prepaid"
                                              ELSE IF cust.frt-pay EQ "B" THEN "Bill"
                                              ELSE IF cust.frt-pay EQ "T" THEN "3rd Party"
                                              ELSE "Collect" .

     RUN oe/custxship.p (oe-relh.company,
                         oe-relh.cust-no,
                         oe-relh.ship-id,
                         BUFFER shipto).

     if avail shipto then assign ship_name = shipto.ship-name
                                 ship_addr1 = shipto.ship-addr[1]
                                 ship_addr2 = shipto.ship-addr[2]
                                 ship_city = shipto.ship-city
                                 ship_state = shipto.ship-state
                                 ship_zip   = shipto.ship-zip.                                                                               
  end.
  else 
       assign ship_name = ""
              ship_addr1 = ""
              ship_addr2 = ""
              ship_city  = ""
              ship_state = ""
              ship_zip = ""
              cust_name = ""
              cust_addr1 = ""
              cust_addr2 = ""
              cust_city = ""
              freight_term = ""
              cust_state = ""
              cust_zip = "".
  display cust_name cust_addr1 cust_addr2 cust_city cust_state cust_zip
          ship_name ship_addr1 ship_addr2 ship_city ship_state ship_zip freight_term
          with frame {&frame-name}.

  RUN check-hold.

  /* gdm - 02020902 */
  DISP fi_hold WITH FRAME {&FRAME-NAME}.

  find first b-oe-rell-2
      WHERE b-oe-rell-2.company EQ oe-relh.company
        AND b-oe-rell-2.r-no    EQ oe-relh.r-no
      USE-INDEX r-no no-lock no-error.
  lv-recid2 = if avail b-oe-rell-2 then recid(b-oe-rell-2) else ?.
  run display-items (lv-recid2).

  /* If it's blank, will be added automatically with dialog box */
  IF NOT l-update-reason-perms 
        OR oe-relh.spare-char-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN
      ASSIGN oe-relh.spare-char-1:READ-ONLY IN FRAME {&FRAME-NAME} = TRUE 
             oe-relh.spare-char-1:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-adding-record AS LOG NO-UNDO.
  DEF VAR lv-date LIKE oe-relh.rel-date NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust-user NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-ship-id NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-rel-date NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-date-change NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   /* ========== end of validation ==========*/

  ASSIGN
   lv-adding-record = adm-adding-record
   lv-date          = oe-relh.rel-date.
  IF lv-adding-record THEN
     ASSIGN oe-relh.spare-char-1:READ-ONLY IN FRAME {&FRAME-NAME} = TRUE 
            oe-relh.spare-char-1:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF oe-relh.rel-date NE lv-date AND
     NOT lv-adding-record        THEN do:
      RUN oe/d-dudate.w (ROWID(oe-relh)). 
      ASSIGN 
        oe-relh.printed = NO /* task 05211304 */
        oe-relh.spare-char-3 = "". 
  END.

  run dispatch ('row-changed').
  ll-got-ship-id = no.
  IF lv-adding-record THEN DO:
     DEF VAR char-hdl AS cha NO-UNDO.
     RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-target",OUTPUT char-hdl).
     RUN add-line IN WIDGET-HANDLE(char-hdl).
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
   

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Buttons were made not sensitive during add, so reverse that here */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  adm-adding-record = NO.
  
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

  DO WITH FRAME {&FRAME-NAME}:
    FIND cust
        WHERE cust.company EQ cocode
          AND cust.cust-no EQ oe-relh.cust-no:SCREEN-VALUE
          AND INDEX("AX",cust.active) GT 0
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN RUN display-cust-detail (RECID(cust)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-first-record V-table-Win 
PROCEDURE new-first-record :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF INPUT PARAM ip-recid AS ROWID NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-ship-id V-table-Win 
PROCEDURE new-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN oe/custxship.p (oe-relh.company,
                        oe-relh.cust-no:SCREEN-VALUE,
                        oe-relh.ship-id:SCREEN-VALUE,
                        BUFFER shipto).

    IF AVAIL shipto THEN DO:
           ASSIGN
        oe-relh.ship-id:SCREEN-VALUE = shipto.ship-id
        ship_name:SCREEN-VALUE       = shipto.ship-name
        ship_addr1:SCREEN-VALUE      = shipto.ship-addr[1]
        ship_addr2:SCREEN-VALUE      = shipto.ship-addr[2]
        ship_city:SCREEN-VALUE       = shipto.ship-city
        ship_state:SCREEN-VALUE      = shipto.ship-state
        ship_zip:SCREEN-VALUE        = shipto.ship-zip
        oe-relh.carrier:SCREEN-VALUE = shipto.carrier
        lv-ship-no                   = shipto.ship-no.


    END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE security V-table-Win 
PROCEDURE security :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-secureflg AS LOG NO-UNDO.
{&methods/lValidateError.i YES}
RUN sys/ref/d-passwd.w (8, OUTPUT v-secureflg).

IF NOT v-secureflg THEN  DO:
    MESSAGE 
        " Wrong Password entered." 
        "Please contact your supervisor for security clearance."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.

END.

IF v-secureflg THEN
   RUN hold-release.
{&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE select_notes V-table-Win 
PROCEDURE select_notes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

IF AVAIL oe-relh THEN
  RUN windows/datenote.w (INPUT oe-relh.rec_key, INPUT PROGRAM-NAME(1), "RDC", "R").

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
  {src/adm/template/snd-list.i "oe-relh"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNavigation V-table-Win 
PROCEDURE setNavigation :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
def var char-hdl as cha no-undo.
DEF VAR phandle AS HANDLE NO-UNDO.
run get-link-handle in adm-broker-hdl(this-procedure,"tableIO",output char-hdl).

IF avail(oe-relh)  THEN DO:
  IF oe-relh.posted = YES THEN 
    run set-buttons in widget-handle(char-hdl) ("disable-all").
  ELSE 
    run set-buttons in widget-handle(char-hdl) ("INITIAL").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-no V-table-Win 
PROCEDURE valid-cust-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    lv-msg = "".

    IF oe-relh.cust-no:SCREEN-VALUE EQ "" OR
       NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ cocode
                      AND cust.cust-no EQ oe-relh.cust-no:SCREEN-VALUE
                      AND INDEX("AXSE",cust.active) GT 0) THEN
      lv-msg = IF oe-relh.cust-no:SCREEN-VALUE EQ "" THEN
                 "may not be blank" ELSE " invalid".

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(oe-relh.cust-no:LABEL) +
              lv-msg + ", try help...." VIEW-AS ALERT-BOX.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-cust-user B-table-Win 
PROCEDURE valid-cust-user :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 custcount = "".
DEF VAR lActive AS LOG NO-UNDO.
RUN sys/ref/CustList.p (INPUT cocode,
                            INPUT 'OT1',
                            INPUT YES,
                            OUTPUT lActive).
 {sys/inc/chblankcust.i ""OT1""}
 
  IF ou-log THEN
    DO WITH FRAME {&FRAME-NAME}:
      IF LOOKUP(oe-relh.cust-no:SCREEN-VALUE ,custcount) = 0 THEN DO:
          MESSAGE "Customer is not on Users Customer List.  "  SKIP
              "Please add customer to Network Admin - Users Customer List."  VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO oe-relh.cust-no .
          RETURN ERROR.
      END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date-change V-table-Win 
PROCEDURE valid-date-change :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    DEF VAR v-reject-code AS CHAR NO-UNDO.
    v-reject-code = oe-relh.spare-char-1:SCREEN-VALUE.

   FIND FIRST rejct-cd 
       WHERE rejct-cd.TYPE = "R" 
         AND rejct-cd.CODE = v-reject-code
       NO-LOCK NO-ERROR.


    IF NOT AVAIL rejct-cd AND v-reject-code GT "" THEN DO:
      MESSAGE "Invalid " + TRIM(oe-relh.spare-char-1:LABEL) +
              ", try help..." VIEW-AS ALERT-BOX.
      APPLY "entry" TO oe-relh.spare-char-1.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-id V-table-Win 
PROCEDURE valid-ship-id :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    RUN oe/custxship.p (oe-relh.company,
                        oe-relh.cust-no:SCREEN-VALUE,
                        oe-relh.ship-id:SCREEN-VALUE,
                        BUFFER shipto).

    IF NOT AVAIL shipto OR oe-relh.ship-id:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE TRIM(oe-relh.ship-id:LABEL) +
              " doesn't exist for this customer, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-relh.ship-id.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

