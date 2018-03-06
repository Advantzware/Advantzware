&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: po\v-purord.w

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
&SCOPED-DEFINE check-dropship dropship
&SCOPED-DEFINE post-enable post-enable

{custom/globdefs.i}
{sys/inc/VAR.i "new shared"}
DEFINE            VARIABLE v-exp-limit         AS INTEGER   NO-UNDO INIT 10.
DEFINE            VARIABLE nufile              AS LOG       NO-UNDO.
DEFINE            VARIABLE fil_id              AS RECID     NO-UNDO.
DEFINE            VARIABLE ls-drop-custno      AS cha       NO-UNDO.
DEFINE            VARIABLE ls-ship-choice      AS cha       NO-UNDO.
DEFINE            VARIABLE lv-ship-no          LIKE shipto.ship-no NO-UNDO.
DEFINE            VARIABLE ll-got-vendor       AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-type             LIKE po-ord.type NO-UNDO.
DEFINE            VARIABLE lv-prev-val         AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-copy-from-po-num AS INTEGER   NO-UNDO.
DEFINE            VARIABLE ip-company          AS CHARACTER NO-UNDO.
DEFINE            VARIABLE ip-po-no            AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-order-list        AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-copied-from       AS INTEGER   NO-UNDO.
DEFINE            VARIABLE ll-ord-no-override  AS LOG       NO-UNDO.

/* Used in vend-cost */
DEFINE            VARIABLE v-qty              AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-cost             AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-pb-qty           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-pb-stp           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-pb-cst           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-pb-cns           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-save-qty         AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-setup            AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE li                 AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-added-cost      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-added-cons-cost AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-adder-setup     AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lv-recid           AS RECID     NO-UNDO.
DEFINE            VARIABLE lv-t-cost          AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE ld-dim-charge      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-index            AS INTEGER   NO-UNDO.
DEFINE            VARIABLE fg-uom-list        AS CHARACTER NO-UNDO.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DEFINE TEMP-TABLE tt-ei NO-UNDO
FIELD std-uom AS CHARACTER.

DEFINE TEMP-TABLE tt-eiv NO-UNDO
FIELD run-qty  AS DECIMAL   DECIMALS 3 EXTENT 20
FIELD run-cost AS DECIMAL   DECIMALS 4 EXTENT 20
FIELD setups   AS DECIMAL   DECIMALS 2 EXTENT 20
FIELD rec_key  AS CHARACTER.

DEFINE NEW SHARED VARIABLE factor#             AS DECIMAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE v-default-gl-log    AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE v-default-gl-cha    AS cha       NO-UNDO.
DEFINE NEW SHARED VARIABLE v-po-qty            AS LOG       INITIAL TRUE NO-UNDO.
DEFINE NEW SHARED VARIABLE v-po-msf            LIKE sys-ctrl.int-fld NO-UNDO.

DEFINE TEMP-TABLE tt-ord-no 
    FIELD LINE   AS INTEGER
    FIELD ord-no AS INTEGER .

ASSIGN 
    cocode = g_company
    locode = g_loc.

DO TRANSACTION:
    {sys/inc/aptax.i}
    {sys/ref/postatus.i} 
    {sys/inc/poqty.i}
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
&Scoped-define EXTERNAL-TABLES po-ord
&Scoped-define FIRST-EXTERNAL-TABLE po-ord


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR po-ord.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS po-ord.po-date po-ord.type po-ord.vend-no ~
po-ord.ship-id po-ord.buyer po-ord.contact po-ord.due-date ~
po-ord.last-ship-date po-ord.under-pct po-ord.over-pct po-ord.carrier ~
po-ord.tax-gr po-ord.terms po-ord.frt-pay po-ord.fob-code po-ord.t-freight 
&Scoped-define ENABLED-TABLES po-ord
&Scoped-define FIRST-ENABLED-TABLE po-ord
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 btnCalendar-2 btnCalendar-3 ~
RECT-1 RECT-13 
&Scoped-Define DISPLAYED-FIELDS po-ord.po-no po-ord.po-date po-ord.type ~
po-ord.stat po-ord.vend-no po-ord.ship-id po-ord.ship-name ~
po-ord.ship-addr[1] po-ord.ship-addr[2] po-ord.ship-city po-ord.ship-state ~
po-ord.ship-zip po-ord.buyer po-ord.contact po-ord.due-date ~
po-ord.last-ship-date po-ord.under-pct po-ord.over-pct po-ord.carrier ~
po-ord.tax-gr po-ord.terms po-ord.frt-pay po-ord.fob-code po-ord.t-freight ~
po-ord.tax po-ord.t-cost po-ord.approved-date po-ord.approved-id 
&Scoped-define DISPLAYED-TABLES po-ord
&Scoped-define FIRST-DISPLAYED-TABLE po-ord
&Scoped-Define DISPLAYED-OBJECTS fc_app_time lv_vend-name lv_vend-add1 ~
lv_vend-add2 lv_vend-city lv_vend-state lv_vend-zip shipAreaCode shipPhone ~
lv_vend-area-code lv_vend-phone typeDescr approved_text 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-ASSIGN-FIELDS po-ord.po-no po-ord.ship-name ~
po-ord.ship-addr[1] po-ord.ship-addr[2] po-ord.ship-city po-ord.ship-state ~
po-ord.ship-zip 
&Scoped-define ROW-AVAILABLE btnCalendar-1 btnCalendar-2 btnCalendar-3 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _XFTR "Foreign Keys" V-table-Win _INLINE
/* Actions: ? adm/support/keyedit.w ? ? ? */
/* STRUCTURED-DATA
<KEY-OBJECT>
THIS-PROCEDURE
</KEY-OBJECT>
<FOREIGN-KEYS>
company||y|ASI.po-ord.company
Carrier||y|ASI.po-ord.Carrier
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = ,
     Keys-Supplied = "company,Carrier"':U).
/**************************
</EXECUTING-CODE> */
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalendar-1 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U NO-FOCUS FLAT-BUTTON
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-2 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE BUTTON btnCalendar-3 
     IMAGE-UP FILE "Graphics/16x16/calendar.bmp":U
     LABEL "" 
     SIZE 4.6 BY 1.05 TOOLTIP "PopUp Calendar".

DEFINE VARIABLE fc_app_time AS CHARACTER FORMAT "X(8)":U 
     LABEL "Time" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1 NO-UNDO.

DEFINE VARIABLE lv_vend-add1 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE lv_vend-add2 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE lv_vend-area-code AS CHARACTER FORMAT "(999)" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE lv_vend-city AS CHARACTER FORMAT "x(16)" 
     VIEW-AS FILL-IN 
     SIZE 21.2 BY 1.

DEFINE VARIABLE lv_vend-name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE lv_vend-phone AS CHARACTER FORMAT "999-9999" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE lv_vend-state AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1.

DEFINE VARIABLE lv_vend-zip AS CHARACTER FORMAT "xxxxx-xxxx" INITIAL "00000-0000" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE shipAreaCode AS CHARACTER FORMAT "(999)" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE shipPhone AS CHARACTER FORMAT "999-9999" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE typeDescr AS CHARACTER FORMAT "X(256)":U INITIAL "Type Description" 
      VIEW-AS TEXT 
     SIZE 20 BY 1 NO-UNDO. 

DEFINE VARIABLE approved_text AS CHARACTER FORMAT "X(8)":U INITIAL "Approved" 
      VIEW-AS TEXT 
     SIZE 12 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 148 BY 15.48.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 21 BY 3.57.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fc_app_time AT ROW 2.91 COL 133.8 COLON-ALIGNED WIDGET-ID 8
     btnCalendar-1 AT ROW 1.24 COL 68.4
     po-ord.po-no AT ROW 1.24 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     po-ord.po-date AT ROW 1.24 COL 50 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     po-ord.type AT ROW 1.24 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     po-ord.stat AT ROW 1.24 COL 119 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     po-ord.vend-no AT ROW 2.43 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     po-ord.ship-id AT ROW 2.43 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     lv_vend-name AT ROW 3.38 COL 23 COLON-ALIGNED HELP
          "Enter Vendor name." NO-LABEL
     po-ord.ship-name AT ROW 3.38 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     po-ord.ship-addr[1] AT ROW 4.33 COL 83 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     lv_vend-add1 AT ROW 4.38 COL 23 COLON-ALIGNED NO-LABEL
     po-ord.ship-addr[2] AT ROW 5.29 COL 83 COLON-ALIGNED NO-LABEL FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     lv_vend-add2 AT ROW 5.38 COL 23 COLON-ALIGNED NO-LABEL
     po-ord.ship-city AT ROW 6.24 COL 83 COLON-ALIGNED NO-LABEL FORMAT "x(16)"
          VIEW-AS FILL-IN 
          SIZE 21.2 BY 1
     po-ord.ship-state AT ROW 6.24 COL 105 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     po-ord.ship-zip AT ROW 6.24 COL 110 COLON-ALIGNED NO-LABEL FORMAT "xxxxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     lv_vend-city AT ROW 6.38 COL 23 COLON-ALIGNED NO-LABEL
     lv_vend-state AT ROW 6.38 COL 45 COLON-ALIGNED HELP
          "Enter the vendor's state." NO-LABEL
     lv_vend-zip AT ROW 6.38 COL 50 COLON-ALIGNED NO-LABEL
     shipAreaCode AT ROW 7.19 COL 83 COLON-ALIGNED NO-LABEL
     shipPhone AT ROW 7.19 COL 90 COLON-ALIGNED HELP
          "Enter the Vendor's telephone number." NO-LABEL
     lv_vend-area-code AT ROW 7.43 COL 23 COLON-ALIGNED NO-LABEL
     lv_vend-phone AT ROW 7.43 COL 30 COLON-ALIGNED HELP
          "Enter the Vendor's telephone number." NO-LABEL
     po-ord.buyer AT ROW 9.1 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     po-ord.contact AT ROW 10.29 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     po-ord.due-date AT ROW 11.48 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     po-ord.last-ship-date AT ROW 12.67 COL 23 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 18 BY 1
     btnCalendar-2 AT ROW 11.43 COL 43.4
     btnCalendar-3 AT ROW 12.62 COL 43.4
     po-ord.under-pct AT ROW 13.86 COL 23 COLON-ALIGNED
          LABEL "Under / Overrun %"
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     po-ord.over-pct AT ROW 13.86 COL 36 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     po-ord.carrier AT ROW 9.1 COL 83 COLON-ALIGNED FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     po-ord.tax-gr AT ROW 10.29 COL 83 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     po-ord.terms AT ROW 11.48 COL 83 COLON-ALIGNED FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     po-ord.frt-pay AT ROW 13.86 COL 85 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Prepaid", "P":U,
"Collect", "C":U,
"Bill", "B":U
          SIZE 46 BY 1
     po-ord.fob-code AT ROW 15.05 COL 85 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL EXPAND 
          RADIO-BUTTONS 
                    "Destination", "Dest":U,
"Origination", "Orig":U
          SIZE 35 BY 1
     po-ord.t-freight AT ROW 9.1 COL 119 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     po-ord.tax AT ROW 10.29 COL 119 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12.8 BY 1
     po-ord.t-cost AT ROW 11.48 COL 119 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.6 BY 1
     typeDescr AT ROW 1.24 COL 88 COLON-ALIGNED NO-LABEL
     po-ord.approved-date AT ROW 1.95 COL 133.8 COLON-ALIGNED WIDGET-ID 2
          LABEL "Date"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     po-ord.approved-id AT ROW 3.95 COL 133.8 COLON-ALIGNED WIDGET-ID 4
          LABEL "By"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          "FOB:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 15.05 COL 78
     "Freight Payment:" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 13.86 COL 65
     /*"Approved" VIEW-AS TEXT*/
      approved_text
          /*SIZE 12 BY .62*/ AT ROW 1.24 COL 130.2 WIDGET-ID 12 NO-LABEL
     RECT-1 AT ROW 1 COL 1
     RECT-13 AT ROW 1.71 COL 128 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.po-ord
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
         HEIGHT             = 16.52
         WIDTH              = 158.2.
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

/* SETTINGS FOR FILL-IN po-ord.approved-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN po-ord.approved-id IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-3 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN po-ord.carrier IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN fc_app_time IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_vend-add1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_vend-add2 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_vend-area-code IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_vend-city IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_vend-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_vend-phone IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_vend-state IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN lv_vend-zip IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN po-ord.po-no IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN po-ord.ship-addr[1] IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN po-ord.ship-addr[2] IN FRAME F-Main
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN po-ord.ship-city IN FRAME F-Main
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN po-ord.ship-name IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN po-ord.ship-state IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN po-ord.ship-zip IN FRAME F-Main
   NO-ENABLE 2 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN shipAreaCode IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN shipPhone IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN po-ord.stat IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN po-ord.t-cost IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN po-ord.tax IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN po-ord.tax-gr IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN po-ord.terms IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN typeDescr IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN po-ord.under-pct IN FRAME F-Main
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
ON CTRL-O OF FRAME F-Main
OR ctrl-o OF btnCalendar-1 ANYWHERE
DO:
  DEFINE VARIABLE char-hdl AS CHARACTER.

  ll-ord-no-override = TRUE.
  /* Add with ctrl-o allows user to specify an order number */
  /* prior to getting the next sequence value for order number */
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"tableio-source",OUTPUT char-hdl).
  RUN manual-apply-add IN WIDGET-HANDLE(char-hdl).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-Main V-table-Win
ON HELP OF FRAME F-Main
DO:
    DEFINE VARIABLE char-val AS cha    NO-UNDO.
    DEFINE VARIABLE rec-val  AS RECID  NO-UNDO.
    DEFINE VARIABLE lw-focus AS HANDLE NO-UNDO.


    lw-focus = FOCUS.

    CASE FOCUS:NAME:
        WHEN "vend-no" THEN DO:
            RUN windows/l-vendno.w (g_company, "A", lw-focus:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" AND TRIM(lw-focus:SCREEN-VALUE) NE TRIM(ENTRY(1,char-val)) THEN DO:
               lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
               FIND FIRST vend
                   WHERE vend.company EQ po-ord.company
                   AND vend.vend-no EQ lw-focus:SCREEN-VALUE
                   NO-LOCK NO-ERROR.
               IF AVAILABLE vend THEN DO:
                   ASSIGN po-ord.due-date:SCREEN-VALUE = STRING(DATE(po-ord.po-date:SCREEN-VALUE) + vend.disc-days) .
               END.
               RUN new-vend-no.
            END.
        END.  
        WHEN "ship-id" THEN DO:
            IF ls-ship-choice = "C" THEN DO:
               RUN windows/l-shipt2.w (g_company,g_loc, ls-drop-custno, lw-focus:SCREEN-VALUE, OUTPUT char-val, OUTPUT rec-val).
               IF char-val NE "" THEN RUN display-shipto (rec-val).
            END.
            ELSE IF ls-ship-choice = "V" THEN DO:
                 RUN windows/l-vendno.w (g_company, "A", lw-focus:SCREEN-VALUE, OUTPUT char-val).
                 IF char-val NE "" THEN DO:
                    ASSIGN lw-focus:SCREEN-VALUE = ENTRY(1,char-val)
                       .
                    RUN display-vend-to.
                 END.
            END.
        END.  /* when ship-id */
        WHEN "buyer" THEN DO:
            RUN windows/l-buyer.w (g_company, lw-focus:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        END.
        WHEN "carrier" THEN DO:
            RUN windows/l-carrie.w (g_company, g_loc, lw-focus:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        END. 
        WHEN "tax-gr" THEN DO:
            RUN windows/l-stax.w (g_company, lw-focus:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        END.
        WHEN "terms" THEN DO:
            RUN windows/l-terms.w (g_company,  lw-focus:SCREEN-VALUE, OUTPUT char-val).
            IF char-val NE "" THEN lw-focus:SCREEN-VALUE = ENTRY(1,char-val).
        END.
    END CASE.

    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 V-table-Win
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i po-ord.po-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 V-table-Win
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i po-ord.due-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-3 V-table-Win
ON CHOOSE OF btnCalendar-3 IN FRAME F-Main
DO:
  {methods/btnCalendar.i po-ord.last-ship-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.carrier V-table-Win
ON LEAVE OF po-ord.carrier IN FRAME F-Main /* Shipping Carrier */
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}
    IF SELF:MODIFIED THEN DO:
       FIND FIRST carrier WHERE carrier.company = g_company AND
                                carrier.carrier = SELF:SCREEN-VALUE
                   NO-LOCK NO-ERROR.
       IF NOT AVAILABLE carrier THEN DO:
          MESSAGE "Invalid Carrier. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
    END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.due-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.due-date V-table-Win
ON HELP OF po-ord.due-date IN FRAME F-Main /* Required Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.fob-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.fob-code V-table-Win
ON VALUE-CHANGED OF po-ord.fob-code IN FRAME F-Main /* FOB Origin/Dest */
DO:
  RUN new-fob-code.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.frt-pay
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.frt-pay V-table-Win
ON VALUE-CHANGED OF po-ord.frt-pay IN FRAME F-Main /* Freight Payment */
DO:
  IF po-ord.fob-code:SCREEN-VALUE = "Orig" THEN
     RUN new-fob-code.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.last-ship-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.last-ship-date V-table-Win
ON HELP OF po-ord.last-ship-date IN FRAME F-Main /* Last Ship Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.po-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.po-date V-table-Win
ON HELP OF po-ord.po-date IN FRAME F-Main /* PO Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.ship-id V-table-Win
ON ENTRY OF po-ord.ship-id IN FRAME F-Main /* Ship To */
DO:
  IF po-ord.type:SCREEN-VALUE NE "D" THEN DO:
    APPLY 'entry' TO po-ord.buyer.
    RETURN NO-APPLY.
  END. 
  ELSE
  IF NOT adm-new-record THEN RUN is-dropship. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.ship-id V-table-Win
ON LEAVE OF po-ord.ship-id IN FRAME F-Main /* Ship To */
DO:
  {&methods/lValidateError.i YES}
  IF LASTKEY NE -1 THEN DO:     
    IF ls-drop-custno NE "" THEN DO:
      FIND FIRST shipto NO-LOCK WHERE shipto.company EQ cocode
                          AND shipto.cust-no EQ ls-drop-custno
                          AND shipto.ship-id EQ INPUT po-ord.ship-id
                          NO-ERROR.
         IF NOT AVAILABLE shipto THEN
         DO:
            MESSAGE "Shipto " + INPUT po-ord.ship-id +
                   " Unavailable for Customer " + ls-drop-custno + ". Please Re-Enter."
                  VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO po-ord.ship-id.
            RETURN NO-APPLY.
         END.
         ELSE DO :
             FIND cust OF shipto NO-LOCK NO-ERROR.
             ASSIGN po-ord.ship-id:SCREEN-VALUE      = shipto.ship-id
                    po-ord.ship-name:SCREEN-VALUE    = shipto.ship-name
                    po-ord.ship-addr[1]:SCREEN-VALUE = shipto.ship-addr[1]
                    po-ord.ship-addr[2]:SCREEN-VALUE = shipto.ship-addr[2]
                    po-ord.ship-city:SCREEN-VALUE    = shipto.ship-city
                    po-ord.ship-state:SCREEN-VALUE   = shipto.ship-state
                    po-ord.ship-zip:SCREEN-VALUE     = shipto.ship-zip
                    lv-ship-no                       = shipto.ship-no
                    shipAreaCode:SCREEN-VALUE        = IF AVAILABLE cust THEN cust.area-code ELSE ""
                    shipPhone:SCREEN-VALUE           = IF AVAILABLE cust THEN cust.phone ELSE "".
             ASSIGN fil_id = RECID(shipto).
         END.
    END.
    ELSE DO:  /* vendor */
        FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                          AND vend.vend-no EQ INPUT po-ord.ship-id
                        NO-ERROR.
        IF AVAILABLE vend THEN DO:
            ASSIGN po-ord.ship-id:SCREEN-VALUE = vend.vend-no
                   fil_id                      = RECID(vend).
            ASSIGN po-ord.ship-name:SCREEN-VALUE    = vend.name
                   po-ord.ship-addr[1]:SCREEN-VALUE = vend.add1
                   po-ord.ship-addr[2]:SCREEN-VALUE = vend.add2
                   po-ord.ship-city:SCREEN-VALUE    = vend.city
                   po-ord.ship-state:SCREEN-VALUE   = vend.state
                   po-ord.ship-zip:SCREEN-VALUE     = vend.zip
                   ls-drop-custno                   = ""
                   shipAreaCode:SCREEN-VALUE        = vend.area-code
                   shipPhone:SCREEN-VALUE           = vend.phone.
        END.
        ELSE DO:
            MESSAGE "Invalid Vendor. Try help. " VIEW-AS ALERT-BOX ERROR.
            RETURN NO-APPLY.
        END.
    END.
    IF po-ord.ship-id:SCREEN-VALUE = "" THEN DO WITH FRAME {&FRAME-NAME} :
       ENABLE po-ord.ship-name
              po-ord.ship-addr[1]
              po-ord.ship-addr[2]
              po-ord.ship-city
              po-ord.ship-state
              po-ord.ship-zip.
       APPLY "entry" TO po-ord.ship-name.
    END.
 END. /* modified */
 {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.t-freight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.t-freight V-table-Win
ON ENTRY OF po-ord.t-freight IN FRAME F-Main /* Total Freight */
DO:
  lv-prev-val = {&self-name}:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.t-freight V-table-Win
ON VALUE-CHANGED OF po-ord.t-freight IN FRAME F-Main /* Total Freight */
DO:
  RUN new-t-freight.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.tax-gr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.tax-gr V-table-Win
ON LEAVE OF po-ord.tax-gr IN FRAME F-Main /* Tax Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-gr (FOCUS) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.terms
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.terms V-table-Win
ON LEAVE OF po-ord.terms IN FRAME F-Main /* Payment Terms */
DO:
    IF LASTKEY = -1 THEN RETURN.
    {&methods/lValidateError.i YES}
    IF SELF:MODIFIED THEN DO:
       FIND FIRST terms NO-LOCK WHERE terms.company = g_company AND
                              terms.t-code = SELF:SCREEN-VALUE
                   NO-ERROR.
       IF NOT AVAILABLE terms THEN DO:
          MESSAGE "Invalid Terms. Try Help. " VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
       END.
    END.
    {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.type V-table-Win
ON LEAVE OF po-ord.type IN FRAME F-Main /* Type */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME po-ord.vend-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.vend-no V-table-Win
ON LEAVE OF po-ord.vend-no IN FRAME F-Main /* Vendor */
DO:
  IF LASTKEY NE -1 THEN DO:
    IF SELF:MODIFIED THEN RUN new-vend-no.

    RUN valid-vend-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF SELF:MODIFIED AND po-ord.type:SCREEN-VALUE EQ "D" THEN RUN is-dropship.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL po-ord.vend-no V-table-Win
ON VALUE-CHANGED OF po-ord.vend-no IN FRAME F-Main /* Vendor */
DO:

  FIND FIRST vend
      WHERE vend.company EQ po-ord.company
      AND vend.vend-no EQ po-ord.vend-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}
      NO-LOCK NO-ERROR.

  IF AVAILABLE vend THEN DO:
      ASSIGN po-ord.due-date:SCREEN-VALUE = STRING(DATE(po-ord.po-date:SCREEN-VALUE) + vend.disc-days /*+
                                                                                        IF WEEKDAY(po-ord.po-date) EQ 6 THEN 3 ELSE 1*/ ) .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-po V-table-Win 
PROCEDURE add-po :
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

  /* No Foreign keys are accepted by this SmartObject. */

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
  {src/adm/template/row-list.i "po-ord"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "po-ord"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE change-page-logic V-table-Win 
PROCEDURE change-page-logic :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Give focus to something in frame so that ctrl-o will be caught */
  /* This procedure called from change-page in w-order */
  APPLY 'entry' TO btnCalendar-1 IN FRAME f-main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-dropship V-table-Win 
PROCEDURE check-dropship :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*IF NOT adm-new-record THEN IF po-ord.TYPE = "D" THEN RUN is-dropship.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE close-reopen V-table-Win 
PROCEDURE close-reopen :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).

  RUN browse-rowid IN WIDGET-HANDLE(char-hdl) (OUTPUT lv-rowid).

  RUN po/d-clspo.w (ROWID(po-ord)).

  RUN repo-query IN WIDGET-HANDLE(char-hdl) (lv-rowid).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-shipto V-table-Win 
PROCEDURE display-shipto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME} :
    FIND FIRST shipto WHERE RECID(shipto) = ip-recid
                          NO-LOCK NO-ERROR.
  IF NOT AVAILABLE shipto THEN
         DO:
            MESSAGE "Shipto " + INPUT po-ord.ship-id +
                   " Unavailable for Customer " + ls-drop-custno + ". Please Re-Enter."
                  VIEW-AS ALERT-BOX ERROR.
            RETURN ERROR.
  END.
  ELSE DO:
       FIND cust OF shipto NO-LOCK NO-ERROR.
       ASSIGN po-ord.ship-id:SCREEN-VALUE      = shipto.ship-id
              po-ord.ship-name:SCREEN-VALUE    = shipto.ship-name
              po-ord.ship-addr[1]:SCREEN-VALUE = shipto.ship-addr[1]
              po-ord.ship-addr[2]:SCREEN-VALUE = shipto.ship-addr[2]
              po-ord.ship-city:SCREEN-VALUE    = shipto.ship-city
              po-ord.ship-state:SCREEN-VALUE   = shipto.ship-state
              po-ord.ship-zip:SCREEN-VALUE     = shipto.ship-zip
              lv-ship-no                       = shipto.ship-no
              shipAreaCode:SCREEN-VALUE        = IF AVAILABLE cust THEN cust.area-code ELSE ""
              shipPhone:SCREEN-VALUE           = IF AVAILABLE cust THEN cust.phone ELSE "".
    END.
  END.
  {&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-vend V-table-Win 
PROCEDURE display-vend :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&frame-name}:
    FIND FIRST vend NO-LOCK 
        WHERE vend.company EQ po-ord.company
          AND vend.vend-no EQ po-ord.vend-no:SCREEN-VALUE
        USE-INDEX vend NO-ERROR.
    IF AVAILABLE vend THEN DO:
      ASSIGN
       lv_vend-name:SCREEN-VALUE  = vend.name
       lv_vend-add1:SCREEN-VALUE  = vend.add1
       lv_vend-add2:SCREEN-VALUE  = vend.add2
       lv_vend-city:SCREEN-VALUE  = vend.city
       lv_vend-state:SCREEN-VALUE = vend.state
       lv_vend-zip:SCREEN-VALUE   = vend.zip.

      ASSIGN
       lv_vend-area-code:SCREEN-VALUE = vend.area-code
       lv_vend-phone:SCREEN-VALUE     = vend.phone NO-ERROR.

      IF ERROR-STATUS:ERROR THEN
        MESSAGE "Phone# from Vendor File invalid, please correct..."
            VIEW-AS ALERT-BOX WARNING.
    END.
  END.
  {&methods/lValidateError.i NO}
END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-vend-to V-table-Win 
PROCEDURE display-vend-to :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:
    FIND FIRST vend NO-LOCK 
        WHERE vend.company EQ po-ord.company
          AND vend.vend-no EQ po-ord.ship-id:SCREEN-VALUE
        USE-INDEX vend NO-ERROR.
    IF AVAILABLE vend THEN
      ASSIGN
       po-ord.ship-name:SCREEN-VALUE   = IF AVAILABLE vend THEN vend.name ELSE ''
       po-ord.ship-add[1]:SCREEN-VALUE = IF AVAILABLE vend THEN vend.add1 ELSE ''
       po-ord.ship-add[2]:SCREEN-VALUE = IF AVAILABLE vend THEN vend.add2 ELSE ''
       po-ord.ship-city:SCREEN-VALUE   = IF AVAILABLE vend THEN vend.city ELSE ''
       po-ord.ship-state:SCREEN-VALUE  = IF AVAILABLE vend THEN vend.state ELSE ''
       po-ord.ship-zip:SCREEN-VALUE    = IF AVAILABLE vend THEN vend.zip ELSE ''
       shipAreaCode:SCREEN-VALUE       = IF AVAILABLE vend THEN vend.area-code ELSE ''
       shipPhone:SCREEN-VALUE          = IF AVAILABLE vend THEN vend.phone ELSE ''.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-parameters V-table-Win 
PROCEDURE get-parameters :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER op-company AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-po-no   AS INTEGER NO-UNDO.
op-company = ip-company.
op-po-no   = ip-po-no.

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
 IF po-ord.po-date:SENSITIVE IN FRAME {&FRAME-NAME} THEN DO:
    MESSAGE "You can not change status middle of modification. " VIEW-AS ALERT-BOX
        ERROR.
    RETURN.
 END.
 IF AVAILABLE po-ord THEN DO:
     MESSAGE "Are you sure you wish to " +
          trim(STRING(po-ord.stat EQ "H","release/hold")) + " this PO?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice AS LOG.
     IF choice THEN DO:  
        DEFINE BUFFER bf-po-ord FOR po-ord.
        FIND bf-po-ord EXCLUSIVE-LOCK WHERE RECID(bf-po-ord) EQ recid(po-ord) NO-ERROR.
        IF bf-po-ord.stat = "H" then
           ASSIGN bf-po-ord.approved-date = today
                  bf-po-ord.approved-id = userid('nosweat')
                  bf-po-ord.approved-time = TIME. 
        ELSE ASSIGN bf-po-ord.approved-date = ?
                  bf-po-ord.approved-id = ""
                  bf-po-ord.approved-time = 0.           
        bf-po-ord.stat = IF bf-po-ord.stat EQ "H" THEN "O" ELSE "H".   

     END.
     FIND CURRENT bf-po-ord NO-LOCK NO-ERROR.
     FIND CURRENT po-ord NO-LOCK NO-ERROR.
     IF AVAILABLE po-ord THEN DO: 
        DISPLAY po-ord.stat po-ord.approved-date po-ord.approved-id RECT-13 approved_text
             WITH FRAME {&FRAME-NAME}.
        fc_app_time:SCREEN-VALUE = IF AVAILABLE po-ord THEN string(po-ord.approved-time,"HH:MM") ELSE "".
        fc_app_time:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .
        po-ord.approved-date:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .
        po-ord.approved-id:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE  .
        RECT-13:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .
        approved_text:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .
     END.        
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE is-dropship V-table-Win 
PROCEDURE is-dropship :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE look-recid  AS RECID     NO-UNDO.
  DEFINE VARIABLE ll-choice   AS LOG       INIT NO NO-UNDO.
  DEFINE VARIABLE lv-stat     AS CHARACTER NO-UNDO.

  DEFINE VARIABLE ship-custno AS CHARACTER LABEL "Enter Customer Number" NO-UNDO.
  DEFINE VARIABLE ship-choice AS LO        LABEL "Ship To" VIEW-AS RADIO-SET HORIZONTAL
       RADIO-BUTTONS "Vendor", YES,
                     "Customer", NO
       SIZE 44 BY 3 NO-UNDO.

  DEFINE BUTTON Btn_OK AUTO-GO 
       LABEL "OK" 
       SIZE 15 BY 1.14
       BGCOLOR 8 .

  DEFINE FRAME f-DROP
         ship-choice SKIP
         SPACE(5)
         ship-custno SKIP
         btn_ok AT ROW 6 COL 20
         WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE .
  {&methods/lValidateError.i YES}
  ON "value-changed" OF ship-choice
  DO:
      ASSIGN ship-choice.
      IF NOT ship-choice THEN DO:
         ship-custno:HIDDEN = NO.  
         ls-ship-choice = "C".
         ENABLE ship-custno WITH FRAME f-drop.
         APPLY "entry" TO ship-custno.
      END.
      ELSE DO:
          ship-custno:HIDDEN = YES. 
          ls-ship-choice = "V".
          DISABLE ship-custno WITH FRAME f-drop.
      END.
  END.
  ON 'leave':U OF ship-custno 
  DO:
      IF NOT CAN-FIND(FIRST cust
                      WHERE cust.company EQ cocode
                        AND cust.cust-no EQ ship-custno:SCREEN-VALUE
                        AND INDEX("AXSE",cust.active) GT 0)
      THEN DO:
          MESSAGE "Invalid Customer#. Try Help." VIEW-AS ALERT-BOX ERROR.
          RETURN NO-APPLY.
      END.
      APPLY "choose" TO btn_ok IN FRAME f-drop.
  END.
  ON 'help':U OF ship-custno 
  DO:
      DEFINE VARIABLE char-val AS cha NO-UNDO.
      RUN windows/l-custact.w (cocode, ship-custno:SCREEN-VALUE, OUTPUT char-val,OUTPUT look-recid).
      IF char-val NE "" THEN DO:
        ship-custno:SCREEN-VALUE = ENTRY(1,char-val).
        APPLY "leave" TO ship-custno IN FRAME f-drop.
      END.
      RETURN NO-APPLY.
  END.
  ON 'choose':U OF btn_ok
  DO:
      ASSIGN
       ship-choice
       ship-custno.

      DO WITH FRAME {&FRAME-NAME}:
        IF ship-choice NE (ls-drop-custno EQ "") OR
           lv-type NE po-ord.type:SCREEN-VALUE   THEN DO: 
          ls-drop-custno = IF ship-choice THEN "" ELSE ship-custno.

          IF ship-choice THEN
            ASSIGN
             po-ord.ship-id:SCREEN-VALUE      = ""
             po-ord.ship-name:SCREEN-VALUE    = ""
             po-ord.ship-addr[1]:SCREEN-VALUE = ""
             po-ord.ship-addr[2]:SCREEN-VALUE = ""
             po-ord.ship-city:SCREEN-VALUE    = ""
             po-ord.ship-state:SCREEN-VALUE   = ""
             po-ord.ship-zip:SCREEN-VALUE     = ""
             shipAreaCode:SCREEN-VALUE        = ""
             shipPhone:SCREEN-VALUE           = "".
          ELSE DO: /*Task 07181204 */
              EACHPOORDL:
              FOR EACH po-ordl NO-LOCK WHERE 
                po-ordl.company = g_company
                AND po-ordl.po-no = po-ord.po-no:
                FOR EACH oe-rel WHERE
                    oe-rel.company EQ g_company AND
                    oe-rel.ord-no = INT(po-ordl.ord-no) AND
                    oe-rel.i-no = po-ordl.i-no
                    NO-LOCK:
                    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
                    IF LOOKUP(lv-stat,"S,I,L") = 0 THEN NEXT.
                    IF oe-rel.ship-id NE ls-drop-custno THEN DO:
                        MESSAGE "PO Shipto does not match Shipto for Order " oe-rel.ord-no "." SKIP
                            "Update Shipto on PO?"
                        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-choice.
                       LEAVE.
                    END. /*if oe-rel.ship-id ne po-ord.ship-id */
                END. /* each oe-rel */
              END. /*each po-ordl*/
              IF ll-choice THEN 
                    FIND FIRST shipto WHERE shipto.company EQ g_company AND
                      shipto.cust-no EQ po-ord.cust-no AND
                      shipto.ship-id EQ oe-rel.ship-id
                      NO-LOCK NO-ERROR.
              ELSE
                    FIND FIRST shipto WHERE shipto.company EQ g_company AND
                      shipto.cust-no EQ ls-drop-custno
                      NO-LOCK NO-ERROR.
              IF AVAILABLE shipto THEN DO:
                    FIND cust OF shipto NO-LOCK NO-ERROR.
                    ASSIGN
                            po-ord.ship-id:SCREEN-VALUE      = shipto.ship-id
                            po-ord.ship-name:SCREEN-VALUE    = shipto.ship-name
                            po-ord.ship-addr[1]:SCREEN-VALUE = shipto.ship-addr[1]
                            po-ord.ship-addr[2]:SCREEN-VALUE = shipto.ship-addr[2]
                            po-ord.ship-city:SCREEN-VALUE    = shipto.ship-city
                            po-ord.ship-state:SCREEN-VALUE   = shipto.ship-state
                            po-ord.ship-zip:SCREEN-VALUE     = shipto.ship-zip
                            lv-ship-no                       = shipto.ship-no
                            shipAreaCode:SCREEN-VALUE        = IF AVAILABLE cust THEN cust.area-code ELSE ""
                            shipPhone:SCREEN-VALUE           = IF AVAILABLE cust THEN cust.phone ELSE "".
                     IF po-ord.frt-pay:SCREEN-VALUE NE "P" THEN
                         po-ord.carrier:SCREEN-VALUE = shipto.carrier.
              END. /*avail shipto */                                                                          
/*           FOR EACH shipto                                                                   */
/*               WHERE shipto.company EQ cocode                                                */
/*                 AND shipto.cust-no EQ ls-drop-custno                                        */
/*               NO-LOCK                                                                       */
/*               BREAK BY shipto.ship-no DESC:                                                 */
/*                                                                                             */
/*             IF LAST(shipto.ship-no)             OR                                          */
/*                shipto.ship-id EQ ls-drop-custno THEN DO:                                    */
/*               FIND cust OF shipto NO-LOCK NO-ERROR.                                         */
/*               ASSIGN                                                                        */
/*                po-ord.ship-id:SCREEN-VALUE      = shipto.ship-id                            */
/*                po-ord.ship-name:SCREEN-VALUE    = shipto.ship-name                          */
/*                po-ord.ship-addr[1]:SCREEN-VALUE = shipto.ship-addr[1]                       */
/*                po-ord.ship-addr[2]:SCREEN-VALUE = shipto.ship-addr[2]                       */
/*                po-ord.ship-city:SCREEN-VALUE    = shipto.ship-city                          */
/*                po-ord.ship-state:SCREEN-VALUE   = shipto.ship-state                         */
/*                po-ord.ship-zip:SCREEN-VALUE     = shipto.ship-zip                           */
/*                lv-ship-no                       = shipto.ship-no                            */
/*                shipAreaCode:SCREEN-VALUE        = IF AVAIL cust THEN cust.area-code ELSE "" */
/*                shipPhone:SCREEN-VALUE           = IF AVAIL cust THEN cust.phone ELSE "".    */
/*                                                                                             */
/*               IF po-ord.frt-pay:SCREEN-VALUE NE "P" THEN                                    */
/*                 po-ord.carrier:SCREEN-VALUE = shipto.carrier.                               */
/*                                                                                             */
/*               LEAVE.                                                                        */
             END. /* ELSE ship-choice */
          END. /* if ship-choice ne "" */
        END. /* do with frame */
  END. /* on-choose */
  {&methods/lValidateError.i NO}
  CREATE WIDGET-POOL "w-drop".
  ASSIGN
   ship-custno = ls-drop-custno
   ship-choice = ls-drop-custno EQ "".

  DISPLAY ship-choice ship-custno WHEN NOT ship-choice WITH FRAME f-DROP.
  ENABLE ship-choice btn_ok WITH FRAME f-DROP.

  APPLY "value-changed" TO ship-choice.
  /*APPLY "entry" TO ship-choice.*/
  WAIT-FOR GO OF FRAME f-drop.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iv-copy-from-rec AS RECID NO-UNDO.
  DEFINE BUFFER bx-poord  FOR po-ord.
  DEFINE BUFFER bx-poline FOR po-ordl.
  DEFINE VARIABLE iv-poline-copied AS LOG NO-UNDO.
  DEFINE VARIABLE lv-prev-vend-no  AS cha NO-UNDO.
  DEFINE BUFFER bx-notes FOR notes.
  DEFINE VARIABLE lv-due-date  AS DATE      NO-UNDO.
  DEFINE VARIABLE v-new-orders AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vi           AS INTEGER   NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  EMPTY TEMP-TABLE tt-ord-no.
  ASSIGN
  iv-copy-from-rec = IF AVAILABLE po-ord THEN RECID(po-ord) ELSE ?
  lv-prev-vend-no  = IF AVAILABLE po-ord THEN po-ord.vend-no ELSE ""
  lv-due-date      = po-ord.due-date.
     FIND bx-poord WHERE RECID(bx-poord) = iv-copy-from-rec NO-LOCK NO-ERROR.
     IF AVAILABLE bx-poord THEN DO:
         ASSIGN ip-company    = bx-poord.company
            ip-po-no      = bx-poord.po-no
            v-copied-from = bx-poord.po-no.
        RUN set-copy-from (INPUT bx-poord.po-no).
        FIND FIRST bx-poline WHERE bx-poline.company = bx-poord.company 
            AND bx-poline.po-no = bx-poord.po-no
            AND bx-poline.ord-no NE 0 
            NO-LOCK NO-ERROR.
        ip-company = bx-poord.company.
        ip-po-no   = bx-poord.po-no.
     END.
  IF adm-new-record AND NOT adm-adding-record AND AVAILABLE bx-poline THEN DO:
      RUN po/w-cppoln.w (INPUT ip-company, INPUT ip-po-no, OUTPUT v-new-orders).

      IF v-order-list GT "" THEN DO:
          DO vi = 1 TO NUM-ENTRIES(v-new-orders, "|"):
             CREATE tt-ord-no .
             tt-ord-no.LINE = INTEGER(ENTRY(1, ENTRY(vi , v-new-orders, "|"))).
             tt-ord-no.ord-no = INTEGER(ENTRY(2, ENTRY(vi , v-new-orders, "|"))).
          END.
      END.
  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   po-ord.ship-no = lv-ship-no
   po-ord.cust-no = ls-drop-custno .
  DO WITH FRAME {&FRAME-NAME} :
     IF trim(v-postatus-cha) = "Hold" THEN
         IF po-ord.stat:SCREEN-VALUE NE "C" THEN
             po-ord.stat    = "H" .
  END.

  /* 10021210 */
  FIND FIRST shipto WHERE shipto.company EQ cocode
                      AND shipto.cust-no EQ ls-drop-custno
                      AND shipto.ship-id EQ po-ord.ship-id
                      NO-LOCK NO-ERROR.
     IF AVAILABLE shipto AND shipto.loc GT "" THEN
         po-ord.loc = shipto.loc.

  IF adm-new-record AND NOT adm-adding-record THEN DO: /* copy*/
     po-ord.opened = YES.
     ROWID(po-ord). /* force a buffer flush */

     FIND bx-poord WHERE RECID(bx-poord) = iv-copy-from-rec NO-LOCK .
     iv-poline-copied = NO.

     FOR EACH bx-notes WHERE bx-notes.rec_key EQ bx-poord.rec_key NO-LOCK:
       CREATE notes.
       BUFFER-COPY bx-notes TO notes
       ASSIGN
        notes.rec_key   = po-ord.rec_key
        notes.note_date = TODAY.
     END.

     IF po-ord.vend-no NE bx-poord.vend-no THEN DO:
       FIND FIRST vend NO-LOCK
           WHERE vend.company EQ po-ord.company
             AND vend.vend-no EQ po-ord.vend-no
           NO-ERROR.
       IF AVAILABLE vend THEN po-ord.tax-gr = vend.tax-gr.
     END.


     FOR EACH bx-poline NO-LOCK WHERE
         bx-poline.company EQ bx-poord.company AND
         bx-poline.po-no EQ bx-poord.po-no :

         CREATE po-ordl.
         BUFFER-COPY bx-poline EXCEPT po-no rec_key rel-qty t-rel-qty 
                                      t-inv-qty deleted t-rec-qty opened
                                      job-no job-no2
                               TO po-ordl.
         ASSIGN po-ordl.po-no    = po-ord.po-no
                po-ordl.vend-no  = po-ord.vend-no
                po-ordl.due-date = po-ord.due-date
                po-ordl.stat     = "O"
                /*po-ordl.over-pct = po-ord.over-pct
                po-ordl.under-pct = po-ord.under-pct*/.

         IF po-ord.vend-no NE bx-poord.vend-no THEN
           po-ordl.tax = po-ord.tax-gr NE "" AND aptax-chr EQ "Vendor".

         IF AVAILABLE vend THEN DO:
           po-ordl.disc = vend.disc-%.
           RUN po/po-sysct.p.
           IF v-default-gl-log AND v-default-gl-cha EQ "Vendor" THEN po-ordl.actnum = vend.actnum.
         END.

         FIND FIRST tt-ord-no WHERE tt-ord-no.LINE = po-ordl.LINE
                                AND tt-ord-no.ord-no NE po-ordl.ord-no
                              NO-ERROR.
         IF AVAILABLE tt-ord-no THEN
             po-ordl.ord-no = tt-ord-no.ord-no.
         IF po-ord.printed OR po-ord.stat <> "N" THEN po-ordl.stat = "A".

         ROWID(po-ordl). /* flush it */


         iv-poline-copied = YES.

         FOR EACH bx-notes NO-LOCK WHERE bx-notes.rec_key EQ bx-poline.rec_key :
           CREATE notes.
           BUFFER-COPY bx-notes TO notes
           ASSIGN
            notes.rec_key   = po-ordl.rec_key
            notes.note_date = TODAY.
         END.
     END.

     /* need to delete dummy po-ordl record created from local-create-record */
     IF iv-poline-copied THEN DO:
        FIND FIRST bx-poline WHERE
             bx-poline.company EQ po-ord.company AND
             bx-poline.po-no   EQ po-ord.po-no AND
             bx-poline.LINE = 0
             NO-ERROR.
        IF AVAILABLE bx-poline THEN DELETE bx-poline.
     END.
     FIND CURRENT po-ordl NO-LOCK NO-ERROR.
  END.

  IF adm-new-record OR lv-prev-vend-no <> po-ord.vend-no THEN DO:
     /* create po notes from vendor */
     IF po-ord.rec_key = "" THEN DO:
        DEFINE VARIABLE ls-key AS cha NO-UNDO.
        ASSIGN
        ls-key         = STRING(TODAY,"99999999") +
                  string(NEXT-VALUE(rec_key_seq,nosweat),"99999999")
        po-ord.rec_key = ls-key.               
        CREATE rec_key.
        ASSIGN rec_key.rec_key    = po-ord.rec_key
               rec_key.table_name = "PO".
     END.

     FOR EACH bx-notes WHERE bx-notes.rec_key = vend.rec_key
                         AND bx-notes.note_type = "G"
                         AND bx-notes.note_group = "PO" NO-LOCK:
         FIND FIRST notes EXCLUSIVE-LOCK WHERE notes.rec_key = po-ord.rec_key 
                            AND notes.note_title = bx-notes.note_title NO-ERROR.
         IF NOT AVAILABLE notes THEN CREATE notes.

         ASSIGN notes.rec_key   = po-ord.rec_key
                notes.note_date = TODAY .
         BUFFER-COPY bx-notes EXCEPT bx-notes.rec_key bx-notes.note_date TO notes.

     END.  /* for each bx-notes  vendor notes */
  END.

  IF NOT adm-new-record AND lv-prev-vend-no NE po-ord.vend-no THEN
     FOR EACH po-ordl EXCLUSIVE-LOCK WHERE
         po-ordl.company = po-ord.company AND
         po-ordl.po-no = po-ord.po-no:
         po-ordl.vend-no = po-ord.vend-no.
     END.

  IF NOT adm-new-record AND lv-due-date <> po-ord.due-date THEN DO:
     MESSAGE "Do you want to update the due date for all line items?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-ans AS LOG.
     IF ll-ans THEN 
         FOR EACH po-ordl EXCLUSIVE-LOCK WHERE po-ordl.company = po-ord.company
                            AND po-ordl.po-no = po-ord.po-no:
             po-ordl.due-date = po-ord.due-date.

         END.
  END.

  IF NOT adm-new-record AND lv-prev-vend-no NE po-ord.vend-no THEN DO:
     MESSAGE "Do you want to update the Item Cost for all line items?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll-anse AS LOG.

     IF ll-anse THEN 
         FOR EACH po-ordl EXCLUSIVE-LOCK WHERE po-ordl.company = po-ord.company
                            AND po-ordl.po-no = po-ord.po-no:
          {po/vend-cost.i YES }
     END.
  END.  /* change vender */

  FIND CURRENT po-ord NO-LOCK NO-ERROR.
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
  DISABLE po-ord.ship-id WITH FRAME {&FRAME-NAME}.

  /* To allow ctrl-o to be picked up */
  APPLY 'entry' TO btnCalendar-1 IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER b-po-ordl FOR po-ordl.
  DEFINE VARIABLE iNextPo LIKE po-ctrl.next-po-no NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  RUN sys/ref/asiseq.p (cocode,'po_seq',OUTPUT iNextPO) NO-ERROR.

  ASSIGN po-ord.company        = cocode
         po-ord.po-no          = inextPO         
         po-ord.po-date        = TODAY
         po-ord.loc            = locode
         po-ord.buyer          = USERID("NOSWEAT")  /*global-uid*/
         po-ord.under-pct      = 10
         po-ord.over-pct       = 10         
         po-ord.due-date       = po-ord.po-date + 
                                 IF WEEKDAY(po-ord.po-date) EQ 6 THEN 3 ELSE 1
         po-ord.last-ship-date = po-ord.due-date
         fil_id                = RECID (po-ord)
         po-ord.user-id        = USERID('nosweat')
         .
  
  DISPLAY po-ord.po-no WITH FRAME {&FRAME-NAME}.

  IF trim(v-postatus-cha) = "Hold" THEN DO:
     po-ord.stat = "H" .
  END.
  DISPLAY po-ord.stat fc_app_time RECT-13 approved_text po-ord.approved-date
          po-ord.approved-id WITH FRAME {&frame-name}.
          fc_app_time:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .
          po-ord.approved-date:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .
          po-ord.approved-id:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE  .
          RECT-13:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .
          approved_text:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .

  IF NOT copy-record THEN ls-drop-custno = "".

  CREATE b-po-ordl.
  ASSIGN
   b-po-ordl.company  = po-ord.company
   b-po-ordl.po-no    = po-ord.po-no
   b-po-ordl.line     = 0
   b-po-ordl.due-date = po-ord.due-date.

  ASSIGN lv_vend-name      = ""
         lv_vend-add1      = ""
         lv_vend-add2      = ""
         lv_vend-city      = ""
         lv_vend-state     = ""
         lv_vend-zip       = ""
         lv_vend-area-code = ""
         lv_vend-phone     = "".
  DISPLAY lv_vend-name lv_vend-add1 lv_vend-add2 lv_vend-city
          lv_vend-state lv_vend-zip lv_vend-area-code lv_vend-phone
    WITH FRAME {&FRAME-NAME}.

  FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
  IF AVAILABLE company THEN ASSIGN po-ord.ship-id      = company.company
                                   po-ord.ship-name    = company.NAME
                                   po-ord.ship-addr[1] = company.addr[1]
                                   po-ord.ship-addr[2] = company.addr[2]
                                   po-ord.ship-city    = company.city
                                   po-ord.ship-state   = company.state
                                   po-ord.ship-zip     = company.zip.

 RELEASE po-ctrl.
 FIND CURRENT po-ord NO-LOCK NO-ERROR.
 FIND CURRENT po-ordl NO-LOCK NO-ERROR.
 FIND CURRENT b-po-ordl NO-LOCK NO-ERROR.
 

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
  RUN setTypeDescr.
  IF AVAILABLE po-ord THEN RUN display-vend.
  v-copied-from = INTEGER(po-ord.po-no:SCREEN-VALUE IN FRAME {&FRAME-NAME}).
  fc_app_time:SCREEN-VALUE = IF AVAILABLE po-ord THEN string(po-ord.approved-time,"HH:MM") ELSE "".

  fc_app_time:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .
  po-ord.approved-date:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .
  po-ord.approved-id:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE  .
  RECT-13:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .
  approved_text:HIDDEN = IF AVAILABLE po-ord and po-ord.stat EQ "H" THEN TRUE ELSE FALSE .


/* IF po-ord.stat <> "H" THEN ENABLE po-ord.approved-date fc_app_time.
 IF po-ord.stat <> "H" THEN ENABLE po-ord.approved-id.*/

 RUN po/po-sysct.p.  /* for vars factor#.... need for vend-cost  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ll-is-new-rec AS LOG   NO-UNDO.
  DEFINE VARIABLE lv-rowid      AS ROWID NO-UNDO.

  DEFINE BUFFER b-notes  FOR notes.
  DEFINE BUFFER b-po-ord FOR po-ord.

  /* Code placed here will execute PRIOR to standard behavior. */
  /* == validations ==== */
  {&methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME} :
    RUN valid-vend-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
    IF /*adm-new-record and ??*/
       NOT ll-got-vendor AND po-ord.vend-no <> po-ord.vend-no:SCREEN-VALUE
    THEN RUN new-vend-no.

    RUN valid-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

    IF po-ord.carrier:MODIFIED THEN DO:
       FIND FIRST carrier WHERE carrier.company = g_company AND
                                carrier.carrier = po-ord.carrier:SCREEN-VALUE
                   NO-LOCK NO-ERROR.
       IF NOT AVAILABLE carrier THEN DO:
          MESSAGE "Invalid Carrier. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO po-ord.carrier.
          RETURN NO-APPLY.
       END.
    END.   

    IF po-ord.terms:MODIFIED THEN DO:
       FIND FIRST terms WHERE terms.company = g_company AND
                              terms.t-code = po-ord.terms:SCREEN-VALUE
               NO-LOCK NO-ERROR.
       IF NOT AVAILABLE terms THEN DO:
          MESSAGE "Invalid Terms. Try Help. " VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO po-ord.terms.
          RETURN NO-APPLY.
       END.
    END.
    {&methods/lValidateError.i NO}
     RUN valid-tax-gr (po-ord.tax-gr:HANDLE) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END. /* frame */

  ASSIGN ll-is-new-rec = adm-new-record.

  IF ll-is-new-rec THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
    RUN record-added IN WIDGET-HANDLE(char-hdl).
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  lv-rowid = ROWID(po-ord).

  FIND CURRENT po-ord NO-LOCK NO-ERROR.

  DISABLE po-ord.ship-id po-ord.ship-name po-ord.ship-addr[1] po-ord.ship-addr[2]
          po-ord.ship-city po-ord.ship-state po-ord.ship-zip
         WITH FRAME {&FRAME-NAME}.

  RUN po/po-total.p (RECID(po-ord)).

  IF copy-record THEN
  DO:
     /*when copying p.o., delete vendor notes created from write
       trigger and copy notes from original p.o. instead */
     FOR EACH b-notes
         WHERE b-notes.rec_key EQ po-ord.rec_key
         EXCLUSIVE-LOCK:

         DELETE b-notes.
     END.

     FIND FIRST b-po-ord NO-LOCK WHERE
          b-po-ord.company EQ po-ord.company AND
          b-po-ord.po-no   EQ lv-copy-from-po-num
          NO-ERROR.

     IF AVAILABLE b-po-ord THEN
     DO:
        FOR EACH b-notes WHERE
            b-notes.rec_key EQ b-po-ord.rec_key
            NO-LOCK:

            CREATE notes.
            BUFFER-COPY b-notes EXCEPT rec_key note_date note_time user_id
                        viewed note_type note_code note_group TO notes
            ASSIGN
               notes.rec_key   = po-ord.rec_key
               notes.note_date = TODAY
               notes.note_time = TIME
               notes.user_id   = USERID("NOSWEAT").
        END.

        RELEASE b-po-ord.
     END.

     /* update on order ticket 05050811 gbw */
     FOR EACH po-ordl WHERE
         po-ordl.company EQ po-ord.company AND
         po-ordl.po-no EQ po-ord.po-no NO-LOCK:
         RUN po/poordlup.p (RECID(po-ordl), 1, YES).
     END.

     ASSIGN
       copy-record         = NO
       lv-copy-from-po-num = 0.  /*clear variables for next operations*/
  END.

  /* ===  don't go item page yet. -> move page to 2 */
  IF ll-is-new-rec THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
    RUN record-added IN WIDGET-HANDLE(char-hdl).
    RUN reopen-query1 IN WIDGET-HANDLE(char-hdl) (lv-rowid).
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"Container-source",OUTPUT char-hdl).
    RUN select-page IN WIDGET-HANDLE(char-hdl) (3).
  END.

  ELSE RUN dispatch ("display-fields").

  ll-got-vendor = NO.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-fob-code V-table-Win 
PROCEDURE new-fob-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ld AS DECIMAL NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF po-ord.fob-code:SCREEN-VALUE EQ "Orig" AND 
       po-ord.frt-pay:SCREEN-VALUE NE "P" THEN DO:
      /*lv-prev-val = "".*/
      RUN new-t-freight.
    END.
    ELSE 
      ASSIGN po-ord.t-freight:SCREEN-VALUE = STRING(0)
             ld                            = DEC(po-ord.t-cost:SCREEN-VALUE) - dec(lv-prev-val)
             po-ord.t-cost:SCREEN-VALUE    = STRING(ld)
             lv-prev-val                   = "".
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-t-freight V-table-Win 
PROCEDURE new-t-freight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ld AS DECIMAL EXTENT 3 NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF po-ord.fob-code:SCREEN-VALUE EQ "Orig" AND
       po-ord.frt-pay:SCREEN-VALUE NE "P"     THEN DO:
      ASSIGN
       ld[1]                      = DEC(lv-prev-val)
       lv-prev-val                = po-ord.t-freight:SCREEN-VALUE
       ld[2]                      = DEC(lv-prev-val)
       ld[3]                      = DEC(po-ord.t-cost:SCREEN-VALUE)
       ld[3]                      = ld[3] - ld[1] + ld[2]
       po-ord.t-cost:SCREEN-VALUE = STRING(ld[3]).

      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-type V-table-Win 
PROCEDURE new-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    RUN setTypeDescr.

    IF lv-type EQ "D" AND po-ord.type:SCREEN-VALUE NE "D" THEN DO:
      FIND FIRST company NO-LOCK WHERE company.company EQ cocode NO-ERROR.
      IF AVAILABLE company THEN
        ASSIGN
         po-ord.ship-id:SCREEN-VALUE      = company.company
         po-ord.ship-name:SCREEN-VALUE    = company.name
         po-ord.ship-addr[1]:SCREEN-VALUE = company.addr[1]
         po-ord.ship-addr[2]:SCREEN-VALUE = company.addr[2]
         po-ord.ship-city:SCREEN-VALUE    = company.city
         po-ord.ship-state:SCREEN-VALUE   = company.state
         po-ord.ship-zip:SCREEN-VALUE     = company.zip.
      DISABLE po-ord.ship-id.
    END.

    ELSE
    IF lv-type NE "D" AND po-ord.type:SCREEN-VALUE EQ "D" AND
       po-ord.vend-no:SCREEN-VALUE NE ""                  THEN DO:
      RUN is-dropship.
      IF ls-drop-custno EQ "" THEN DO:
        lv-type = "D".
        ENABLE po-ord.ship-id.
        APPLY "entry" TO po-ord.ship-id.
        RETURN ERROR.
      END.
    END.

    lv-type = po-ord.type:SCREEN-VALUE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-vend-no V-table-Win 
PROCEDURE new-vend-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&frame-name}:
    FIND vend
        WHERE vend.company EQ po-ord.company
          AND vend.vend-no BEGINS po-ord.vend-no:SCREEN-VALUE
          AND vend.active  EQ "A"
        NO-LOCK NO-ERROR.
    FIND CURRENT po-ord EXCLUSIVE-LOCK.
    IF AVAILABLE vend THEN DO:
      ASSIGN
       po-ord.vend-no:SCREEN-VALUE   = vend.vend-no
       po-ord.carrier:SCREEN-VALUE   = vend.carrier
       po-ord.contact:SCREEN-VALUE   = vend.contact
       po-ord.terms:SCREEN-VALUE     = vend.terms
       po-ord.fob-code:SCREEN-VALUE  = IF vend.fob-code NE "" THEN vend.fob-code ELSE "DEST"
       po-ord.frt-pay:SCREEN-VALUE   = IF vend.frt-pay NE "" THEN vend.frt-pay ELSE "B"
       po-ord.over-pct:SCREEN-VALUE  = STRING(vend.over-pct)   
       po-ord.under-pct:SCREEN-VALUE = STRING(vend.under-pct)
       po-ord.tax-gr:SCREEN-VALUE    = vend.tax-gr.

      RUN display-vend.
    END.
  END.
  FIND CURRENT po-ord NO-LOCK NO-ERROR.
  ll-got-vendor = YES.
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

  DO WITH FRAME {&FRAME-NAME}: 
    IF AVAILABLE po-ord THEN 
        ASSIGN
         ls-drop-custno = po-ord.cust-no
         lv-type        = po-ord.type.

    IF adm-new-record AND NOT adm-adding-record THEN
      po-ord.po-date:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(TODAY).

    IF AVAIL(po-ord) AND po-ord.type NE "D" AND NOT adm-new-record THEN DO:
      DISABLE po-ord.ship-id.
      ls-drop-custno = "".
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reopen-query1 V-table-Win 
PROCEDURE reopen-query1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-rowid AS ROWID.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
   RUN record-added IN WIDGET-HANDLE(char-hdl).
   RUN reopen-query1 IN WIDGET-HANDLE(char-hdl)  (ip-rowid).

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
  {src/adm/template/sndkycas.i "company" "po-ord" "company"}
  {src/adm/template/sndkycas.i "Carrier" "po-ord" "Carrier"}

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
  {src/adm/template/snd-list.i "po-ord"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-copy-from V-table-Win 
PROCEDURE set-copy-from :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipi-po-num AS INTEGER NO-UNDO.

    v-copied-from = ipi-po-num.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-order-list V-table-Win 
PROCEDURE set-order-list :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-order-list AS CHARACTER NO-UNDO.
v-order-list = ip-order-list. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setTypeDescr V-table-Win 
PROCEDURE setTypeDescr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  typeDescr:SCREEN-VALUE IN FRAME {&FRAME-NAME} =
            ENTRY(LOOKUP(po-ord.type:SCREEN-VALUE,',D,R,S'),
                  ',Drop Ship,Regular,Sheets from Roll').
  IF po-ord.type:SCREEN-VALUE EQ 'D' THEN
  DO:
    FIND cust NO-LOCK WHERE cust.company EQ cocode
                        AND cust.cust-no EQ po-ord.cust-no NO-ERROR.
    ASSIGN
      shipAreaCode:SCREEN-VALUE = IF AVAILABLE cust THEN cust.area-code ELSE ''
      shipPhone:SCREEN-VALUE    = IF AVAILABLE cust THEN cust.phone ELSE ''.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tax-gr V-table-Win 
PROCEDURE valid-tax-gr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-focus AS HANDLE NO-UNDO.

  {methods/lValidateError.i YES}
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    IF ip-focus:SCREEN-VALUE NE "" AND
       NOT CAN-FIND(FIRST stax
                    WHERE stax.company   EQ cocode
                      AND stax.tax-group EQ ip-focus:SCREEN-VALUE)
    THEN DO:
      MESSAGE TRIM(ip-focus:LABEL) + " is invalid, try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO ip-focus.
      RETURN ERROR.
    END.
  END.
  {methods/lValidateError.i NO}

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-type V-table-Win 
PROCEDURE valid-type :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    po-ord.type:SCREEN-VALUE = CAPS(po-ord.type:SCREEN-VALUE).

    IF INDEX("RDS",po-ord.type:SCREEN-VALUE) LE 0 THEN DO:
      MESSAGE TRIM(po-ord.type:LABEL) +
              " is invalid, enter 'R'egular, 'D'rop ship, or 'S'heets from Roll"
          VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.

    RUN new-type NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR.
  END.
  {methods/lValidateError.i NO}

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-vend-no V-table-Win 
PROCEDURE valid-vend-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST vend
        WHERE vend.company EQ po-ord.company
          AND vend.vend-no EQ po-ord.vend-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE vend                                                      OR
       (vend.active NE "A" AND
        (po-ord.vend-no NE po-ord.vend-no:SCREEN-VALUE OR adm-new-record)) THEN DO:
      IF AVAILABLE vend THEN
        MESSAGE TRIM(po-ord.vend-no:LABEL) + " not active, try help..."
            VIEW-AS ALERT-BOX ERROR.
      ELSE 
        MESSAGE "Invalid " + TRIM(po-ord.vend-no:LABEL) + ", try help..."
            VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
  END.
  {methods/lValidateError.i NO}

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




