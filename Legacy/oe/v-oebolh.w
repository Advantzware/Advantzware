&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File:

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
{sys/inc/VAR.i "new shared"}
DEF VAR v-n-bol LIKE oe-bolh.bol-no NO-UNDO.
DEF VAR lv-ship-no LIKE oe-bolh.ship-no NO-UNDO.
ASSIGN cocode = g_company
       locode = g_loc.

DEF VAR lv-newbol AS ROWID NO-UNDO.
DEF VAR lv-cust-x LIKE oe-bolh.cust-no NO-UNDO.
DEF VAR lv-type-code AS CHAR NO-UNDO.
DEF VAR lv-type-dscr AS CHAR NO-UNDO.
DEF VAR lr-rel-lib AS HANDLE NO-UNDO.
DEF VAR llNewBol AS LOG NO-UNDO.
DEFINE VARIABLE lFreightEntered AS LOGICAL     NO-UNDO.
DEF BUFFER bf-bolh FOR oe-bolh.
DEF BUFFER bf-boll FOR oe-boll.
DEF BUFFER alt-rell FOR oe-rell.

DEF TEMP-TABLE tt-ord NO-UNDO FIELD ord-no LIKE oe-boll.ord-no.

{methods\defines\hndldefs.i }
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ g_company
                        AND sys-ctrl.name    EQ "BOLFMT"
                        NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN sys-ctrl.company  = g_company
         sys-ctrl.name     = "BOLFMT"
         sys-ctrl.descrip  = "Bill of Lading Format"
         sys-ctrl.char-fld = "ASI".
  MESSAGE "System control record not found. Update BOL Print Format"
     UPDATE sys-ctrl.char-fld.
END.

RUN oe/s-codes.p (OUTPUT lv-type-code, OUTPUT lv-type-dscr).

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
&Scoped-define EXTERNAL-TABLES oe-bolh
&Scoped-define FIRST-EXTERNAL-TABLE oe-bolh


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR oe-bolh.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS oe-bolh.bol-no oe-bolh.bol-date ~
oe-bolh.carrier oe-bolh.ship-id oe-bolh.trailer oe-bolh.frt-pay ~
oe-bolh.airway-bill oe-bolh.freight oe-bolh.cwt oe-bolh.tot-wt ~
oe-bolh.tot-pallets 
&Scoped-define ENABLED-TABLES oe-bolh
&Scoped-define FIRST-ENABLED-TABLE oe-bolh
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS oe-bolh.bol-no oe-bolh.bol-date ~
oe-bolh.stat oe-bolh.release# oe-bolh.cust-no oe-bolh.carrier ~
oe-bolh.ship-id oe-bolh.trailer oe-bolh.frt-pay oe-bolh.airway-bill ~
oe-bolh.freight oe-bolh.cwt oe-bolh.tot-wt oe-bolh.tot-pallets ~
oe-bolh.user-id oe-bolh.upd-date 
&Scoped-define DISPLAYED-TABLES oe-bolh
&Scoped-define FIRST-DISPLAYED-TABLE oe-bolh
&Scoped-Define DISPLAYED-OBJECTS tgSigned cust_name ship_name cust_addr1 ~
ship_addr1 cust_addr2 ship_addr2 cust_city cust_state cust_zip ship_city ~
ship_state ship_zip fi_upd-time 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */
&Scoped-define ADM-CREATE-FIELDS oe-bolh.release# oe-bolh.cust-no 
&Scoped-define ADM-ASSIGN-FIELDS oe-bolh.stat 
&Scoped-define List-3 btnCalendar-1 

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
     SIZE 42 BY 1.

DEFINE VARIABLE cust_addr2 AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cust_city AS CHARACTER FORMAT "x(15)" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE cust_name AS CHARACTER FORMAT "x(30)" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE cust_state AS CHARACTER FORMAT "x(2)" 
     VIEW-AS FILL-IN 
     SIZE 4.4 BY 1.

DEFINE VARIABLE cust_zip AS CHARACTER FORMAT "x(10)" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE fi_upd-time AS CHARACTER FORMAT "x(8)" 
     LABEL "At" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

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

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143.6 BY 8.33.

DEFINE VARIABLE tgSigned AS LOGICAL INITIAL no 
     LABEL "Signed" 
     VIEW-AS TOGGLE-BOX
     SIZE 13.4 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     tgSigned AT ROW 2.38 COL 10 WIDGET-ID 4
     oe-bolh.bol-no AT ROW 1.24 COL 8 COLON-ALIGNED
          LABEL "BOL#" FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.bol-date AT ROW 1.24 COL 37 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.stat AT ROW 2.33 COL 121 COLON-ALIGNED HELP
          "Order Status (R)eleased or (H)old"
          LABEL "BOL Status" FORMAT "x(20)"
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "(H)old","(R)eleased" 
          DROP-DOWN-LIST
          SIZE 19 BY 1 TOOLTIP "Hold,Released"
     oe-bolh.release# AT ROW 3.38 COL 14 COLON-ALIGNED FORMAT ">>>>>>>9"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     oe-bolh.cust-no AT ROW 4.33 COL 14 COLON-ALIGNED HELP
          "Enter customer number."
          LABEL "Customer#" FORMAT "x(8)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.carrier AT ROW 3.38 COL 63 COLON-ALIGNED FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.ship-id AT ROW 4.33 COL 63 COLON-ALIGNED
          LABEL "Ship To#"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.trailer AT ROW 3.38 COL 106 COLON-ALIGNED
          LABEL "Trailer#"
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     oe-bolh.frt-pay AT ROW 4.33 COL 105.4 COLON-ALIGNED HELP
          "B=Bill, C=Collect, P=Prepaid, T=Third Party"
          LABEL "Freight Terms"
          VIEW-AS FILL-IN 
          SIZE 3.2 BY 1
     oe-bolh.airway-bill AT ROW 4.33 COL 121 COLON-ALIGNED HELP
          "" WIDGET-ID 2
          LABEL "Seal#" FORMAT "X(12)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     oe-bolh.freight AT ROW 5.29 COL 121 COLON-ALIGNED 
          LABEL "Freight Cost" FORMAT "->,>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     cust_name AT ROW 5.29 COL 14 COLON-ALIGNED NO-LABEL
     ship_name AT ROW 5.29 COL 63 COLON-ALIGNED NO-LABEL
     cust_addr1 AT ROW 6.24 COL 14 COLON-ALIGNED NO-LABEL
     oe-bolh.cwt AT ROW 6.24 COL 121 COLON-ALIGNED
          LABEL "Rate/100 Wt"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ship_addr1 AT ROW 6.29 COL 63 COLON-ALIGNED NO-LABEL
     cust_addr2 AT ROW 7.19 COL 14 COLON-ALIGNED NO-LABEL
     oe-bolh.tot-wt AT ROW 7.19 COL 121 COLON-ALIGNED
          LABEL "Total Weight" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     ship_addr2 AT ROW 7.29 COL 63 COLON-ALIGNED NO-LABEL
     cust_city AT ROW 8.14 COL 14 COLON-ALIGNED NO-LABEL
     cust_state AT ROW 8.14 COL 34 COLON-ALIGNED NO-LABEL
     cust_zip AT ROW 8.14 COL 38 COLON-ALIGNED NO-LABEL
     ship_city AT ROW 8.14 COL 63 COLON-ALIGNED NO-LABEL
     ship_state AT ROW 8.14 COL 83 COLON-ALIGNED NO-LABEL
     ship_zip AT ROW 8.14 COL 87 COLON-ALIGNED NO-LABEL
     oe-bolh.tot-pallets AT ROW 8.14 COL 121 COLON-ALIGNED
          LABEL "Total Pallets" FORMAT "->,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     oe-bolh.user-id AT ROW 1.24 COL 81 COLON-ALIGNED
          LABEL "Added/Updated By"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     oe-bolh.upd-date AT ROW 1.24 COL 103 COLON-ALIGNED
          LABEL "On"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     fi_upd-time AT ROW 1.24 COL 125 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     btnCalendar-1 AT ROW 1.24 COL 54
     RECT-2 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.oe-bolh
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
         HEIGHT             = 15
         WIDTH              = 144.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB V-table-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/viewer.i}

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

/* SETTINGS FOR FILL-IN oe-bolh.airway-bill IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN oe-bolh.bol-no IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN oe-bolh.carrier IN FRAME F-Main
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN oe-bolh.cust-no IN FRAME F-Main
   NO-ENABLE 1 EXP-LABEL EXP-FORMAT EXP-HELP                            */
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
/* SETTINGS FOR FILL-IN oe-bolh.cwt IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN fi_upd-time IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.freight IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-bolh.frt-pay IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.release# IN FRAME F-Main
   NO-ENABLE 1 EXP-FORMAT                                               */
/* SETTINGS FOR FILL-IN oe-bolh.ship-id IN FRAME F-Main
   EXP-LABEL                                                            */
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
/* SETTINGS FOR COMBO-BOX oe-bolh.stat IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL EXP-FORMAT EXP-HELP                            */
/* SETTINGS FOR TOGGLE-BOX tgSigned IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.tot-pallets IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-bolh.tot-wt IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN oe-bolh.trailer IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN oe-bolh.upd-date IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN oe-bolh.user-id IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
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
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR look-recid AS RECID NO-UNDO.


    CASE FOCUS:NAME :
         WHEN "bol-no" THEN DO:
              RUN windows/l-newbol.w (g_company, oe-bolh.cust-no:screen-value IN FRAME {&frame-name}, oe-bolh.ship-id:screen-value IN FRAME {&frame-name}, FOCUS:SCREEN-VALUE IN FRAME {&frame-name}, OUTPUT char-val).
              IF char-val <> "" THEN DO:
                FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
              END.
         END.
         WHEN "frt-pay" THEN DO:
              RUN windows/l-frtcod.w (output char-val).
              IF char-val <> "" THEN DO:
                FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = entry(1,char-val).
              END.
         END.
         WHEN "cust-no" THEN DO:
              RUN windows/l-custact.w (g_company, FOCUS:SCREEN-VALUE IN FRAME {&frame-name}, OUTPUT char-val, OUTPUT look-recid).
              FIND cust WHERE RECID(cust) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL cust AND cust.cust-no NE FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN DO:
                 FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cust.cust-no.
                 RUN new-cust-no.
              END.       
         END.  
         WHEN "ship-id" THEN DO:
              RUN windows/l-shipt2.w (g_company,g_loc, oe-bolh.cust-no:screen-value IN FRAME {&frame-name}, FOCUS:SCREEN-VALUE, OUTPUT char-val, OUTPUT look-recid).
              FIND shipto WHERE RECID(shipto) EQ look-recid NO-LOCK NO-ERROR.
              IF AVAIL shipto AND shipto.ship-id NE FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN DO:
                 FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} = shipto.ship-id.
                 RUN new-ship-id.
              END.      
          END.  
          WHEN "carrier" THEN DO:
              FIND FIRST oe-boll NO-LOCK 
                WHERE oe-boll.company EQ oe-bolh.company
                  AND oe-boll.b-no    EQ oe-bolh.b-no NO-ERROR.
              RUN windows/l-carrie.w (g_company,oe-boll.loc, FOCUS:SCREEN-VALUE, OUTPUT char-val).
              IF char-val NE "" AND entry(1,char-val) NE FOCUS:SCREEN-VALUE IN FRAME {&FRAME-NAME} THEN DO:
                 FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
                 RUN new-carrier.
              END.       
          END.  
          WHEN "release#" THEN DO:
              RUN windows/l-releas.w (g_company,FOCUS:SCREEN-VALUE,OUTPUT char-val,OUTPUT look-recid).
              IF char-val <> "" THEN DO:
                 FOCUS:SCREEN-VALUE = ENTRY(1,char-val). 
              END.
              
          END.
          /* gdm - */
          WHEN "trailer" THEN DO:              
              RUN browsers/l-truck2.w (g_company,
                                      g_loc,
                                      oe-bolh.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                                      oe-bolh.trailer:SCREEN-VALUE IN FRAME {&FRAME-NAME}, 
                                      OUTPUT char-val).
              IF char-val <> "" THEN 
                ASSIGN oe-bolh.trailer:SCREEN-VALUE = ENTRY(1,char-val).
             
          END.
         /* gdm - */

    END CASE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-bolh.bol-date
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.bol-date V-table-Win
ON HELP OF oe-bolh.bol-date IN FRAME F-Main /* BOL Date */
DO:
  {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.bol-date V-table-Win
ON LEAVE OF oe-bolh.bol-date IN FRAME F-Main /* BOL Date */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-bol-date NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-bolh.bol-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.bol-no V-table-Win
ON LEAVE OF oe-bolh.bol-no IN FRAME F-Main /* BOL# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-bol-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 V-table-Win
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i oe-bolh.bol-date}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-bolh.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.carrier V-table-Win
ON LEAVE OF oe-bolh.carrier IN FRAME F-Main /* Carrier */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-carrier NO-ERROR.
    IF ERROR-STATU:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.carrier V-table-Win
ON VALUE-CHANGED OF oe-bolh.carrier IN FRAME F-Main /* Carrier */
DO:
  RUN new-carrier.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-bolh.cust-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.cust-no V-table-Win
ON LEAVE OF oe-bolh.cust-no IN FRAME F-Main /* Customer# */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-cust-no NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.cust-no V-table-Win
ON VALUE-CHANGED OF oe-bolh.cust-no IN FRAME F-Main /* Customer# */
DO:
  RUN new-cust-no.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-bolh.freight
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.freight V-table-Win
ON LEAVE OF oe-bolh.freight IN FRAME F-Main /* Freight Cost */
DO:
  IF LASTKEY NE -1 THEN DO:
      IF oe-bolh.frt-pay:SCREEN-VALUE EQ "B" AND oe-bolh.freight:SCREEN-VALUE EQ "0.00" AND
            CAN-FIND(FIRST sys-ctrl WHERE sys-ctrl.company EQ oe-bolh.company
                             AND sys-ctrl.name    EQ "BOLFreight"
                             AND sys-ctrl.log-fld EQ YES) THEN DO:
           MESSAGE "Freight cost cannot be zero.  Please update." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO oe-bolh.freight .
           RETURN NO-APPLY.
       END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.freight V-table-Win
ON VALUE-CHANGED OF oe-bolh.freight IN FRAME F-Main /* Freight Cost */
DO:
    
  lFreightEntered = YES.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-bolh.release#
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.release# V-table-Win
ON LEAVE OF oe-bolh.release# IN FRAME F-Main /* Release# */
DO:
    IF LASTKEY = -1 THEN RETURN.
    RUN valid-release NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME oe-bolh.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.ship-id V-table-Win
ON LEAVE OF oe-bolh.ship-id IN FRAME F-Main /* Ship To# */
DO:
   IF LASTKEY NE -1 THEN DO:
     RUN valid-ship-id NO-ERROR.
     IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL oe-bolh.ship-id V-table-Win
ON VALUE-CHANGED OF oe-bolh.ship-id IN FRAME F-Main /* Ship To# */
DO:
  RUN new-ship-id.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgSigned
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgSigned V-table-Win
ON ENTRY OF tgSigned IN FRAME F-Main /* Signed */
DO:
    DEF VAR cSigned AS CHAR NO-UNDO.
    cSigned = SELF:SCREEN-VALUE.

   IF NOT asi.oe-bolh.bol-date:SENSITIVE THEN DO:
       /* reverse what the user clicked since not in update mode */
       SELF:SCREEN-VALUE = IF cSigned EQ "YES" THEN "NO" ELSE "YES".
       RETURN NO-APPLY.
   END. /* not in update mode */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgSigned V-table-Win
ON VALUE-CHANGED OF tgSigned IN FRAME F-Main /* Signed */
DO:
  DEF VAR lContinue AS LOGICAL NO-UNDO.
  DEF VAR cFile AS CHAR NO-UNDO.
  DEF VAR ls-image1 AS CHAR NO-UNDO.
  DEF VAR ls-image2 AS CHAR NO-UNDO.
  DEF VAR ls-pdf-image AS CHAR NO-UNDO.

  IF NOT asi.oe-bolh.bol-date:SENSITIVE THEN 
     RETURN NO-APPLY.

  /* If was unchecked and now checked, do this: */
  lContinue = YES. cFile = "".
  /* Check if signature file exists */
    RUN oe/GetBOLSign.p(INPUT cocode,
                        INPUT ROWID(oe-bolh),
                        INPUT NO, /* Execute ftp? */
                        INPUT YES, /* build ftp command? */
                        OUTPUT cFile).

  /* If signature file does not exist, check web service for it */
    IF cFile = "" THEN
      RUN oe/GetBOLSign.p(INPUT cocode,
                          INPUT ROWID(oe-bolh),
                          INPUT YES, /* Execute ftp? */
                          INPUT YES, /* build ftp command? */
                          OUTPUT cFile).

  IF tgSigned:SCREEN-VALUE = "Yes" THEN DO:
                 
      /* If Signature file still not found, warn user */
      IF cFile EQ "" THEN DO:  
          MESSAGE "Signature image does not exist.  Do you want to mark BOL as signed anyway?" 
              UPDATE lContinue
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO.
          
          IF NOT lContinue THEN DO:
              tgSigned:SCREEN-VALUE = "NO".
              RETURN NO-APPLY.                            
          END.

      END. /* Signature not found */

  END. /* change to 'yes' found */
  ELSE DO:

      /* If Signature file still not found, warn user */
      IF cFile GT "" THEN DO:  

          lContinue = NO.
          MESSAGE "A signature has been captured for this BOL.  Do you want to delete it?" 
              UPDATE lContinue
              VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO.

          IF lContinue THEN DO:


            IF SEARCH(cFile) NE ? THEN
                OS-DELETE VALUE(cFile) NO-ERROR.

            FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
               AND sys-ctrl.name    EQ "BOLSIGN" NO-LOCK NO-ERROR.

            ASSIGN
            ls-image1 = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE ""
            ls-image2 = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".
                    
            ls-image1 = ls-image2.
            IF ls-image1 <> "" AND 
              SUBSTRING(ls-image1,LENGTH(ls-image1),1) <> "\" AND
              SUBSTRING(ls-image1,LENGTH(ls-image1),1) <> "/"
            THEN ls-image1 = ls-image1 + "\".
          
            ls-pdf-image = ls-image1 + TRIM(asi.oe-bolh.bol-no:SCREEN-VALUE) + ".pdf".

            IF SEARCH(ls-pdf-image) <> ? THEN
              OS-DELETE VALUE(ls-pdf-image) NO-ERROR.

          END. /* If continue to delete files */
              
      END. /* signature found */

  END. /* change to 'no' */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK V-table-Win 


/* ***************************  Main Block  *************************** */
{sys/inc/f3help.i}  /* asi field contents help */
SESSION:DATA-ENTRY-RETURN = YES.

lv-cust-x = "".
FOR EACH cust NO-LOCK
    WHERE cust.company EQ cocode
      AND cust.active  EQ "X":
  lv-cust-x = cust.cust-no.
  LEAVE.
END.

RUN sbo/oerel-recalc-act.p PERSISTENT SET lr-rel-lib.

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
  {src/adm/template/row-list.i "oe-bolh"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "oe-bolh"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-freight V-table-Win 
PROCEDURE calc-freight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ld AS DEC NO-UNDO.
DEF VAR dFreight AS DEC DECIMALS 6 NO-UNDO.
DEF VAR dTotFreight AS DEC DECIMALS 6 NO-UNDO.
DEF VAR hItemBrowse AS HANDLE NO-UNDO.
DEF VAR hContainer AS HANDLE NO-UNDO.
DEF VAR char-hdl AS CHAR NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO.
DEF VAR v-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS DEC NO-UNDO.
DEF VAR v-frt-chg AS DEC NO-UNDO.
DEF VAR v-del-zone LIKE oe-ordl.del-zone NO-UNDO.
DEF VAR v-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR dTotBasis AS DECIMAL NO-UNDO DECIMALS 10.
DEF VAR tot-other-freight AS DEC NO-UNDO DECIMALS 10.
DEF VAR ldMinRate AS DEC NO-UNDO.


  
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL oe-bolh                                     AND
       oe-bolh.cust-no:SCREEN-VALUE NE ""                AND
       oe-bolh.ship-id:SCREEN-VALUE NE ""                AND
       oe-bolh.carrier:SCREEN-VALUE NE ""                AND
       CAN-FIND(FIRST oe-boll
                WHERE oe-boll.company EQ oe-bolh.company
                  AND oe-boll.b-no    EQ oe-bolh.b-no)   THEN DO:

      IF oe-bolh.freight:SENSITIVE THEN DO:

/*         RUN oe/getBolFrt.p (ROWID(oe-bolh),      */
/*                    oe-bolh.cust-no:SCREEN-VALUE, */
/*                    oe-bolh.ship-id:SCREEN-VALUE, */
/*                    oe-bolh.carrier:SCREEN-VALUE, */
/*                    OUTPUT ld ).                  */
        RUN oe/calcBolFrt.p (ROWID(oe-bolh), OUTPUT ld).
        oe-bolh.freight:SCREEN-VALUE = STRING(ld).
      END.
      ELSE DO: 
        FIND CURRENT oe-bolh.
        oe-bolh.freight = 0.
        dTotFreight = 0.        

 
        oe-bolh.tot-pallets = 0.
        FOR EACH oe-boll
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no:

          oe-bolh.tot-pallets = oe-bolh.tot-pallets + oe-boll.tot-pallets.
        END. /* each oe-boll */        
        RUN oe/calcBolFrt.p (ROWID(oe-bolh), OUTPUT dTotFreight).
        oe-bolh.freight = dTotFreight.
        
        FIND CURRENT oe-bolh NO-LOCK.
        RUN dispatch ("row-available").
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'container-source':U,OUTPUT char-hdl).
        hContainer = HANDLE(char-hdl).
   
        IF VALID-HANDLE(hContainer) THEN
          RUN send-item-browse IN hContainer (OUTPUT hItemBrowse).
      
        IF VALID-HANDLE(hItemBrowse) THEN DO:     
 
          RUN adm-row-available IN hItemBrowse.
          RUN local-open-query IN hItemBrowse.
          RUN apply-value-changed IN hItemBrowse.
                        
        END. /* Valid-handle hitembrowse */
       END.  /* If not freight is sensitive */
    END. /* if avail oe-bolh */
  END. /* do with frame */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-freight-header V-table-Win 
PROCEDURE calc-freight-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Runs from b-oeboll.w to display newly calculated freight
------------------------------------------------------------------------------*/
  
  DEF INPUT PARAMETER ipFreight AS DEC DECIMALS 6 NO-UNDO.
  DEF VAR dFreight AS DEC DECIMALS 6 NO-UNDO.
  DEF VAR dTotFreight AS DEC DECIMALS 6 NO-UNDO.
  DEF VAR hItemBrowse AS HANDLE NO-UNDO.
  DEF VAR hContainer AS HANDLE NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR char-val AS CHAR NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    IF AVAIL oe-bolh                                     AND
       oe-bolh.cust-no:SCREEN-VALUE NE ""                AND
       oe-bolh.ship-id:SCREEN-VALUE NE ""                AND
       oe-bolh.carrier:SCREEN-VALUE NE ""                AND
       CAN-FIND(FIRST oe-boll
                WHERE oe-boll.company EQ oe-bolh.company
                  AND oe-boll.b-no    EQ oe-bolh.b-no)   THEN DO:

      IF oe-bolh.freight:SENSITIVE THEN DO:
        /*RUN oe/getBolFrt.p (ROWID(oe-bolh),
                           oe-bolh.cust-no:SCREEN-VALUE,
                           oe-bolh.ship-id:SCREEN-VALUE,
                           oe-bolh.carrier:SCREEN-VALUE,
                           OUTPUT ipFreight).        */
        oe-bolh.freight:SCREEN-VALUE = STRING(ipFreight).
     
      END.
       ELSE DO:
        FIND CURRENT oe-bolh.
        oe-bolh.freight = 0.

        oe-bolh.freight:SCREEN-VALUE = STRING(ipFreight).        
        oe-bolh.freight = ipFreight.

        FIND CURRENT oe-bolh NO-LOCK.
/*         RUN dispatch ("row-available"). */

       END.
    END.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE clear-header V-table-Win 
PROCEDURE clear-header :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR char-hdl AS cha NO-UNDO.

   RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
   /*RUN dispatch IN WIDGET-HANDLE(char-hdl) ('get-next').  problem when last record deleted */
   RUN clear-header IN WIDGET-HANDLE(char-hdl).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE custom-panel-state V-table-Win 
PROCEDURE custom-panel-state :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT-OUTPUT PARAM io-panel-state AS CHAR NO-UNDO.


  IF NOT AVAIL oe-bolh OR oe-bolh.posted THEN
    io-panel-state = "disable-all".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE delete-bolh V-table-Win 
PROCEDURE delete-bolh :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR v-do-bol AS LOG NO-UNDO.
DEF VAR v-rel-row AS ROWID NO-UNDO.
DEF VAR v-rell-row AS ROWID NO-UNDO.
DEF VAR v-qty-prior AS INT NO-UNDO.
DEF VAR d-out AS DEC NO-UNDO.
DEF VAR v-last-tag AS CHAR NO-UNDO.

FOR EACH oe-boll
    WHERE oe-boll.company EQ oe-bolh.company
      AND oe-boll.b-no    EQ oe-bolh.b-no: 
    v-qty-prior = 0.
    
    FIND FIRST tt-ord WHERE tt-ord.ord-no EQ oe-boll.ord-no NO-ERROR.
    IF NOT AVAIL tt-ord THEN DO: 
      CREATE tt-ord.
      tt-ord.ord-no = oe-boll.ord-no.
    END.
    
    

    check-oe-rel:
    FOR EACH oe-rel
        WHERE oe-rel.company EQ oe-boll.company
          AND oe-rel.ord-no  EQ oe-boll.ord-no
          AND oe-rel.LINE    EQ oe-boll.LINE
          AND oe-rel.i-no    EQ oe-boll.i-no
          AND INDEX("SILC", oe-rel.stat) EQ 0
          AND oe-rel.link-no NE 0
        USE-INDEX ord-item NO-LOCK:
      v-rell-row = ?.
      FOR EACH oe-rell
          WHERE oe-rell.company  EQ oe-rel.company
            AND oe-rell.r-no     EQ oe-rel.link-no
            AND oe-rell.r-no     EQ oe-boll.r-no
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
            AND oe-rell.rel-no   EQ oe-rel.rel-no
            AND oe-rell.b-ord-no EQ oe-rel.b-ord-no
            AND oe-rell.po-no    EQ oe-rel.po-no
            AND oe-rell.tag      EQ oe-boll.tag
            AND oe-rell.posted   EQ YES
          USE-INDEX r-no
          BREAK BY oe-rell.r-no:
        IF LAST(oe-rell.r-no) THEN LEAVE.
      END.

      v-rel-row = ROWID(oe-rel).  
      IF AVAIL oe-rell THEN
      v-rell-row = ROWID(oe-rell).

      IF AVAIL oe-rell THEN DO:
          v-qty-prior = v-qty-prior + oe-rell.qty.  
          IF oe-rell.r-no EQ oe-boll.r-no THEN
              LEAVE check-oe-rel.
      END.
                  
    END. /* each oe-rel */

    v-last-tag = oe-boll.tag.
    IF oe-bolh.posted THEN oe-boll.deleted = YES.
    ELSE DO:
        {oe/bollrell.i}
        DELETE oe-boll.
    END.
    
    /* wfk - 6/13/13 - this occurs in delete trigger */
    /* RUN oe/reduce-actrel.p (INPUT v-rel-row, INPUT v-rell-row, INPUT v-qty-prior). */

END. /* each oe-boll */

{oe/bolhrell.i}

FIND FIRST oe-relh WHERE oe-relh.company EQ oe-bolh.company
                     AND oe-relh.release# EQ oe-bolh.release#
                     EXCLUSIVE-LOCK NO-ERROR.

/*if avail oe-relh THEN if v-do-bol then delete oe-relh.
                      else*/ IF AVAIL oe-relh THEN oe-relh.posted = NO.

IF oe-bolh.posted THEN DO:
    oe-bolh.deleted = YES.
    RETURN ERROR.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-bol-fields V-table-Win 
PROCEDURE disable-bol-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    
    DISABLE tgSigned.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-fields V-table-Win 
PROCEDURE disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-cust-detail V-table-Win 
PROCEDURE display-cust-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.
  FIND cust WHERE RECID(cust) = ip-recid NO-LOCK NO-ERROR.
  DEFINE VARIABLE relpost-chr LIKE sys-ctrl.char-fld NO-UNDO.
  DEFINE VARIABLE relpost-log LIKE sys-ctrl.log-fld  NO-UNDO.

  IF AVAIL cust THEN DO WITH FRAME {&frame-name} :
      
       RUN  relpost-values(cust.cust-no,"",OUTPUT relpost-chr ,OUTPUT relpost-log) .

       ASSIGN oe-bolh.cust-no:screen-value   = cust.cust-no
              cust_name:screen-value = cust.name
              cust_addr1:screen-value   = cust.addr[1]
              cust_addr2:screen-value   = cust.addr[2]
              cust_city:screen-value      = cust.city
              cust_state:screen-value     = cust.state
              cust_zip:screen-value       = cust.zip.
              oe-bolh.stat:screen-value     = STRING(relpost-log AND relpost-chr BEGINS "BOL","(H)OLD/(R)eleased") .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-shipto-detail V-table-Win 
PROCEDURE display-shipto-detail :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-recid AS RECID NO-UNDO.
  FIND shipto WHERE RECID(ship) = ip-recid NO-LOCK NO-ERROR.
  DEFINE VARIABLE relpost-chr LIKE sys-ctrl.char-fld NO-UNDO.
  DEFINE VARIABLE relpost-log LIKE sys-ctrl.log-fld  NO-UNDO.

  IF AVAIL shipto THEN DO WITH FRAME {&frame-name} :
     
     RUN  relpost-values(oe-bolh.cust-no:screen-value,shipto.ship-id,OUTPUT relpost-chr ,OUTPUT relpost-log) .

     ASSIGN oe-bolh.ship-id:screen-value   = shipto.ship-id
            ship_name:screen-value = shipto.ship-name
            ship_addr1:screen-value   = shipto.ship-addr[1]
            ship_addr2:screen-value   = shipto.ship-addr[2]
            ship_city:screen-value      = shipto.ship-city
            ship_state:screen-value     = shipto.ship-state
            ship_zip:screen-value       = shipto.ship-zip
            lv-ship-no = shipto.ship-no.
            oe-bolh.stat:screen-value     = STRING(relpost-log AND relpost-chr BEGINS "BOL","(H)OLD/(R)eleased") .
            
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE relpost-values  V-table-Win 
PROCEDURE relpost-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-custno AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ip-shipto AS CHAR NO-UNDO.
  DEFINE OUTPUT PARAMETER op-relpost-chr LIKE sys-ctrl.char-fld NO-UNDO.
  DEFINE OUTPUT PARAMETER op-relpost-log LIKE sys-ctrl.log-fld  NO-UNDO.
  DEF VAR cRtnChar AS CHAR NO-UNDO.
  DEFINE VARIABLE lRecFound AS LOGICAL     NO-UNDO.


   RUN sys/ref/nk1look.p (INPUT cocode, "RELPOST", "L" /* Logical */, YES /* check by cust */, 
       INPUT YES /* use cust not vendor */, ip-custno /* cust */, ip-shipto /* ship-to*/,
       OUTPUT cRtnChar, OUTPUT lRecFound).
       IF lRecFound THEN
       op-relpost-log = LOGICAL(cRtnChar) NO-ERROR.

     RUN sys/ref/nk1look.p (INPUT cocode, "RELPOST", "C" /* Logical */, YES /* check by cust */, 
     INPUT YES /* use cust not vendor */, ip-custno  /* cust */, ip-shipto /* ship-to*/,
     OUTPUT cRtnChar, OUTPUT lRecFound).
     IF lRecFound THEN
      op-relpost-chr = cRtnChar NO-ERROR. 
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-status V-table-Win 
PROCEDURE display-status :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL oe-bolh THEN
  DO WITH FRAME {&FRAME-NAME}:
    CASE oe-bolh.stat:
      WHEN "H" THEN oe-bolh.stat:SCREEN-VALUE = "(H)old".
          OTHERWISE oe-bolh.stat:SCREEN-VALUE = "(R)eleased".
    END CASE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-bol-fields V-table-Win 
PROCEDURE enable-bol-fields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    
    ENABLE tgSigned.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-add-record V-table-Win 
PROCEDURE local-add-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company
                        AND sys-ctrl.NAME = "RELPOST"
                        NO-LOCK NO-ERROR.
  IF AVAIL sys-ctrl AND sys-ctrl.char-fld <> "Nothing" THEN DO:
     MESSAGE "BOL should be added from Order/Release." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'add-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR old-weight LIKE oe-bolh.tot-wt NO-UNDO.
  DEF VAR old-freight LIKE oe-bolh.freight NO-UNDO.
  DEF VAR old-carrier LIKE oe-bolh.carrier NO-UNDO.
  DEF VAR old-shipid LIKE oe-bolh.ship-id NO-UNDO.
  DEF VAR new-shipid LIKE oe-bolh.ship-id NO-UNDO.
  DEF VAR new-carrier LIKE oe-bolh.carrier NO-UNDO.
  DEF VAR new-weight LIKE oe-bolh.tot-wt NO-UNDO.
  DEF VAR new-freight LIKE oe-bolh.freight NO-UNDO.
  DEF VAR old-bol LIKE oe-bolh.bol-no NO-UNDO.
  DEF VAR new-bol LIKE oe-bolh.bol-no NO-UNDO.
  DEF VAR ll AS LOG NO-UNDO.
  DEF VAR ld AS DEC NO-UNDO.
  DEF VAR dFreight AS DEC NO-UNDO.
  DEF VAR hItemBrowse AS HANDLE NO-UNDO.
  DEF VAR hContainer AS HANDLE NO-UNDO.
  DEF VAR char-hdl AS CHAR NO-UNDO.
  DEF VAR char-val AS CHAR NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   old-weight  = oe-bolh.tot-wt
   old-freight = oe-bolh.freight
   old-bol     = oe-bolh.bol-no
   old-carrier = oe-bolh.carrier
   old-shipid  = oe-bolh.ship-id.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
   oe-bolh.stat = SUBSTR(oe-bolh.stat,2,1)
   new-weight   = oe-bolh.tot-wt
   new-freight  = oe-bolh.freight
   new-bol      = oe-bolh.bol-no
   new-carrier  = oe-bolh.carrier
   new-shipid   = oe-bolh.ship-id.
  oe-bolh.spare-int-1 = (IF tgSigned:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "YES" THEN 1 ELSE 0).

  IF old-weight NE new-weight THEN DO:
    ASSIGN
     ll = NO
     ld = 0.

    /*MESSAGE "Update weight on line items?"
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE ll.

    IF ll THEN*/ DO:
      old-weight = 0.

      FOR EACH oe-boll
          WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no
          BREAK BY oe-boll.b-no:

        FIND FIRST itemfg
            WHERE itemfg.company EQ oe-boll.company
              AND itemfg.i-no    EQ oe-boll.i-no
            NO-ERROR.
        IF AVAIL itemfg THEN DO:
          IF itemfg.weight-100 EQ 0 THEN itemfg.weight-100 = 1.
          oe-boll.weight = oe-boll.qty / 100 * itemfg.weight-100.
        END.

        old-weight = old-weight + oe-boll.weight.
      END.

      FOR EACH oe-boll
          WHERE oe-boll.company EQ oe-bolh.company
            AND oe-boll.b-no    EQ oe-bolh.b-no
          BREAK BY oe-boll.b-no:

        ASSIGN
         oe-boll.weight = new-weight * (oe-boll.weight / old-weight)
         ld             = ld + oe-boll.weight.

        IF LAST(oe-boll.b-no) AND ld NE new-weight THEN
          oe-boll.weight = oe-boll.weight + (new-weight - ld).
      END.
    END.
  END.

  IF (old-carrier NE new-carrier OR old-shipid NE new-shipid)
      AND (NOT lFreightEntered) THEN DO:
    RUN oe/calcBolFrt.p (INPUT ROWID(oe-bolh), OUTPUT dFreight).
  END.

  IF old-freight NE new-freight THEN DO:
/* Task 04171407 - was to no longer update line items when header freight is updated, reinstating this 09161402 */   
        RUN oe/bolfrteq.p (BUFFER oe-bolh, new-freight, old-freight).
        RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'container-source':U,OUTPUT char-hdl).
        hContainer = HANDLE(char-hdl).
        IF VALID-HANDLE(hContainer) THEN
          RUN send-item-browse IN hContainer (OUTPUT hItemBrowse).

        IF VALID-HANDLE(hItemBrowse) THEN DO:

          RUN adm-row-available IN hItemBrowse.
          RUN local-open-query IN hItemBrowse.
          RUN apply-value-changed IN hItemBrowse.

        END.
  END.
    


  FIND CURRENT oe-boll NO-LOCK NO-ERROR.
  FIND CURRENT itemfg NO-LOCK NO-ERROR.
  
  /* refresh browse if bol # changes */

  IF new-bol NE old-bol THEN DO:  
      /* Refreshes browse with new details of combined BOL */
      llNewBol = TRUE.
      RUN send-records ("oe-bolh", OUTPUT char-val).
      RUN adm-open-query.
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'container-source':U,OUTPUT char-hdl).
      hContainer = HANDLE(char-hdl).
   
      IF VALID-HANDLE(hContainer) THEN
          RUN send-item-browse IN hContainer (OUTPUT hItemBrowse).
      
      IF VALID-HANDLE(hItemBrowse) THEN DO:     
 
         RUN adm-row-available IN hItemBrowse.
         RUN local-open-query IN hItemBrowse.
         RUN apply-value-changed IN hItemBrowse.
                        
      END.
          
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

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-bol-fields.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-next-bol AS INT NO-UNDO.
  DEF VAR li-next-release AS INT NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
  FIND LAST bf-bolh USE-INDEX b-no NO-LOCK NO-ERROR.
  li-next-bol = IF AVAIL bf-bolh THEN bf-bolh.b-no + 1 ELSE 1.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN oe/oe-bolno.p (g_company, OUTPUT li-next-bol).

  RUN oe/release#.p (g_company, OUTPUT li-next-release).

  ASSIGN oe-bolh.company = g_company
         oe-bolh.loc = g_loc
         oe-bolh.b-no = li-next-bol
         oe-bolh.bol-date = TODAY
         oe-bolh.bol-no = v-n-bol
         oe-bolh.release# = li-next-release
         oe-bolh.stat = "R".
         .

  DO WITH FRAME {&FRAME-NAME}:
    DISPLAY oe-bolh.bol-no oe-bolh.bol-date oe-bolh.release#.
  END.    

  RUN display-status.

  RUN dispatch ('row-changed').


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR d-out AS DEC NO-UNDO.
DEF VAR lrOrdlRow AS ROWID NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */
  IF AVAIL oe-bolh AND oe-bolh.posted THEN DO:
     MESSAGE "BOL has been posted, delete not allowed..." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.

  EMPTY TEMP-TABLE tt-ord.

  RUN delete-bolh NO-ERROR.
  IF AVAIL oe-ordl THEN
      lrOrdlRow = ROWID(oe-ordl).

  /* Dispatch standard ADM method.                             */
  IF NOT ERROR-STATUS:ERROR                                              AND
     NOT CAN-FIND(FIRST oe-boll WHERE oe-boll.company EQ oe-bolh.company
                                  AND oe-boll.b-no    EQ oe-bolh.b-no)   THEN
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  FOR EACH tt-ord WHERE tt-ord.ord-no NE 0
      BREAK BY tt-ord.ord-no:

    IF LAST-OF(tt-ord.ord-no) THEN
    FOR EACH oe-rel
        WHERE oe-rel.company EQ cocode
          AND oe-rel.ord-no  EQ tt-ord.ord-no:
      RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).

      IF AVAIL(oe-rel) AND VALID-HANDLE(lr-rel-lib) THEN 
        RUN recalc-act-qty IN lr-rel-lib (INPUT ROWID(oe-rel), OUTPUT d-out).

    END.

    DELETE tt-ord.
  END.

  /* Record was being locked, make sure it's not */
  FIND oe-ordl WHERE ROWID(oe-ordl) EQ lrOrdlRow NO-LOCK NO-ERROR.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-destroy V-table-Win 
PROCEDURE local-destroy :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  IF VALID-HANDLE(lr-rel-lib) THEN
     DELETE OBJECT lr-rel-lib.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-disable-fields V-table-Win 
PROCEDURE local-disable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'disable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-display-fields V-table-Win 
PROCEDURE local-display-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR li-hh AS INT NO-UNDO.
  DEF VAR li-ss AS INT NO-UNDO.
  DEF VAR li-mm AS INT NO-UNDO.
  
  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
   cust_name  = ""
   cust_addr1 = ""
   cust_addr2 = ""
   cust_city  = ""
   cust_state = ""
   cust_zip   = ""
   ship_name  = ""
   ship_addr1 = ""
   ship_addr2 = ""
   ship_city  = ""
   ship_state = ""
   ship_zip   = "".

  IF AVAIL oe-bolh THEN DO:
      FIND cust WHERE cust.company = oe-bolh.company AND
                      cust.cust-no = oe-bolh.cust-no NO-LOCK NO-ERROR.
      IF AVAIL cust THEN ASSIGN cust_name = cust.name
                                cust_addr1 = cust.addr[1]
                                cust_addr2 = cust.addr[2]
                                cust_city = cust.city
                                cust_state = cust.state
                                cust_zip = cust.zip.

      RUN oe/custxship.p (oe-bolh.company,
                          oe-bolh.cust-no,
                          oe-bolh.ship-id,
                          BUFFER shipto).

      IF AVAIL shipto THEN ASSIGN ship_name = shipto.ship-name
                                  ship_addr1 = shipto.ship-addr[1]
                                  ship_addr2 = shipto.ship-addr[2]
                                  ship_city = shipto.ship-city
                                  ship_state = shipto.ship-state
                                  ship_zip   = shipto.ship-zip.

    ASSIGN
     li-mm       = TRUNC(oe-bolh.upd-time / 60,0)
     li-hh       = TRUNC(li-mm / 60,0)
     li-mm       = li-mm - (li-hh * 60)
     li-ss       = oe-bolh.upd-time - (li-hh * 3600) - (li-mm * 60) 
     fi_upd-time = STRING(li-hh,"99") + ":" +
                   STRING(li-mm,"99") + ":" +
                   STRING(li-ss,"99").   
     tgSigned    = (IF oe-bolh.spare-int-1 EQ 1 THEN TRUE ELSE FALSE).

  END.
                                                                 
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN display-status.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-enable-fields V-table-Win 
PROCEDURE local-enable-fields :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  /*IF NOT adm-new-record and
     AVAIL oe-bolh AND oe-bolh.posted THEN DO:
     MESSAGE "BOL has been posted, update not allowed..." VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.*/
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'enable-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  DO WITH FRAME {&FRAME-NAME}:
    IF adm-adding-record THEN DISABLE oe-bolh.bol-no.

    IF AVAIL oe-bolh AND oe-bolh.posted THEN DO:
      DISABLE ALL.
      ENABLE oe-bolh.carrier
             oe-bolh.trailer.
    END.
  END.
  RUN enable-bol-fields.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit V-table-Win 
PROCEDURE local-exit :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'exit':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  IF VALID-HANDLE(lr-rel-lib) THEN
     DELETE OBJECT lr-rel-lib.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-hide V-table-Win 
PROCEDURE local-hide :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'hide':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-row-available V-table-Win 
PROCEDURE local-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'row-available':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF AVAIL oe-bolh AND oe-bolh.spare-int-1 EQ 1 THEN
    tgSigned = YES.
  ELSE
    tgSigned = NO.

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
  DEF VAR lv-oldbol LIKE lv-newbol NO-UNDO.
  DEF VAR char-val AS CHAR.
  DEF VAR hContainer AS HANDLE.
  DEF VAR hItemBrowse AS HANDLE.
  DEF BUFFER b-oe-bolh FOR oe-bolh.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN valid-bol-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-release NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-carrier NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-cust-no NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-ship-id NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-frt-pay NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-bol-date NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  
  lv-newbol = ROWID(oe-bolh).

  DO WITH FRAME {&FRAME-NAME}:
       IF oe-bolh.frt-pay:SCREEN-VALUE EQ "B" AND oe-bolh.freight:SCREEN-VALUE EQ "0.00" AND
            CAN-FIND(FIRST sys-ctrl WHERE sys-ctrl.company EQ oe-bolh.company
                             AND sys-ctrl.name    EQ "BOLFreight"
                             AND sys-ctrl.log-fld EQ YES) THEN DO:
           MESSAGE "Freight cost cannot be zero.  Please update." VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO oe-bolh.freight .
           RETURN ERROR.
       END.
  END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF lv-newbol NE ? THEN DO:
    RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"record-source",OUTPUT char-hdl).
    RUN reopen-query1 IN WIDGET-HANDLE(char-hdl) (lv-newbol).
  END.

  RUN disable-bol-fields.
  IF llNewBol EQ TRUE THEN DO:
      /* Refreshes browse showing new records from combined BOL */
      RUN send-records ("oe-bolh", OUTPUT char-val).
      RUN adm-open-query.
      RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,'container-source':U,OUTPUT char-hdl).
      hContainer = HANDLE(char-hdl).
   
      IF VALID-HANDLE(hContainer) THEN
          RUN send-item-browse IN hContainer (OUTPUT hItemBrowse).
      
      IF VALID-HANDLE(hItemBrowse) THEN DO:     
         RUN adm-row-available IN hItemBrowse.
         RUN local-open-query IN hItemBrowse.
         RUN apply-value-changed IN hItemBrowse.
      END.
      llNewBol = FALSE.
  END. 
    
  lFreightEntered = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new-carrier V-table-Win 
PROCEDURE new-carrier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    FIND carrier
        WHERE carrier.company EQ g_company
          AND carrier.loc     EQ g_loc                       
          AND carrier.carrier EQ oe-bolh.carrier:SCREEN-VALUE
        NO-LOCK NO-ERROR.
        
    IF AVAIL carrier AND NOT lFreightEntered THEN RUN calc-freight.
  END.

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
          AND cust.cust-no EQ oe-bolh.cust-no:SCREEN-VALUE
          AND INDEX("AX",cust.active) GT 0
        NO-LOCK NO-ERROR.
    IF AVAIL cust THEN DO:
      RUN display-cust-detail (RECID(cust)).

      FIND FIRST shipto
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ oe-bolh.cust-no:SCREEN-VALUE
            AND shipto.ship-id EQ oe-bolh.cust-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF NOT AVAIL shipto THEN
      FIND FIRST shipto
          WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ oe-bolh.cust-no:SCREEN-VALUE
          NO-LOCK NO-ERROR.
      IF AVAIL shipto THEN oe-bolh.ship-id:SCREEN-VALUE = shipto.ship-id.
      RUN new-ship-id.
    END.
  END.

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
    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no:SCREEN-VALUE,
                        oe-bolh.ship-id:SCREEN-VALUE,
                        BUFFER shipto).

    IF AVAIL shipto THEN DO:
      RUN display-shipto-detail (RECID(shipto)).
      IF NOT lFreightEntered THEN
      RUN calc-freight.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE release-update V-table-Win 
PROCEDURE release-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  IF AVAIL oe-bolh THEN DO:
    IF oe-bolh.posted THEN DO:
      MESSAGE "BOL has been posted, release not allowed..." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.

  
    FIND CURRENT oe-bolh.

    /*if oe-bolh.trailer EQ "HOLD" then
      assign
       oe-bolh.trailer = ""
       oe-bolh.printed = yes.

    else
    if oe-bolh.trailer EQ "" then
      assign
       oe-bolh.trailer = "HOLD"
       oe-bolh.printed = no.*/

    IF oe-bolh.stat EQ "H" THEN
      ASSIGN
       oe-bolh.stat = "R"
       oe-bolh.printed = YES.

    ELSE ASSIGN oe-bolh.stat = "H"
                oe-bolh.printed = NO.

    FIND CURRENT oe-bolh NO-LOCK.

    RUN dispatch ("display-fields").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE check-release-update V-table-Win 
PROCEDURE check-release-update :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER cCheck-rel AS CHARACTER NO-UNDO .
    
    IF AVAIL oe-bolh THEN
        ASSIGN
        cCheck-rel = oe-bolh.stat .

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
  {src/adm/template/snd-list.i "oe-bolh"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bol-date V-table-Win 
PROCEDURE valid-bol-date :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lValid    AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.
    DEFINE VARIABLE ldDate    AS DATE    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN lValid = TRUE
               lContinue = TRUE
               ldDate = DATE(oe-bolh.bol-date:SCREEN-VALUE).
        RUN oe/dateFuture.p (INPUT cocode, INPUT lddate, INPUT YES /* prompt */, OUTPUT lValid, OUTPUT lContinue).    
        IF lValid EQ NO AND lContinue EQ NO THEN 
        DO:
            APPLY "entry" TO oe-bolh.bol-date.
            RETURN ERROR.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-bol-no V-table-Win 
PROCEDURE valid-bol-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-msg AS CHAR NO-UNDO.


  DO WITH FRAME {&FRAME-NAME}:
    IF lv-msg EQ ""                          AND
       INT(oe-bolh.bol-no:SCREEN-VALUE) EQ 0 THEN
      lv-msg = "may not be 0".

    FIND FIRST bf-bolh NO-LOCK
        WHERE bf-bolh.company  EQ cocode
          AND bf-bolh.bol-no   EQ INT(oe-bolh.bol-no:SCREEN-VALUE)
          AND ROWID(bf-bolh)   NE ROWID(oe-bolh)
        NO-ERROR.

    IF AVAIL bf-bolh THEN DO:
      IF lv-msg EQ ""                                       AND
         (bf-bolh.cust-no NE oe-bolh.cust-no:SCREEN-VALUE OR
          bf-bolh.ship-id NE oe-bolh.ship-id:SCREEN-VALUE)  THEN
        lv-msg = "exists for another customer/shipto, cannot change".

      IF lv-msg EQ "" AND bf-bolh.posted THEN
        lv-msg = "has been posted, cannot change".

      IF lv-msg EQ "" THEN DO:
        FIND FIRST oe-boll WHERE oe-boll.b-no EQ oe-bolh.b-no NO-LOCK NO-ERROR.
        IF NOT AVAIL oe-boll THEN
          lv-msg = "has no line items, cannot change".
      END.

      IF lv-msg EQ "" THEN DO:
        FIND FIRST bf-boll NO-LOCK
            WHERE bf-boll.b-no   EQ bf-bolh.b-no
              AND bf-boll.s-code NE oe-boll.s-code
            NO-ERROR.
        IF AVAIL bf-boll THEN
          lv-msg = "is " +
                   TRIM(ENTRY(LOOKUP(bf-boll.s-code,lv-type-code),lv-type-dscr)) +
                   ", you cannot add a BOL that is " +
                   TRIM(ENTRY(LOOKUP(oe-boll.s-code,lv-type-code),lv-type-dscr)) +
                   " to it".
      END.
    END.

    IF lv-msg NE "" THEN DO:
      MESSAGE TRIM(oe-bolh.bol-no:LABEL) + " " + TRIM(lv-msg) + "..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-bolh.bol-no.
      RETURN ERROR.
    END.


  END.

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

    FIND FIRST carrier WHERE carrier.company = g_company
                         AND carrier.loc = g_loc                       
                         AND carrier.carrier = oe-bolh.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME}                          
                         NO-LOCK NO-ERROR.
    IF NOT AVAIL carrier THEN DO:
       MESSAGE "Invalid Carrier. Try Help. " VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO oe-bolh.carrier.
       RETURN ERROR.
    END.

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

  DO WITH FRAME {&FRAME-NAME}:
    oe-bolh.cust-no:SCREEN-VALUE = CAPS(oe-bolh.cust-no:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST cust
                    WHERE cust.company EQ cocode
                      AND cust.cust-no EQ oe-bolh.cust-no:SCREEN-VALUE
                      AND INDEX("AXSE",cust.active) GT 0)
     THEN DO:
       MESSAGE "Invalid " + TRIM(oe-bolh.cust-no:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO oe-bolh.cust-no.
       RETURN ERROR.
    END.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-frt-pay V-table-Win 
PROCEDURE valid-frt-pay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF LOOKUP(oe-bolh.frt-pay:SCREEN-VALUE IN FRAME {&FRAME-NAME} ,"B,C,P,T") <= 0 THEN DO:
     MESSAGE "Invalid Freight Payment. Must be one of (B)ill,(C)ollect,(P)repaid.(T)3rd Party. " VIEW-AS ALERT-BOX.
     APPLY "entry" TO oe-bolh.frt-pay.
     RETURN ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-release V-table-Win 
PROCEDURE valid-release :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:


  IF int(oe-bolh.release#:SCREEN-VALUE) = 0 THEN DO:
       MESSAGE "Release# must be entered. Try Help. " VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO oe-bolh.RELEASE#.
       RETURN ERROR.
  END.

  FIND FIRST bf-bolh WHERE bf-bolh.company = g_company AND
                           bf-bolh.release# = INT(oe-bolh.release#:SCREEN-VALUE)
                        AND RECID(bf-bolh) <> RECID(oe-bolh)
                        NO-LOCK NO-ERROR.
  IF AVAIL bf-bolh THEN DO:
     MESSAGE "Release# already exists for BOL#:" bf-bolh.bol-no ". Try help or Enter new number."
              VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO oe-bolh.RELEASE#.
     RETURN ERROR.
  END.
END.  

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

  DO WITH FRAME {&FRAME-NAME}:
    oe-bolh.ship-id:SCREEN-VALUE = CAPS(oe-bolh.ship-id:SCREEN-VALUE).

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no:SCREEN-VALUE,
                        oe-bolh.ship-id:SCREEN-VALUE,
                        BUFFER shipto).

    IF NOT AVAIL shipto THEN DO:
      MESSAGE "Invalid " + TRIM(oe-bolh.ship-id:LABEL) + ", try help..."
          VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO oe-bolh.ship-id.
      RETURN ERROR.
    END.
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

