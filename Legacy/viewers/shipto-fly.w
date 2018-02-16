&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/shipto.w

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
DEFINE VARIABLE op-company AS CHARACTER NO-UNDO.
DEFINE VARIABLE op-cust-no AS CHARACTER NO-UNDO.

DEF VAR lv-bolwhse LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR lv-autopost LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR lv-tax-mand AS LOG NO-UNDO.
DEF VAR v-cust-fmt AS CHAR NO-UNDO.
DEF VAR v-cust-log AS LOGICAL NO-UNDO.

{sys/inc/var.i NEW SHARED}

&scoped-define copy-proc proc-copy

{custom/globdefs.i}
{custom/gcompany.i}
{custom/gloc.i}

ASSIGN
 cocode   = g_company
 locode   = g_loc
 gcompany = g_company
 gloc     = g_loc.

do transaction:
  {sys/inc/taxcode.i}
end.

do transaction:
  {sys/inc/autopost.i}
end.

do transaction:
  {sys/inc/bolwhse.i}
end.

do transaction:
   find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "CustXfer"
                        no-lock no-error.
    IF AVAIL sys-ctrl  THEN
        assign
        v-cust-log = sys-ctrl.log-fld 
        v-cust-fmt = sys-ctrl.char-fld.
end.



{methods/defines/shipto.i &NEW="NEW"}

DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

&SCOPED-DEFINE enable-shipto enable-shipto



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
&Scoped-define EXTERNAL-TABLES shipto cust
&Scoped-define FIRST-EXTERNAL-TABLE shipto


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR shipto, cust.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS shipto.ship-name shipto.ship-addr[1] ~
shipto.ship-addr[2] shipto.ship-city shipto.ship-state shipto.ship-zip ~
shipto.contact shipto.area-code shipto.phone shipto.spare-char-1 ~
shipto.tax-code shipto.notes[1] shipto.notes[2] shipto.notes[3] ~
shipto.notes[4] shipto.loc shipto.loc-bin shipto.carrier shipto.dest-code ~
shipto.pallet shipto.spare-char-4 shipto.spare-char-5 shipto.dock-loc ~
shipto.dock-hour shipto.del-chg shipto.del-time shipto.ship-meth ~
shipto.broker shipto.bill 
&Scoped-define ENABLED-TABLES shipto
&Scoped-define FIRST-ENABLED-TABLE shipto
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS shipto.ship-id shipto.ship-name ~
shipto.ship-addr[1] shipto.ship-addr[2] shipto.ship-city shipto.ship-state ~
shipto.ship-zip shipto.contact shipto.area-code shipto.phone ~
shipto.spare-char-1 shipto.tax-code shipto.notes[1] shipto.notes[2] ~
shipto.notes[3] shipto.notes[4] shipto.loc shipto.loc-bin shipto.carrier ~
shipto.dest-code shipto.pallet shipto.spare-char-4 shipto.spare-char-5 ~
shipto.dock-loc shipto.dock-hour shipto.del-chg shipto.del-time ~
shipto.ship-meth shipto.broker shipto.bill 
&Scoped-define DISPLAYED-TABLES shipto
&Scoped-define FIRST-DISPLAYED-TABLE shipto
&Scoped-Define DISPLAYED-OBJECTS fi_sname faxAreaCode faxNumber ~
tb_mandatory-tax fi_jded-id 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS shipto.ship-id 
&Scoped-define ADM-ASSIGN-FIELDS tb_mandatory-tax fi_jded-id 
&Scoped-define DISPLAY-FIELD shipto.ship-state shipto.tax-code shipto.loc ~
shipto.carrier shipto.dest-code 
&Scoped-define List-5 faxAreaCode faxNumber 

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

/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSalesmanName V-table-Win 
FUNCTION getSalesmanName RETURNS CHARACTER
  (ipcSalesman AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE VARIABLE faxAreaCode AS CHARACTER FORMAT "(xxx)":U 
     LABEL "Fax #" 
     VIEW-AS FILL-IN 
     SIZE 7.4 BY 1 NO-UNDO.

DEFINE VARIABLE faxNumber AS CHARACTER FORMAT "xxx-xxxx":U 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE fi_jded-id AS CHARACTER FORMAT "X(256)":U 
     LABEL "Export ID#" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE fi_sname AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 108 BY 10.48.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 13.81.

DEFINE VARIABLE tb_mandatory-tax AS LOGICAL INITIAL no 
     LABEL "Mandatory Tax?" 
     VIEW-AS TOGGLE-BOX
     SIZE 21.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     fi_sname AT ROW 4.43 COL 77.6 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     shipto.ship-id AT ROW 1.43 COL 14.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
          FONT 4
     shipto.ship-name AT ROW 2.43 COL 14.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          FONT 4
     shipto.ship-addr[1] AT ROW 3.48 COL 14.6 COLON-ALIGNED
          LABEL "Address"
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          FONT 4
     shipto.ship-addr[2] AT ROW 4.48 COL 14.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 38 BY 1
          FONT 4
     shipto.ship-city AT ROW 5.43 COL 14.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          FONT 4
     shipto.ship-state AT ROW 5.43 COL 34.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 5.4 BY 1
          FONT 4
     shipto.ship-zip AT ROW 5.43 COL 39.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
          FONT 4
     shipto.contact AT ROW 1.43 COL 68.6 COLON-ALIGNED
          LABEL "Contact"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     shipto.area-code AT ROW 2.48 COL 68.6 COLON-ALIGNED
          LABEL "Phone" FORMAT "(xxx)"
          VIEW-AS FILL-IN 
          SIZE 7.4 BY 1
     shipto.phone AT ROW 2.48 COL 76.4 COLON-ALIGNED NO-LABEL FORMAT "xxx-xxxx"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     faxAreaCode AT ROW 3.48 COL 68.6 COLON-ALIGNED AUTO-RETURN 
     faxNumber AT ROW 3.48 COL 76.4 COLON-ALIGNED NO-LABEL
     shipto.spare-char-1 AT ROW 4.43 COL 68.6 COLON-ALIGNED
          LABEL "Salesman" FORMAT "xxx"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     shipto.tax-code AT ROW 5.38 COL 68.6 COLON-ALIGNED
          LABEL "Tax Code"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          FONT 4
     tb_mandatory-tax AT ROW 5.62 COL 87.8
     shipto.notes[1] AT ROW 7.1 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 100.6 BY 1
     shipto.notes[2] AT ROW 8 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 100.6 BY 1
     shipto.notes[3] AT ROW 8.91 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 100.6 BY 1
     shipto.notes[4] AT ROW 9.81 COL 5 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 100.6 BY 1
     shipto.loc AT ROW 1.62 COL 125 COLON-ALIGNED
          LABEL "Warehouse"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          FONT 4
     shipto.loc-bin AT ROW 2.57 COL 125 COLON-ALIGNED
          LABEL "Bin"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     shipto.carrier AT ROW 3.52 COL 124.8 COLON-ALIGNED
          LABEL "Carrier"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          FONT 4
     shipto.dest-code AT ROW 4.48 COL 124.8 COLON-ALIGNED
          LABEL "Zone"
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
          FONT 4
     shipto.pallet AT ROW 5.43 COL 124.8 COLON-ALIGNED
          LABEL "Pallet"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     shipto.spare-char-4 AT ROW 6.43 COL 124.8 COLON-ALIGNED
         LABEL "Shipper ID" FORMAT "X(10)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     shipto.spare-char-5 AT ROW 7.43 COL 124.8 COLON-ALIGNED
          LABEL "Member#" FORMAT "X(10)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     fi_jded-id AT ROW 8.43 COL 124.8 COLON-ALIGNED
     shipto.dock-loc AT ROW 9.43 COL 124.8 COLON-ALIGNED
          LABEL "Dock#"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     shipto.dock-hour AT ROW 10.43 COL 124.8 COLON-ALIGNED
          LABEL "Dock Hours"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     shipto.del-chg AT ROW 11.62 COL 119 COLON-ALIGNED
          LABEL "Charge"
          VIEW-AS FILL-IN 
          SIZE 9.2 BY 1
          FONT 4
     shipto.del-time AT ROW 11.62 COL 136.2 COLON-ALIGNED
          LABEL "Time"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          FONT 4
     shipto.ship-meth AT ROW 12.81 COL 124 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Case", yes,
"Pallet", no
          SIZE 22 BY 1
     shipto.broker AT ROW 14 COL 116.6
          LABEL "Broker?"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
     shipto.bill AT ROW 13.95 COL 146.8 RIGHT-ALIGNED
          LABEL "Billable"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .81
     "N" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 7.05 COL 3.2
     "O" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 7.76 COL 3.2
     "T" VIEW-AS TEXT
          SIZE 3 BY .48 AT ROW 8.48 COL 3.2
     "E" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 9.19 COL 3.2
     "S" VIEW-AS TEXT
          SIZE 3 BY .62 AT ROW 9.95 COL 3.2
     "Shp Meth.:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 12.95 COL 111.4
     RECT-1 AT ROW 1.24 COL 2
     RECT-2 AT ROW 1.24 COL 110 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.shipto,ASI.cust
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
         HEIGHT             = 14.05
         WIDTH              = 149.8.
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit Custom                            */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN shipto.area-code IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR TOGGLE-BOX shipto.bill IN FRAME F-Main
   ALIGN-R EXP-LABEL                                                    */
/* SETTINGS FOR TOGGLE-BOX shipto.broker IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN shipto.carrier IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN shipto.contact IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN shipto.del-chg IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN shipto.del-time IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN shipto.dest-code IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN shipto.dock-hour IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN shipto.dock-loc IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN faxAreaCode IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN faxNumber IN FRAME F-Main
   NO-ENABLE 5                                                          */
/* SETTINGS FOR FILL-IN fi_jded-id IN FRAME F-Main
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN fi_sname IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN shipto.loc IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN shipto.loc-bin IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN shipto.notes[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN shipto.phone IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN shipto.ship-addr[1] IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN shipto.ship-id IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN shipto.ship-state IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN shipto.spare-char-1 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN shipto.spare-char-4 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN shipto.spare-char-5 IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN shipto.tax-code IN FRAME F-Main
   4 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX tb_mandatory-tax IN FRAME F-Main
   NO-ENABLE 2                                                          */
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
    DEF VAR lv-handle AS HANDLE NO-UNDO.


    CASE FOCUS:NAME :
         when "ship-id" then do:
            run windows/l-cust.w  (gcompany,focus:screen-value, output char-val). 
            if char-val <> "" then DO:
               focus:screen-value in frame {&frame-name} = entry(1,char-val).
               RUN display-new-shipto.
            END.
          end.
          when "tax-code" then do:
            run windows/l-stax.w  (gcompany,focus:screen-value, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
          end.
          when "loc" then do:
            run windows/l-loc.w  (gcompany,focus:screen-value, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
          end.
          when "loc-bin" then do:
            run windows/l-fgbin.w  (gcompany,shipto.loc:SCREEN-VALUE IN FRAME {&FRAME-NAME},focus:screen-value, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
          end.
          when "carrier" then do:
            run windows/l-carrie.w  (gcompany, shipto.loc:SCREEN-VALUE IN FRAME {&FRAME-NAME}, focus:screen-value, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
          end.
          when "dest-code" then do:
            run windows/l-delzon.w  (gcompany, shipto.loc:SCREEN-VALUE IN FRAME {&FRAME-NAME}, shipto.carrier:SCREEN-VALUE IN FRAME {&FRAME-NAME}, focus:screen-value, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
          end.
          when "spare-char-1" then do:
            run windows/l-sman.w  (gcompany, output char-val). 
            if char-val <> "" then 
              focus:screen-value in frame {&frame-name} = entry(1,char-val).
          end.
          when "pallet" then do:
           run windows/l-itemp.w 
              (gcompany,"",focus:screen-value in frame {&frame-name}, output char-val).
           if char-val <> "" then 
              assign focus:screen-value in frame {&frame-name} = entry(1,char-val).
           return no-apply.  
          end.
          otherwise do:
             lv-handle = focus:handle.
             run applhelp.p.

             if g_lookup-var <> "" then do:
                lv-handle:screen-value = g_lookup-var.        
             end.   /* g_lookup-var <> "" */
             g_lookup-var = "".
             return no-apply.
           END.  /* otherwise */
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.bill
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.bill V-table-Win
ON VALUE-CHANGED OF shipto.bill IN FRAME F-Main /* Billable */
DO:
  DEF BUFFER b-cust FOR cust.


  RUN valid-ship-id NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  IF {&self-name}:SCREEN-VALUE EQ "Yes" AND adm-new-record THEN DO:
    FIND FIRST b-cust
        WHERE b-cust.company EQ cocode
          AND b-cust.cust-no EQ shipto.ship-id:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL b-cust THEN DO:
      ASSIGN
       shipto.ship-name:SCREEN-VALUE    = b-cust.name
       shipto.ship-addr[1]:SCREEN-VALUE = b-cust.addr[1]
       shipto.ship-addr[2]:SCREEN-VALUE = b-cust.addr[2]
       shipto.ship-city:SCREEN-VALUE    = b-cust.city
       shipto.ship-state:SCREEN-VALUE   = b-cust.state
       shipto.ship-zip:SCREEN-VALUE     = b-cust.zip
       shipto.carrier:SCREEN-VALUE      = b-cust.carrier
       shipto.loc:SCREEN-VALUE          = b-cust.loc
       shipto.dest-code:SCREEN-VALUE    = b-cust.del-zone
       shipto.tax-code:SCREEN-VALUE     = b-cust.tax-gr.

      {methods/dispflds.i}
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.broker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.broker V-table-Win
ON return OF shipto.broker IN FRAME F-Main /* Broker? */
DO:
    apply "tab" to self.
  return no-apply.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.carrier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.carrier V-table-Win
ON ENTRY OF shipto.carrier IN FRAME F-Main /* Carrier */
DO:
  s-loc = shipto.loc:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.carrier V-table-Win
ON LEAVE OF shipto.carrier IN FRAME F-Main /* Carrier */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-carrier NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.dest-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.dest-code V-table-Win
ON ENTRY OF shipto.dest-code IN FRAME F-Main /* Zone */
DO:
  ASSIGN
    s-loc = shipto.loc:SCREEN-VALUE
    s-carrier = shipto.carrier:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.dest-code V-table-Win
ON LEAVE OF shipto.dest-code IN FRAME F-Main /* Zone */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-dest-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faxAreaCode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faxAreaCode V-table-Win
ON LEAVE OF faxAreaCode IN FRAME F-Main /* Fax # */
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faxNumber
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faxNumber V-table-Win
ON LEAVE OF faxNumber IN FRAME F-Main
DO:
  ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.loc
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.loc V-table-Win
ON LEAVE OF shipto.loc IN FRAME F-Main /* Warehouse */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.loc-bin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.loc-bin V-table-Win
ON LEAVE OF shipto.loc-bin IN FRAME F-Main /* Bin */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-loc-bin NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.pallet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.pallet V-table-Win
ON LEAVE OF shipto.pallet IN FRAME F-Main /* Corrugated Pallet */
DO:
    {&methods/lValidateError.i YES}
    if lastkey <> -1 and shipto.pallet:screen-value <> "" and
          not can-find(first item where item.company = gcompany and item.mat-type = "D" and
                                        item.i-no = shipto.pallet:screen-value)
       then do:
          message "Invalid Pallet Code. Try Help." view-as alert-box error.
          return no-apply.     
       end.
       {&methods/lValidateError.i NO}
END.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.ship-id
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.ship-id V-table-Win
ON LEAVE OF shipto.ship-id IN FRAME F-Main /* Ship To ID */
DO:
  /*IF LASTKEY NE -1 THEN DO:
    RUN valid-ship-id NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  {methods/dispflds.i}
  */
  /* will run only create new record */
  RUN display-new-shipto.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.ship-state
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.ship-state V-table-Win
ON LEAVE OF shipto.ship-state IN FRAME F-Main /* State */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-ship-state NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.ship-zip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.ship-zip V-table-Win
ON HELP OF shipto.ship-zip IN FRAME F-Main /* Zip */
DO:
  RUN applhelp.p.
  {&self-name}:SCREEN-VALUE = g_lookup-var.
  RUN ship-zip.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.ship-zip V-table-Win
ON LEAVE OF shipto.ship-zip IN FRAME F-Main /* Zip */
DO:
  IF LASTKEY NE -1 THEN RUN ship-zip.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.spare-char-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.spare-char-1 V-table-Win
ON LEAVE OF shipto.spare-char-1 IN FRAME F-Main /* Salesman */
DO:
   IF LASTKEY NE -1 THEN DO:
    RUN valid-sman NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY. 
  END.

  {methods/dispflds.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME shipto.tax-code
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL shipto.tax-code V-table-Win
ON LEAVE OF shipto.tax-code IN FRAME F-Main /* Tax Code */
DO:
  IF LASTKEY NE -1 THEN DO:
    RUN valid-tax-code NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  END.

  {methods/dispflds.i}
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
  {src/adm/template/row-list.i "shipto"}
  {src/adm/template/row-list.i "cust"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "shipto"}
  {src/adm/template/row-find.i "cust"}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable-shipto V-table-Win 
PROCEDURE disable-shipto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    DISABLE fi_jded-id tb_mandatory-tax.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE display-new-shipto V-table-Win 
PROCEDURE display-new-shipto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DEF BUFFER bf-cust FOR cust.

 DO WITH FRAME {&FRAME-NAME}:
    FIND FIRST bf-cust WHERE bf-cust.company = g_company AND
                            bf-cust.cust-no = shipto.ship-id:SCREEN-VALUE
                            NO-LOCK NO-ERROR.
    IF AVAIL bf-cust THEN
         assign shipto.ship-addr[1]:SCREEN-VALUE = bf-cust.addr[1]
                shipto.ship-addr[2]:SCREEN-VALUE = bf-cust.addr[2]
                shipto.ship-city:SCREEN-VALUE = bf-cust.city
                shipto.ship-id:SCREEN-VALUE = bf-cust.cust-no
                shipto.ship-name:SCREEN-VALUE = bf-cust.name
                shipto.ship-state:SCREEN-VALUE = bf-cust.state
                shipto.ship-zip:SCREEN-VALUE = bf-cust.zip
                shipto.carrier:SCREEN-VALUE = bf-cust.carrier
                shipto.dest-code:SCREEN-VALUE = bf-cust.del-zone
                shipto.tax-code:SCREEN-VALUE = bf-cust.tax-gr
                shipto.loc:SCREEN-VALUE = g_loc
                shipto.spare-char-1:SCREEN-VALUE = bf-cust.sman
                fi_sname:SCREEN-VALUE = getSalesmanName( bf-cust.sman )
                .
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-shipto V-table-Win 
PROCEDURE enable-shipto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE fi_jded-id tb_mandatory-tax.

    IF TRIM(shipto.ship-id:SCREEN-VALUE) EQ TRIM(cust.cust-no) THEN DO:
      ASSIGN
       shipto.bill:SCREEN-VALUE   = "No"
       shipto.broker:SCREEN-VALUE = "No".

      DISABLE
       shipto.bill
       shipto.broker.
    END.

    ELSE
      ENABLE
       shipto.bill
       shipto.broker.
    ENABLE faxareacode faxnumber .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE import-data V-table-Win 
PROCEDURE import-data :
/*------------------------------------------------------------------------------
  Purpose:     Run excel import program.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 Define variable hBrowse   as handle  no-undo.

  IF AVAIL shipto THEN
      RUN util/xlship2.w (INPUT shipto.company,
                          INPUT shipto.cust-no,
                          INPUT shipto.carrier,
                          INPUT shipto.loc,
                          INPUT shipto.loc-bin,
                          INPUT shipto.dest-code).



  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"record-source",OUTPUT hBrowse).

  IF VALID-HANDLE(hBrowse) THEN
      RUN dispatch IN hBrowse  ('open-query':U).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER ycust FOR cust.
  DEF BUFFER yshipto FOR shipto.

  /* Code placed here will execute PRIOR to standard behavior. */
   IF shipto.spare-char-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" AND AVAIL cust THEN do:
   ASSIGN shipto.spare-char-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cust.sman .
    FIND FIRST sman
        WHERE sman.company EQ cocode
          AND sman.sman    EQ shipto.spare-char-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        NO-LOCK NO-ERROR.
    IF AVAIL sman THEN
    fi_sname:SCREEN-VALUE = sman.sname.
   END.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN reftable-values (INPUT NO).


  IF cust.active EQ "X" THEN DO: 
    SESSION:SET-WAIT-STATE ("general").

    FOR EACH ycust NO-LOCK:
      FOR EACH yshipto OF ycust
          WHERE yshipto.loc-bin EQ "" 
            AND yshipto.loc     EQ shipto.loc
          EXCLUSIVE-LOCK:
        yshipto.loc-bin = shipto.loc-bin.
      END. /*inner for each*/
    END. /*outer for each*/

    SESSION:SET-WAIT-STATE ("").
  END.

  shipto.fax = faxAreaCode + faxNumber.
  disable faxareacode faxnumber WITH FRAME {&FRAME-NAME}.

  IF adm-new-record THEN DO:
     IF v-cust-log THEN 
         RUN shipto-new-log.
   END.
   ELSE DO:
     IF v-cust-log THEN 
         RUN shipto-update-log.
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
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "new-record-target", OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN RETURN NO-APPLY.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN disable-shipto.
  disable faxareacode faxnumber WITH FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute BEFORE standard behavior.   */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  {methods/viewers/create/shipto.i}
   fi_sname:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "".

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
  DEFINE BUFFER buff-shipto FOR shipto .

  /* Code placed here will execute PRIOR to standard behavior. */
  IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
  END.
    /* Code placed here will execute BEFORE standard behavior.    */ 
   IF v-cust-log THEN do:
      FIND CURRENT shipto NO-LOCK NO-ERROR.
      DO I = 1 TO NUM-ENTRIES(v-cust-fmt):
          ASSIGN thisOne = ENTRY(i,v-cust-fmt).
          FIND FIRST buff-shipto WHERE buff-shipto.cust-no = shipto.cust-no 
                                  AND buff-shipto.ship-id  = shipto.ship-id
                                  AND buff-shipto.company = thisOne EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL buff-shipto THEN
              DELETE buff-shipto .
      END.
   END.

  /* Dispatch standard ADM method.                             */
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

  /* Code placed here will execute PRIOR to standard behavior. */
  RUN reftable-values (INPUT YES).

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'display-fields':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  IF NOT adm-new-record AND AVAIL shipto THEN
  DO:
      ASSIGN
        faxAreaCode = SUBSTR(shipto.fax,1,3)
        faxNumber = SUBSTR(shipto.fax,4)
        fi_sname = getSalesmanName(shipto.spare-char-1).

      DISPLAY faxareacode faxnumber fi_sname WITH FRAME {&FRAME-NAME}.

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
DEF VAR ip-shipnotes AS CHAR NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */
/*   RUN ship-zip. */

  RUN valid-ship-id NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-ship-state NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-tax-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-sman NO-ERROR. 
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-loc-bin NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-carrier NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.

  RUN valid-dest-code NO-ERROR.
  IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.
  {&methods/lValidateError.i YES}
  if shipto.pallet:screen-value IN FRAME {&FRAME-NAME} <> "" and
        not can-find(first item where item.company = gcompany and item.mat-type = "D" and
                                      item.i-no = shipto.pallet:screen-value)
     then do:
        message "Invalid Pallet Code. Try Help." view-as alert-box error.
        apply "entry" to shipto.pallet.
        return .     
     end.
    {&methods/lValidateError.i NO}
  IF adm-new-record and 
     shipto.bill:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "Yes" THEN DO:

  END.
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN
      ip-shipnotes = TRIM(shipto.notes[1]) + "|" +
                     TRIM(shipto.notes[2]) + "|" +
                     TRIM(shipto.notes[3]) + "|" +
                     TRIM(shipto.notes[4]).

  RUN oe\d-shp2nt.w(INPUT shipto.company, 
                    INPUT shipto.cust-no, 
                    INPUT shipto.ship-id,
                    INPUT ip-shipnotes).

  RUN disable-shipto.

END PROCEDURE.


/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE proc-copy V-table-Win 
PROCEDURE proc-copy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ll-copied AS LOG NO-UNDO.

  DEF VAR ls-focus AS CHAR NO-UNDO.


  IF AVAIL shipto THEN DO WITH FRAME {&FRAME-NAME}:
    ls-focus = FOCUS:SCREEN-VALUE.

    APPLY "entry" TO FRAME {&FRAME-NAME}.

    RUN ar/copyship.w (ROWID(shipto), OUTPUT ll-copied).

    FOCUS:SCREEN-VALUE = ls-focus.

    IF ll-copied THEN RUN dispatch ("cancel-record").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reftable-values V-table-Win 
PROCEDURE reftable-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-display AS LOG NO-UNDO.

 DO WITH FRAME {&FRAME-NAME}:
  IF AVAIL shipto THEN DO TRANSACTION:
    IF ip-display THEN
      fi_jded-id:SCREEN-VALUE = shipto.ExportCustID .
    ELSE
      shipto.ExportCustID  = fi_jded-id:SCREEN-VALUE.

   
    IF ip-display THEN
      tb_mandatory-tax:SCREEN-VALUE = IF shipto.tax-mandatory THEN "Yes" ELSE "No".
    ELSE
      shipto.tax-mandatory = IF tb_mandatory-tax:SCREEN-VALUE = "Yes" THEN TRUE ELSE FALSE .

    FIND CURRENT reftable NO-LOCK.
  END.
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
  {src/adm/template/snd-list.i "shipto"}
  {src/adm/template/snd-list.i "cust"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ship-zip V-table-Win 
PROCEDURE ship-zip :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*   DO WITH FRAME {&FRAME-NAME}:                                        */
/*     IF shipto.ship-zip:SCREEN-VALUE NE "" THEN                        */
/*     FIND FIRST nosweat.zipcode                                        */
/*         WHERE nosweat.zipcode.zipcode EQ shipto.ship-zip:SCREEN-VALUE */
/*         NO-LOCK NO-ERROR.                                             */
/*     IF AVAIL nosweat.zipcode THEN DO:                                 */
/*       shipto.ship-state:SCREEN-VALUE = nosweat.zipcode.state.         */
/*       IF shipto.ship-city:SCREEN-VALUE EQ "" THEN                     */
/*         shipto.ship-city:SCREEN-VALUE = nosweat.zipcode.city.         */
/*     END.                                                              */
/*   END.                                                                */
  DO WITH FRAME {&FRAME-NAME}:
    IF shipto.ship-zip:SCREEN-VALUE NE "" THEN
    FIND FIRST nosweat.zipcode
        WHERE nosweat.zipcode.zipcode EQ shipto.ship-zip:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL nosweat.zipcode THEN DO:
      shipto.ship-state:SCREEN-VALUE = nosweat.zipcode.state.
/*       IF shipto.ship-city:SCREEN-VALUE EQ "" THEN */
        shipto.ship-city:SCREEN-VALUE = nosweat.zipcode.city.
      ASSIGN
         shipto.carrier:SCREEN-VALUE = nosweat.zipcode.carrier 
         shipto.dest-code:SCREEN-VALUE = nosweat.zipcode.del-zone.
/*       DISPLAY shipto.ship-state:SCREEN-VALUE shipto.ship-city:SCREEN-VALUE shipto.carrier:SCREEN-VALUE shipto.dest-code:SCREEN-VALUE WITH FRAME {&FRAME-NAME}. */
    END.
  END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ship-zip-lookup V-table-Win 
PROCEDURE ship-zip-lookup :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF BUFFER b-zipcode FOR nosweat.zipcode.

  DO WITH FRAME {&FRAME-NAME}:
    IF shipto.ship-zip:SCREEN-VALUE NE "" THEN
    FIND FIRST nosweat.b-zipcode
        WHERE nosweat.b-zipcode.zipcode EQ shipto.ship-zip:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    IF AVAIL nosweat.b-zipcode THEN DO:
      shipto.ship-state:SCREEN-VALUE = nosweat.b-zipcode.state.
/*       IF shipto.ship-city:SCREEN-VALUE EQ "" THEN */
        shipto.ship-city:SCREEN-VALUE = nosweat.b-zipcode.city.
      ASSIGN
         shipto.carrier:SCREEN-VALUE = nosweat.b-zipcode.carrier 
         shipto.dest-code:SCREEN-VALUE = nosweat.b-zipcode.del-zone.
/*       DISPLAY shipto.ship-state:SCREEN-VALUE shipto.ship-city:SCREEN-VALUE shipto.carrier:SCREEN-VALUE shipto.dest-code:SCREEN-VALUE WITH FRAME {&FRAME-NAME}. */
    END.
  END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shipto-new-log V-table-Win 
PROCEDURE shipto-new-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR thisOne AS CHAR NO-UNDO.
DEFINE BUFFER buff-shipto FOR shipto .

 FIND CURRENT shipto NO-LOCK.
 DO I = 1 TO NUM-ENTRIES(v-cust-fmt):
     ASSIGN thisOne = ENTRY(i,v-cust-fmt).
     CREATE buff-shipto .
     ASSIGN buff-shipto.company = thisone.
     BUFFER-COPY shipto EXCEPT company  TO buff-shipto.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE shipto-update-log V-table-Win 
PROCEDURE shipto-update-log :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR thisOne AS CHAR NO-UNDO.
DEFINE BUFFER buff-shipto FOR shipto .

    FIND CURRENT shipto NO-LOCK NO-ERROR.

   DO I = 1 TO NUM-ENTRIES(v-cust-fmt):
     ASSIGN thisOne = ENTRY(i,v-cust-fmt).
     FIND FIRST buff-shipto WHERE buff-shipto.cust-no = shipto.cust-no 
                          AND buff-shipto.ship-id = shipto.ship-id 
                          AND buff-shipto.company = thisOne EXCLUSIVE-LOCK NO-ERROR.
     IF AVAIL buff-shipto THEN do:
     BUFFER-COPY shipto EXCEPT cust-no company ship-id TO buff-shipto.
     END.
     ELSE DO:
        CREATE buff-shipto .
        ASSIGN buff-shipto.company = thisone.
        BUFFER-COPY shipto EXCEPT company  TO buff-shipto.
     END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE update-shipto V-table-Win 
PROCEDURE update-shipto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-ship-id    LIKE shipto.ship-id    NO-UNDO.
  DEF INPUT PARAM ip-ship-name  LIKE shipto.ship-name  NO-UNDO.
  DEF INPUT PARAM ip-ship-addr1 LIKE shipto.ship-name  NO-UNDO.
  DEF INPUT PARAM ip-ship-addr2 LIKE shipto.ship-name  NO-UNDO.
  DEF INPUT PARAM ip-ship-city  LIKE shipto.ship-city  NO-UNDO.
  DEF INPUT PARAM ip-ship-state LIKE shipto.ship-state NO-UNDO.
  DEF INPUT PARAM ip-ship-zip   LIKE shipto.ship-zip   NO-UNDO.

  FIND CURRENT shipto.
  ASSIGN
   shipto.ship-id      = ip-ship-id
   shipto.ship-name    = ip-ship-name
   shipto.ship-addr[1] = ip-ship-addr1
   shipto.ship-addr[2] = ip-ship-addr2
   shipto.ship-city    = ip-ship-city
   shipto.ship-state   = IF ip-ship-state EQ "" THEN cust.state
                                                ELSE ip-ship-state
   shipto.ship-zip     = ip-ship-zip
   shipto.loc          = cust.loc
   shipto.carrier      = cust.carrier
   shipto.dest-code    = cust.del-zone.
  FIND CURRENT shipto NO-LOCK.

  RUN dispatch ("display-fields").

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE, "tableio-source", OUTPUT char-hdl).

  RUN set-buttons IN WIDGET-HANDLE(char-hdl) ("action-chosen").

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
    shipto.carrier:SCREEN-VALUE = CAPS(shipto.carrier:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST carrier
                    WHERE carrier.company EQ cocode
                      AND carrier.loc     EQ shipto.loc:SCREEN-VALUE
                      AND carrier.carrier EQ shipto.carrier:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO shipto.carrier.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-dest-code V-table-Win 
PROCEDURE valid-dest-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    shipto.dest-code:SCREEN-VALUE = CAPS(shipto.dest-code:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST carr-mtx
                    WHERE carr-mtx.company  EQ cocode
                      AND carr-mtx.loc      EQ shipto.loc:SCREEN-VALUE
                      AND carr-mtx.carrier  EQ shipto.carrier:SCREEN-VALUE
                      AND carr-mtx.del-zone EQ shipto.dest-code:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO shipto.dest-code.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc V-table-Win 
PROCEDURE valid-loc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    shipto.loc:SCREEN-VALUE = CAPS(shipto.loc:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST loc
                    WHERE loc.company EQ cocode
                      AND loc.loc     EQ shipto.loc:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO shipto.loc.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-loc-bin V-table-Win 
PROCEDURE valid-loc-bin :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    shipto.loc-bin:SCREEN-VALUE = CAPS(shipto.loc-bin:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST fg-bin
                    WHERE fg-bin.company EQ cocode
                      AND fg-bin.loc     EQ shipto.loc:SCREEN-VALUE
                      AND fg-bin.loc-bin EQ shipto.loc-bin:SCREEN-VALUE) THEN DO:
      MESSAGE "Bin does not exist in this warehouse..." VIEW-AS ALERT-BOX ERROR.
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
  DEF BUFFER b-shipto FOR shipto.
  DEF BUFFER b-cust   FOR cust.


  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    shipto.ship-id:SCREEN-VALUE = CAPS(shipto.ship-id:SCREEN-VALUE).

    IF CAN-FIND(FIRST b-shipto
                WHERE b-shipto.company       EQ cocode
                  AND b-shipto.cust-no       EQ shipto.cust-no
                  AND TRIM(b-shipto.ship-id) EQ shipto.ship-id:SCREEN-VALUE
                  AND ROWID(b-shipto)        NE ROWID(shipto)) OR
       shipto.ship-id:SCREEN-VALUE EQ ""                       THEN DO:
      IF shipto.ship-id:SCREEN-VALUE EQ "" THEN
        MESSAGE "ShipTo ID may not be blank..." VIEW-AS ALERT-BOX ERROR.
      ELSE
        MESSAGE "ShipTo ID already exists for this customer..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO shipto.ship-id.
      RETURN ERROR.
    END.

    IF shipto.bill:SCREEN-VALUE EQ "Yes" AND
       NOT CAN-FIND(FIRST b-cust
                    WHERE b-cust.company EQ cocode
                      AND b-cust.cust-no EQ shipto.ship-id:SCREEN-VALUE) THEN DO:
      MESSAGE "Billable ShipTo must be a valid customer..." VIEW-AS ALERT-BOX ERROR.
      shipto.bill:SCREEN-VALUE = "No".
      APPLY "entry" TO shipto.ship-id.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-ship-state V-table-Win 
PROCEDURE valid-ship-state :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    shipto.ship-state:SCREEN-VALUE = CAPS(shipto.ship-state:SCREEN-VALUE).

    IF NOT CAN-FIND(FIRST state
                    WHERE state.state EQ shipto.ship-state:SCREEN-VALUE) THEN DO:
      MESSAGE "Invalid entry, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO shipto.ship-state.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-sman V-table-Win 
PROCEDURE valid-sman :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {methods/lValidateError.i YES}
   IF shipto.spare-char-1:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN do:
    FIND FIRST sman
        WHERE sman.company EQ cocode
          AND sman.sman    EQ shipto.spare-char-1:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        NO-LOCK NO-ERROR.

    IF NOT AVAIL sman THEN DO:
       MESSAGE "Invalid Sales Rep. Try help." VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO shipto.spare-char-1.
       RETURN ERROR.
    END.
    ELSE fi_sname:SCREEN-VALUE = sman.sname.
   END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-tax-code V-table-Win 
PROCEDURE valid-tax-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  DO WITH FRAME {&FRAME-NAME}:
    shipto.tax-code:SCREEN-VALUE = CAPS(shipto.tax-code:SCREEN-VALUE).

    IF NOT AVAIL cust THEN
    FIND FIRST cust
        WHERE cust.company EQ shipto.company
          AND cust.cust-no EQ shipto.cust-no
        NO-LOCK NO-ERROR.

    IF ((AVAIL cust AND cust.SORT EQ "Y") or v-tax-mand)                    AND
       (shipto.tax-code:SCREEN-VALUE EQ "" OR
        NOT CAN-FIND(FIRST stax
                     WHERE stax.company   EQ shipto.company
                       AND stax.tax-group EQ shipto.tax-code:SCREEN-VALUE)) THEN DO:
      MESSAGE "Must enter a valid tax code, try help..." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO shipto.tax-code.
      RETURN ERROR.
    END.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSalesmanName V-table-Win 
FUNCTION getSalesmanName RETURNS CHARACTER
  (ipcSalesman AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Return name of salesman from salesman code
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-sman FOR sman.

    IF ipcSalesman NE "" THEN
        FIND FIRST bf-sman 
            WHERE bf-sman.company EQ cocode
              AND bf-sman.sman EQ ipcSalesman
            NO-LOCK NO-ERROR.
    IF AVAIL bf-sman THEN
        RETURN bf-sman.sname.   /* Function return value. */
    ELSE RETURN "".

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

