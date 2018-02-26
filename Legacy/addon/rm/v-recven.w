&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: addon/rm/v-recven.w

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
{custom/gcompany.i}
{custom/gloc.i}

{sys/inc/var.i NEW SHARED} 

ASSIGN
 cocode = g_company
 locode = g_loc.
DEF VAR v-po-no   AS INT NO-UNDO.
DEF VAR v-po-line AS INT NO-UNDO.
DEF VAR v-qty     AS INT NO-UNDO.

DEF TEMP-TABLE tt-mat NO-UNDO
    FIELD frm LIKE job-mat.frm
    FIELD qty LIKE job-mat.qty
    INDEX frm frm.

DEF TEMP-TABLE tt-po NO-UNDO
    FIELD po-no AS INT
    FIELD LINE AS INT
    FIELD tot-rec-qty AS DEC
    FIELD cons-uom AS CHAR
    FIELD overrun-qty AS DEC
    INDEX po po-no ASC LINE ASC.

DEFINE TEMP-TABLE w-po NO-UNDO
  FIELD i-no LIKE po-ordl.i-no
  FIELD i-name LIKE po-ordl.i-name
  FIELD over-pct LIKE po-ord.over-pct
  FIELD cost LIKE po-ordl.cost
  FIELD po-no LIKE po-ord.po-no
  FIELD b-num LIKE po-ordl.b-num
  FIELD cons-cost LIKE po-ordl.cons-cost
  FIELD cons-qty LIKE po-ordl.cons-qty
  FIELD cons-uom LIKE po-ordl.cons-uom
  FIELD job-no LIKE po-ordl.job-no
  FIELD job-no2 LIKE po-ordl.job-no2
  FIELD ord-no LIKE po-ordl.ord-no
  FIELD ord-qty LIKE po-ordl.ord-qty
  FIELD pr-uom LIKE po-ordl.pr-uom
  FIELD s-len LIKE po-ordl.s-len
  FIELD s-num LIKE po-ordl.s-num
  FIELD s-wid LIKE po-ordl.s-wid
  FIELD loc AS CHAR
  FIELD loc-bin LIKE item.loc-bin
  FIELD LINE AS INT
  FIELD rcpt-qty LIKE rm-rctd.qty
  FIELD tag-date AS DATE
  FIELD total-tags AS INT
  FIELD overrun-qty AS INT
  FIELD TYPE AS CHAR
  FIELD setup AS DEC
  FIELD add-setup AS LOG
  INDEX po IS PRIMARY po-no ASC.

DEF BUFFER b-company FOR company.

{windows/l-poitmw.i NEW}

find first loc where
     loc.company eq cocode AND
     loc.loc eq locode
     no-lock no-error.

DEF VAR v-bin AS CHAR NO-UNDO.
DEF VAR v-loadtag AS CHAR NO-UNDO INIT "ASI". /* sys ctrl option */
DEFINE VARIABLE iPalletMark AS INTEGER NO-UNDO.
FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ g_company
       AND sys-ctrl.name    EQ "RMWHSBIN" NO-ERROR.
IF NOT AVAIL sys-ctrl THEN
DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = g_company
   sys-ctrl.name     = "RMWHSBIN"
   sys-ctrl.descrip  = "Default Location for RM Warehouse / Bin?"
   sys-ctrl.char-fld = "RMITEM".
  FIND CURRENT sys-ctrl NO-LOCK.
END.
v-bin = sys-ctrl.char-fld.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ g_company
       AND sys-ctrl.name    EQ "LOADTAG" NO-ERROR.

IF NOT AVAIL sys-ctrl THEN
   DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN
       sys-ctrl.company  = g_company
       sys-ctrl.name     = "LOADTAG"
       sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
       sys-ctrl.char-fld = "ASI".
      MESSAGE "System control record NOT found. Please enter the load tag option"
              UPDATE sys-ctrl.char-fld.
      FIND CURRENT sys-ctrl NO-LOCK.
  END.

ASSIGN
   v-loadtag = sys-ctrl.char-fld.
DO TRANSACTION:
   {sys/inc/sspostvt.i}
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

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS scr-vend-tag begin_po-no scr-po-line scr-qty ~
scr-uom btn_receive lv-search Btn_Clear_Find-2 RECT-9 
&Scoped-Define DISPLAYED-OBJECTS scr-vend-tag begin_po-no scr-po-line ~
scr-qty scr-uom scr-item-no scr-item-name lv-search 

/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,List-3,List-4,List-5,List-6      */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD checkWhsBin V-table-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
  (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Clear_Find-2 
     LABEL "&Clear Find" 
     SIZE 13 BY 1
     FONT 4.

DEFINE BUTTON btn_receive 
     LABEL "Receive Tag" 
     SIZE 15.6 BY 1.14.

DEFINE VARIABLE begin_po-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 
     LABEL "P.O.#" 
     VIEW-AS FILL-IN 
     SIZE 13.4 BY .95
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE lv-search AS CHARACTER FORMAT "X(256)":U 
     LABEL "Search for Vendor Tag#" 
     VIEW-AS FILL-IN 
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE scr-item-name AS CHARACTER FORMAT "X(30)":U 
     LABEL "Item Name" 
     VIEW-AS FILL-IN 
     SIZE 40 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE scr-item-no AS CHARACTER FORMAT "X(256)":U 
     LABEL "Item #" 
     VIEW-AS FILL-IN 
     SIZE 17.6 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE scr-po-line AS INTEGER FORMAT "ZZ9":U INITIAL 0 
     LABEL "P.O. Line #" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .95
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE scr-qty AS DECIMAL FORMAT "->,>>>,>>9.9<<":U INITIAL 0 
     LABEL "Qty" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .95
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE scr-uom AS CHARACTER FORMAT "X(4)":U 
     LABEL "U/M" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE scr-vend-tag AS CHARACTER FORMAT "X(20)":U 
     LABEL "Vendor Tag#" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 143.4 BY .1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     scr-vend-tag AT ROW 1.19 COL 14 COLON-ALIGNED WIDGET-ID 6
     begin_po-no AT ROW 1.19 COL 54.4 COLON-ALIGNED HELP
          "Enter Beginning PO Number" WIDGET-ID 8
     scr-po-line AT ROW 1.19 COL 80 COLON-ALIGNED HELP
          "Enter Beginning PO Number" WIDGET-ID 10
     scr-qty AT ROW 1.19 COL 91 COLON-ALIGNED HELP
          "Enter Beginning PO Number" WIDGET-ID 12
     scr-uom AT ROW 1.24 COL 115 COLON-ALIGNED WIDGET-ID 26
     btn_receive AT ROW 1.19 COL 127.2 WIDGET-ID 4
     scr-item-no AT ROW 2.38 COL 60.4 COLON-ALIGNED WIDGET-ID 22
     scr-item-name AT ROW 2.38 COL 91 COLON-ALIGNED WIDGET-ID 20
     lv-search AT ROW 4 COL 24 COLON-ALIGNED HELP
          "Enter Auto Find Value" WIDGET-ID 16
     Btn_Clear_Find-2 AT ROW 4.05 COL 66 HELP
          "CLEAR AUTO FIND Value" WIDGET-ID 14
     RECT-9 AT ROW 3.57 COL 1 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         HEIGHT             = 4.19
         WIDTH              = 143.4.
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

ASSIGN 
       begin_po-no:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

/* SETTINGS FOR FILL-IN scr-item-name IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN scr-item-no IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       scr-po-line:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

ASSIGN 
       scr-qty:PRIVATE-DATA IN FRAME F-Main     = 
                "parm".

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

&Scoped-define SELF-NAME begin_po-no
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no V-table-Win
ON HELP OF begin_po-no IN FRAME F-Main /* P.O.# */
DO:
   DEF VAR char-val AS CHAR no-undo.

   DO WITH FRAME {&FRAME-NAME}:
   
      run windows/l-poordl.w (cocode,begin_po-no:screen-value, output char-val).
      if char-val <> "" THEN
      DO:
         assign begin_po-no:screen-value = ENTRY(1,char-val)
                scr-item-no:screen-value = entry(2,char-val)
                scr-item-name:screen-value = entry(3,char-val)
                scr-po-line:SCREEN-VALUE = ENTRY(6,char-val).

         FIND FIRST po-ordl WHERE
              po-ordl.company EQ cocode AND
              po-ordl.po-no EQ INT(begin_po-no:screen-value) AND
              po-ordl.LINE EQ INT(scr-po-line:SCREEN-VALUE)
              NO-LOCK NO-ERROR.

         IF AVAIL po-ordl THEN
         DO:
            scr-uom:SCREEN-VALUE = po-ordl.pr-uom.
            RELEASE po-ordl.
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL begin_po-no V-table-Win
ON LEAVE OF begin_po-no IN FRAME F-Main /* P.O.# */
DO:
   IF begin_po-no:SCREEN-VALUE NE STRING(begin_po-no) AND
      begin_po-no:SCREEN-VALUE NE "0" THEN
      DO:
         FIND po-ordl WHERE /*find unique line item*/
              po-ordl.company EQ cocode AND
              po-ordl.po-no EQ INT(begin_po-no:SCREEN-VALUE) AND
              po-ordl.stat NE "C"
              NO-LOCK NO-ERROR.
        
         IF AVAIL po-ordl THEN
         DO:
           FIND FIRST ITEM WHERE ITEM.company = cocode AND
                ITEM.i-no = po-ordl.i-no
                NO-LOCK NO-ERROR.
           IF AVAIL ITEM THEN
               scr-uom:SCREEN-VALUE = ITEM.cons-uom.
            ASSIGN
               scr-item-no:SCREEN-VALUE = po-ordl.i-no
               scr-item-name:SCREEN-VALUE = po-ordl.i-name
               scr-po-line:SCREEN-VALUE = STRING(po-ordl.LINE).

            RELEASE po-ordl.
         END.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Clear_Find-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Clear_Find-2 V-table-Win
ON CHOOSE OF Btn_Clear_Find-2 IN FRAME F-Main /* Clear Find */
DO:
    DEF VAR char-hdl AS cha NO-UNDO.
    lv-search = "".
    DISPLAY lv-search WITH FRAME {&FRAME-NAME}.
    /*APPLY "leave" TO lv-search.*/
    RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"srch-target",OUTPUT char-hdl).
    RUN do-search IN WIDGET-HANDLE(char-hdl) (lv-search).
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_receive
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_receive V-table-Win
ON CHOOSE OF btn_receive IN FRAME F-Main /* Receive Tag */
DO:
   DEF VAR op-error AS LOG NO-UNDO.
   DEF VAR op-po-no AS INT NO-UNDO.
   DEF VAR op-po-line AS INT NO-UNDO.
   DEF VAR op-qty AS INT NO-UNDO.
   DEF VAR lv-uom-list AS cha INIT ["EA,TON,MSF,MSH,LB,LF"] NO-UNDO.
   DEF VAR lv-uom-help AS CHAR NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
      
      ASSIGN begin_po-no scr-po-line scr-qty scr-qty scr-uom
             scr-vend-tag scr-item-no.

      IF scr-vend-tag EQ "" THEN
      DO:
         op-error = YES.
         MESSAGE "Vendor Tag# cannot be blank."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO scr-vend-tag.
         RETURN NO-APPLY.
      END.

      IF NOT CAN-FIND(FIRST po-ord WHERE
         po-ord.company EQ cocode AND
         po-ord.po-no EQ begin_po-no) THEN
         DO:
            op-error = YES.
            MESSAGE "Invalid Purchase Order #."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            APPLY "ENTRY" TO begin_po-no.
            RETURN NO-APPLY.
         END.

      FIND FIRST po-ordl WHERE
           po-ordl.company EQ cocode AND
           po-ordl.po-no EQ begin_po-no AND
           po-ordl.LINE EQ scr-po-line
           NO-LOCK NO-ERROR.
     
      IF NOT AVAIL po-ordl THEN
      DO:
         op-error = YES.
         MESSAGE "Invalid Purchase Order Line #."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO scr-po-line.
         RETURN NO-APPLY.
      END.
 /*     ELSE IF po-ordl.stat EQ "C" THEN
      DO:
         op-error = YES.
         MESSAGE "Sorry, PO Line is Closed."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO scr-po-line.
         RETURN NO-APPLY.
      END.*/
      ELSE IF po-ordl.item-type EQ NO THEN
      DO:
         op-error = YES.
         MESSAGE "Only Raw Materials can be Received."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO scr-vend-tag.
         RETURN NO-APPLY.
      END.
     
      IF scr-qty EQ 0 THEN
      DO:
         op-error = YES.
         MESSAGE "Qty. cannot be 0."
             VIEW-AS ALERT-BOX ERROR BUTTONS OK.
         APPLY "ENTRY" TO scr-qty.
         RETURN NO-APPLY.
      END.
  
      FIND FIRST ITEM WHERE
           item.company EQ cocode AND
           item.i-no    EQ scr-item-no
           NO-LOCK NO-ERROR.

       IF AVAIL item THEN
          RUN sys/ref/uom-rm.p (INPUT item.mat-type, OUTPUT lv-uom-list).

       lv-uom-help = "Must enter one of the following as the UOM: " + lv-uom-list.

       IF INDEX(lv-uom-list,scr-uom) LE 0 THEN DO:
          MESSAGE TRIM(lv-uom-help) + "."
                  VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY "entry" TO scr-uom.
          RETURN NO-APPLY.
       END.

       IF NOT op-error THEN
       DO:
          RUN process-tag-proc.
          ASSIGN
             scr-vend-tag:SCREEN-VALUE = ""
             begin_po-no:SCREEN-VALUE = ""
             scr-po-line:SCREEN-VALUE = "0"
             scr-qty:SCREEN-VALUE = "0"
             scr-item-no:SCREEN-VALUE = ""
             scr-item-name:SCREEN-VALUE = ""
             scr-uom:SCREEN-VALUE = "".
       END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME lv-search
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search V-table-Win
ON HELP OF lv-search IN FRAME F-Main /* Search for Vendor Tag# */
DO:
    DEF VAR char-val AS cha NO-UNDO.
    DEF VAR rec-val AS RECID NO-UNDO.
    DEF VAR char-hdl AS cha NO-UNDO.

    run addon/rm/l-tagrct.w (g_company,YES,focus:screen-value,output char-val,OUTPUT rec-val).
    if char-val <> "" then do :
       FOCUS:SCREEN-VALUE = ENTRY(1,char-val).
       ASSIGN lv-search.
       /*APPLY "leave" TO lv-search.*/
       RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"srch-target",OUTPUT char-hdl).
       RUN do-search IN WIDGET-HANDLE(char-hdl) (lv-search).
       RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL lv-search V-table-Win
ON LEAVE OF lv-search IN FRAME F-Main /* Search for Vendor Tag# */
or return of lv-search
DO:
  IF LASTKEY = -1 THEN RETURN.

  DEF VAR char-hdl AS cha NO-UNDO.

  assign lv-search.

  RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"srch-target",OUTPUT char-hdl).
  RUN do-search IN WIDGET-HANDLE(char-hdl) (lv-search).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-po-line
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-po-line V-table-Win
ON HELP OF scr-po-line IN FRAME F-Main /* P.O. Line # */
DO:
   DEF VAR lv-rowid AS ROWID NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
   
      RUN windows/l-poitmw.w (YES, cocode, begin_po-no:SCREEN-VALUE,
                              YES, "", "", OUTPUT lv-rowid).

      FIND po-ordl WHERE
           ROWID(po-ordl) EQ lv-rowid
           NO-LOCK NO-ERROR.

      IF AVAIL po-ordl THEN
         scr-po-line:SCREEN-VALUE = STRING(po-ordl.LINE).

      IF scr-po-line:SCREEN-VALUE NE "0" THEN
      DO:
         ASSIGN begin_po-no
                scr-po-line
                scr-item-no:SCREEN-VALUE = ""
                scr-item-name:SCREEN-VALUE = "".
      
         IF NOT AVAIL po-ordl THEN
            FIND FIRST po-ordl WHERE
                 po-ordl.company EQ cocode AND
                 po-ordl.po-no EQ begin_po-no AND
                 po-ordl.LINE EQ scr-po-line
                 NO-LOCK NO-ERROR.
      
         IF AVAIL po-ordl THEN
         DO:
            ASSIGN
               scr-item-no:SCREEN-VALUE = po-ordl.i-no
               scr-item-name:SCREEN-VALUE = po-ordl.i-name
               scr-uom:SCREEN-VALUE = po-ordl.pr-uom.
      
            RELEASE po-ordl.
         END.
      END.
         
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-po-line V-table-Win
ON LEAVE OF scr-po-line IN FRAME F-Main /* P.O. Line # */
DO:
   IF LASTKEY NE -1 AND begin_po-no:SCREEN-VALUE NE "0" AND
      scr-po-line:SCREEN-VALUE NE "0" THEN
      DO:
         ASSIGN begin_po-no
                scr-po-line
                scr-item-no:SCREEN-VALUE = ""
                scr-item-name:SCREEN-VALUE = "".
  
         FIND FIRST po-ordl WHERE
              po-ordl.company EQ cocode AND
              po-ordl.po-no EQ begin_po-no AND
              po-ordl.LINE EQ scr-po-line
              NO-LOCK NO-ERROR.
  
         IF AVAIL po-ordl THEN
         DO:
           FIND FIRST ITEM WHERE ITEM.company = cocode AND
                ITEM.i-no = po-ordl.i-no
                NO-LOCK NO-ERROR.
           IF AVAIL ITEM THEN
               scr-uom:SCREEN-VALUE = ITEM.cons-uom.
            ASSIGN
               scr-item-no:SCREEN-VALUE = po-ordl.i-no
               scr-item-name:SCREEN-VALUE = po-ordl.i-name.
  
            RELEASE po-ordl.
         END.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME scr-vend-tag
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL scr-vend-tag V-table-Win
ON LEAVE OF scr-vend-tag IN FRAME F-Main /* Vendor Tag# */
DO:
   DEFINE VARIABLE lEdDocFound AS LOGICAL NO-UNDO.
   
   DO WITH FRAME {&FRAME-NAME}:

      IF LASTKEY NE -1 THEN
      DO:
         ASSIGN
            scr-vend-tag
            ERROR-STATUS:ERROR = NO
            begin_po-no:SCREEN-VALUE = "0"
            scr-po-line:SCREEN-VALUE = "0"
            scr-qty:SCREEN-VALUE = "0"
            scr-item-no:SCREEN-VALUE = ""
            scr-item-name:SCREEN-VALUE = ""
            scr-uom:SCREEN-VALUE = "".
         
         IF scr-vend-tag NE "" AND
            CAN-FIND(FIRST loadtag WHERE
            loadtag.company EQ cocode AND
            loadtag.item-type EQ YES AND
            loadtag.is-case-tag EQ NO AND
            loadtag.misc-char[1] EQ scr-vend-tag) THEN
            DO:
               MESSAGE "This Vendor Tag Number Has Already Been Used." skip
                  "Please Enter A Unique Tag Number." 
                  VIEW-AS ALERT-BOX ERROR.
               RETURN NO-APPLY.
            END.
            
         RUN edDocSearch (OUTPUT lEdDocFound).
         IF lEdDocFound THEN DO:
             RUN poSearch.
         END. 
         ELSE DO:              
             v-po-no = INT(SUBSTR(scr-vend-tag,1,6)) NO-ERROR.
             
             IF NOT ERROR-STATUS:ERROR THEN
             DO:
                IF NOT CAN-FIND(FIRST po-ord WHERE
                   po-ord.company EQ cocode AND
                   po-ord.po-no EQ v-po-no) THEN
                   LEAVE.
            
                begin_po-no:SCREEN-VALUE = STRING(v-po-no).
             END.
             ELSE
                LEAVE.
                
             v-po-line = INT(SUBSTR(scr-vend-tag,7,3)) NO-ERROR.
             
             IF NOT ERROR-STATUS:ERROR THEN
                scr-po-line:SCREEN-VALUE = STRING(v-po-line).
             
             v-qty = INT(SUBSTR(scr-vend-tag,10,5)) NO-ERROR.
             
             IF NOT ERROR-STATUS:ERROR THEN
                scr-qty:SCREEN-VALUE = STRING(v-qty).
            
                RUN poSearch.
         END.
      END.
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

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-qty-proc V-table-Win 
PROCEDURE convert-qty-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like v-len no-undo.
  def var v-dep like v-len no-undo. 
  def var v-bwt like v-len no-undo.
  def var lv-out-qty LIKE rm-rctd.qty no-undo.
  DEF VAR lv-qty-uom AS CHAR NO-UNDO.

  find FIRST item where
       item.company eq cocode AND
       item.i-no  eq scr-item-no
       NO-LOCK no-error.

  IF NOT AVAIL item THEN LEAVE.

  IF item.cons-uom EQ "" THEN
  DO:
     find FIRST item where
          item.company eq cocode AND
          item.i-no  eq scr-item-no
          no-error.
      item.cons-uom = scr-uom.
      FIND CURRENT ITEM NO-LOCK.
  END.

  assign
   lv-qty-uom  = item.cons-uom
   v-dep       = item.s-dep
   v-len = po-ordl.s-len
   v-wid = po-ordl.s-wid
   v-bwt = 0.

  {rm/pol-dims.i}

  IF scr-uom EQ lv-qty-uom THEN
     lv-out-qty = scr-qty.
  ELSE
     IF ITEM.mat-type <> "P" THEN
        run custom/convquom.p (cocode,
                               scr-uom,
                               lv-qty-uom,
                               v-bwt,
                               v-len,
                               input v-wid,
                               input v-dep,
                               scr-qty,
                               output lv-out-qty).
     ELSE
        run custom/convquom.p (cocode,
                               item.cons-uom ,
                               lv-qty-uom,
                               v-bwt,
                               v-len,
                               input v-wid,
                               input v-dep,
                               scr-qty,
                               output lv-out-qty).

  ASSIGN
     w-po.rcpt-qty = lv-out-qty
     w-po.pr-uom = lv-qty-uom.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE convert-vend-comp-curr V-table-Win 
PROCEDURE convert-vend-comp-curr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DEC DECIMALS 4 NO-UNDO.
   
   FIND FIRST vend WHERE
        vend.company EQ po-ord.company AND
        vend.vend-no EQ po-ord.vend-no
        NO-LOCK NO-ERROR.
  
   IF AVAIL vend THEN
   DO:
      FIND FIRST b-company WHERE
           b-company.company EQ cocode
           NO-LOCK.
  
      IF vend.curr-code NE b-company.curr-code THEN
      DO:
         FIND FIRST currency WHERE
              currency.company EQ po-ord.company AND
              currency.c-code EQ vend.curr-code
              NO-LOCK NO-ERROR.
  
         IF AVAIL currency THEN
         DO:
            ip-cost = ip-cost * currency.ex-rate.
  
            RELEASE currency.
         END.
      END.
  
      RELEASE b-company.
      RELEASE vend.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag-proc V-table-Win 
PROCEDURE create-loadtag-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR i AS INT NO-UNDO.
   DEF VAR tagNo AS CHAR NO-UNDO.
   DEF VAR ld AS DEC NO-UNDO.
   DEF VAR ipTagNo AS INT INIT 1 NO-UNDO.
   DEF VAR char-hdl AS CHAR NO-UNDO.

   DO WHILE TRUE:
      tagNo = STRING(w-po.po-no,'9999999') + STRING(w-po.line,'999') + STRING(ipTagNo,'9999999').
      IF NOT CAN-FIND(FIRST loadtag
                      WHERE loadtag.company EQ cocode
                        AND loadtag.item-type EQ YES
                        AND loadtag.tag-no EQ tagNo) THEN LEAVE.
      ipTagNo = ipTagNo + 1.
   END. /* do while */
  
   CREATE loadtag.
   ASSIGN
    loadtag.company      = cocode
    loadtag.tag-no       = tagNo
    loadtag.item-type    = YES
    loadtag.po-no        = w-po.po-no
    loadtag.line         = w-po.line
    loadtag.job-no       = w-po.job-no
    loadtag.job-no2      = w-po.job-no2
    loadtag.form-no      = w-po.s-num
    loadtag.blank-no     = w-po.b-num
    loadtag.ord-no       = w-po.ord-no
    loadtag.i-no         = CAPS(w-po.i-no)
    loadtag.i-name       = w-po.i-name
    loadtag.qty          = w-po.ord-qty
    loadtag.qty-case     = w-po.rcpt-qty
    loadtag.case-bundle  = 1
    loadtag.pallet-count = w-po.rcpt-qty
    loadtag.loc          = w-po.loc
    loadtag.loc-bin      = w-po.loc-bin
    loadtag.tot-cases    = 0
    loadtag.sts          = "Printed"
    loadtag.tag-date     = TODAY
    loadtag.tag-time     = TIME
    loadtag.misc-char[1] = scr-vend-tag.
   
   FIND CURRENT loadtag NO-LOCK NO-ERROR.

   RUN get-link-handle IN adm-broker-hdl (THIS-PROCEDURE,"srch-target",OUTPUT char-hdl).
   RUN create-rec-from-vend-tag IN WIDGET-HANDLE(char-hdl) (INPUT tagNo,
                                                            INPUT scr-vend-tag,
                                                            INPUT w-po.cons-uom,
                                                            INPUT w-po.cost,
                                                            INPUT w-po.pr-uom,
                                                            INPUT w-po.TYPE,
                                                            INPUT w-po.setup,
                                                            INPUT w-po.rcpt-qty,
                                                            INPUT w-po.add-setup).
   
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE edDocSearch V-table-Win
PROCEDURE edDocSearch:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplFound AS LOGICAL NO-UNDO.
IF CAN-FIND (FIRST EDCode NO-LOCK WHERE edcode.setid = "856"
    AND edcode.direction EQ "I") THEN 
DO WITH FRAME {&FRAME-NAME}:
    iPalletMark = INTEGER(scr-vend-tag) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN 
        FIND FIRST edShLine NO-LOCK 
            WHERE edShLine.pallet-mark = int(scr-vend-tag)  
            NO-ERROR.
    IF AVAILABLE  edShLine THEN 
    DO: 
        ASSIGN  v-po-no = INTEGER(edShLine.Cust-po) NO-ERROR. 
        IF NOT ERROR-STATUS:ERROR THEN 
        DO:
            IF NOT CAN-FIND(FIRST po-ord WHERE
                po-ord.company EQ cocode AND
                po-ord.po-no EQ v-po-no) THEN 
            DO:
                ASSIGN 
                    begin_po-no:SCREEN-VALUE = STRING(v-po-no)                       
                    scr-qty:SCREEN-VALUE     = STRING(edShLine.Tot-cartons)
                    scr-po-line:SCREEN-VALUE = EDSHLine.cust-po-line
                    v-po-line                = integer(EDSHLine.cust-po-line)
                    .
            END.
        END.
    END.                      
END.
IF AVAILABLE EDSHLine THEN 
  oplFound = TRUE.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entry-vend-tag-proc V-table-Win 
PROCEDURE entry-vend-tag-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   APPLY "ENTRY" TO scr-vend-tag IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE poSearch V-table-Win
PROCEDURE poSearch:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    IF begin_po-no:SCREEN-VALUE in frame {&frame-name} NE "0" AND
        scr-po-line:SCREEN-VALUE in frame {&frame-name} NE "0" THEN
    DO with frame {&frame-name}:
        ASSIGN begin_po-no scr-po-line.
        
        FIND FIRST po-ordl WHERE
            po-ordl.company EQ cocode AND
            po-ordl.po-no EQ begin_po-no AND
            po-ordl.LINE EQ scr-po-line
            NO-LOCK NO-ERROR.
        
        IF AVAIL po-ordl THEN
        DO:
            FIND FIRST ITEM WHERE ITEM.company = cocode AND
                ITEM.i-no = po-ordl.i-no
                NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN
                scr-uom:SCREEN-VALUE = ITEM.cons-uom.
            ASSIGN
                scr-item-no:SCREEN-VALUE   = po-ordl.i-no
                scr-item-name:SCREEN-VALUE = po-ordl.i-name
                .
        
            RELEASE po-ordl.
     
            IF NOT ERROR-STATUS:ERROR THEN
            DO:
                IF SSPostFGVT-log OR SSPostFGVT-log EQ ? THEN 
                DO:
                    APPLY "CHOOSE" TO btn_receive IN FRAME {&FRAME-NAME}.
                /*ASSIGN scr-vend-tag:SCREEN-VALUE = */
                END.
                ELSE APPLY "ENTRY" TO btn_receive IN FRAME {&FRAME-NAME}.
                     
                RETURN NO-APPLY.
            END.
        END.
    END. /* Do */

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-tag-proc V-table-Win 
PROCEDURE process-tag-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-qty AS DEC NO-UNDO.
   DEF VAR v-overrun AS DEC NO-UNDO.
   DEF VAR choice2 AS LOG INIT YES NO-UNDO.

   FIND FIRST po-ordl WHERE
        po-ordl.company EQ cocode AND
        po-ordl.po-no EQ begin_po-no AND
        po-ordl.LINE EQ scr-po-line
        NO-LOCK NO-ERROR.

   IF NOT AVAIL po-ordl THEN
      LEAVE.

   EMPTY TEMP-TABLE w-po.
  
   FIND FIRST po-ord WHERE
        po-ord.company EQ cocode AND
        po-ord.po-no EQ po-ordl.po-no
        NO-LOCK.

   CREATE w-po.
   ASSIGN
     w-po.b-num = po-ordl.b-num
     w-po.cons-cost = po-ordl.cons-cost
     w-po.cons-qty = po-ordl.cons-qty
     w-po.cons-uom = po-ordl.cons-uom
     w-po.LINE     = po-ordl.LINE
     w-po.i-name = po-ordl.i-name
     w-po.i-no = po-ordl.i-no
     w-po.job-no = po-ordl.job-no
     w-po.job-no2 = po-ordl.job-no2
     w-po.ord-no = po-ordl.ord-no
     w-po.ord-qty = po-ordl.ord-qty
     w-po.over-pct = po-ord.over-pct
     w-po.po-no = po-ord.po-no
     w-po.s-len = IF po-ordl.pr-qty-uom EQ "ROLL" THEN 12 ELSE po-ordl.s-len
     w-po.s-num = po-ordl.s-num
     w-po.s-wid = po-ordl.s-wid
     w-po.tag-date = TODAY
     w-po.total-tags = 1
     w-po.setup = po-ordl.setup
     w-po.TYPE = po-ord.TYPE.
     
   RUN convert-qty-proc.

   def var v-len like po-ordl.s-len no-undo.
   def var v-wid like po-ordl.s-len no-undo.
   def var v-dep like po-ordl.s-len no-undo. 
   def var v-bwt like po-ordl.s-len no-undo.
   def var lv-out-qty LIKE rm-rctd.qty no-undo.
   DEF VAR lv-qty-uom LIKE rm-rctd.pur-uom NO-UNDO.

   FIND FIRST ITEM where
        item.company eq po-ordl.company AND
        item.i-no eq po-ordl.i-no
        use-index i-no
        NO-LOCK no-error.

   assign
      lv-qty-uom  = po-ordl.pr-qty-uom
      v-len = po-ordl.s-len
      v-wid = po-ordl.s-wid
      v-bwt = 0.

   IF AVAIL ITEM THEN
   DO:
      {rm/pol-dims.i}
   END.

   IF w-po.cons-uom EQ lv-qty-uom THEN
      lv-out-qty = w-po.rcpt-qty.
   ELSE
      run custom/convquom.p (INPUT po-ordl.company,
                           INPUT w-po.cons-uom,
                           INPUT lv-qty-uom,
                           INPUT v-bwt,
                           INPUT v-len,
                           input v-wid,
                           input v-dep,
                           INPUT w-po.rcpt-qty,
                           output lv-out-qty).

   FIND FIRST po-ord WHERE
        po-ord.company EQ po-ordl.company AND
        po-ord.po-no EQ po-ordl.po-no
        NO-LOCK NO-ERROR.

   IF lv-out-qty LT po-ordl.ord-qty THEN
      w-po.cost = po-ordl.cost +
               (po-ordl.setup /
               ((po-ordl.t-cost - po-ordl.setup) / po-ordl.cost)).
   ELSE
      ASSIGN
         w-po.cost = po-ordl.cost
         w-po.add-setup = IF AVAIL po-ord AND po-ord.type NE "S" THEN YES
                          ELSE NO.
   w-po.pr-uom = po-ordl.pr-uom.

   RUN rm/getpocst.p (BUFFER po-ordl, w-po.pr-uom, INPUT-OUTPUT w-po.cost).

   RUN convert-vend-comp-curr(INPUT-OUTPUT w-po.cost).
   RUN convert-vend-comp-curr(INPUT-OUTPUT w-po.setup).
   RUN convert-vend-comp-curr(INPUT-OUTPUT w-po.cons-cost).
   
   FIND FIRST item WHERE
        item.company EQ cocode AND
        item.i-no EQ po-ordl.i-no
        NO-LOCK NO-ERROR.
   
   IF AVAIL item THEN
      ASSIGN
         w-po.loc = item.loc
         w-po.loc-bin = item.loc-bin
         w-po.cons-uom = item.cons-uom.
   
   ASSIGN
      v-overrun  = IF AVAIL po-ordl THEN po-ordl.over-pct
                   ELSE IF AVAIL po-ord  THEN po-ord.over-pct
                   ELSE IF AVAIL vend    THEN vend.over-pct
                   ELSE 0
      v-qty = po-ordl.ord-qty.
   
   IF w-po.cons-uom NE po-ordl.pr-qty-uom THEN
      RUN sys/ref/convquom.p(po-ordl.pr-qty-uom,
                             w-po.cons-uom, IF AVAIL ITEM THEN ITEM.basis-w ELSE 0,
                             w-po.s-len,  w-po.s-wid, IF AVAIL ITEM THEN item.s-dep ELSE 0,
                             v-qty, OUTPUT v-qty).
   
   w-po.overrun-qty = v-qty + (v-qty * (v-overrun / 100)).
   
   IF NOT checkWhsBin(cocode,w-po.loc,w-po.loc-bin) THEN
   DO:

     IF v-bin NE 'RMITEM' THEN
        ASSIGN
           w-po.loc = SUBSTR(v-bin,1,5)
           w-po.loc-bin = SUBSTR(v-bin,6).

     IF NOT checkWhsBin(cocode,w-po.loc,w-po.loc-bin) THEN
     DO:
        FIND FIRST rm-bin WHERE
             rm-bin.company EQ cocode AND
             rm-bin.loc EQ locode AND
             rm-bin.i-no EQ '' AND
             rm-bin.loc-bin NE ''
             NO-LOCK NO-ERROR.
       
        ASSIGN
          w-po.loc = IF AVAILABLE loc THEN loc.loc ELSE ''
          w-po.loc-bin = IF AVAILABLE rm-bin THEN rm-bin.loc-bin ELSE ''.
     END.
   END.
   
   IF NOT CAN-DO("SSLABEL,CentBox",v-loadtag) THEN
      w-po.total-tags = w-po.total-tags + 1.

   RUN tot-rec-qty-proc.

   IF CAN-FIND(FIRST tt-po WHERE
      tt-po.tot-rec-qty GT tt-po.overrun-qty) THEN
      MESSAGE "Receipt Qty Exceeds P.O Qty + Allowed Overrun%,  Continue?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE choice2.

   IF choice2 THEN
      RUN create-loadtag-proc.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tot-rec-qty-proc V-table-Win 
PROCEDURE tot-rec-qty-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF VAR v-qty-2 LIKE rm-rctd.qty NO-UNDO.

   EMPTY TEMP-TABLE tt-po.

   FIND FIRST tt-po WHERE
        tt-po.po-no EQ w-po.po-no AND
        tt-po.LINE  EQ w-po.LINE
        NO-ERROR.

   IF NOT AVAIL tt-po THEN
   DO:
      FIND FIRST po-ordl WHERE
           po-ordl.company EQ cocode AND
           po-ordl.po-no EQ w-po.po-no AND
           po-ordl.LINE EQ w-po.LINE
           NO-LOCK NO-ERROR.

      CREATE tt-po.
      ASSIGN tt-po.po-no = w-po.po-no
             tt-po.LINE  = w-po.LINE
             tt-po.cons-uom = w-po.cons-uom
             tt-po.tot-rec-qty = IF AVAIL po-ordl THEN po-ordl.t-rec-qty
                                 ELSE 0
             tt-po.overrun-qty = w-po.overrun-qty.

      IF AVAIL po-ordl AND w-po.cons-uom NE po-ordl.cons-uom THEN
      DO:
         FIND FIRST ITEM WHERE 
              ITEM.company EQ cocode AND
              ITEM.i-no EQ w-po.i-no
              NO-LOCK NO-ERROR.

         RUN sys/ref/convquom.p(po-ordl.cons-uom,
                                w-po.cons-uom, IF AVAIL ITEM THEN ITEM.basis-w ELSE 0,
                                w-po.s-len, w-po.s-wid, IF AVAIL ITEM THEN item.s-dep ELSE 0,
                                tt-po.tot-rec-qty, OUTPUT tt-po.tot-rec-qty).
      END.
   END.

   v-qty-2 = w-po.rcpt-qty * w-po.total-tags.

   IF w-po.cons-uom NE tt-po.cons-uom THEN
   DO:
      IF w-po.cons-uom NE tt-po.cons-uom THEN
      DO:
         FIND FIRST ITEM WHERE 
              ITEM.company EQ cocode AND
              ITEM.i-no EQ w-po.i-no
              NO-LOCK NO-ERROR.

         RUN sys/ref/convquom.p(w-po.cons-uom,
                                tt-po.cons-uom, IF AVAIL ITEM THEN ITEM.basis-w ELSE 0,
                                w-po.s-len, w-po.s-wid, IF AVAIL ITEM THEN item.s-dep ELSE 0,
                                v-qty-2, OUTPUT v-qty-2).
      END.
   END.

   tt-po.tot-rec-qty = tt-po.tot-rec-qty + v-qty-2.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION checkWhsBin V-table-Win 
FUNCTION checkWhsBin RETURNS LOGICAL
  (ipCompany AS CHARACTER,ipLoc AS CHARACTER,ipLocBin AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN CAN-FIND(FIRST loc
                  WHERE loc.company EQ ipCompany
                    AND loc.loc EQ ipLoc) AND
         CAN-FIND(FIRST rm-bin
                  WHERE rm-bin.company EQ ipCompany
                    AND rm-bin.loc EQ ipLoc
                    AND rm-bin.i-no EQ ''
                    AND rm-bin.loc-bin EQ ipLocBin).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

