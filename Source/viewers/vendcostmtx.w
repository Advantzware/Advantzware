&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS V-table-Win 
/*------------------------------------------------------------------------

  File: viewers/vendcostmtx.w

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
&Scoped-define proc-enable proc-enable 
&SCOPED-DEFINE procDisable
 
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

/*&scoped-def vendItemCost-maint vendItemCost*/

{sys/inc/var.i new shared}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEFINE NEW SHARED VARIABLE uom-list AS CHARACTER INIT "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.
DEFINE BUFFER bf-vendItemCost FOR vendItemCost .
DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
 RUN util/Validate.p PERSISTENT SET hdValidator.
     THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdValidator).
DEFINE VARIABLE hVendorCostProcs AS HANDLE NO-UNDO.
RUN system\VendorCostProcs.p PERSISTENT SET hVendorCostProcs.

DEFINE VARIABLE cVendItemCostSourceFrom AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostItem# AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostItemType AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostEst# AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostVendor AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostCustomer AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostForm# AS CHAR NO-UNDO.
DEFINE VARIABLE cVendItemCostBlank# AS CHAR NO-UNDO.
DEFINE VARIABLE lCheckEditMode AS LOGICAL NO-UNDO.
&Scoped-define VendItemCostCreateAfter procCreateAfter

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
&Scoped-define EXTERNAL-TABLES vendItemCost
&Scoped-define FIRST-EXTERNAL-TABLE vendItemCost


/* Need to scope the external tables to this procedure                  */
DEFINE QUERY external_tables FOR vendItemCost.
/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS vendItemCost.vendorUOM ~
vendItemCost.useQuantityFromBase vendItemCost.itemType vendItemCost.itemID ~
vendItemCost.vendorID vendItemCost.customerID vendItemCost.estimateNo ~
vendItemCost.formNo vendItemCost.blankNo vendItemCost.vendorItemID ~
vendItemCost.effectiveDate vendItemCost.expirationDate 
&Scoped-define ENABLED-TABLES vendItemCost
&Scoped-define FIRST-ENABLED-TABLE vendItemCost
&Scoped-Define ENABLED-OBJECTS btnCalendar-1 btnCalendar-2 Btn_multi 
&Scoped-Define DISPLAYED-FIELDS vendItemCost.vendorUOM ~
vendItemCost.useQuantityFromBase vendItemCost.itemType vendItemCost.itemID ~
vendItemCost.vendorID vendItemCost.customerID vendItemCost.estimateNo ~
vendItemCost.formNo vendItemCost.blankNo vendItemCost.vendorItemID ~
vendItemCost.effectiveDate vendItemCost.expirationDate ~
vendItemCost.createdDate vendItemCost.createdID vendItemCost.updatedID ~
vendItemCost.updatedDate 
&Scoped-define DISPLAYED-TABLES vendItemCost
&Scoped-define FIRST-DISPLAYED-TABLE vendItemCost


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS vendItemCost.itemID vendItemCost.vendorID ~
vendItemCost.customerID vendItemCost.estimateNo 
&Scoped-define ADM-ASSIGN-FIELDS vendItemCost.vendorUOM vendItemCost.itemID ~
vendItemCost.vendorID vendItemCost.customerID vendItemCost.estimateNo ~
vendItemCost.formNo vendItemCost.blankNo vendItemCost.vendorItemID ~
vendItemCost.effectiveDate vendItemCost.expirationDate ~
vendItemCost.createdDate vendItemCost.createdID vendItemCost.updatedID ~
vendItemCost.updatedDate 
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
company|y|y|ASI.vendItemCost.company
itemID||y|ASI.vendItemCost.itemID
</FOREIGN-KEYS> 
<EXECUTING-CODE>
**************************
* Set attributes related to FOREIGN KEYS
*/
RUN set-attribute-list (
    'Keys-Accepted = "company",
     Keys-Supplied = "company,itemID"':U).
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

DEFINE BUTTON Btn_multi 
     LABEL "&Update Add Multiple" 
     SIZE 26 BY 1.29
     FONT 4.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 143.2 BY 16.33.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 141.2 BY 15.62.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 1 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 93 BY 13.48.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     vendItemCost.vendorUOM AT ROW 1.67 COL 79 COLON-ALIGNED
          LABEL "Cost and Quantity UOM"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 15 
     vendItemCost.useQuantityFromBase AT ROW 1.76 COL 113 NO-LABEL WIDGET-ID 82
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Up To", No,
"From", Yes
          SIZE 22 BY .95
     vendItemCost.itemType AT ROW 2.29 COL 14.2 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEM-PAIRS "FG","FG",
                     "RM","RM"
          DROP-DOWN-LIST
          SIZE 9 BY 1
     vendItemCost.itemID AT ROW 3.43 COL 14.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
          BGCOLOR 15 
     vendItemCost.vendorID AT ROW 4.57 COL 14.2 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     vendItemCost.customerID AT ROW 5.71 COL 14.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     vendItemCost.estimateNo AT ROW 6.95 COL 14.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
          BGCOLOR 15 
     vendItemCost.formNo AT ROW 6.95 COL 31.4 COLON-ALIGNED
          LABEL "F"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 
     vendItemCost.blankNo AT ROW 6.95 COL 40.2 COLON-ALIGNED
          LABEL "B"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
          BGCOLOR 15 
     vendItemCost.vendorItemID AT ROW 8.14 COL 14.2 COLON-ALIGNED
          LABEL "Vend Item"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
          BGCOLOR 15 
     vendItemCost.effectiveDate AT ROW 9.33 COL 14.2 COLON-ALIGNED
          LABEL "Effective"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     btnCalendar-1 AT ROW 9.33 COL 32 WIDGET-ID 76
     btnCalendar-2 AT ROW 10.52 COL 32 WIDGET-ID 78
     vendItemCost.expirationDate AT ROW 10.57 COL 14.2 COLON-ALIGNED
          LABEL "Expires"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     vendItemCost.createdDate AT ROW 11.81 COL 14.2 COLON-ALIGNED
          LABEL "Created"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     vendItemCost.createdID AT ROW 11.81 COL 35 COLON-ALIGNED
          LABEL "By"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.updatedID AT ROW 13.1 COL 35 COLON-ALIGNED
          LABEL "By"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 15 
     vendItemCost.updatedDate AT ROW 13.14 COL 14.2 COLON-ALIGNED
          LABEL "Updated"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
          BGCOLOR 15 
     Btn_multi AT ROW 15.19 COL 115.2
     "Quantity Basis:" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.91 COL 95 WIDGET-ID 86
     " Cost Levels" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.76 COL 51 WIDGET-ID 8
     " Vendor Item Cost Details" VIEW-AS TEXT
          SIZE 30 BY .62 AT ROW 1.33 COL 4.2
     RECT-1 AT ROW 1 COL 1
     RECT-5 AT ROW 1.48 COL 2
     RECT-6 AT ROW 3.14 COL 49.2 WIDGET-ID 4
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FGCOLOR 1 FONT 6.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartViewer
   External Tables: ASI.vendItemCost
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
         HEIGHT             = 17.86
         WIDTH              = 161.2.
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

/* SETTINGS FOR FILL-IN vendItemCost.blankNo IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR BUTTON btnCalendar-1 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR BUTTON btnCalendar-2 IN FRAME F-Main
   3                                                                    */
/* SETTINGS FOR FILL-IN vendItemCost.createdDate IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.createdID IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.customerID IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR FILL-IN vendItemCost.effectiveDate IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN vendItemCost.estimateNo IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR FILL-IN vendItemCost.expirationDate IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN vendItemCost.formNo IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN vendItemCost.itemID IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR RECTANGLE RECT-1 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-5 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-6 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vendItemCost.updatedDate IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.updatedID IN FRAME F-Main
   NO-ENABLE 2 EXP-LABEL                                                */
/* SETTINGS FOR FILL-IN vendItemCost.vendorID IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR FILL-IN vendItemCost.vendorItemID IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN vendItemCost.vendorUOM IN FRAME F-Main
   2 EXP-LABEL                                                          */
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
  DEFINE VARIABLE char-val AS CHARACTER NO-UNDO.
  DEFINE VARIABLE riLookup AS RECID NO-UNDO.
  DEFINE VARIABLE lv-handle AS HANDLE NO-UNDO.
  DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
  DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.
  DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

  {&methods/lValidateError.i YES}
  CASE FOCUS:NAME :
    WHEN "vendorUOM" THEN DO:
        RUN pSetUomList(cocode,vendItemCost.itemID:SCREEN-VALUE,vendItemCost.itemType:SCREEN-VALUE).
        RUN windows/l-stduom.w (cocode, uom-list, FOCUS:SCREEN-VALUE, OUTPUT char-val).
        IF char-val NE "" THEN 
            FOCUS:SCREEN-VALUE IN FRAME {&frame-name} = ENTRY(1,char-val).
    END.
    WHEN "customerID" THEN DO:        
         RUN system/openlookup.p (g_company, "cust-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN vendItemCost.customerID:SCREEN-VALUE = cMainField.        
    END.
    WHEN "vendorID" THEN DO:
         RUN system/openlookup.p (g_company, "vend-no", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
          IF cMainField <> "" THEN vendItemCost.vendorID:SCREEN-VALUE = cMainField.
    END.
    WHEN "itemID" THEN DO:        
        IF vendItemCost.itemType:SCREEN-VALUE = "RM" THEN DO: 
            RUN system/openlookup.p (g_company, "item", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
             IF cMainField <> "" THEN vendItemCost.itemID:SCREEN-VALUE = cMainField. 
        END.
        ELSE DO:  /* finished good */
            RUN system/openlookup.p (g_company, "itemfg", 0, "", 0, OUTPUT cAllFields, OUTPUT cMainField, OUTPUT recRecordID).
             IF cMainField <> "" THEN vendItemCost.itemID:SCREEN-VALUE = cMainField. 
        END.
    END.
    WHEN "estimateNo" THEN DO:        
         RUN windows/l-est.w (g_company,g_loc,vendItemCost.estimateNo:SCREEN-VALUE, OUTPUT char-val).
         IF char-val NE "" THEN DO:
             FIND FIRST eb NO-LOCK
                  WHERE RECID(eb) EQ INT(char-val)
                  NO-ERROR.
             IF AVAILABLE eb THEN
             vendItemCost.estimateNo:screen-value = eb.est-no.
         END.
    END.
    OTHERWISE DO:
      lv-handle = FOCUS:HANDLE.
      RUN applhelp.p.
      IF g_lookup-var NE "" THEN lv-handle:SCREEN-VALUE = g_lookup-var.
      APPLY "entry" TO lv-handle.
      RETURN NO-APPLY.
    END.
  END CASE.  
  {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-1 V-table-Win
ON CHOOSE OF btnCalendar-1 IN FRAME F-Main
DO:
  {methods/btnCalendar.i vendItemCost.effectiveDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalendar-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalendar-2 V-table-Win
ON CHOOSE OF btnCalendar-2 IN FRAME F-Main
DO:
  {methods/btnCalendar.i vendItemCost.expirationDate}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_multi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_multi V-table-Win
ON CHOOSE OF Btn_multi IN FRAME F-Main /* Update Add Multiple */
DO:
   DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel .
    IF AVAILABLE vendItemCost AND NOT adm-new-record THEN DO:
        RUN viewers/dVendCostLevelM.w (ROWID(vendItemCost),"update") .
        RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"reopen-target",OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:
           FIND LAST  bf-vendItemCostLevel NO-LOCK
               WHERE bf-vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID NO-ERROR .
            IF AVAILABLE bf-vendItemCostLevel THEN
                RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(vendItemCost),ROWID(bf-vendItemCostLevel)).
        END.
    END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.customerID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.customerID V-table-Win
ON LEAVE OF vendItemCost.customerID IN FRAME F-Main /* Customer */
DO:
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
   
   IF LASTKEY <> -1 AND vendItemCost.customerID:SCREEN-VALUE <> "" THEN DO:
      RUN valid-cust-no ( OUTPUT lCheckError) NO-ERROR.
      IF lCheckError THEN RETURN NO-APPLY.
   END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.effectiveDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.effectiveDate V-table-Win
ON HELP OF vendItemCost.effectiveDate IN FRAME F-Main /* Effective */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.effectiveDate V-table-Win
ON LEAVE OF vendItemCost.effectiveDate IN FRAME F-Main /* Effective */
DO:
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
    IF LASTKEY <> -1 THEN DO:
        RUN valid-date(1,date(vendItemCost.effectiveDate:SCREEN-VALUE), OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.estimateNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.estimateNo V-table-Win
ON LEAVE OF vendItemCost.estimateNo IN FRAME F-Main /* Estimate */
DO:
   DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
   DEFINE VARIABLE cEstNo AS CHARACTER NO-UNDO.

   IF LASTKEY <> -1 AND vendItemCost.estimateNo:SCREEN-VALUE NE "" THEN DO:
       RUN valid-estimate(OUTPUT lCheckError)  NO-ERROR.
       IF lCheckError THEN RETURN NO-APPLY.
   END.
   
   IF SELF:SCREEN-VALUE NE "" THEN
   DO:
     cEstNo = vendItemCost.estimateNo:SCREEN-VALUE.
     
     RUN util/rjust.p (INPUT-OUTPUT cEstNo,8).
     vendItemCost.estimateNo:SCREEN-VALUE = cEstNo.
  END.    
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.expirationDate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.expirationDate V-table-Win
ON HELP OF vendItemCost.expirationDate IN FRAME F-Main /* Expires */
DO:
    {methods/calendar.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.expirationDate V-table-Win
ON LEAVE OF vendItemCost.expirationDate IN FRAME F-Main /* Expires */
DO:
     DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
   IF LASTKEY <> -1 THEN DO:
      RUN valid-expdate ( OUTPUT lCheckError) NO-ERROR.
      IF lCheckError THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.itemID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.itemID V-table-Win
ON LEAVE OF vendItemCost.itemID IN FRAME F-Main /* Item ID */
DO:      
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
   IF LASTKEY <> -1 THEN DO:
       RUN valid-i-no( OUTPUT lCheckError) NO-ERROR.
       IF lCheckError THEN RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.itemType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.itemType V-table-Win
ON LEAVE OF vendItemCost.itemType IN FRAME F-Main /* Item Type */
DO: 
        IF LASTKEY <> -1 THEN DO:
         IF LOOKUP(vendItemCost.itemType:SCREEN-VALUE,"RM,FG") EQ 0  THEN DO:
             MESSAGE "Item Type must be RM or FG" VIEW-AS ALERT-BOX ERROR .
             APPLY "entry" TO vendItemCost.itemType .
             RETURN NO-APPLY.
         END.
        END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.itemType V-table-Win
ON VALUE-CHANGED OF vendItemCost.itemType IN FRAME F-Main /* Item Type */
DO:      
    IF LENGTH(vendItemCost.itemType:SCREEN-VALUE) GE 2 THEN DO:
        IF LOOKUP(vendItemCost.itemType:SCREEN-VALUE,"RM,FG") EQ 0  THEN DO:
             MESSAGE "Item Type must be RM or FG" VIEW-AS ALERT-BOX ERROR .
             APPLY "entry" TO vendItemCost.itemType .
             RETURN NO-APPLY.
         END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.vendorID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.vendorID V-table-Win
ON LEAVE OF vendItemCost.vendorID IN FRAME F-Main /* Vendor */
DO:
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
    IF LASTKEY <> -1 THEN DO:
        RUN valid-vend-no( OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
    END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.vendorUOM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.vendorUOM V-table-Win
ON ENTRY OF vendItemCost.vendorUOM IN FRAME F-Main /* Cost and Quantity UOM */
DO:
/*        IF vendItemCost.ItemID:SCREEN-VALUE = "" THEN DO:  */
/*           MESSAGE "Item ID is blank. Enter Item ID first!"*/
/*             VIEW-AS ALERT-BOX error.                      */
/*           APPLY "ENTRY" TO vendItemCost.ItemID.           */
/*           RETURN NO-APPLY.                                */
/*        END.                                               */

    END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.vendorUOM V-table-Win
ON LEAVE OF vendItemCost.vendorUOM IN FRAME F-Main /* Cost and Quantity UOM */
DO:
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
    IF LASTKEY <> -1 THEN DO:
        RUN valid-uom( OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-item V-table-Win 
PROCEDURE add-item :
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
    WHEN 'company':U THEN
       {src/adm/template/find-tbl.i
           &TABLE = vendItemCost
           &WHERE = "WHERE vendItemCost.company eq key-value"
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
  {src/adm/template/row-list.i "vendItemCost"}

  /* Get the record ROWID's from the RECORD-SOURCE.                  */
  {src/adm/template/row-get.i}

  /* FIND each record specified by the RECORD-SOURCE.                */
  {src/adm/template/row-find.i "vendItemCost"}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable-vendItemCost-field V-table-Win 
PROCEDURE enable-vendItemCost-field :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
    ENABLE ALL.
  END.
  RUN set-panel (0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getSourceAttributes V-table-Win 
PROCEDURE getSourceAttributes :
/*------------------------------------------------------------------------------
         Purpose:
         Notes:
    -------------------------------------------------------------------------------*/
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostSourceFrom' ).
    cVendItemCostSourceFrom = IF RETURN-VALUE EQ ? THEN "" ELSE RETURN-VALUE. 
   
    RUN GET-ATTRIBUTE IN adm-broker-hdl ('OneVendItemCost'). 
    cVendItemCostItem# = IF RETURN-VALUE EQ ? THEN "" ELSE RETURN-VALUE.
    
    RUN GET-ATTRIBUTE IN adm-broker-hdl ('OneVendItemCostEst#').    
    cVendItemCostEst# = IF RETURN-VALUE EQ ? THEN "" ELSE RETURN-VALUE.
    
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostType' ).
    cVendItemCostItemType = IF RETURN-VALUE EQ ? THEN "" ELSE RETURN-VALUE.
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostVendor' ).
    cVendItemCostVendor = IF RETURN-VALUE EQ ? THEN "" ELSE RETURN-VALUE.
    
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostCustomer' ).  
    cVendItemCostCustomer = IF RETURN-VALUE EQ ? THEN "" ELSE RETURN-VALUE.
    
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostForm#' ).  
    cVendItemCostForm# = IF RETURN-VALUE EQ ? THEN "" ELSE RETURN-VALUE.
    
    RUN get-attribute IN adm-broker-hdl ('OneVendItemCostBlank#' ).  
    cVendItemCostBlank# = IF RETURN-VALUE EQ ? THEN "" ELSE RETURN-VALUE.
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-assign-record V-table-Win 
PROCEDURE local-assign-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR liFromVendItemCostID LIKE vendItemCost.vendItemCostID NO-UNDO.
  DEF BUFFER bf-vendItemCostLevel FOR vendItemCostLevel.
  DEF BUFFER bf-vendItemCost FOR vendItemCost.
    
  /* Code placed here will execute PRIOR to standard behavior. */
  liFromVendItemCostID = vendItemCost.vendItemCostID.
  
  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'assign-record':U ) .

    /* Code placed here will execute AFTER standard behavior.    */
    
  IF adm-new-record AND NOT adm-adding-record THEN DO: /* copy */
     FIND bf-venditemcost NO-LOCK WHERE bf-venditemcost.venditemcostID = liFromVendItemCostID NO-ERROR.
     IF AVAIL bf-venditemcost THEN DO:  /* copy restriction values */
        ASSIGN vendItemCost.dimLengthMaximum = bf-vendItemCost.dimLengthMaximum
               vendItemCost.dimLengthMinimum = bf-vendItemCost.dimLengthMinimum 
               vendItemCost.dimWidthMinimum = bf-vendItemCost.dimWidthMinimum 
               vendItemCost.dimWidthMaximum = bf-vendItemCost.dimWidthMaximum 
               vendItemCost.dimWidthOver = bf-vendItemCost.dimWidthOver
               vendItemCost.dimLengthOver = bf-vendItemCost.dimLengthOver
               vendItemCost.dimLengthUnder = bf-vendItemCost.dimLengthUnder
               vendItemCost.dimLengthUnderCharge = bf-vendItemCost.dimLengthUnderCharge
               vendItemCost.dimWidthUnder = bf-vendItemCost.dimWidthUnder
               vendItemCost.dimWidthUnderCharge = bf-vendItemCost.dimWidthUnderCharge
               vendItemCost.validWidth = bf-vendItemCost.validWidth
               vendItemCost.validLength = bf-vendItemCost.validLength
               vendItemCost.quantityMinimumOrder = bf-vendItemCost.quantityMinimumOrder
               vendItemCost.quantityMaximumOrder = bf-vendItemCost.quantityMaximumOrder
               .
               
     END.
     FOR EACH vendItemCostLevel WHERE vendItemCostLevel.vendItemCostID = liFromVendItemCostID NO-LOCK: 
       CREATE bf-vendItemCostLevel.
       BUFFER-COPY vendItemCostLevel except vendItemCostLevel.vendItemCostID vendItemCostLevel.vendItemCostLevelID vendItemCostLevel.rec_key TO bf-vendItemCostLevel.      
       ASSIGN 
          bf-vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID           
/*          bf-vendItemCostLevel.quantityBase   = vendItemCostLevel.quantityBase                   */
/*          bf-vendItemCostLevel.costPerUOM     = vendItemCostLevel.costPerUOM*/
/*          bf-vendItemCostLevel.costSetup      = vendItemCostLevel.costSetup */
          .
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
  DISABLE ALL WITH FRAME {&frame-name}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN set-panel (1).
  adm-adding-record = NO .
  adm-new-record = NO .
  lCheckEditMode = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE BUFFER bf-vendItemCostLevel FOR vendItemCostLevel .   
  DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO .
  DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO .
    
  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  vendItemCost.company = cocode.
  
  IF adm-adding-record THEN
  DO WITH FRAME {&FRAME-NAME}:
    CREATE bf-vendItemCostLevel .
        ASSIGN bf-vendItemCostLevel.vendItemCostID = vendItemCost.vendItemCostID 
               bf-vendItemCostLevel.quantityBase    = 99999999 .
        FIND CURRENT bf-vendItemCostLevel NO-LOCK NO-ERROR .

     RUN RecalculateFromAndTo IN hVendorCostProcs (vendItemCost.vendItemCostID, OUTPUT lReturnError ,OUTPUT cReturnMessage ) .

  END.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"reopen-target",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(vendItemCost), ?).
  
  

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

 DO WITH FRAME {&FRAME-NAME}:
     IF vendItemCost.effectiveDate:SCREEN-VALUE LT  "01/01/1900" 
         THEN vendItemCost.effectiveDate:SCREEN-VALUE =  "01/01/1900"  .             
         
 END.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-update-record V-table-Win 
PROCEDURE local-update-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lNewRecord AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-rowid AS ROWID NO-UNDO .
    DEFINE VARIABLE rdRowidLevel AS ROWID NO-UNDO .
    DEFINE VARIABLE lReturnError AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cReturnMessage AS CHARACTER NO-UNDO.
    
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-i-no( OUTPUT lCheckError) NO-ERROR.
       IF lCheckError THEN RETURN NO-APPLY.


    RUN valid-vend-no( OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.

    IF vendItemCost.customerID:SCREEN-VALUE <> "" THEN DO:
        RUN valid-cust-no ( OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
    END.

    IF vendItemCost.estimateNo:SCREEN-VALUE NE "" THEN DO:
        RUN valid-estimate(OUTPUT lCheckError)  NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
    END.

    RUN valid-uom( OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.

    RUN valid-date(1,date(vendItemCost.effectiveDate:SCREEN-VALUE), OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.

    RUN valid-date(2,date(vendItemCost.expirationDate:SCREEN-VALUE), OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.

    RUN valid-expdate ( OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.
    
    RUN valid-duplicateRecord ( OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.
    
  END.
 
  DISABLE ALL WITH FRAME {&frame-name}.

  IF adm-adding-record THEN lNewRecord = YES .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN set-panel (0).

  IF lNewRecord THEN
      RUN viewers/dVendCostLevel.w(
          INPUT ROWID(vendItemCost),
          INPUT lv-rowid,
          INPUT "Create",
          INPUT NO, /* Do not Change quantityFrom */
          OUTPUT rdRowidLevel
          ) .
  
  RUN RecalculateFromAndTo IN hVendorCostProcs (vendItemCost.vendItemCostID, OUTPUT lReturnError ,OUTPUT cReturnMessage).
       
  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"reopen-target",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(vendItemCost), rdRowidLevel).
  
  adm-adding-record = NO .
  adm-new-record = NO .
  lCheckEditMode = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pDisableButtons V-table-Win
PROCEDURE pDisableButtons PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {methods/run_link.i "getPanel-SOURCE" "EnablePanel"}.
    DO WITH FRAME {&FRAME-NAME}:
        btn_multi:SENSITIVE = YES.
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

     RUN getSourceAttributes.
                
     IF cVendItemCostSourceFrom = "EST" THEN DO WITH FRAME {&frame-name}:
        DISABLE vendItemCost.ItemType vendItemCost.customerID vendItemCost.ItemID vendItemCost.estimateNo vendItemCost.formNo 
                venditemCost.blankNo /*vendItemCost.effectiveDate vendItemCost.ExpirationDate*/.
           
     END.
     ELSE IF cVendItemCostSourceFrom = "OF" THEN DO WITH FRAME {&frame-name}:
        DISABLE /*vendItemCost.ItemType*/ vendItemCost.customerID vendItemCost.ItemID 
            /*vendItemCost.estimateNo vendItemCost.formNo venditemCost.blankNo vendItemCost.effectiveDate vendItemCost.ExpirationDate*/.           
     END.
     ELSE IF cVendItemCostSourceFrom NE "" THEN DO WITH FRAME {&frame-name}:
         DISABLE /*vendItemCost.ItemType*/ vendItemCost.ItemID vendItemCost.estimateNo vendItemCost.formNo 
             venditemCost.blankNo 
                    /*vendItemCost.effectiveDate vendItemCost.ExpirationDate*/ .
     END.  
     lCheckEditMode = YES.
     {methods/run_link.i "getPanel-SOURCE" "DisablePanel"}.
     Btn_multi:SENSITIVE = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE procCreateAfter V-table-Win 
PROCEDURE procCreateAfter :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
------------------------------------------------------------------------------*/

    IF cVendItemCostSourceFrom = "EST" THEN     
    DO WITH FRAME {&frame-name}:
        DISABLE vendItemCost.ItemType vendItemCost.customerID vendItemCost.ItemID vendItemCost.estimateNo vendItemCost.formNo 
            venditemCost.blankNo /*vendItemCost.effectiveDate vendItemCost.ExpirationDate*/.

      IF adm-new-record THEN do: 
          ASSIGN vendItemCost.itemID:SCREEN-VALUE = cVendItemCostItem#
              vendItemCost.vendorID:SCREEN-VALUE = cVendItemCostVendor
              vendItemCost.customer:SCREEN-VALUE = cVendItemCostCustomer
              vendItemCost.EstimateNo:SCREEN-VALUE = cVendItemCostEst#
              vendItemCost.FormNo:SCREEN-VALUE = STRING(cVendItemCostForm#)
              vendItemCost.BlankNo:SCREEN-VALUE = STRING(cVendItemCostBlank#)
              .     
      END.
    END.
    ELSE IF cVendItemCostSourceFrom NE "" THEN 
    DO WITH FRAME {&frame-name}:
            DISABLE /*vendItemCost.ItemType*/ vendItemCost.ItemID vendItemCost.estimateNo vendItemCost.formNo 
                venditemCost.blankNo 
                /*vendItemCost.effectiveDate vendItemCost.ExpirationDate*/ .
        ASSIGN 
            vendItemCost.itemID:SCREEN-VALUE = cVendItemCostItem#
            vendItemCost.vendorID:SCREEN-VALUE = cVendItemCostVendor
            vendItemCost.customer:SCREEN-VALUE = cVendItemCostCustomer
            vendItemCost.EstimateNo:SCREEN-VALUE = cVendItemCostEst#
            vendItemCost.FormNo:SCREEN-VALUE = STRING(cVendItemCostForm#)
            vendItemCost.BlankNo:SCREEN-VALUE = STRING(cVendItemCostBlank#)
            .         
    END.  
    IF adm-adding-record THEN DO:
        RUN pSetDefaultValues.
          RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"reopen-target",OUTPUT char-hdl).
        IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
        RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(vendItemCost), ?).
    END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetDefaultValues V-table-Win
PROCEDURE pSetDefaultValues PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Set the default in the screen display
 Notes:
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}: 
        ASSIGN 
            vendItemCost.effectiveDate:SCREEN-VALUE  = STRING(TODAY) /*"12/31/2099" */ 
            vendItemCost.expirationDate:SCREEN-VALUE = STRING(12/31/2099) 
            vendItemCost.itemType:SCREEN-VALUE       = IF cVendItemCostSourceFrom NE "" THEN cVendItemCostItemType ELSE "FG" 
            vendItemCost.vendorUOM:SCREEN-VALUE      = "EA" 
            VendItemCost.createdID:SCREEN-VALUE      = USERID('ASI')
            vendItemCost.createdDate:SCREEN-VALUE    = STRING(TODAY)
            vendItemCost.updatedID:SCREEN-VALUE      = USERID('ASI')
            vendItemCost.updatedDate:SCREEN-VALUE    = STRING(TODAY)
            .   
    END.                 
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pSetUomList V-table-Win 
PROCEDURE pSetUomList PRIVATE :
/*------------------------------------------------------------------------------
     Purpose:  
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcItemType AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lError   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    IF ipcItemType EQ "RM" THEN DO:
          FIND FIRST ITEM NO-LOCK
              WHERE ITEM.company EQ ipcCompany
                AND ITEM.i-no EQ  ipcItemID
                NO-ERROR.
          IF AVAILABLE ITEM THEN
              RUN sys/ref/uom-rm.p  (ITEM.mat-type, OUTPUT uom-list).
    END.
    ELSE DO:
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ ipcCompany
            AND itemfg.i-no EQ  ipcItemID
            NO-ERROR.
        lError = YES.
        IF AVAILABLE itemfg THEN
            RUN Conv_GetValidCostUOMsForItem(ROWID(itemfg), OUTPUT uom-list, OUTPUT lError, OUTPUT cMessage).
        IF lError THEN  
            RUN Conv_GetValidCostUOMs(
                OUTPUT uom-list
                ).
    END.
    
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
  {src/adm/template/sndkycas.i "company" "vendItemCost" "company"}
  {src/adm/template/sndkycas.i "itemID" "vendItemCost" "itemID"}

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
  {src/adm/template/snd-list.i "vendItemCost"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE set-panel V-table-Win 
PROCEDURE set-panel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ip-switch AS INTEGER NO-UNDO.

  DEFINE VARIABLE char-hdl AS CHARACTER NO-UNDO.
  
  DO WITH FRAME {&FRAME-NAME}:
    ENABLE Btn_multi .
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

  {methods/lValidateError.i YES}
  IF NOT CAN-FIND(FIRST cust WHERE cust.company = g_company
                      AND cust.cust-no = vendItemCost.customerID:SCREEN-VALUE IN FRAME {&FRAME-NAME})
  THEN DO:
      MESSAGE "Invalid Customer#. Try Help." VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
  END.         
  
  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-custtype V-table-Win 
PROCEDURE valid-custtype :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
  IF NOT CAN-FIND(FIRST custype WHERE custype.company = g_company
                      AND custype.custype = vendItemCost.itemID:SCREEN-VALUE IN FRAME {&FRAME-NAME})
  THEN DO:
      MESSAGE "Invalid Type#. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vendItemCost.itemID.
      RETURN ERROR.
  END.



  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-date V-table-Win 
PROCEDURE valid-date :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiType AS INTEGER NO-UNDO .
    DEFINE INPUT PARAMETER ipdtDate AS DATE NO-UNDO .
    DEFINE OUTPUT PARAMETER opcReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.

   DO WITH FRAME {&FRAME-NAME}:
        {methods/lValidateError.i YES}
        IF ipdtDate LT 01/01/1900 OR ipdtDate GT 12/31/3000 THEN DO:
            MESSAGE "Calendar year should be between 1900 to 3000 years " 
                VIEW-AS ALERT-BOX INFO .
            IF ipiType EQ 1 THEN
                APPLY "entry" TO vendItemCost.effectiveDate .
            ELSE APPLY "entry" TO vendItemCost.expirationDate .
                opcReturnError = YES .
        END.
        {methods/lValidateError.i NO}
   END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-duplicateRecord V-table-Win 
PROCEDURE valid-duplicateRecord :
/*------------------------------------------------------------------------------
     Purpose:
     Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER opcReturnError AS LOGICAL NO-UNDO .
 
  opcReturnError = 
      can-find(FIRST bf-venditemcost WHERE bf-vendItemCost.company = venditemcost.company
                                   AND bf-vendItemCost.itemID = vendItemCost.ItemID:SCREEN-VALUE IN FRAME {&frame-name}
                                   AND bf-vendItemCost.ItemType = vendItemCost.ItemType:SCREEN-VALUE IN FRAME {&frame-name}
                                   AND bf-vendItemCost.vendorID = vendItemCost.vendorID:SCREEN-VALUE IN FRAME {&frame-name}
                                   AND bf-vendItemCost.customerID = vendItemCost.customerID:SCREEN-VALUE IN FRAME {&frame-name}
                                   AND trim(bf-vendItemCost.EstimateNo) = trim(vendItemCost.EstimateNo:SCREEN-VALUE IN FRAME {&frame-name})
                                   AND bf-vendItemCost.formNo EQ INTEGER(vendItemCost.formNo:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                                   AND bf-vendItemCost.effectiveDate EQ DATE(vendItemCost.effectiveDate:SCREEN-VALUE IN FRAME {&FRAME-NAME})
                                   AND bf-vendItemCost.vendItemCostID <> venditemcost.venditemcostID                                    
             )    
      .  
      
  IF opcReturnError THEN do:
       MESSAGE "This record is a duplicate of a previous entry; please adjust." VIEW-AS ALERT-BOX ERROR. 
       APPLY "entry" TO vendItemCost.vendorID.
  END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-entry V-table-Win 
PROCEDURE valid-entry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  {methods/lValidateError.i YES}
   
   IF adm-new-record THEN 
    FIND FIRST bf-vendItemCost NO-LOCK 
      WHERE bf-vendItemCost.company EQ g_company
        AND bf-vendItemCost.vendorID EQ vendItemCost.vendorID:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND bf-vendItemCost.customerID EQ vendItemCost.customerID:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND bf-vendItemCost.itemType EQ vendItemCost.itemType:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND bf-vendItemCost.estimateNo EQ vendItemCost.estimateNo:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND bf-vendItemCost.itemID EQ vendItemCost.itemID:SCREEN-VALUE IN FRAME {&FRAME-NAME}
        AND bf-vendItemCost.effectiveDate EQ date(vendItemCost.effectiveDate:SCREEN-VALUE IN FRAME {&FRAME-NAME})
        NO-ERROR .

  IF AVAILABLE bf-vendItemCost THEN DO:
      MESSAGE "This record is a duplicate of a previous entry; please adjust." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vendItemCost.customerID.
      RETURN ERROR.
  END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-estimate V-table-Win 
PROCEDURE valid-estimate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.

    {methods/lValidateError.i YES}
    lv-msg = "".
    DO WITH FRAME {&FRAME-NAME}:
            IF NOT CAN-FIND(FIRST eb WHERE 
                            eb.company EQ cocode AND 
                            trim(eb.est-no) EQ trim(vendItemCost.estimateNo:SCREEN-VALUE)) THEN 
                lv-msg = TRIM(vendItemCost.estimateNo:LABEL) + " " + vendItemCost.estimateNo:SCREEN-VALUE +  " is invalid, try help".
        
        IF lv-msg NE "" THEN DO:
            MESSAGE 
                TRIM(lv-msg)
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            APPLY "entry" TO vendItemCost.estimateNo .
            opcReturnError = YES .
        END.
    END.
    {methods/lValidateError.i NO}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-expdate V-table-Win 
PROCEDURE valid-expdate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO .

  {methods/lValidateError.i YES}
  IF DATE(vendItemCost.expirationDate:SCREEN-VALUE IN FRAME {&frame-name}) LT DATE(vendItemCost.effectiveDate:SCREEN-VALUE IN FRAME {&frame-name}) THEN DO:
          MESSAGE "Expiration date should be greater than Effective Date." VIEW-AS ALERT-BOX ERROR.
          APPLY "entry" TO vendItemCost.effectiveDate.
          oplReturnError = YES .
      END.

  {methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-i-no V-table-Win 
PROCEDURE valid-i-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO .

    IF vendItemCost.itemID:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
        {methods/lValidateError.i YES}
        IF vendItemCost.itemType:SCREEN-VALUE EQ "FG"
            AND NOT CAN-FIND(FIRST itemfg WHERE itemfg.company EQ g_company
                             AND itemfg.i-no EQ vendItemCost.itemID:SCREEN-VALUE)
        THEN DO:
            MESSAGE "Invalid Item#. Try Help." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO vendItemCost.itemID.
            oplReturnError = YES .
        END.
        ELSE IF vendItemCost.itemType:SCREEN-VALUE EQ "RM"
            AND NOT CAN-FIND(FIRST ITEM WHERE ITEM.company EQ g_company
                            AND ITEM.i-no EQ vendItemCost.itemID:SCREEN-VALUE)
        THEN DO:
            MESSAGE "Invalid Item ID#. Try Help." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO vendItemCost.itemID.
            oplReturnError = YES .
        END.
        {methods/lValidateError.i NO}
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE valid-uom V-table-Win 
PROCEDURE valid-uom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER oplReturnError AS LOGICAL NO-UNDO .
  DEFINE VARIABLE cUom AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lValid AS LOGICAL NO-UNDO .
  DEFINE VARIABLE cValidMessage AS CHARACTER NO-UNDO .
  DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
  
{&methods/lValidateError.i YES}
  
  DO WITH FRAME {&FRAME-NAME}:
      RUN pSetUomList(cocode,vendItemCost.itemID:SCREEN-VALUE,vendItemCost.itemType:SCREEN-VALUE).
      
      cUom = vendItemCost.vendorUOM:SCREEN-VALUE.

      RUN pIsValidUOM IN hdValidator (cUom, YES, OUTPUT lValid, OUTPUT cValidMessage).
      IF NOT lValid THEN DO:
          MESSAGE  cValidMessage
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          oplReturnError = YES .
          lCheckError = YES .
          APPLY "entry" TO vendItemCost.vendorUOM .
      END.

      RUN pIsValidFromList IN hdValidator ("Uom", cUom, uom-list, OUTPUT lValid, OUTPUT cValidMessage). 
      
      IF NOT lValid AND NOT lCheckError THEN DO:
          MESSAGE  cValidMessage
              VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
          oplReturnError = YES .
          APPLY "entry" TO vendItemCost.vendorUOM .
      END.
   END.
   
{&methods/lValidateError.i NO}
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
    DEFINE OUTPUT PARAMETER opcReturnError AS LOGICAL NO-UNDO .
    DEFINE VARIABLE lv-msg AS CHARACTER NO-UNDO.

    IF vendItemCost.vendorID:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN DO:
        {methods/lValidateError.i YES}
        lv-msg = "".
        IF NOT CAN-FIND(FIRST vend WHERE 
                        vend.company EQ cocode AND 
                        vend.vend-no EQ vendItemCost.vendorID:SCREEN-VALUE) THEN 
            lv-msg = TRIM(vendItemCost.vendorID:LABEL) + " " + vendItemCost.vendorID:SCREEN-VALUE +  " is invalid, try help".
        
        IF lv-msg NE "" THEN DO:
            MESSAGE 
                TRIM(lv-msg)
                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            APPLY "entry" TO vendItemCost.vendorID .
            opcReturnError = YES .
        END.
        {methods/lValidateError.i NO}
    END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vendcost-newitem V-table-Win 
PROCEDURE vendcost-newitem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAMETER oplExists AS LOG NO-UNDO.

ASSIGN oplExists = lCheckEditMode .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

