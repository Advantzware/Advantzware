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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

&IF DEFINED(UIB_is_Running) NE 0 &THEN
&Scoped-define NEW NEW GLOBAL
&ENDIF
DEFINE {&NEW} SHARED VARIABLE g_lookup-var AS CHARACTER NO-UNDO.

/*&scoped-def vendItemCost-maint vendItemCost*/

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

DEFINE NEW SHARED VARIABLE uom-list AS CHARACTER INIT "M,EA,L,CS,C,LB,DRM,ROL,PKG,SET,DOZ,BDL" NO-UNDO.
DEFINE BUFFER bf-vendItemCost FOR vendItemCost .
DEFINE VARIABLE hdValidator AS HANDLE    NO-UNDO.
 RUN util/Validate.p PERSISTENT SET hdValidator.
     THIS-PROCEDURE:ADD-SUPER-PROCEDURE(hdValidator).

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
&Scoped-Define ENABLED-FIELDS vendItemCost.itemType vendItemCost.itemID ~
vendItemCost.dimWidthMinimum vendItemCost.dimWidthMaximum ~
vendItemCost.vendorID vendItemCost.dimLengthMinimum ~
vendItemCost.dimLengthMaximum vendItemCost.customerID ~
vendItemCost.estimateNo vendItemCost.formNo vendItemCost.blankNo ~
vendItemCost.vendorItemID vendItemCost.dimWidthUnder ~
vendItemCost.dimWidthOver vendItemCost.effectiveDate ~
vendItemCost.dimLengthUnder vendItemCost.dimLengthOver ~
vendItemCost.expirationDate vendItemCost.quantityMinimumOrder ~
vendItemCost.quantityMaximumOrder vendItemCost.vendorUOM 
&Scoped-define ENABLED-TABLES vendItemCost
&Scoped-define FIRST-ENABLED-TABLE vendItemCost
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-5 RECT-6 RECT-7 
&Scoped-Define DISPLAYED-FIELDS vendItemCost.itemType vendItemCost.itemID ~
vendItemCost.dimWidthMinimum vendItemCost.dimWidthMaximum ~
vendItemCost.vendorID vendItemCost.dimLengthMinimum ~
vendItemCost.dimLengthMaximum vendItemCost.customerID ~
vendItemCost.estimateNo vendItemCost.formNo vendItemCost.blankNo ~
vendItemCost.vendorItemID vendItemCost.dimWidthUnder ~
vendItemCost.dimWidthOver vendItemCost.effectiveDate ~
vendItemCost.dimLengthUnder vendItemCost.dimLengthOver ~
vendItemCost.expirationDate vendItemCost.quantityMinimumOrder ~
vendItemCost.createdDate vendItemCost.createdID ~
vendItemCost.quantityMaximumOrder vendItemCost.updatedID ~
vendItemCost.updatedDate vendItemCost.vendorUOM
&Scoped-define DISPLAYED-TABLES vendItemCost
&Scoped-define FIRST-DISPLAYED-TABLE vendItemCost


/* Custom List Definitions                                              */
/* ADM-CREATE-FIELDS,ADM-ASSIGN-FIELDS,ROW-AVAILABLE,DISPLAY-FIELD,List-5,F1 */
&Scoped-define ADM-CREATE-FIELDS vendItemCost.itemType vendItemCost.itemID ~
vendItemCost.vendorID vendItemCost.customerID vendItemCost.estimateNo 
&Scoped-define ADM-ASSIGN-FIELDS vendItemCost.itemType vendItemCost.itemID ~
vendItemCost.vendorID vendItemCost.customerID vendItemCost.estimateNo ~
vendItemCost.effectiveDate 

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
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 142 BY 16.19.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141.2 BY 15.24.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 53 BY 12.81.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33.4 BY 12.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     vendItemCost.itemType AT ROW 2.38 COL 15  COLON-ALIGNED 
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     
     vendItemCost.itemID AT ROW 3.52 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     
     vendItemCost.vendorID AT ROW 4.67 COL 15 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
    
     vendItemCost.customerID AT ROW 5.81 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     vendItemCost.estimateNo AT ROW 7.05 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     vendItemCost.formNo AT ROW 7.05 COL 32.2 COLON-ALIGNED
          LABEL "F"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     vendItemCost.blankNo AT ROW 7.05 COL 41 COLON-ALIGNED
          LABEL "B"
          VIEW-AS FILL-IN 
          SIZE 4.4 BY 1
     vendItemCost.vendorItemID AT ROW 8.24 COL 15 COLON-ALIGNED
          LABEL "vend Item"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
    vendItemCost.effectiveDate AT ROW 9.43 COL 15 COLON-ALIGNED
          LABEL "Effective"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
    vendItemCost.expirationDate AT ROW 10.67 COL 15 COLON-ALIGNED
          LABEL "Expires"
          VIEW-AS FILL-IN 
          SIZE 19.6 BY 1
    vendItemCost.vendorUOM AT ROW 2.38 COL 86 COLON-ALIGNED
          LABEL "Cost and Quantity UOM"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     vendItemCost.dimWidthMinimum AT ROW 4.33 COL 111 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     vendItemCost.dimWidthMaximum AT ROW 4.33 COL 125 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     vendItemCost.dimLengthMinimum AT ROW 5.52 COL 111 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     vendItemCost.dimLengthMaximum AT ROW 5.52 COL 125.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     vendItemCost.dimWidthUnder AT ROW 8.86 COL 111 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     vendItemCost.dimWidthOver AT ROW 8.86 COL 125 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     
     vendItemCost.dimLengthUnder AT ROW 9.95 COL 111 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     vendItemCost.dimLengthOver AT ROW 9.95 COL 125 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13 BY 1

     vendItemCost.quantityMinimumOrder AT ROW 11.71 COL 124 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     vendItemCost.createdDate AT ROW 11.91 COL 15 COLON-ALIGNED
          LABEL "Created"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     vendItemCost.createdID AT ROW 11.91 COL 36 COLON-ALIGNED
          LABEL "By" 
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     vendItemCost.quantityMaximumOrder AT ROW 13.14 COL 124 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
     vendItemCost.updatedID AT ROW 13.19 COL 36 COLON-ALIGNED
          LABEL "By"
          VIEW-AS FILL-IN 
          SIZE 14.2 BY 1
     vendItemCost.updatedDate AT ROW 13.24 COL 15 COLON-ALIGNED
          LABEL "Updated"
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     "L:" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 5.62 COL 109 WIDGET-ID 22
     "W:" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 4.67 COL 109 WIDGET-ID 20
     "Max" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 3.71 COL 128 WIDGET-ID 26
     "Min" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 3.71 COL 116 WIDGET-ID 24
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "L:" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 9.95 COL 109 WIDGET-ID 18
     "W:" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 9 COL 109 WIDGET-ID 16
     "Upcharge" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 8.05 COL 128 WIDGET-ID 14
     "Under" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 8.05 COL 116 WIDGET-ID 12
     "Restrictions" VIEW-AS TEXT
          SIZE 16.4 BY .62 AT ROW 1.95 COL 113 WIDGET-ID 10
     "Vendor Item Cost Details" VIEW-AS TEXT
          SIZE 29 BY .62 AT ROW 1.43 COL 5
     "Cost Lavels" VIEW-AS TEXT
          SIZE 16.4 BY .62 AT ROW 1.91 COL 57.8 WIDGET-ID 8
     RECT-1 AT ROW 1.1 COL 1.8
     RECT-5 AT ROW 1.57 COL 2.8
     RECT-6 AT ROW 2.14 COL 54 WIDGET-ID 4
     RECT-7 AT ROW 2.14 COL 108 WIDGET-ID 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         FONT 6.


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
         WIDTH              = 144.2.
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

/* SETTINGS FOR FILL-IN vendItemCost.customerID IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR FILL-IN vendItemCost.effectiveDate IN FRAME F-Main
   2 EXP-LABEL                                                          */ 
/* SETTINGS FOR FILL-IN vendItemCost.vendorItemID IN FRAME F-Main
   2 EXP-LABEL                                                          */ 
/* SETTINGS FOR FILL-IN vendItemCost.expirationDate IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN vendItemCost.createdID IN FRAME F-Main
   2 EXP-LABEL  no-enable                                               */
/* SETTINGS FOR FILL-IN vendItemCost.quantityMaximumOrder IN FRAME F-Main
   2 EXP-LABEL no-enable                                                */
/* SETTINGS FOR FILL-IN vendItemCost.updatedID IN FRAME F-Main
   2 EXP-LABEL  no-enable                                               */
/* SETTINGS FOR FILL-IN vendItemCost.updatedDate IN FRAME F-Main
   2 EXP-LABEL no-enable                                                */
/* SETTINGS FOR FILL-IN vendItemCost.formNo IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN vendItemCost.vendorUOM IN FRAME F-Main
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN vendItemCost.blankNo IN FRAME F-Main
   2 EXP-LABEL                                                          */

/* SETTINGS FOR FILL-IN vendItemCost.estimateNo IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR FILL-IN vendItemCost.itemID IN FRAME F-Main
   1 2                                                                  */
/* SETTINGS FOR FILL-IN vendItemCost.itemType IN FRAME F-Main
   EXP-LABEL EXP-FORMAT                                                                 */
/* SETTINGS FOR FILL-IN vendItemCost.vendorID IN FRAME F-Main
   1 2                                                                  */
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
  DEFINE VARIABLE riLookup AS RECID NO-UNDO.
  def var lv-handle as handle no-undo.
  DEFINE VARIABLE cMainField AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cAllFields AS CHARACTER NO-UNDO.
  DEFINE VARIABLE recRecordID AS RECID    NO-UNDO.

  {&methods/lValidateError.i YES}
  case FOCUS:NAME :
    when "vendorUOM" then do:
        IF vendItemCost.itemType:SCREEN-VALUE EQ "RM" THEN do:
            FIND FIRST ITEM NO-LOCK
                WHERE ITEM.company EQ cocode
                AND ITEM.i-no EQ  vendItemCost.itemID:SCREEN-VALUE NO-ERROR.
            IF AVAIL ITEM THEN
                RUN sys/ref/uom-rm.p  (ITEM.mat-type, output uom-list).
        END.
        ELSE DO:
            RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).
        END.

        run windows/l-stduom.w (cocode, uom-list, focus:screen-value, output char-val).
        if char-val ne "" then 
            focus:screen-value in frame {&frame-name} = entry(1,char-val).
    end.
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
            RUN windows/l-itmall.w (g_company, "","", vendItemCost.itemID:SCREEN-VALUE,vendItemCost.vendorID:SCREEN-VALUE, OUTPUT char-val, OUTPUT recRecordID).
            IF char-val NE "" THEN 
                ASSIGN vendItemCost.itemID:SCREEN-VALUE       = ENTRY(1,char-val) .
        END.
        ELSE DO:  /* finished good */
            RUN windows/l-itemf2.w (g_company, "", vendItemCost.itemID:screen-value,vendItemCost.vendorID:SCREEN-VALUE, OUTPUT char-val, OUTPUT recRecordID).
            IF char-val NE "" THEN
                ASSIGN vendItemCost.itemID:SCREEN-VALUE       = ENTRY(1,char-val) .
        END.
    END.
    WHEN "estimateNo" THEN DO:
        
         RUN windows/l-est.w (g_company,g_loc,vendItemCost.estimateNo:screen-value, OUTPUT char-val).
         IF char-val <> "" THEN DO:
             FIND FIRST eb WHERE STRING(RECID(eb)) = char-val NO-LOCK NO-ERROR.
             IF AVAIL eb THEN 
                 vendItemCost.estimateNo:screen-value = eb.est-no.
         END.
    END.

    otherwise do:
      lv-handle = focus:handle.
      run applhelp.p.

      if g_lookup-var ne "" then lv-handle:screen-value = g_lookup-var. 

      apply "entry" to lv-handle.
      return no-apply.
    end.
  end case.  
  {&methods/lValidateError.i NO}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.customerID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.customerID V-table-Win
ON ENTRY OF vendItemCost.customerID IN FRAME F-Main /* Customer */
DO:
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.customerID V-table-Win
ON LEAVE OF vendItemCost.customerID IN FRAME F-Main /* Customer */
DO:
    DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .
   
   IF LASTKEY <> -1 AND vendItemCost.customerID:SCREEN-VALUE <> "" THEN do:
      RUN valid-cust-no ( OUTPUT lCheckError) NO-ERROR.
      IF lCheckError THEN RETURN NO-APPLY.
   END.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.expirationDate
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


&Scoped-define SELF-NAME vendItemCost.estimateNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.estimateNo V-table-Win
ON LEAVE OF vendItemCost.estimateNo IN FRAME F-Main /* Estimate */
DO:
   DEFINE VARIABLE lCheckError AS LOGICAL NO-UNDO .

   IF LASTKEY <> -1 AND vendItemCost.estimateNo:SCREEN-VALUE NE "" THEN do:
       RUN valid-estimate(OUTPUT lCheckError)  NO-ERROR.
       IF lCheckError THEN RETURN NO-APPLY.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME vendItemCost.formNo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.formNo V-table-Win
ON LEAVE OF vendItemCost.formNo IN FRAME F-Main /* Form */
DO:  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.formNo V-table-Win
ON VALUE-CHANGED OF vendItemCost.formNo IN FRAME F-Main /* Form */
DO:   
  
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

&Scoped-define SELF-NAME vendItemCost.vendorID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL vendItemCost.vendorID V-table-Win
ON LEAVE OF vendItemCost.vendorID IN FRAME F-Main /* Item Type */
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
ON LEAVE OF vendItemCost.vendorUOM IN FRAME F-Main /* Uom */
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
  session:data-entry-return = yes.

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
  disable all with frame {&frame-name}.

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN set-panel (1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-create-record V-table-Win 
PROCEDURE local-create-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'create-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  vendItemCost.company = cocode.
  
  IF adm-adding-record THEN
  DO WITH FRAME {&FRAME-NAME}:
    /*eff-date:SCREEN-VALUE = STRING(TODAY,"99/99/9999").*/
    vendItemCost.effectiveDate:SCREEN-VALUE =  "12/31/2099"  .
    vendItemCost.effectiveDate = 12/31/2099 .
    vendItemCost.dimWidthMaximum = 99999.99.
    vendItemCost.dimLengthMaximum = 99999.99.
    vendItemCost.dimWidthOver = 99999.99.
    vendItemCost.dimLengthOver = 99999.99.
    vendItemCost.quantityMaximumOrder = 99999.99.
    vendItemCost.itemType = "FG" .
  END.

  RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"reopen-target",OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
      RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(vendItemCost)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-delete-record V-table-Win 
PROCEDURE local-delete-record :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

    IF NOT adm-new-record THEN DO:
    {custom/askdel.i}
    END.

    FOR EACH vendItemCostLevel EXCLUSIVE-LOCK
            WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID 
            BY vendItemCostLevel.vendItemCostLevelID :
        DELETE vendItemCostLevel .
    END.

    RUN dispatch IN THIS-PROCEDURE ( INPUT 'delete-record':U ) .
    /* Code placed here will execute AFTER standard behavior.    */
    
    /* task 10301314  */
        FIND CURRENT vendItemCost NO-LOCK NO-ERROR .
        IF NOT AVAIL vendItemCost THEN
            FIND FIRST vendItemCost WHERE vendItemCost.company = cocode NO-LOCK NO-ERROR.
        RUN local-display-fields.
        {methods/template/local/deleteAfter.i}       /* task 10301314  */
   
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
  /* Code placed here will execute PRIOR to standard behavior. */
  DO WITH FRAME {&FRAME-NAME}:
    RUN valid-i-no( OUTPUT lCheckError) NO-ERROR.
       IF lCheckError THEN RETURN NO-APPLY.


    RUN valid-vend-no( OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.

    IF vendItemCost.customerID:SCREEN-VALUE <> "" THEN do:
        RUN valid-cust-no ( OUTPUT lCheckError) NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
    END.

    IF vendItemCost.estimateNo:SCREEN-VALUE NE "" THEN do:
        RUN valid-estimate(OUTPUT lCheckError)  NO-ERROR.
        IF lCheckError THEN RETURN NO-APPLY.
    END.

    RUN valid-uom( OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.

    RUN valid-expdate ( OUTPUT lCheckError) NO-ERROR.
    IF lCheckError THEN RETURN NO-APPLY.
  END.
 
  disable all with frame {&frame-name}.

  IF adm-adding-record THEN lNewRecord = YES .

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'update-record':U ) .

  /* Code placed here will execute AFTER standard behavior.    */
  RUN set-panel (1).

  IF lNewRecord THEN do:
      RUN viewers/dVendCostLevel.w(ROWID(vendItemCost),"Create") .
      RUN get-link-handle IN adm-broker-hdl(THIS-PROCEDURE,"reopen-target",OUTPUT char-hdl).
      IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN
            RUN reopen-query IN WIDGET-HANDLE(char-hdl) (ROWID(vendItemCost)).
  END.
  adm-adding-record = NO .
  

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
  DEF INPUT PARAM ip-switch AS INT NO-UNDO.

  DEF VAR char-hdl AS CHAR NO-UNDO.


  RUN get-link-handle IN adm-broker-hdl  (THIS-PROCEDURE,'disable-button-target':U,OUTPUT char-hdl).
  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN do:
      IF ip-switch EQ 0 THEN 
          RUN disable-all IN WIDGET-HANDLE(char-hdl).
      ELSE
          RUN enable-all IN WIDGET-HANDLE(char-hdl) .
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

  IF AVAIL bf-vendItemCost THEN DO:
      MESSAGE "This record is a duplicate of a previous entry; please adjust." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vendItemCost.customerID.
      RETURN ERROR.
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
  IF DATE(vendItemCost.expirationDate:SCREEN-VALUE in frame {&frame-name}) LT DATE(vendItemCost.effectiveDate:SCREEN-VALUE in frame {&frame-name}) THEN DO:
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

  {methods/lValidateError.i YES}

  IF vendItemCost.itemType:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "FG"
      AND NOT CAN-FIND(FIRST itemfg WHERE itemfg.company = g_company
                       AND itemfg.i-no = vendItemCost.itemID:SCREEN-VALUE IN FRAME {&FRAME-NAME})
  THEN DO:
      MESSAGE "Invalid Item#. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vendItemCost.itemID.
      oplReturnError = YES .
  END.
  ELSE IF vendItemCost.itemType:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "RM"
      AND NOT CAN-FIND(FIRST ITEM WHERE ITEM.company = g_company
                      AND ITEM.i-no = vendItemCost.itemID:SCREEN-VALUE IN FRAME {&FRAME-NAME})
  THEN DO:
      MESSAGE "Invalid Item ID#. Try Help." VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO vendItemCost.itemID.
      oplReturnError = YES .
  END. 


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
    DEFINE OUTPUT PARAMETER opcReturnError AS LOGICAL NO-UNDO .
    DEF VAR lv-msg AS CHAR NO-UNDO.

    {methods/lValidateError.i YES}
    lv-msg = "".
    DO WITH FRAME {&FRAME-NAME}:
            IF NOT CAN-FIND(FIRST vend WHERE 
                            vend.company EQ cocode AND 
                            vend.vend-no EQ vendItemCost.vendorID:SCREEN-VALUE) THEN 
                lv-msg = TRIM(vendItemCost.vendorID:LABEL) + " " + vendItemCost.vendorID:SCREEN-VALUE +  " is invalid, try help".
        
        IF lv-msg NE "" THEN DO:
            MESSAGE 
                TRIM(lv-msg)
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "entry" TO vendItemCost.vendorID .
            opcReturnError = YES .
        END.
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
    DEF VAR lv-msg AS CHAR NO-UNDO.

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
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            APPLY "entry" TO vendItemCost.estimateNo .
            opcReturnError = YES .
        END.
    END.
    {methods/lValidateError.i NO}
    
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

      IF vendItemCost.itemType:SCREEN-VALUE EQ "RM" THEN do:
          FIND FIRST ITEM NO-LOCK
              WHERE ITEM.company EQ cocode
                AND ITEM.i-no EQ  vendItemCost.itemID:SCREEN-VALUE NO-ERROR.
          IF AVAIL ITEM THEN
              RUN sys/ref/uom-rm.p  (ITEM.mat-type, output uom-list).
      END.
      ELSE DO:
          RUN sys/ref/uom-fg.p (NO, OUTPUT uom-list).
      END.


      cUom = vendItemCost.vendorUOM:SCREEN-VALUE.

      RUN pIsValidUOM IN hdValidator (cUom, YES, OUTPUT lValid, OUTPUT cValidMessage).
      IF NOT lValid THEN DO:
          MESSAGE  cValidMessage
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          oplReturnError = YES .
          lCheckError = YES .
          APPLY "entry" TO vendItemCost.vendorUOM .
      END.

      RUN pIsValidFromList IN hdValidator ("Uom", cUom, uom-list, OUTPUT lValid, OUTPUT cValidMessage). 
      
      IF NOT lValid AND NOT lCheckError THEN DO:
          MESSAGE  cValidMessage
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          oplReturnError = YES .
          APPLY "entry" TO vendItemCost.vendorUOM .
      END.
   END.
   
{&methods/lValidateError.i NO}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
