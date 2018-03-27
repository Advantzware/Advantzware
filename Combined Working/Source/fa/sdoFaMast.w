&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          asi              PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS dTables 
/*------------------------------------------------------------------------

  File:  

  Description: from DATA.W - Template For SmartData objects in the ADM

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Modified:     February 24, 1999
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

&glob DATA-LOGIC-PROCEDURE .p

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataObject
&Scoped-define DB-AWARE yes

&Scoped-define ADM-SUPPORTED-LINKS Data-Source,Data-Target,Navigation-Target,Update-Target,Commit-Target,Filter-Target


/* Db-Required definitions. */
&IF DEFINED(DB-REQUIRED) = 0 &THEN
    &GLOBAL-DEFINE DB-REQUIRED TRUE
&ENDIF
&GLOBAL-DEFINE DB-REQUIRED-START   &IF {&DB-REQUIRED} &THEN
&GLOBAL-DEFINE DB-REQUIRED-END     &ENDIF


&Scoped-define QUERY-NAME Query-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fa-mast

/* Definitions for QUERY Query-Main                                     */
&Scoped-Define ENABLED-FIELDS  acc-dep-book acc-dep-tax1 acc-dep-tax2 asset-code asset-desc asset-status~
 auto business-% child-par cost-book Cust-no cy-dep-book cy-dep-tax-1~
 cy-dep-tax-2 date-aquired date-retired date-service dep-basis-bk~
 dep-basis-t1 dep-basis-t2 Depr-alt Document Entity-code Exch-rate fa-entity~
 gl-code group-code itc-amt itc-recap Job-no last-autodepr-date life-amt~
 life-book life-tax-1 life-tax-2 location memo method-book method-tax-1~
 method-tax-2 mth-year multiple new-used obal-acc-bk obal-acc-t1 obal-acc-t2~
 par-asset period1 period2 period3 period4 period5 period6 period7 period8~
 period9 period10 period11 period12 period13 proceeds processed profit-bk~
 profit-t1 profit-t2 purch-order# recalc ret-code ret-prd Sale-date salvage~
 sec-179 serial# Sl-conv-amt sort-code1 sort-code2 start-prd tag-nof tag-not~
 tax1-period1 tax1-period2 tax1-period3 tax1-period4 tax1-period5~
 tax1-period6 tax1-period7 tax1-period8 tax1-period9 tax1-period10~
 tax1-period11 tax1-period12 tax1-period13 tax2-period1 tax2-period2~
 tax2-period3 tax2-period4 tax2-period5 tax2-period6 tax2-period7~
 tax2-period8 tax2-period9 tax2-period10 tax2-period11 tax2-period12~
 tax2-period13 yr-of-depr
&Scoped-define ENABLED-FIELDS-IN-fa-mast acc-dep-book acc-dep-tax1 ~
acc-dep-tax2 asset-code asset-desc asset-status auto business-% child-par ~
cost-book Cust-no cy-dep-book cy-dep-tax-1 cy-dep-tax-2 date-aquired ~
date-retired date-service dep-basis-bk dep-basis-t1 dep-basis-t2 Depr-alt ~
Document Entity-code Exch-rate fa-entity gl-code group-code itc-amt ~
itc-recap Job-no last-autodepr-date life-amt life-book life-tax-1 ~
life-tax-2 location memo method-book method-tax-1 method-tax-2 mth-year ~
multiple new-used obal-acc-bk obal-acc-t1 obal-acc-t2 par-asset period1 ~
period2 period3 period4 period5 period6 period7 period8 period9 period10 ~
period11 period12 period13 proceeds processed profit-bk profit-t1 profit-t2 ~
purch-order# recalc ret-code ret-prd Sale-date salvage sec-179 serial# ~
Sl-conv-amt sort-code1 sort-code2 start-prd tag-nof tag-not tax1-period1 ~
tax1-period2 tax1-period3 tax1-period4 tax1-period5 tax1-period6 ~
tax1-period7 tax1-period8 tax1-period9 tax1-period10 tax1-period11 ~
tax1-period12 tax1-period13 tax2-period1 tax2-period2 tax2-period3 ~
tax2-period4 tax2-period5 tax2-period6 tax2-period7 tax2-period8 ~
tax2-period9 tax2-period10 tax2-period11 tax2-period12 tax2-period13 ~
yr-of-depr 
&Scoped-Define DATA-FIELDS  acc-dep-book acc-dep-tax1 acc-dep-tax2 asset-code asset-desc asset-status~
 auto business-% child-par cost-book Cust-no cy-dep-book cy-dep-tax-1~
 cy-dep-tax-2 date-aquired date-retired date-service dep-basis-bk~
 dep-basis-t1 dep-basis-t2 Depr-alt Document Entity-code Exch-rate fa-entity~
 gl-code group-code itc-amt itc-recap Job-no last-autodepr-date life-amt~
 life-book life-tax-1 life-tax-2 location memo method-book method-tax-1~
 method-tax-2 mth-year multiple new-used obal-acc-bk obal-acc-t1 obal-acc-t2~
 par-asset period1 period2 period3 period4 period5 period6 period7 period8~
 period9 period10 period11 period12 period13 proceeds processed profit-bk~
 profit-t1 profit-t2 purch-order# recalc ret-code ret-prd Sale-date salvage~
 sec-179 serial# Sl-conv-amt sort-code1 sort-code2 start-prd tag-nof tag-not~
 tax1-period1 tax1-period2 tax1-period3 tax1-period4 tax1-period5~
 tax1-period6 tax1-period7 tax1-period8 tax1-period9 tax1-period10~
 tax1-period11 tax1-period12 tax1-period13 tax2-period1 tax2-period2~
 tax2-period3 tax2-period4 tax2-period5 tax2-period6 tax2-period7~
 tax2-period8 tax2-period9 tax2-period10 tax2-period11 tax2-period12~
 tax2-period13 yr-of-depr
&Scoped-define DATA-FIELDS-IN-fa-mast acc-dep-book acc-dep-tax1 ~
acc-dep-tax2 asset-code asset-desc asset-status auto business-% child-par ~
cost-book Cust-no cy-dep-book cy-dep-tax-1 cy-dep-tax-2 date-aquired ~
date-retired date-service dep-basis-bk dep-basis-t1 dep-basis-t2 Depr-alt ~
Document Entity-code Exch-rate fa-entity gl-code group-code itc-amt ~
itc-recap Job-no last-autodepr-date life-amt life-book life-tax-1 ~
life-tax-2 location memo method-book method-tax-1 method-tax-2 mth-year ~
multiple new-used obal-acc-bk obal-acc-t1 obal-acc-t2 par-asset period1 ~
period2 period3 period4 period5 period6 period7 period8 period9 period10 ~
period11 period12 period13 proceeds processed profit-bk profit-t1 profit-t2 ~
purch-order# recalc ret-code ret-prd Sale-date salvage sec-179 serial# ~
Sl-conv-amt sort-code1 sort-code2 start-prd tag-nof tag-not tax1-period1 ~
tax1-period2 tax1-period3 tax1-period4 tax1-period5 tax1-period6 ~
tax1-period7 tax1-period8 tax1-period9 tax1-period10 tax1-period11 ~
tax1-period12 tax1-period13 tax2-period1 tax2-period2 tax2-period3 ~
tax2-period4 tax2-period5 tax2-period6 tax2-period7 tax2-period8 ~
tax2-period9 tax2-period10 tax2-period11 tax2-period12 tax2-period13 ~
yr-of-depr 
&Scoped-Define MANDATORY-FIELDS  Cust-no Entity-code Job-no
&Scoped-Define APPLICATION-SERVICE 
&Scoped-Define ASSIGN-LIST   rowObject.period1 = fa-mast.period[1]~
  rowObject.period2 = fa-mast.period[2]~
  rowObject.period3 = fa-mast.period[3]~
  rowObject.period4 = fa-mast.period[4]~
  rowObject.period5 = fa-mast.period[5]~
  rowObject.period6 = fa-mast.period[6]~
  rowObject.period7 = fa-mast.period[7]~
  rowObject.period8 = fa-mast.period[8]~
  rowObject.period9 = fa-mast.period[9]~
  rowObject.period10 = fa-mast.period[10]~
  rowObject.period11 = fa-mast.period[11]~
  rowObject.period12 = fa-mast.period[12]~
  rowObject.period13 = fa-mast.period[13]~
  rowObject.tax1-period1 = fa-mast.tax1-period[1]~
  rowObject.tax1-period2 = fa-mast.tax1-period[2]~
  rowObject.tax1-period3 = fa-mast.tax1-period[3]~
  rowObject.tax1-period4 = fa-mast.tax1-period[4]~
  rowObject.tax1-period5 = fa-mast.tax1-period[5]~
  rowObject.tax1-period6 = fa-mast.tax1-period[6]~
  rowObject.tax1-period7 = fa-mast.tax1-period[7]~
  rowObject.tax1-period8 = fa-mast.tax1-period[8]~
  rowObject.tax1-period9 = fa-mast.tax1-period[9]~
  rowObject.tax1-period10 = fa-mast.tax1-period[10]~
  rowObject.tax1-period11 = fa-mast.tax1-period[11]~
  rowObject.tax1-period12 = fa-mast.tax1-period[12]~
  rowObject.tax1-period13 = fa-mast.tax1-period[13]~
  rowObject.tax2-period1 = fa-mast.tax2-period[1]~
  rowObject.tax2-period2 = fa-mast.tax2-period[2]~
  rowObject.tax2-period3 = fa-mast.tax2-period[3]~
  rowObject.tax2-period4 = fa-mast.tax2-period[4]~
  rowObject.tax2-period5 = fa-mast.tax2-period[5]~
  rowObject.tax2-period6 = fa-mast.tax2-period[6]~
  rowObject.tax2-period7 = fa-mast.tax2-period[7]~
  rowObject.tax2-period8 = fa-mast.tax2-period[8]~
  rowObject.tax2-period9 = fa-mast.tax2-period[9]~
  rowObject.tax2-period10 = fa-mast.tax2-period[10]~
  rowObject.tax2-period11 = fa-mast.tax2-period[11]~
  rowObject.tax2-period12 = fa-mast.tax2-period[12]~
  rowObject.tax2-period13 = fa-mast.tax2-period[13]
&Scoped-Define DATA-FIELD-DEFS "fa/sdoFaMast.i"
&Scoped-Define DATA-TABLE-NO-UNDO NO-UNDO
&Scoped-define QUERY-STRING-Query-Main FOR EACH fa-mast NO-LOCK INDEXED-REPOSITION
{&DB-REQUIRED-START}
&Scoped-define OPEN-QUERY-Query-Main OPEN QUERY Query-Main FOR EACH fa-mast NO-LOCK INDEXED-REPOSITION.
{&DB-REQUIRED-END}
&Scoped-define TABLES-IN-QUERY-Query-Main fa-mast
&Scoped-define FIRST-TABLE-IN-QUERY-Query-Main fa-mast


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

{&DB-REQUIRED-START}

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Query-Main FOR 
      fa-mast SCROLLING.
&ANALYZE-RESUME
{&DB-REQUIRED-END}


/* ************************  Frame Definitions  *********************** */


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataObject
   Allow: Query
   Frames: 0
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER DB-AWARE
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
  CREATE WINDOW dTables ASSIGN
         HEIGHT             = 1.62
         WIDTH              = 46.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB dTables 
/* ************************* Included-Libraries *********************** */

{src/adm2/data.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW dTables
  VISIBLE,,RUN-PERSISTENT                                               */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK QUERY Query-Main
/* Query rebuild information for SmartDataObject Query-Main
     _TblList          = "ASI.fa-mast"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > ASI.fa-mast.acc-dep-book
"acc-dep-book" "acc-dep-book" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[2]   > ASI.fa-mast.acc-dep-tax1
"acc-dep-tax1" "acc-dep-tax1" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[3]   > ASI.fa-mast.acc-dep-tax2
"acc-dep-tax2" "acc-dep-tax2" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[4]   > ASI.fa-mast.asset-code
"asset-code" "asset-code" ? ? "character" ? ? ? ? ? ? yes ? no 11 yes ?
     _FldNameList[5]   > ASI.fa-mast.asset-desc
"asset-desc" "asset-desc" ? ? "character" ? ? ? ? ? ? yes ? no 30 yes ?
     _FldNameList[6]   > ASI.fa-mast.asset-status
"asset-status" "asset-status" ? ? "character" ? ? ? ? ? ? yes ? no 12.2 yes ?
     _FldNameList[7]   > ASI.fa-mast.auto
"auto" "auto" ? ? "logical" ? ? ? ? ? ? yes ? no 10.6 yes ?
     _FldNameList[8]   > ASI.fa-mast.business-%
"business-%" "business-%" ? ? "integer" ? ? ? ? ? ? yes ? no 11 yes ?
     _FldNameList[9]   > ASI.fa-mast.child-par
"child-par" "child-par" ? ? "logical" ? ? ? ? ? ? yes ? no 8.6 yes ?
     _FldNameList[10]   > ASI.fa-mast.cost-book
"cost-book" "cost-book" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[11]   > ASI.fa-mast.Cust-no
"Cust-no" "Cust-no" ? ? "character" ? ? ? ? ? ? yes ? yes 8 yes ?
     _FldNameList[12]   > ASI.fa-mast.cy-dep-book
"cy-dep-book" "cy-dep-book" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[13]   > ASI.fa-mast.cy-dep-tax-1
"cy-dep-tax-1" "cy-dep-tax-1" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[14]   > ASI.fa-mast.cy-dep-tax-2
"cy-dep-tax-2" "cy-dep-tax-2" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[15]   > ASI.fa-mast.date-aquired
"date-aquired" "date-aquired" ? ? "date" ? ? ? ? ? ? yes ? no 13.6 yes ?
     _FldNameList[16]   > ASI.fa-mast.date-retired
"date-retired" "date-retired" ? ? "date" ? ? ? ? ? ? yes ? no 12.2 yes ?
     _FldNameList[17]   > ASI.fa-mast.date-service
"date-service" "date-service" ? ? "date" ? ? ? ? ? ? yes ? no 12.2 yes ?
     _FldNameList[18]   > ASI.fa-mast.dep-basis-bk
"dep-basis-bk" "dep-basis-bk" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[19]   > ASI.fa-mast.dep-basis-t1
"dep-basis-t1" "dep-basis-t1" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[20]   > ASI.fa-mast.dep-basis-t2
"dep-basis-t2" "dep-basis-t2" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[21]   > ASI.fa-mast.Depr-alt
"Depr-alt" "Depr-alt" ? ? "decimal" ? ? ? ? ? ? yes ? no 19 yes ?
     _FldNameList[22]   > ASI.fa-mast.Document
"Document" "Document" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes ?
     _FldNameList[23]   > ASI.fa-mast.Entity-code
"Entity-code" "Entity-code" ? ? "character" ? ? ? ? ? ? yes ? yes 8.8 yes ?
     _FldNameList[24]   > ASI.fa-mast.Exch-rate
"Exch-rate" "Exch-rate" ? ? "decimal" ? ? ? ? ? ? yes ? no 15.2 yes ?
     _FldNameList[25]   > ASI.fa-mast.fa-entity
"fa-entity" "fa-entity" ? ? "character" ? ? ? ? ? ? yes ? no 8.8 yes ?
     _FldNameList[26]   > ASI.fa-mast.gl-code
"gl-code" "gl-code" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[27]   > ASI.fa-mast.group-code
"group-code" "group-code" ? ? "character" ? ? ? ? ? ? yes ? no 5.8 yes ?
     _FldNameList[28]   > ASI.fa-mast.itc-amt
"itc-amt" "itc-amt" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[29]   > ASI.fa-mast.itc-recap
"itc-recap" "itc-recap" ? ? "decimal" ? ? ? ? ? ? yes ? no 16.2 yes ?
     _FldNameList[30]   > ASI.fa-mast.Job-no
"Job-no" "Job-no" ? ? "character" ? ? ? ? ? ? yes ? yes 8 yes ?
     _FldNameList[31]   > ASI.fa-mast.last-autodepr-date
"last-autodepr-date" "last-autodepr-date" ? ? "date" ? ? ? ? ? ? yes ? no 19.6 yes ?
     _FldNameList[32]   > ASI.fa-mast.life-amt
"life-amt" "life-amt" ? ? "integer" ? ? ? ? ? ? yes ? no 7.8 yes ?
     _FldNameList[33]   > ASI.fa-mast.life-book
"life-book" "life-book" ? ? "integer" ? ? ? ? ? ? yes ? no 10.2 yes ?
     _FldNameList[34]   > ASI.fa-mast.life-tax-1
"life-tax-1" "life-tax-1" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ?
     _FldNameList[35]   > ASI.fa-mast.life-tax-2
"life-tax-2" "life-tax-2" ? ? "integer" ? ? ? ? ? ? yes ? no 10.4 yes ?
     _FldNameList[36]   > ASI.fa-mast.location
"location" "location" ? ? "character" ? ? ? ? ? ? yes ? no 8 yes ?
     _FldNameList[37]   > ASI.fa-mast.memo
"memo" "memo" ? ? "character" ? ? ? ? ? ? yes ? no 25 yes ?
     _FldNameList[38]   > ASI.fa-mast.method-book
"method-book" "method-book" ? ? "character" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[39]   > ASI.fa-mast.method-tax-1
"method-tax-1" "method-tax-1" ? ? "character" ? ? ? ? ? ? yes ? no 14.2 yes ?
     _FldNameList[40]   > ASI.fa-mast.method-tax-2
"method-tax-2" "method-tax-2" ? ? "character" ? ? ? ? ? ? yes ? no 14.2 yes ?
     _FldNameList[41]   > ASI.fa-mast.mth-year
"mth-year" "mth-year" ? ? "logical" ? ? ? ? ? ? yes ? no 11.2 yes ?
     _FldNameList[42]   > ASI.fa-mast.multiple
"multiple" "multiple" ? ? "integer" ? ? ? ? ? ? yes ? no 13 yes ?
     _FldNameList[43]   > ASI.fa-mast.new-used
"new-used" "new-used" ? ? "character" ? ? ? ? ? ? yes ? no 9.8 yes ?
     _FldNameList[44]   > ASI.fa-mast.obal-acc-bk
"obal-acc-bk" "obal-acc-bk" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.8 yes ?
     _FldNameList[45]   > ASI.fa-mast.obal-acc-t1
"obal-acc-t1" "obal-acc-t1" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.8 yes ?
     _FldNameList[46]   > ASI.fa-mast.obal-acc-t2
"obal-acc-t2" "obal-acc-t2" ? ? "decimal" ? ? ? ? ? ? yes ? no 18.8 yes ?
     _FldNameList[47]   > ASI.fa-mast.par-asset
"par-asset" "par-asset" ? ? "character" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[48]   > ASI.fa-mast.period[1]
"period[1]" "period1" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[49]   > ASI.fa-mast.period[2]
"period[2]" "period2" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[50]   > ASI.fa-mast.period[3]
"period[3]" "period3" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[51]   > ASI.fa-mast.period[4]
"period[4]" "period4" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[52]   > ASI.fa-mast.period[5]
"period[5]" "period5" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[53]   > ASI.fa-mast.period[6]
"period[6]" "period6" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[54]   > ASI.fa-mast.period[7]
"period[7]" "period7" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[55]   > ASI.fa-mast.period[8]
"period[8]" "period8" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[56]   > ASI.fa-mast.period[9]
"period[9]" "period9" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[57]   > ASI.fa-mast.period[10]
"period[10]" "period10" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[58]   > ASI.fa-mast.period[11]
"period[11]" "period11" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[59]   > ASI.fa-mast.period[12]
"period[12]" "period12" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[60]   > ASI.fa-mast.period[13]
"period[13]" "period13" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[61]   > ASI.fa-mast.proceeds
"proceeds" "proceeds" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[62]   > ASI.fa-mast.processed
"processed" "processed" ? ? "logical" ? ? ? ? ? ? yes ? no 9.8 yes ?
     _FldNameList[63]   > ASI.fa-mast.profit-bk
"profit-bk" "profit-bk" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[64]   > ASI.fa-mast.profit-t1
"profit-t1" "profit-t1" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[65]   > ASI.fa-mast.profit-t2
"profit-t2" "profit-t2" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[66]   > ASI.fa-mast.purch-order#
"purch-order#" "purch-order#" ? ? "character" ? ? ? ? ? ? yes ? no 10 yes ?
     _FldNameList[67]   > ASI.fa-mast.recalc
"recalc" "recalc" ? ? "logical" ? ? ? ? ? ? yes ? no 6.2 yes ?
     _FldNameList[68]   > ASI.fa-mast.ret-code
"ret-code" "ret-code" ? ? "integer" ? ? ? ? ? ? yes ? no 16.2 yes ?
     _FldNameList[69]   > ASI.fa-mast.ret-prd
"ret-prd" "ret-prd" ? ? "integer" ? ? ? ? ? ? yes ? no 13.6 yes ?
     _FldNameList[70]   > ASI.fa-mast.Sale-date
"Sale-date" "Sale-date" ? ? "date" ? ? ? ? ? ? yes ? no 9.2 yes ?
     _FldNameList[71]   > ASI.fa-mast.salvage
"salvage" "salvage" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[72]   > ASI.fa-mast.sec-179
"sec-179" "sec-179" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[73]   > ASI.fa-mast.serial#
"serial#" "serial#" ? ? "character" ? ? ? ? ? ? yes ? no 20 yes ?
     _FldNameList[74]   > ASI.fa-mast.Sl-conv-amt
"Sl-conv-amt" "Sl-conv-amt" ? ? "decimal" ? ? ? ? ? ? yes ? no 18 yes ?
     _FldNameList[75]   > ASI.fa-mast.sort-code1
"sort-code1" "sort-code1" ? ? "character" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[76]   > ASI.fa-mast.sort-code2
"sort-code2" "sort-code2" ? ? "character" ? ? ? ? ? ? yes ? no 10.8 yes ?
     _FldNameList[77]   > ASI.fa-mast.start-prd
"start-prd" "start-prd" ? ? "integer" ? ? ? ? ? ? yes ? no 8.6 yes ?
     _FldNameList[78]   > ASI.fa-mast.tag-nof
"tag-nof" "tag-nof" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[79]   > ASI.fa-mast.tag-not
"tag-not" "tag-not" ? ? "integer" ? ? ? ? ? ? yes ? no 12 yes ?
     _FldNameList[80]   > ASI.fa-mast.tax1-period[1]
"tax1-period[1]" "tax1-period1" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[81]   > ASI.fa-mast.tax1-period[2]
"tax1-period[2]" "tax1-period2" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[82]   > ASI.fa-mast.tax1-period[3]
"tax1-period[3]" "tax1-period3" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[83]   > ASI.fa-mast.tax1-period[4]
"tax1-period[4]" "tax1-period4" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[84]   > ASI.fa-mast.tax1-period[5]
"tax1-period[5]" "tax1-period5" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[85]   > ASI.fa-mast.tax1-period[6]
"tax1-period[6]" "tax1-period6" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[86]   > ASI.fa-mast.tax1-period[7]
"tax1-period[7]" "tax1-period7" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[87]   > ASI.fa-mast.tax1-period[8]
"tax1-period[8]" "tax1-period8" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[88]   > ASI.fa-mast.tax1-period[9]
"tax1-period[9]" "tax1-period9" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[89]   > ASI.fa-mast.tax1-period[10]
"tax1-period[10]" "tax1-period10" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[90]   > ASI.fa-mast.tax1-period[11]
"tax1-period[11]" "tax1-period11" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[91]   > ASI.fa-mast.tax1-period[12]
"tax1-period[12]" "tax1-period12" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[92]   > ASI.fa-mast.tax1-period[13]
"tax1-period[13]" "tax1-period13" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[93]   > ASI.fa-mast.tax2-period[1]
"tax2-period[1]" "tax2-period1" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[94]   > ASI.fa-mast.tax2-period[2]
"tax2-period[2]" "tax2-period2" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[95]   > ASI.fa-mast.tax2-period[3]
"tax2-period[3]" "tax2-period3" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[96]   > ASI.fa-mast.tax2-period[4]
"tax2-period[4]" "tax2-period4" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[97]   > ASI.fa-mast.tax2-period[5]
"tax2-period[5]" "tax2-period5" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[98]   > ASI.fa-mast.tax2-period[6]
"tax2-period[6]" "tax2-period6" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[99]   > ASI.fa-mast.tax2-period[7]
"tax2-period[7]" "tax2-period7" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[100]   > ASI.fa-mast.tax2-period[8]
"tax2-period[8]" "tax2-period8" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[101]   > ASI.fa-mast.tax2-period[9]
"tax2-period[9]" "tax2-period9" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[102]   > ASI.fa-mast.tax2-period[10]
"tax2-period[10]" "tax2-period10" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[103]   > ASI.fa-mast.tax2-period[11]
"tax2-period[11]" "tax2-period11" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[104]   > ASI.fa-mast.tax2-period[12]
"tax2-period[12]" "tax2-period12" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[105]   > ASI.fa-mast.tax2-period[13]
"tax2-period[13]" "tax2-period13" ? ? "decimal" ? ? ? ? ? ? yes ? no 14 yes ?
     _FldNameList[106]   > ASI.fa-mast.yr-of-depr
"yr-of-depr" "yr-of-depr" ? ? "integer" ? ? ? ? ? ? yes ? no 9.6 yes ?
     _Design-Parent    is WINDOW dTables @ ( 1.14 , 2.6 )
*/  /* QUERY Query-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK dTables 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI dTables  _DEFAULT-DISABLE
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
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

