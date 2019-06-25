/*------------------------------------------------------------------------
    File        : Fgcost.p
    Purpose     : DL, Mat, & GSA by Whs/Bin/Tag
    Main File   : fgrep/r-fgcost.w
    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}   
{sys/inc/var.i new shared}
{fg/rep/fg-ibtg1.i NEW SHARED}

DEFINE TEMP-TABLE ttFgCostRep NO-UNDO
    FIELD vFgCostFile AS CHAR.

DEFINE DATASET dsFgCostRep FOR ttFgCostRep.

    DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAsofdt     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginWhse  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndWhse    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginItem  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndItem    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCat   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCat     AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmIcode      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSort       AS CHAR NO-UNDO.    
    DEFINE INPUT PARAMETER prmPrint      AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER prmCustPart   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmOut        AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError       AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFgCostRep.
    IF prmUser       = ?  THEN ASSIGN prmUser       = "".
    IF prmAction     = ?  THEN ASSIGN prmAction     = "".
    IF prmAsofdt     = ?  THEN ASSIGN prmAsofdt     = "".
    IF prmBeginCust  = ?  THEN ASSIGN prmBeginCust  = "".
    IF prmEndCust    = ?  THEN ASSIGN prmEndCust    = "".
    IF prmBeginWhse  = ?  THEN ASSIGN prmBeginWhse  = "".
    IF prmEndWhse    = ?  THEN ASSIGN prmEndWhse    = "".
    IF prmBeginItem  = ?  THEN ASSIGN prmBeginItem  = "".
    IF prmEndItem    = ?  THEN ASSIGN prmEndItem    = "".
    IF prmBeginCat   = ?  THEN ASSIGN prmBeginCat   = "". 
    IF prmEndCat     = ?  THEN ASSIGN prmEndCat     = "". 
    IF prmIcode      = ?  THEN ASSIGN prmIcode      = "". 
    IF prmSort       = ?  THEN ASSIGN prmSort       = "". 
    IF prmPrint      = ?  THEN ASSIGN prmPrint      = "". 
    IF prmCustPart   = ?  THEN ASSIGN prmCustPart   = "".
    IF prmOut        = ?  THEN ASSIGN prmOut        = "". 

    DEF VAR ll-secure AS LOG NO-UNDO.
    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.

    DEF VAR prmComp AS CHAR NO-UNDO.
    DEFINE VAR custcount AS CHAR NO-UNDO.
                                                  
    DEFINE VARIABLE as-of-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/01 NO-UNDO.
    DEFINE VARIABLE begin_cat AS CHARACTER FORMAT "X(8)":U NO-UNDO.
    DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)" NO-UNDO.
    DEFINE VARIABLE begin_i-no AS CHARACTER FORMAT "X(15)":U NO-UNDO.
    DEFINE VARIABLE begin_whse AS CHARACTER FORMAT "X(5)" NO-UNDO.
    DEFINE VARIABLE end_cat AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_i-no AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE end_whse AS CHARACTER FORMAT "X(5)" INITIAL "zzzzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99 NO-UNDO.
    DEFINE VARIABLE rd_i-code AS CHARACTER INITIAL "All" NO-UNDO.
    DEFINE VARIABLE rd_msf AS CHARACTER INITIAL "Qty" NO-UNDO.
    DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Customer#" NO-UNDO.
    DEFINE VARIABLE tb_cust-pt AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
    DEF STREAM excel.   
 

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN 
    cocode = prmComp .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .

ASSIGN    
 v-today = TODAY .

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeginCust NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid begin customer for the user.....".
    RETURN.
END.

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCust  OR prmEndCust = "zzzzzzzz" ) NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Invalid end customer for the user.....".
    RETURN.
END.


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.


FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


IF prmAction = "Fgcost" THEN DO:
    ASSIGN
        as-of-date     =  DATE(prmAsofdt)
        begin_cat      =  prmBeginCat               
        begin_cust-no  =  prmBeginCust                 
        begin_i-no     =  prmBeginItem                
        begin_whse     =  prmBeginWhse                 
        end_cat        =  prmEndCat               
        end_cust-no    =  prmEndCust                 
        end_i-no       =  prmEndItem                
        end_whse       =  prmEndWhse                  
        rd_i-code      =  prmIcode                   
        rd_msf         =  prmPrint                     
        rd_sort        =  prmSort  
        .        

    ASSIGN                                                                   
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE            
        tb_cust-pt   = IF prmCustPart = "yes" THEN TRUE ELSE FALSE. 

    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "fgcost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME)  +".csv".  
        excel-file   = "fgcost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME)  + ".csv".  

        vtextfile = "fgcost" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME)  + ".txt".  
              
    run run-report.

    IF  tb_excel  THEN  DO:
        CREATE ttFgCostRep.
        ASSIGN ttFgCostRep.vFgCostFile = excel-file.
    END.
    ELSE DO:
        CREATE ttFgCostRep.
        ASSIGN ttFgCostRep.vFgCostFile = vtextfile.
    END.
END.


/*********************************************************************************************/
PROCEDURE run-report :
{sys/form/r-topwas.f}

DEF VAR excelheader AS CHAR NO-UNDO.

form header
     "        "
     "               "
     "                         "
     "     "
     "        "
     "        "
     "         "
     v-label1[4]
     "COST"
     v-label1[1]
     v-label2[1]
     v-label2[2]
     "       GS&A"
     "       GS&A"
     "           "
     skip
     "CUSTOMER"
     "ITEM #         "
     "DESCRIPTION              "
     "WHSE "
     "BIN     "
     "TAG #   "
     "JOB #    "
     " ON HAND"
     "UOM "
     v-label1[2]
     v-label2[3]
     v-label2[4]
     " LABOR COST"
     "   MAT COST"
     " TOTAL COST"
     skip
     "--------"
     "---------------"
     "-------------------------"
     "-----"
     "--------"
     "--------"
     "---------"
     "--------"
     "----"
     v-label1[3]
     v-label2[5]
     v-label2[6]
     "-----------"
     "-----------"
     "-----------"

    with frame r-top1 row 1 column 1 stream-io width 183
         no-labels no-box no-underline page-top.

form header
     "        "
     "               "
     "               "
     "                         "
     "     "
     "        "
     "        "
     "         "
     v-label1[4]
     "COST"
     v-label1[1]
     v-label2[1]
     v-label2[2]
     "       GS&A"
     "       GS&A"
     "           "
     skip
     "CUSTOMER"
     "ITEM #         "
     "CUST PART #    "
     "DESCRIPTION              "
     "WHSE "
     "BIN     "
     "TAG #   "
     "JOB #    "
     " ON HAND"
     "UOM "
     v-label1[2]
     v-label2[3]
     v-label2[4]
     " LABOR COST"
     "   MAT COST"
     " TOTAL COST"
     skip
     "--------"
     "---------------"
     "---------------"
     "-------------------------"
     "-----"
     "--------"
     "--------"
     "---------"
     "--------"
     "----"
     v-label1[3]
     v-label2[5]
     v-label2[6]
     "-----------"
     "-----------"
     "-----------"

    with frame r-top2 row 1 column 1 stream-io width 183
         no-labels no-box no-underline page-top.

assign
 str-tit2 = "DL, Mat, & GSA by Whs/Bin/Tag"
 {sys/inc/ctrtext.i str-tit2 112}

 vdat           = as-of-date
 as-of-day_str  = as-of-day_str + STRING(vdat,"99/99/99")
 fcus           = begin_cust-no
 v-loc[1]       = begin_whse
 v-loc[2]       = end_whse
 tcus           = end_cust-no
 fino           = begin_i-no
 tino           = end_i-no
 fcat           = begin_cat
 tcat           = END_cat
 v-type         = SUBSTR(rd_i-code,1,1)
 v-sort-by-cust = SUBSTR(rd_sort,1,2)
 zbal           = NO
 v-custown      = NO
 v-prt-c        = YES
 v-dl-mat       = YES
 v-prt-p        = NO
 v-prt-cpn      = tb_cust-pt
 v-prt-po       = NO
 v-prt-arqty    = NO
 v-prt-msf      = rd_msf EQ "MSF"
 v-subt         = NO
 v-fgprice      = YES
    
 v-tot-qty      = 0
 v-tot-cst      = 0
 v-tot-ext      = 0
 v-tot-gsl      = 0
 v-tot-gsm      = 0.

IF v-prt-c THEN DO: 
  /*IF NOT ll-secure THEN RUN sys/ref/d-passwd.w (3, OUTPUT ll-secure).*/

  ASSIGN
   ll-secure = YES
   v-prt-c = ll-secure
   v-prt-p = (v-prt-c and v-dl-mat) or (v-prt-p and not v-dl-mat).
END.

SESSION:SET-WAIT-STATE ("general").

if v-prt-c then do:
  assign
   v-label1[2] = "UOM COST"
   v-label1[3] = "--------"
   v-label2[5] = "-----------".
       
  if v-dl-mat then
    assign
     v-label2[1] = "     DIRECT"
     v-label2[3] = " LABOR COST".
  else
    assign
     v-label2[1] = "      TOTAL"
     v-label2[3] = "       COST".
end.    
       
if v-prt-p then do:
  v-label2[6] = "-----------".

  if v-dl-mat then
    assign
     v-label2[2] = "   MATERIAL"
     v-label2[4] = "       COST".
  else
    assign
     v-label2[2] = "    SELLING"
     v-label2[4] = "      VALUE".
end.

IF v-prt-msf THEN
  ASSIGN
   v-label1[4] = "     MSF"
   v-qoh-f     = "->>9.999".
ELSE
  ASSIGN
   v-label1[4] = "QUANTITY"
   v-qoh-f     = "->>>,>>9".

/*{sys/inc/print1.i}*/

  if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign list-name = tmp-dir + vTextFile
       init-dir = tmp-dir.
  {sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then do:
  run show-param.
  PAGE.
END. */

VIEW FRAME r-top.

if v-prt-cpn then VIEW frame r-top2.
             else VIEW frame r-top1.

/* rdb 02/05/07  01090713 */
excelheader = "".
IF tb_excel THEN DO:

  IF v-prt-cpn THEN  /* frame r-top1 */
    excelheader = "CUSTOMER,ITEM#,CUST PART#,DESC,WHSE,BIN,TAG#,JOB#,".
  ELSE /* frame r-top2 */
    excelheader = "CUSTOMER,ITEM#,DESC,WHSE,BIN,TAG#,JOB#,".

  excelheader = IF v-label1[4] NE "" 
                THEN excelheader + TRIM(v-label1[4]) + " ON HAND,"
                ELSE excelheader + "ON HAND,".

  excelheader =  excelheader + "COST UOM,".

  IF TRIM(v-label1[1]) + TRIM(v-label1[2]) NE "" THEN
    excelheader = excelheader + 
                  TRIM(v-label1[1]) + " " + TRIM(v-label1[2]) + ",".
  ELSE
    excelheader = excelheader + ",".

  IF TRIM(v-label2[1]) + TRIM(v-label2[3]) NE "" THEN
    excelheader = excelheader + 
                  TRIM(v-label2[1]) + " " + TRIM(v-label2[3]) + ",".
  ELSE
    excelheader = excelheader + ",".      

  IF TRIM(v-label2[2]) + TRIM(v-label2[4]) NE "" THEN
    excelheader = excelheader + 
                  TRIM(v-label2[2]) + " " + TRIM(v-label2[4]) + ",".
  ELSE
    excelheader = excelheader + ",".
  
  excelheader = excelheader + "GS&A LABOR CST,GS&A MAT CST,TOTAL COST".

END.

    STATUS DEFAULT "Processing...".

    EMPTY TEMP-TABLE tt-fg-bin.
    EMPTY TEMP-TABLE tt-itemfg.

    FOR EACH itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.cust-no GE fcus
          AND itemfg.cust-no LE tcus
          AND itemfg.i-no    GE fino
          AND itemfg.i-no    LE tino
          AND itemfg.procat  GE fcat
          AND itemfg.procat  LE tcat
          AND (itemfg.i-code EQ v-type OR v-type EQ "A")
          AND LOOKUP(itemfg.cust-no, custcount) <> 0
          /*AND (vdat NE TODAY OR zbal OR itemfg.q-onh NE 0)*/
        USE-INDEX customer NO-LOCK:

      STATUS DEFAULT "Processing Customer#/FG Item#: " +
                     TRIM(itemfg.cust-no) + "/" + TRIM(itemfg.i-no).

      FOR EACH fg-bin NO-LOCK
          WHERE fg-bin.company EQ itemfg.company
            AND fg-bin.i-no    EQ itemfg.i-no
            AND fg-bin.loc     GE v-loc[1]
            AND fg-bin.loc     LE v-loc[2]:
        CREATE tt-fg-bin.
        BUFFER-COPY fg-bin TO tt-fg-bin.
      END.

      IF vdat LT TODAY THEN
      FOR EACH fg-rcpth NO-LOCK
          WHERE fg-rcpth.company    EQ itemfg.company
            AND fg-rcpth.i-no       EQ itemfg.i-no
            AND fg-rcpth.trans-date LE vdat
          USE-INDEX tran,
          EACH fg-rdtlh NO-LOCK
          WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            AND fg-rdtlh.loc       GE v-loc[1]
            AND fg-rdtlh.loc       LE v-loc[2]
          BY fg-rcpth.trans-date
          BY fg-rdtlh.trans-time:

        IF NOT CAN-FIND(FIRST tt-fg-bin
                        WHERE tt-fg-bin.company EQ fg-rcpth.company
                          AND tt-fg-bin.i-no    EQ fg-rcpth.i-no
                          AND tt-fg-bin.job-no  EQ fg-rcpth.job-no
                          AND tt-fg-bin.job-no2 EQ fg-rcpth.job-no2
                          AND tt-fg-bin.loc     EQ fg-rdtlh.loc
                          AND tt-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                          AND tt-fg-bin.tag     EQ fg-rdtlh.tag
                          AND tt-fg-bin.cust-no EQ fg-rdtlh.cust-no) THEN DO:

          CREATE tt-fg-bin.

          ASSIGN
           tt-fg-bin.company      = fg-rcpth.company
           tt-fg-bin.job-no       = fg-rcpth.job-no
           tt-fg-bin.job-no2      = fg-rcpth.job-no2
           tt-fg-bin.loc          = fg-rdtlh.loc
           tt-fg-bin.loc-bin      = fg-rdtlh.loc-bin
           tt-fg-bin.tag          = fg-rdtlh.tag
           tt-fg-bin.cust-no      = fg-rdtlh.cust-no
           tt-fg-bin.i-no         = fg-rcpth.i-no
           tt-fg-bin.aging-date   = fg-rcpth.trans-date
           tt-fg-bin.pur-uom      = itemfg.prod-uom
           tt-fg-bin.std-tot-cost = itemfg.total-std-cost
           tt-fg-bin.std-mat-cost = itemfg.std-mat-cost
           tt-fg-bin.std-lab-cost = itemfg.std-lab-cost
           tt-fg-bin.std-var-cost = itemfg.std-var-cost
           tt-fg-bin.std-fix-cost = itemfg.std-fix-cost.

          IF tt-fg-bin.case-count   LE 0 AND fg-rdtlh.qty-case     GT 0 THEN
            tt-fg-bin.case-count   = fg-rdtlh.qty-case.
          IF tt-fg-bin.units-pallet LE 0 AND fg-rdtlh.units-pallet GT 0 THEN
            tt-fg-bin.units-pallet = fg-rdtlh.units-pallet.
          IF tt-fg-bin.cases-unit   LE 0 AND fg-rdtlh.stacks-unit  GT 0 THEN
            tt-fg-bin.cases-unit   = fg-rdtlh.stacks-unit.
        END.
      END.

      FOR EACH tt-fg-bin
          WHERE tt-fg-bin.company EQ itemfg.company
            AND tt-fg-bin.i-no    EQ itemfg.i-no
            AND (v-custown OR (tt-fg-bin.loc NE "CUST" AND tt-fg-bin.cust-no EQ ""))
          USE-INDEX co-ino:

        CREATE tt-itemfg.
        BUFFER-COPY itemfg TO tt-itemfg
        ASSIGN
         tt-itemfg.row-id      = ROWID(itemfg)
         tt-itemfg.job-no      = tt-fg-bin.job-no
         tt-itemfg.job-no2     = tt-fg-bin.job-no2
         tt-itemfg.loc         = tt-fg-bin.loc
         tt-itemfg.loc-bin     = tt-fg-bin.loc-bin
         tt-itemfg.tag         = tt-fg-bin.tag
         tt-itemfg.part-cust   = STRING(tt-itemfg.part-no,"x(20)") +
                                 STRING(tt-itemfg.cust-no,"x(20)")
         tt-itemfg.loc-bin-tag = STRING(tt-itemfg.loc,"x(10)")     +
                                 STRING(tt-itemfg.loc-bin,"x(10)") +
                                 STRING(tt-itemfg.tag,"x(20)").
      END.
    END.

    CASE v-sort-by-cust:
        WHEN "Cu" THEN 
          RUN fg/rep/fg-cost1.p (excelheader,fi_file).
        WHEN "FG" THEN 
          RUN fg/rep/fg-cost2.p (excelheader,fi_file).
        WHEN "Pr" THEN 
          RUN fg/rep/fg-cost3.p (excelheader,fi_file).
        WHEN "Pa" THEN 
          RUN fg/rep/fg-cost4.p (excelheader,fi_file).
        OTHERWISE 
          RUN fg/rep/fg-cost5.p (excelheader,fi_file).
    END CASE.

    put skip(1).

    v-all-sum = v-tot-cst[3] + v-tot-ext[3] + v-tot-gsl[3] + v-tot-gsm[3].

    if v-prt-cpn then do:
      PUT "GRAND TOTALS" TO 87.

      IF v-prt-msf THEN
        PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 109.
      ELSE
        PUT v-tot-qty[3] TO 109.

      if v-prt-c then put v-tot-cst[3] format "->>>,>>9.99" to 135.
      if v-prt-p then put v-tot-ext[3] format "->>>,>>9.99" to 147.
      if v-prt-c then put v-tot-gsl[3] format "->>>,>>9.99" to 159.
      if v-prt-c then put v-tot-gsm[3] format "->>>,>>9.99" to 171.
      if v-prt-c then put v-all-sum    format "->>>,>>9.99" to 183 skip(1).
    end.

    else do:
      put "GRAND TOTALS" TO 71.

      IF v-prt-msf THEN
        PUT v-tot-qty[3] FORMAT "->>>,>>9.999" TO 93.
      ELSE
        PUT v-tot-qty[3] TO 93.

      if v-prt-c then put v-tot-cst[3] format "->>>,>>9.99" to 119.
      if v-prt-p then put v-tot-ext[3] format "->>>,>>9.99" to 131.
      if v-prt-c then put v-tot-gsl[3] format "->>>,>>9.99" to 143.
      if v-prt-c then put v-tot-gsm[3] format "->>>,>>9.99" to 155.
      if v-prt-c then put v-all-sum    format "->>>,>>9.99" to 167 skip(1).
    end.

end procedure.
