

/*------------------------------------------------------------------------
    File        : PrepFileRep.p
    Purpose     : Prep File
    Main File   : cerep\r-prep.w
    Syntax      :

    Description : Return a Dataset of Request For Prep

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

{custom/xprint.i}
    
{sys/inc/var.i new shared}

    DEFINE TEMP-TABLE ttPrepFileReport NO-UNDO
        FIELD vFile AS CHAR
        FIELD dflktzskj AS CHAR
        .

    DEFINE DATASET dsPrepFileReport FOR ttPrepFileReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginPrep       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEndPrep         AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintDetail     AS CHARACTER NO-UNDO.       
    DEFINE INPUT PARAMETER prmOut             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsPrepFileReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.


    IF  prmUser         = ?        THEN ASSIGN     prmUser = "".
    IF  prmAction       = ?        THEN ASSIGN     prmAction = "".
    IF  prmBeginPrep    = ?        THEN ASSIGN     prmBeginPrep = "".
    IF  prmEndPrep      = ?        THEN ASSIGN     prmEndPrep = "".
    IF  prmPrintDetail  = ?        THEN ASSIGN     prmPrintDetail = "".
    IF  prmOut          = ?        THEN ASSIGN     prmOut = "".

    DEFINE VARIABLE excel-file       AS CHAR NO-UNDO.
    DEFINE VARIABLE vtextfile        AS CHAR NO-UNDO.
    DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
    DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
    def var list-name as cha no-undo.
    DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
    def NEW SHARED var g_company  as   char  format "x(3)"  no-undo.


    DEF VAR prmComp AS CHAR NO-UNDO.
    DEFINE VARIABLE begin_prep AS CHARACTER FORMAT "x(15)" NO-UNDO.
    DEFINE VARIABLE end_prep AS CHARACTER FORMAT "x(15)" INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
    DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
    DEFINE VARIABLE tb_excel AS LOGICAL INITIAL yes NO-UNDO.
    DEFINE VARIABLE tb_cust-name AS LOGICAL INITIAL no NO-UNDO.
    DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99.  

    DEF VAR v_exclhdr1 AS CHAR                NO-UNDO.
    DEF VAR v_exclhdr2 AS CHAR                NO-UNDO.
    DEF VAR v_custnum  AS CHAR FORMAT "x(35)" NO-UNDO.
    DEF VAR v_ML       AS CHAR INIT "M"       NO-UNDO.
    DEF VAR v_dfault   AS CHAR INIT "Y"       NO-UNDO.
    DEF STREAM excel.



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

    assign
 cocode = prmComp.


 FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
 
 ASSIGN    
 g_company = cocode
 v-today = TODAY . 

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
 IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


 IF prmAction = "PrepFileRep" THEN DO:
    ASSIGN 
        begin_prep  = prmBeginPrep
        end_prep    = prmEndPrep . 

    ASSIGN
        tb_excel      = IF prmOut = "yes" THEN TRUE ELSE FALSE
        tb_cust-name  = IF prmPrintDetail = "yes" THEN TRUE ELSE FALSE  
        .  
  
    ASSIGN
        init-dir    = v-webrootpath
        fi_file = init-dir + "prepfile" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  
        excel-file   = "prepfile" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".csv".  

        vtextfile = "prepfile" +
            STRING(YEAR(v-today),"9999")
                   + STRING(MONTH(v-today),"99")
                   + STRING(DAY(v-today),"99") + STRING(TIME) + ".txt". 


    ASSIGN
    v_exclhdr1 = "Code,Desc.,Customer Name,Warehouse,Bin Location,Disposal Date,Last Used Date,Markup,Cost,M/L,Amtz,M Type,Use w/ Est,UOM,SIMON,C Type,Account No,Cad #,File #"
    v_exclhdr2 = "Code,Description,Markup,Cost,M/L,Amtz,Mat'l Type,Use in all E,UOM,SIMON,Account No".

    IF tb_excel THEN DO:
      OUTPUT STREAM excel TO VALUE(fi_file).
      
      IF tb_cust-name 
        THEN
          PUT STREAM excel UNFORMATTED
            v_exclhdr1
           SKIP.
        ELSE
          PUT STREAM excel UNFORMATTED
            v_exclhdr2
           SKIP.

    END.

    IF tb_cust-name THEN run run-report.
    ELSE RUN run-report-sum.
              
    /*run run-report.*/

    IF  tb_excel  THEN  DO:
        CREATE ttPrepFileReport.
        ASSIGN ttPrepFileReport.vFile = excel-file.
    END.
    ELSE DO:
        CREATE ttPrepFileReport.
        ASSIGN ttPrepFileReport.vFile = vtextfile.
    END.
END.

  
/*****************************************PROCEDURE run-report :*****************************************************/
 PROCEDURE run-report :
def var ii like i no-undo.

/* gdm - 10130803 */
DEF VAR v_exclhdr1 AS CHAR                NO-UNDO.
DEF VAR v_exclhdr2 AS CHAR                NO-UNDO.
DEF VAR v_custnum  AS CHAR FORMAT "x(35)" NO-UNDO.

{sys/form/r-topl.f}

ASSIGN str-tit2 = "Preparation Code List"
         {sys/inc/ctrtext.i str-tit2 142}.

/*{sys/inc/print1.i}*/

    if tmp-dir = "" then tmp-dir = v-webrootpath .
    assign 
        list-name = tmp-dir + vTextFile
        init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

display "" with frame r-top.

SESSION:SET-WAIT-STATE ("general").

FOR EACH prep WHERE prep.company = g_company
                 AND prep.CODE >= begin_prep
                 AND prep.CODE <= END_prep NO-LOCK BY prep.CODE:

   FIND FIRST reftable NO-LOCK
        WHERE reftable.reftable EQ "PREPCADFILE"
          AND reftable.rec_key  EQ prep.rec_key NO-ERROR.     

    ASSIGN v_ML     = IF prep.ml = TRUE THEN "M" ELSE "L"
           v_dfault = IF prep.dfault = TRUE THEN "Y" ELSE "N".

    IF tb_cust-name THEN DO:
    
       DISPLAY prep.code FORMAT "x(15)"
               prep.dscr
               prep.cust-name 
               prep.loc       
               prep.loc-bin   LABEL "Bin"
               prep.disposal-date COLUMN-LABEL "Disposal!Date"
               prep.last-date    COLUMN-LABEL "Last Used!Date"  
               prep.mkup
               prep.cost
               prep.ml
               prep.amtz
               prep.mat-type COLUMN-LABEL 'M!Type'
               prep.dfault COLUMN-LABEL 'Use!w/ Est'
               prep.uom
               prep.simon
               prep.cost-type COLUMN-LABEL 'C!Type'
               prep.actnum 
               reftable.code  WHEN AVAIL reftable COLUMN-LABEL 'Cad #' FORMAT "x(15)"
               reftable.code2 WHEN AVAIL reftable COLUMN-LABEL 'File #' FORMAT "x(15)"              
           WITH STREAM-IO FRAME prep-det DOWN WIDTH 230 NO-BOX.

       /* gdm - 10130803 */
       IF tb_excel THEN
           PUT STREAM excel UNFORMATTED
              '"' prep.code              '",'
              '"' prep.dscr          '",'    
              '"' prep.cust-name     '",'
              '"' prep.loc           '",'
              '"' prep.loc-bin       '",'
              '"' prep.disposal-date '",'
              '"' prep.last-date     '",'
              '"' prep.mkup          '",'    
              '"' prep.cost          '",'    
              '"' v_ML               '",'    
              '"' prep.amtz          '",'    
              '"' prep.mat-type      '",'
              '"' v_dfault           '",'
              '"' prep.uom           '",'    
              '"' prep.simon         '",'    
              '"' prep.cost-type     '",'
              '"' prep.actnum        '",'
              '"' IF AVAIL reftable THEN reftable.CODE ELSE ""      '",'
              '"' IF AVAIL reftable THEN reftable.code2 ELSE ""     '"'
             SKIP.
                


     /*ELSE DISPLAY prep.code
               prep.dscr
               prep.mkup
               prep.cost
               prep.ml
               prep.amtz
               prep.mat-type COLUMN-LABEL 'MType'
               prep.dfault COLUMN-LABEL 'Use w/ Est'
               prep.uom
               prep.simon
               prep.cost-type COLUMN-LABEL 'CType'
               prep.actnum WITH STREAM-IO FRAME prep DOWN WIDTH 132 NO-BOX.
*/
      END.          
      ELSE DO:

         /* gdm - 10130803*/
         IF tb_excel THEN DO:
             FIND FIRST account NO-LOCK
                 WHERE account.company = cocode 
                   AND account.actnum = prep.actnum NO-ERROR.
             
             ASSIGN
                 v_custnum = ""
                 v_custnum = prep.actnum + " " + account.dscr.            

             PUT STREAM excel UNFORMATTED
               '"' prep.code     '",' 
               '"' prep.dscr     '",' 
               '"' prep.mkup     '",' 
               '"' prep.cost     '",' 
               '"' v_ML          '",' 
               '"' prep.amtz     '",' 
               '"' prep.mat-type '",' 
               '"' prep.dfault   '",' 
               '"' prep.uom      '",' 
               '"' prep.simon    '",' 
               '"' v_custnum     '"' 
              SKIP.
         END. /* IF tb_excel */

         DO WITH FRAME prep 3 COLUMNS STREAM-IO:
             display skip(2).
             {ce/prep.v}.
             DOWN.
         END.

      END.    
END.

end procedure.


/*----------------------------------------------------------------------------------------------------------*/
PROCEDURE run-report-sum :
    
def var ii like i no-undo.

{sys/form/r-top.f}

ASSIGN str-tit2 = "Preparation Code List"
         {sys/inc/ctrtext.i str-tit2 56}.

/*{sys/inc/print1.i}*/


if tmp-dir = "" then tmp-dir = v-webrootpath .
assign 
    list-name = tmp-dir + vTextFile
    init-dir = tmp-dir.

{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/

display "" with frame r-top.

SESSION:SET-WAIT-STATE ("general").

FOR EACH prep WHERE prep.company = g_company
                 AND prep.CODE >= begin_prep
                 AND prep.CODE <= END_prep NO-LOCK BY prep.CODE:
    

    /* gdm - 10130803*/
    ASSIGN v_ML     = IF prep.ml = TRUE THEN "M" ELSE "L"
           v_dfault = IF prep.dfault = TRUE THEN "Y" ELSE "N".

    IF tb_excel THEN DO:
        FIND FIRST account NO-LOCK
            WHERE account.company = cocode 
              AND account.actnum = prep.actnum NO-ERROR.
        
        ASSIGN
            v_custnum = ""
            v_custnum = prep.actnum + " " + account.dscr.            

        PUT STREAM excel UNFORMATTED
           '"' prep.code     '",' 
           '"' prep.dscr     '",' 
           '"' prep.mkup     '",' 
           '"' prep.cost     '",' 
           '"' v_ML          '",' 
           '"' prep.amtz     '",' 
           '"' prep.mat-type '",' 
           '"' v_dfault      '",' 
           '"' prep.uom      '",' 
           '"' prep.simon    '",' 
           '"' v_custnum     '"' 
         SKIP.
    END. /* IF tb_excel */

     DO WITH FRAME prep 3 COLUMNS STREAM-IO:
          display skip(2).
          {ce/prep.v}.
          DOWN.
     END.
     
END.

END PROCEDURE.
