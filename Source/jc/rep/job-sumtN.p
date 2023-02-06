/* ----------------------------------------------- jc/rep/job-sum.p 03/98 JLF */
/* Job Summary Report                                                         */
/* -------------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttGrandTotal 
    FIELD totalField1 AS CHARACTER 
    FIELD totalField2 AS CHARACTER 
    FIELD totalField3 AS CHARACTER .
     
DEFINE INPUT PARAMETER v-prod   AS INTEGER.
DEFINE INPUT PARAMETER v-recid  AS RECID.
DEFINE OUTPUT PARAMETER TABLE FOR  ttGrandTotal.

{sys/inc/var.i shared}

{jc/rep/job-sum.i}

/*def var v-total   as   dec format "->>,>>>,>>9.99" no-undo.*/
DEFINE VARIABLE v-prof AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE v-com  LIKE eb.comm INIT 0 NO-UNDO.
DEFINE VARIABLE v-wt   AS DECIMAL   INIT 0 NO-UNDO.
DEFINE VARIABLE v-rate AS DECIMAL   INIT 0 NO-UNDO.
DEFINE VARIABLE c1     AS CHARACTER INIT "" NO-UNDO.
DEFINE VARIABLE c2     AS CHARACTER INIT "" NO-UNDO.
DEFINE VARIABLE dFrt   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iOrder AS INTEGER   NO-UNDO.
DEFINE STREAM excel2 .
{sys/inc/msfcalc.i}

IF v-est THEN 
DO:
    v-frate = 0.

    FIND job WHERE RECID(job) EQ v-recid NO-LOCK NO-ERROR.
    FIND FIRST est  
        WHERE est.company EQ job.company
        AND est.est-no  EQ job.est-no 
        NO-LOCK NO-ERROR.

    IF AVAILABLE est THEN
        FOR EACH job-hdr
            WHERE job-hdr.company EQ cocode
            AND job-hdr.job     EQ job.job
            AND job-hdr.job-no  EQ job.job-no
            AND job-hdr.job-no2 EQ job.job-no2
            NO-LOCK,

            FIRST itemfg
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ job-hdr.i-no
            NO-LOCK,

            FIRST eb
            WHERE eb.company    EQ job.company
            AND eb.est-no     EQ job.est-no
            AND eb.form-no    EQ job-hdr.frm
            AND (eb.blank-no  EQ job-hdr.blank-no OR
            est.est-type EQ 5                OR
            est.est-type EQ 6)
            AND eb.chg-method EQ "P"
            NO-LOCK,

            FIRST work-item
            WHERE work-item.form-no EQ job-hdr.frm
            AND work-item.i-no    EQ job-hdr.i-no
            NO-LOCK:  
            iOrder = 0.
            iOrder = INTEGER(job.job-no) NO-ERROR.
            FIND FIRST oe-boll WHERE oe-boll.company EQ job.company
                AND oe-boll.ord-no EQ iOrder
                AND oe-boll.i-no   EQ job-hdr.i-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE oe-boll OR iOrder EQ 0 THEN 
            DO:
                /* get freight from boll's */
    
                FOR EACH oe-boll WHERE oe-boll.company EQ job.company
                    AND oe-boll.ord-no EQ iOrder
                    AND oe-boll.i-no   EQ job-hdr.i-no
                    NO-LOCK,
                    FIRST oe-bolh 
                    WHERE oe-bolh.company = oe-boll.company 
                    AND oe-bolh.b-no = oe-boll.b-no                      
                    NO-LOCK.
    
                    v-frate = v-frate + oe-boll.freight.
                END.
            END.
            ELSE 
            DO:
                /* Original Code */
                v-wt = itemfg.weight-100 * work-item.qty-prod / 100.
    
                IF eb.fr-out-c NE 0 THEN
                    v-frate = v-frate + (eb.fr-out-c * (v-wt / 100)).
    
                ELSE
                    IF eb.fr-out-m NE 0 THEN
                        v-frate = v-frate + (eb.fr-out-m * (work-item.qty-prod / 1000)).
    
                    ELSE 
                    DO:
                        v-rate = 0.
    
                        RELEASE carr-mtx.
    
                        FIND FIRST carrier
                            WHERE carrier.company EQ cocode
                            AND carrier.loc     EQ locode
                            AND carrier.carrier EQ eb.carrier
                            NO-LOCK NO-ERROR.
            
                        IF AVAILABLE carrier THEN
                            FIND FIRST carr-mtx
                                WHERE carr-mtx.company  EQ cocode
                                AND carr-mtx.loc      EQ locode
                                AND carr-mtx.carrier  EQ carrier.carrier
                                AND carr-mtx.del-zone EQ eb.dest-code
                                NO-LOCK NO-ERROR.
                   
                        IF AVAILABLE carr-mtx THEN 
                        DO:
                            IF carrier.by-pallet NE ? THEN 
                            DO:
                                RUN util/ucarrier.p (RECID(carrier)).
                                FIND CURRENT carrier NO-LOCK NO-ERROR.
                            END.
    
                            IF carrier.chg-method EQ "P" THEN 
                            DO:
                                v-wt = work-item.qty-prod / (eb.cas-cnt * eb.cas-pal).
              
                                {sys/inc/roundup.i v-wt}
                            END.
            
                            ELSE
                                IF carrier.chg-method EQ "M" THEN
                                    v-wt = (IF v-corr THEN (work-item.qty-prod * itemfg.t-sqin * .007)
                                    ELSE (work-item.qty-prod * itemfg.t-sqin / 144)) /
                                        1000.
            
                            DO i = 1 TO 10:
                                IF carr-mtx.weight[i] GE v-wt THEN 
                                DO:
                                    v-rate = carr-mtx.rate[i].
                                    LEAVE.
                                END.
                            END.
         
                            v-rate = v-rate * v-wt / (IF carrier.chg-method EQ "W" THEN 100 ELSE 1).
            
                            IF v-rate LT carr-mtx.min-rate THEN v-rate = carr-mtx.min-rate.
                        END. /* avail carr-mtx */
          
                        IF v-rate NE ? THEN v-frate = v-frate + v-rate.
                    END. /* eb.fr-out is zero */
            END. /* not avail an oe-boll */
        END. /* each job-hdr */

    FIND FIRST eb
        WHERE eb.company EQ job.company
        AND eb.est-no  EQ job.est-no
        AND eb.form-no  NE 0
        NO-LOCK NO-ERROR.
    IF AVAILABLE eb THEN v-com = eb.comm.
    v-comm  = v-sale * (v-com / 100).
END.
         
/***  Print Grand Totals and Profit ***/
ASSIGN
    v-lab-m = v-labor * v-lab-mrk / 100
    v-total = v-mater + v-prep + v-labor + v-lab-m + v-comm + v-frate
    v-prof  = (v-sale + v-misc-prep) - v-total.

IF v-charge-prep THEN 
    ASSIGN c1 = "PREP CHARGES: "  + STRING(v-misc-prep,"->,>>>,>>9.99") 
                                  + STRING(v-misc-prep  / (v-prod / 1000),"->,>>>,>>9.99")
        c2 = "      TOTALS:"   + STRING((v-misc-prep + v-sale),"->>,>>>,>>9.99")   
                                  + STRING((v-misc-prep  + v-sale) / (v-prod / 1000),"->,>>>,>>9.99").       
ELSE 
    ASSIGN c1 = "" c2 = "".

PUT SKIP(1)
    "GRAND TOTALS           COST        PER/M"                  SKIP
    "    MATERIAL:"
    v-mater
    v-mater / (v-prod / 1000) FORMAT "->,>>>,>>9.99"            SKIP
    "        PREP:"
    v-prep
    v-prep  / (v-prod / 1000) FORMAT "->,>>>,>>9.99"            SKIP
    "       LABOR:"
    v-labor
    v-labor / (v-prod / 1000) FORMAT "->,>>>,>>9.99"            SKIP
    "  LAB MARKUP:"
    v-lab-m
    v-lab-m / (v-prod / 1000) FORMAT "->,>>>,>>9.99"            SKIP
    "  COMMISSION:"
    v-comm
    v-comm  / (v-prod / 1000) FORMAT "->,>>>,>>9.99"            SKIP
    "     FREIGHT:"
    v-frate
    v-frate / (v-prod / 1000) FORMAT "->,>>>,>>9.99"            SKIP
    SKIP(1)
    "  TOTAL COST:"
    v-total
    v-total / (v-prod / 1000) FORMAT "->,>>>,>>9.99"            SKIP
    SKIP(2)
    "                                   PER/M"                  SKIP
    " BOXES SALES:"
    v-sale
    v-sale  / (v-prod / 1000) FORMAT "->,>>>,>>9.99"            SKIP
    c1 FORM "x(80)" SKIP                                             
    c2 FORM "x(80)" SKIP
    SKIP(1)
    "  TOTAL COST:"
    v-total
    v-total / (v-prod / 1000) FORMAT "->,>>>,>>9.99"            SKIP
    SKIP(1)
    "GROSS MARGIN:"
    v-prof
    v-prof  / (v-sale + v-misc-prep) * 100      FORMAT "->>>>,>>9.99%"          SKIP.


DO i = 1 TO 14:
    CREATE ttGrandTotal.
    IF i EQ 1 THEN
        ASSIGN
            ttGrandTotal.totalField1 = "GRAND TOTALS" 
            ttGrandTotal.totalField2 = " COST"
            ttGrandTotal.totalField3 = "PER/M".    
    ELSE IF i EQ 2 THEN
            ASSIGN
                ttGrandTotal.totalField1 = "MATERIAL:" 
                ttGrandTotal.totalField2 = STRING(v-mater)
                ttGrandTotal.totalField3 = STRING(v-mater / (v-prod / 1000)).
        ELSE IF i EQ 3 THEN
                ASSIGN
                    ttGrandTotal.totalField1 = "PREP:" 
                    ttGrandTotal.totalField2 = STRING(v-prep)
                    ttGrandTotal.totalField3 = STRING(v-prep  / (v-prod / 1000)).
            ELSE IF i EQ 4 THEN
                    ASSIGN
                        ttGrandTotal.totalField1 = "LABOR:" 
                        ttGrandTotal.totalField2 = STRING(v-labor)
                        ttGrandTotal.totalField3 = STRING(v-labor / (v-prod / 1000)).
                ELSE IF i EQ 5 THEN
                        ASSIGN
                            ttGrandTotal.totalField1 = "LAB MARKUP:" 
                            ttGrandTotal.totalField2 = STRING(v-lab-m)
                            ttGrandTotal.totalField3 = STRING(v-lab-m / (v-prod / 1000)).
                    ELSE IF i EQ 6 THEN
                            ASSIGN
                                ttGrandTotal.totalField1 = "COMMISSION:" 
                                ttGrandTotal.totalField2 = STRING(v-comm)
                                ttGrandTotal.totalField3 = STRING(v-comm  / (v-prod / 1000)).
                        ELSE IF i EQ 7 THEN
                                ASSIGN
                                    ttGrandTotal.totalField1 = "FREIGHT:" 
                                    ttGrandTotal.totalField2 = STRING(v-frate)
                                    ttGrandTotal.totalField3 = STRING(v-frate / (v-prod / 1000)).
                            ELSE IF i EQ 8 THEN
                                    ASSIGN
                                        ttGrandTotal.totalField1 = "TOTAL COST:" 
                                        ttGrandTotal.totalField2 = STRING(v-total)
                                        ttGrandTotal.totalField3 = STRING(v-total / (v-prod / 1000)).
                                ELSE IF i EQ 9 THEN
                                        ASSIGN
                                            ttGrandTotal.totalField1 = "" 
                                            ttGrandTotal.totalField2 = ""
                                            ttGrandTotal.totalField3 = "PER/M".
                                    ELSE IF i EQ 10 THEN
                                            ASSIGN
                                                ttGrandTotal.totalField1 = "BOXES SALES:" 
                                                ttGrandTotal.totalField2 = STRING(v-sale)
                                                ttGrandTotal.totalField3 = STRING(v-sale  / (v-prod / 1000)).  
                                        ELSE IF i EQ 11 THEN
                                                ASSIGN
                                                    ttGrandTotal.totalField1 = c1 
                                                    ttGrandTotal.totalField2 = ""
                                                    ttGrandTotal.totalField3 = "".
                                            ELSE IF i EQ 12 THEN
                                                    ASSIGN
                                                        ttGrandTotal.totalField1 = c2 
                                                        ttGrandTotal.totalField2 = ""
                                                        ttGrandTotal.totalField3 = "". 
                                                ELSE IF i EQ 13 THEN
                                                        ASSIGN
                                                            ttGrandTotal.totalField1 = "TOTAL COST:"
                                                            ttGrandTotal.totalField2 = STRING(v-total)
                                                            ttGrandTotal.totalField3 = STRING(v-total / (v-prod / 1000)). 
                                                    ELSE IF i EQ 14 THEN
                                                            ASSIGN
                                                                ttGrandTotal.totalField1 = "GROSS MARGIN:" 
                                                                ttGrandTotal.totalField2 = STRING(v-prof)
                                                                ttGrandTotal.totalField3 = STRING(v-prof  / (v-sale + v-misc-prep) * 100).     
END.
    
