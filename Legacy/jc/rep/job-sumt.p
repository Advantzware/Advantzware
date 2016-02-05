/* ----------------------------------------------- jc/rep/job-sum.p 03/98 JLF */
/* Job Summary Report                                                         */
/* -------------------------------------------------------------------------- */

def input parameter v-prod   as int.
def input parameter v-recid  as recid.

{sys/inc/var.i shared}

{jc/rep/job-sum.i}

/*def var v-total   as   dec format "->>,>>>,>>9.99" no-undo.*/
def var v-prof    as   dec format "->>,>>>,>>9.99" no-undo.
def var v-com     like eb.comm init 0              no-undo.
def var v-wt      as   dec init 0                  no-undo.
def var v-rate    as   dec init 0                  no-undo.
DEF VAR c1        AS   CHAR INIT ""                NO-UNDO.
DEF VAR c2        AS   CHAR INIT ""                NO-UNDO.
DEF VAR dFrt      AS   DEC                         NO-UNDO.
DEF VAR iOrder    AS   INT                         NO-UNDO.

{sys/inc/msfcalc.i}

if v-est then do:
  v-frate = 0.

  find job where recid(job) eq v-recid no-lock no-error.
  find first est  
      where est.company EQ job.company
        AND est.est-no  EQ job.est-no 
      no-lock no-error.

  if avail est then
  for each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
      no-lock,

      first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq job-hdr.i-no
      no-lock,

      first eb
      where eb.company    EQ job.company
        AND eb.est-no     EQ job.est-no
        and eb.form-no    EQ job-hdr.frm
        and (eb.blank-no  EQ job-hdr.blank-no OR
             est.est-type EQ 5                OR
             est.est-type EQ 6)
        and eb.chg-method EQ "P"
      no-lock,

      first work-item
      where work-item.form-no eq job-hdr.frm
        and work-item.i-no    eq job-hdr.i-no
      no-lock:  
      iOrder = 0.
      iOrder = INTEGER(job.job-no) NO-ERROR.
    FIND FIRST oe-boll WHERE oe-boll.company EQ job.company
        AND oe-boll.ord-no EQ iOrder
        AND oe-boll.i-no   EQ job-hdr.i-no
        NO-LOCK NO-ERROR.
    IF AVAIL oe-boll OR iOrder EQ 0 THEN DO:
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
    ELSE DO:
        /* Original Code */
        v-wt = itemfg.weight-100 * work-item.qty-prod / 100.
    
        if eb.fr-out-c ne 0 then
          v-frate = v-frate + (eb.fr-out-c * (v-wt / 100)).
    
        else
        if eb.fr-out-m ne 0 then
          v-frate = v-frate + (eb.fr-out-m * (work-item.qty-prod / 1000)).
    
        else do:
          v-rate = 0.
    
          release carr-mtx.
    
          find first carrier
              where carrier.company eq cocode
                and carrier.loc     eq locode
                and carrier.carrier eq eb.carrier
              no-lock no-error.
            
          if avail carrier then
          find first carr-mtx
              where carr-mtx.company  eq cocode
                and carr-mtx.loc      eq locode
                and carr-mtx.carrier  eq carrier.carrier
                and carr-mtx.del-zone eq eb.dest-code
              no-lock no-error.
                   
          if avail carr-mtx then do:
            if carrier.by-pallet ne ? THEN DO:
              run util/ucarrier.p (recid(carrier)).
              FIND CURRENT carrier NO-LOCK NO-ERROR.
            END.
    
            if carrier.chg-method eq "P" then do:
              v-wt = work-item.qty-prod / (eb.cas-cnt * eb.cas-pal).
              
              {sys/inc/roundup.i v-wt}
            end.
            
            else
            if carrier.chg-method eq "M" then
              v-wt = (if v-corr then (work-item.qty-prod * itemfg.t-sqin * .007)
                                else (work-item.qty-prod * itemfg.t-sqin / 144)) /
                     1000.
            
            do i = 1 to 10:
              if carr-mtx.weight[i] ge v-wt then do:
                v-rate = carr-mtx.rate[i].
                leave.
              end.
            end.
         
            v-rate = v-rate * v-wt / (if carrier.chg-method eq "W" then 100 else 1).
            
            if v-rate lt carr-mtx.min-rate then v-rate = carr-mtx.min-rate.
          end. /* avail carr-mtx */
          
          IF v-rate NE ? THEN v-frate = v-frate + v-rate.
        end. /* eb.fr-out is zero */
    END. /* not avail an oe-boll */
  end. /* each job-hdr */

  find first eb
      where eb.company EQ job.company
        AND eb.est-no  EQ job.est-no
        and eb.form-no  ne 0
      no-lock no-error.
  if avail eb then v-com = eb.comm.
  v-comm  = v-sale * (v-com / 100).
end.
         
/***  Print Grand Totals and Profit ***/
assign
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

put skip(1)
    "GRAND TOTALS           COST        PER/M"                  skip
    "    MATERIAL:"
    v-mater
    v-mater / (v-prod / 1000) format "->,>>>,>>9.99"            skip
    "        PREP:"
    v-prep
    v-prep  / (v-prod / 1000) format "->,>>>,>>9.99"            skip
    "       LABOR:"
    v-labor
    v-labor / (v-prod / 1000) format "->,>>>,>>9.99"            skip
    "  LAB MARKUP:"
    v-lab-m
    v-lab-m / (v-prod / 1000) format "->,>>>,>>9.99"            skip
    "  COMMISSION:"
    v-comm
    v-comm  / (v-prod / 1000) format "->,>>>,>>9.99"            skip
    "     FREIGHT:"
    v-frate
    v-frate / (v-prod / 1000) format "->,>>>,>>9.99"            skip
    skip(1)
    "  TOTAL COST:"
    v-total
    v-total / (v-prod / 1000) format "->,>>>,>>9.99"            skip
    skip(2)
    "                                   PER/M"                  skip
    " BOXES SALES:"
    v-sale
    v-sale  / (v-prod / 1000) format "->,>>>,>>9.99"            skip
    c1 FORM "x(80)" SKIP                                             
    c2 FORM "x(80)" SKIP
    skip(1)
    "  TOTAL COST:"
    v-total
    v-total / (v-prod / 1000) format "->,>>>,>>9.99"            skip
    skip(1)
    "GROSS MARGIN:"
    v-prof
    v-prof  / (v-sale + v-misc-prep) * 100      format "->>>>,>>9.99%"          skip.






