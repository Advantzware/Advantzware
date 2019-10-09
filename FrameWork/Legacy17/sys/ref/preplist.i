/* --------------------------------------------- sys/ref/preplist.i 07/97 JLF */
/* Prep List by Material Type                                                 */
/* -------------------------------------------------------------------------- */

        find first cust
            {sys/ref/custW.i}
              and cust.cust-no eq eb.cust-no
            no-lock no-error.
            
        find first oe-ord
            where oe-ord.company eq cocode
              and oe-ord.ord-no  eq eb.ord-no
            no-lock no-error.
       IF v-prt-desc THEN DO:
          display tt-report.key-09 FORM "x(15)"            
                prep.dscr       when avail prep 
                trim(eb.est-no) format "x(8)"  
                eb.part-no
                est.mod-date
                oe-ord.ord-date when avail oe-ord                
                trim(string(eb.ord-no,">>>>>>>>")) format "x(8)"              
                eb.cust-no
                cust.name       when avail cust
                with frame prep no-box no-labels STREAM-IO width 132.
                
          IF tb_excel THEN 
             EXPORT STREAM excel DELIMITER "," 
                tt-report.key-09 FORM "x(15)"           
                (IF AVAILABLE prep THEN prep.dscr ELSE "")
                trim(eb.est-no) format "x(8)"  
                (eb.part-no + CHR(32))
                est.mod-date
                (IF AVAILABLE oe-ord THEN STRING(oe-ord.ord-date) ELSE "")
                STRING(eb.ord-no,"zzzzzzzz")
                eb.cust-no
                (IF AVAILABLE cust THEN cust.name ELSE "")
                SKIP.              
         END.
       ELSE  display tt-report.key-09 FORM "x(15)"
                 
                trim(eb.est-no) format "x(8)"
                eb.part-no
                est.mod-date
                oe-ord.ord-date when avail oe-ord
                 
                trim(string(eb.ord-no,">>>>>>>>")) format "x(8)"
                 
                eb.cust-no
                cust.name       when avail cust
                WITH FRAME prep2 WIDTH 132 DOWN STREAM-IO NO-BOX NO-LABELS.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
