/* --------------------------------------------- sys/ref/preplst2.i 08/97 JLF */
/* Prep List by Material Type                                                 */
/* -------------------------------------------------------------------------- */

          and eb.cust-no                ge fcus
          and eb.cust-no                le tcus
          and eb.sman                   ge fsmn
          and eb.sman                   le tsmn
        use-index {1} no-lock,
        
        first est
        where est.company eq eb.company
          and est.est-no  eq eb.est-no         
          and (findus eq "B"                         or
               (findus eq "F" and est.est-type le 4) or
               (findus eq "C" and est.est-type gt 4))
          and est.mod-date              ge fdat[1]
          and est.mod-date              le tdat[1]
        no-lock
        
        transaction:
        
      release oe-ord.
      
      if eb.ord-no ne 0 then
      find first oe-ord
          where oe-ord.company eq cocode
            and oe-ord.ord-no  eq eb.ord-no
          no-lock no-error.
          
      if avail oe-ord                                               and
         (oe-ord.ord-date lt fdat[2] or oe-ord.ord-date gt tdat[2]) then next.
         
      create tt-report.
      assign
       tt-report.term-id = v-term
       tt-report.rec-id  = recid(eb)
       tt-report.key-09  = if "{1}" eq "die" then eb.die-no else eb.plate-no
       tt-report.key-01  = if v-sort eq "S" then eb.sman
                           else
                           if v-sort eq "O" then
                             if avail oe-ord then
                               (string(year(oe-ord.ord-date),"9999") +
                                string(month(oe-ord.ord-date),"99") +
                                string(day(oe-ord.ord-date),"99"))
                             else ""  
                           else tt-report.key-09
       tt-report.key-02  = if v-sort eq "S" then eb.cust-no else ""
       tt-report.key-03  = if avail oe-ord then
                             string(12/31/9999 - oe-ord.ord-date,"9999999999")
                           else "9999999999"
       tt-report.key-04  = string(est.mod-date,"99/99/9999")
       tt-report.key-04  = substr(tt-report.key-04,7,4) +
                           substr(tt-report.key-04,1,2) +
                           substr(tt-report.key-04,4,2).

/* end ---------------------------------- copr. 1997  advanced software, inc. */
