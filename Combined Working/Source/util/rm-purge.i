
if selection# begins "B" then 
  selection# = "ER".
else 
  selection# = substring(selection#,1,1).
    
for each rm-rcpth 
    where rm-rcpth.company    eq cocode
      and rm-rcpth.trans-date ge v-b-date
      and rm-rcpth.trans-date le v-e-date
      and rm-rcpth.i-no       ge b-item
      and rm-rcpth.i-no       le e-item
      AND rm-rcpth.job-no     GE SUBSTR(v-job-no[1],1,6)
      AND rm-rcpth.job-no     LE SUBSTR(v-job-no[2],1,6)
      AND FILL(" ",6 - LENGTH(TRIM(rm-rcpth.job-no))) +
          TRIM(rm-rcpth.job-no) + STRING(INT(rm-rcpth.job-no2),"99") GE v-job-no[1] 
      AND FILL(" ",6 - LENGTH(TRIM(rm-rcpth.job-no))) +
          TRIM(rm-rcpth.job-no) + STRING(INT(rm-rcpth.job-no2),"99") LE v-job-no[2]
    exclusive-lock,
      
    first item
    where item.company  eq rm-rcpth.company
      and item.i-no     eq rm-rcpth.i-no
      and index(selection#,item.i-code) gt 0
      and ((item.industry eq "1" and tb_fold) or
           (item.industry eq "2" and tb_corr) OR
           (tb_fold AND tb_corr))
    no-lock:

  IF tb_wip THEN
  DO:
     FIND FIRST job WHERE
          job.company EQ cocode AND
          job.job-no EQ rm-rcpth.job-no AND
          job.job-no2 EQ rm-rcpth.job-no2
          NO-LOCK NO-ERROR.

     IF (AVAIL job AND job.stat NE "W") OR
        NOT AVAIL job THEN
        NEXT.
  END.

  IF TRIM(rm-rcpth.po-no) NE "" THEN DO:
    CREATE tt-po.
    tt-po.po-no = INT(rm-rcpth.po-no) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN tt-po.po-no = 0.
  END.

  for each rm-rdtlh
      where rm-rdtlh.company eq cocode
        and rm-rdtlh.r-no    eq rm-rcpth.r-no
      exclusive-lock:
    delete rm-rdtlh. 
  end. /* for each rm-rdtlh */
        
  delete rm-rcpth. 
end. /* for each rm-rcpth */

FOR EACH tt-po WHERE tt-po.po-no GT 0 BREAK BY tt-po.po-no:
  IF LAST-OF(tt-po.po-no) THEN
  FOR EACH po-ord NO-LOCK
      WHERE po-ord.company EQ cocode
        AND po-ord.po-no   EQ tt-po.po-no,
      EACH po-ordl
      WHERE po-ordl.company EQ po-ord.company
        AND po-ordl.po-no   EQ po-ord.po-no:

    RUN po/calc-rmr.p (BUFFER po-ordl).
  END.
END.

if not purge-history# then do:  
  for each item
      where item.company  eq cocode
        and item.i-no     ge b-item
        and item.i-no     le e-item
        and index(selection#,item.i-code) gt 0
        and ((item.industry eq "1" and tb_fold) or
             (item.industry eq "2" and tb_corr) OR
             (tb_fold AND tb_corr)) 
      exclusive-lock:

    for each rm-rcpt
        where rm-rcpt.company eq cocode
          and rm-rcpt.i-no    eq item.i-no
        exclusive-lock:

      for each rm-rdtl
          where rm-rdtl.company eq cocode
            and rm-rdtl.r-no    eq rm-rcpt.r-no
          exclusive-lock:
          
        delete rm-rdtl.
      end. /* for each rm-rdtl */
        
      delete rm-rcpt. 
    end. /* for each rm-rcpt */

    for each rm-bin
        where rm-bin.company eq cocode
          and rm-bin.i-no    eq item.i-no
        exclusive-lock:

      delete rm-bin. 
    end. /* for each rm-bin */

    delete item. 
  end. /* for each item */
end.
                                                                                                               
