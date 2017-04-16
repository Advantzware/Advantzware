/* ----------------------------------------------- po/rep/mat-req.i 01/98 JLF */
/* Material Requirements Report                                               */
/* -------------------------------------------------------------------------- */

DEF VAR ll-no-po AS LOG NO-UNDO.

  for each job
      where job.company eq cocode
        AND job.opened  EQ YES
        and job.job-no  ge substr(v-job-no[1],1,6)
        and job.job-no  le substr(v-job-no[2],1,6)
        
        and (fill(" ",6 - length(trim(job.job-no))) +
             trim(job.job-no)                       +
             string(job.job-no2,"99"))                 ge v-job-no[1]

        and (fill(" ",6 - length(trim(job.job-no))) +
             trim(job.job-no)                       +
             string(job.job-no2,"99"))                 le v-job-no[2]
             
        and can-find(first job-hdr where job-hdr.company eq job.company
                                     and job-hdr.job     eq job.job
                                     and job-hdr.job-no  eq job.job-no
                                     and job-hdr.job-no2 eq job.job-no2)
      use-index /*stat-idx*/ opened no-lock:
      {custom/statusMsg.i " 'Processing Job#  '  + job.job-no "}

    create {1}report.
    assign
     {1}report.term-id = v-term
     {1}report.key-01  = fill(" ",6 - length(trim(job.job-no))) +
                         trim(job.job-no) + "-" + string(job.job-no2,"99")
     {1}report.rec-id  = recid(job).

    for each job-mat
        where job-mat.company eq job.company
          and job-mat.job     eq job.job
          and job-mat.job-no  eq job.job-no
          and job-mat.job-no2 eq job.job-no2
        no-lock,

        first item
        where item.company eq job-mat.company
          and item.i-no    eq job-mat.rm-i-no
          and can-do(v-mattype-list,item.mat-type)
        no-lock

        break by job-mat.rm-i-no:

      if first-of(job-mat.rm-i-no) then do:
        if not first(job-mat.rm-i-no) then do:
          create {1}report.
          assign
           {1}report.term-id = v-term
           {1}report.key-01  = fill(" ",6 - length(trim(job.job-no))) +
                               trim(job.job-no) + "-" + string(job.job-no2,"99")
           {1}report.rec-id  = recid(job).
        end.

        {1}report.key-02 = job-mat.rm-i-no.
        
        if v-sort-by-size then do:
          assign
           {1}report.key-01 = string(job-mat.wid,"9999.9999") +
                              string(job-mat.len,"9999.9999")
           len-score        = "".

          release b-ref1.
          release b-ref2.

          if item.mat-type eq "B" then do:
            /*##PN - This was commented out with no explanation.  */
            /*##PN - Reinstated for task 10081304  */
             {po/poordls1W.i}                     
             IF NOT (AVAIL(b-ref1) OR AVAIL(b-ref2)) THEN DO:
                 /* ##PN - If reftable recs exist, don't recalc since report */
                 /* ##PN - was too slow                                      */
                 run po/po-ordls.p (recid(job-mat)). 
                 {po/poordls1W.i}                            
             END.
          END.
          if avail b-ref1 or avail b-ref2 then do:
            v-lscore-c = "".
             
            if avail b-ref1 then
            do x = 1 to 12:
              if b-ref1.val[x] ne 0 then
                v-lscore-c = v-lscore-c +
                             trim(string(b-ref1.val[x],">>>.99")) + " ".
            end.
             
            if avail b-ref2 then
            do x = 1 to 8:
              if b-ref2.val[x] ne 0 then
                v-lscore-c = v-lscore-c +
                             trim(string(b-ref2.val[x],">>>.99")) + " ".
            end.
            
            if v-lscore-c ne "" then do:
              len-score = "".
                
              do x = 1 to length(v-lscore-c):
                if substr(v-lscore-c,x,1) ne " " then
                  assign
                   len-score = len-score + substring(v-lscore-c,x,1)
                   v-space   = yes.
         
                else
                if v-space then
                  assign
                   len-score = len-score + "  "
                   v-space   = no.
              end.
            end.
          end.
          
          {1}report.key-05 = len-score.
        end.
      end.

      ll-no-po = tb_show.

      for each po-ordl
          where po-ordl.company  eq cocode
            and po-ordl.i-no     eq job-mat.rm-i-no
            and po-ordl.job-no   eq job.job-no
            and po-ordl.job-no2  eq job.job-no2
            and po-ordl.due-date ge begin_due-date
            and po-ordl.due-date le end_due-date
          no-lock

          break by po-ordl.po-no:

        ll-no-po = NO.

        if not first(po-ordl.po-no) then do:
          create {1}report.
          assign
           {1}report.term-id = v-term
           {1}report.key-01  = fill(" ",6 - length(trim(job.job-no))) +
                               trim(job.job-no) + "-" + string(job.job-no2,"99")
           {1}report.key-02  = job-mat.rm-i-no
           {1}report.rec-id  = recid(job).
        end.
        
        assign
         {1}report.key-03 = string(po-ordl.po-no,"9999999999")
         {1}report.key-04 = string(po-ordl.line,"9999999999").

        if v-sort-by-size then do:
          assign
           {1}report.key-01 = string(po-ordl.s-wid,"9999.9999") +
                              string(po-ordl.s-len,"9999.9999")
           len-score        = "".
                             
           run po/po-ordls.p (recid(po-ordl)). 
            
           {po/poprints.i}                     
          
                {1}report.key-05 = TRIM({1}report.key-05) + " " + TRIM(len-score).
              END. 
            END. 
          END.
        end.
      end.

      if avail {1}report and (ll-no-po AND tb_show) then delete {1}report.
    end.

    if avail {1}report and {1}report.key-02 eq "" then delete {1}report.
  end.

for each {1}report where {1}report.term-id eq v-term,

    first job where recid(job) eq {1}report.rec-id no-lock

    break by {1}report.key-01
          by {1}report.key-02
          by {1}report.key-03
          by {1}report.key-04

    transaction:

  assign
   v-job = fill(" ",6 - length(trim(job.job-no))) +
           trim(job.job-no) + "-" + string(job.job-no2,"99")
   v-itm = {1}report.key-02.

  {custom/statusMsg.i " 'Processing Job#  '  + v-job "}

  find first item
      where item.company eq job.company
        and item.i-no    eq v-itm
      no-lock no-error.

  if first-of({1}report.key-02) then do:
    v-qty[1] = 0.

    if v-itm ne "" then
    for each job-mat
        where job-mat.company eq job.company
          and job-mat.job     eq job.job
          and job-mat.rm-i-no eq v-itm
        no-lock:
      assign
       v-uom    = job-mat.qty-uom
       v-qty[1] = v-qty[1] + job-mat.qty.
    end.

    v-qty[4] = v-qty[1].
  end.

  release po-ordl.
  release po-ord.
  release vend.

  find first po-ordl
      where po-ordl.company eq job.company
        and po-ordl.po-no   eq int({1}report.key-03)
        and po-ordl.line    eq int({1}report.key-04)
      no-lock no-error.

  if avail po-ordl then do:

    FIND FIRST po-ord WHERE
         po-ord.company EQ po-ordl.company AND
         po-ord.po-no EQ po-ordl.po-no
         NO-LOCK NO-ERROR.

    if avail po-ord then
    find first vend
        where vend.company eq cocode
          and vend.vend-no eq po-ord.vend-no
        no-lock no-error.

    assign
     v-qty[2] = po-ordl.cons-qty
     v-qty[3] = po-ordl.t-rec-qty.

    if po-ordl.cons-uom ne v-uom then do:
      assign
       v-len = po-ordl.s-len
       v-wid = po-ordl.s-wid
       v-bwt = item.basis-w.

      if po-ordl.cons-qty ne 0 then
        run sys/ref/convquom.p(po-ordl.cons-uom, v-uom,
                               v-bwt, v-len, v-wid, item.s-dep,
                               po-ordl.cons-qty, output v-qty[2]).
                               
      if po-ordl.t-rec-qty ne 0 then
        run sys/ref/convquom.p(po-ordl.cons-uom, v-uom,
                               v-bwt, v-len, v-wid, item.s-dep,
                               po-ordl.t-rec-qty, output v-qty[3]).
    end.

    v-qty[4] = v-qty[4] - v-qty[3].

    if v-qty[4] lt 0 then v-qty[4] = 0.
  end.

  if v-sort-by-size then do:
    assign
     v-wid     = dec(substr({1}report.key-01,01,9))
     v-len     = dec(substr({1}report.key-01,10,9))
     len-score = {1}report.key-05.
    
    display v-job                             when first-of({1}report.key-01)
                                                or v-sort-by-size
            v-itm                             /*when first-of({1}report.key-02)*/
            "PendingJob"                      when v-itm eq "" @ v-itm
            v-uom                             when /*first-of({1}report.key-02)
                                               and*/ v-itm ne ""
            v-qty[1]                          when /*first-of({1}report.key-02)
                                               and */v-itm ne ""
            v-qty[2]                          when avail po-ordl
            v-qty[3]                          when avail po-ordl
            po-ord.vend-no                    when avail po-ord
            v-wid
            v-len
            len-score
            po-ord.due-date                   when avail po-ord

        with frame detail1.

    down with frame detail1.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
          '"' v-job                                                       '",'
          '"' (IF v-itm EQ "" THEN "PendingJob"
               ELSE v-itm )     '",'
          '"' (IF v-itm ne "" THEN v-uom
               ELSE "")                                                   '",'
          '"' (IF v-itm ne "" THEN
                 STRING(v-qty[1],"->>,>>>,>>9.999") ELSE "")              '",'
          '"' (IF AVAIL po-ordl THEN STRING(v-qty[2],"->>,>>>,>>9.999")
               ELSE "")                                                   '",'
          '"' (IF AVAIL po-ordl THEN STRING(v-qty[3],"->>,>>>,>>9.999")
               ELSE "")                                                   '",'
          '"' (IF AVAIL po-ord THEN po-ord.vend-no ELSE "")               '",'
          '"' STRING(v-wid,">>9.9999")                                    '",'
          '"' STRING(v-len,">>9.9999")                                    '",'
          '"' len-score                                                   '",'
          '"' (IF AVAIL po-ord AND po-ord.due-date NE ? THEN
                  STRING(po-ord.due-date,"99/99/9999") ELSE "")           '",'
          SKIP.
  end.
  
  else do:
    display v-job                             when first-of({1}report.key-01)
                                                or v-sort-by-size
            v-itm                             /*when first-of({1}report.key-02)*/
            "PendingJob"                      when v-itm eq "" @ v-itm
            v-uom                             when /*first-of({1}report.key-02)
                                               and */v-itm ne ""
            v-qty[1]                          when /*first-of({1}report.key-02)
                                               and*/ v-itm ne ""
            v-qty[2]                          when avail po-ordl
            v-qty[3]                          when avail po-ordl
            v-qty[4]                          when last-of({1}report.key-02)
                                               and v-itm ne ""
            po-ord.vend-no                    when avail po-ord
            po-ord.po-no                      when avail po-ord
            vend.name                         when avail vend
            po-ord.due-date                   when avail po-ord

        with frame detail2.

    down with frame detail2.

    IF tb_excel THEN
       PUT STREAM excel UNFORMATTED
          '"' (IF first-of({1}report.key-01) THEN v-job ELSE "")          '",'
          '"' (IF v-itm EQ "" THEN "PendingJob"
               ELSE IF first-of({1}report.key-02) THEN v-itm ELSE "")     '",'
          '"' (IF first-of({1}report.key-02) and v-itm ne "" THEN v-uom
               ELSE "")                                                   '",'
          '"' (IF first-of({1}report.key-02) and v-itm ne "" THEN
                 STRING(v-qty[1],"->>,>>>,>>9.999") ELSE "")              '",'
          '"' (IF AVAIL po-ordl THEN STRING(v-qty[2],"->>,>>>,>>9.999")
               ELSE "")                                                   '",'
          '"' (IF AVAIL po-ordl THEN STRING(v-qty[3],"->>,>>>,>>9.999")
               ELSE "")                                                   '",'
          '"' (IF LAST-OF({1}report.key-02) AND v-itm NE "" THEN
                  STRING(v-qty[4],"->>,>>>,>>9.999") ELSE "")             '",'
          '"' (IF AVAIL po-ord THEN po-ord.vend-no ELSE "")               '",'
          '"' (IF AVAIL po-ord THEN STRING(po-ord.po-no) ELSE "")         '",'
          '"' (IF AVAIL vend THEN vend.NAME ELSE "")                      '",'
          '"' (IF AVAIL po-ord AND po-ord.due-date NE ? THEN
                  STRING(po-ord.due-date,"99/99/9999") ELSE "")           '",'
          SKIP.
  end.

  if last-of({1}report.key-01) and not v-sort-by-size then put skip(1).

  delete {1}report.
end.

/* end ---------------------------------- copr. 1998  advanced software, inc. */
