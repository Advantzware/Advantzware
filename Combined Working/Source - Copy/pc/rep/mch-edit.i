/* --------------------------------------------- pc/rep/mch-edit.i 01/96 JLF  */
/* Production Control -transactions edit list (main body)                     */
/* -------------------------------------------------------------------------- */

FOR EACH tt-report NO-LOCK,

    FIRST pc-prdd WHERE RECID(pc-prdd) EQ tt-report.rec-id NO-LOCK,

    first mach
    {sys/ref/machW.i}
      and mach.m-code eq pc-prdd.m-code
    no-lock,

    first job
    where job.company eq cocode
      and job.job     eq pc-prdd.job
      and job.job-no  eq pc-prdd.job-no
      and job.job-no2 eq pc-prdd.job-no2
    NO-LOCK,

    FIRST job-code WHERE job-code.code EQ pc-prdd.code NO-LOCK
        
    break by {1}
             {2}
          by pc-prdd.op-date
          by pc-prdd.start
          by pc-prdd.stopp
 
    with frame mch-edit:

  if first-of(pc-prdd.op-date) then do:
    put " " skip.
    ll-ok-to-post = yes.
  end.

  ASSIGN
   vmr-crusiz  = mach.mr-crusiz
   vrun-crusiz = mach.run-crusiz.

  IF vmr-crusiz  EQ 0 THEN vmr-crusiz = 1.
  IF vrun-crusiz EQ 0 THEN vrun-crusiz = 1.

  RELEASE eb.
  IF TRIM(job.est-no) NE "" THEN
  FIND FIRST eb
      WHERE eb.company   EQ job.company
        AND eb.est-no    EQ job.est-no
        AND eb.form-no   EQ pc-prdd.frm
        AND (eb.blank-no EQ pc-prdd.blank-no OR pc-prdd.blank-no EQ 0)
      NO-LOCK NO-ERROR.

  IF AVAIL eb THEN DO:
    RUN est/getcrusz.p (ROWID(mach), ROWID(eb), pc-prdd.dept, "M R",
                        INPUT-OUTPUT vmr-crusiz).

    RUN est/getcrusz.p (ROWID(mach), ROWID(eb), pc-prdd.dept, "RUN",
                        INPUT-OUTPUT vrun-crusiz).
  END.

  FOR EACH job-hdr OF job NO-LOCK BREAK BY job-hdr.frm:
    ll-one-item = FIRST(job-hdr.frm) AND LAST(job-hdr.frm).
    LEAVE.
  END.

  IF dcpostgl-log THEN
  FOR EACH job-hdr OF job
      WHERE ((job-hdr.frm       EQ pc-prdd.frm AND
              (job-hdr.blank-no EQ pc-prdd.blank-no OR pc-prdd.blank-no EQ 0))
         OR  ll-one-item)
      NO-LOCK,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ job-hdr.i-no
      NO-LOCK,
      FIRST prodl
      WHERE prodl.company EQ cocode
        AND prodl.procat  EQ itemfg.procat
        AND CAN-FIND(FIRST prod
                     WHERE prod.company EQ cocode
                       AND prod.prolin  EQ prodl.prolin)
      NO-LOCK,
      FIRST prod
      WHERE prod.company EQ cocode
        AND prod.prolin  EQ prodl.prolin
        AND prod.wip-mat NE ""
      NO-LOCK:

    ld = pc-prdd.hours * (IF ll-one-item           OR
                             pc-prdd.blank-no NE 0 OR
                             job-hdr.sq-in    LE 0 OR
                             job-hdr.sq-in    EQ ? THEN 1
                          ELSE (job-hdr.sq-in / 100)).

    IF job-code.cat EQ "MR" THEN
      ASSIGN
       app-lab = ROUND(ld * pc-prdd.crew * (mach.run-rate / vmr-crusiz),2)
       app-foh = ROUND(ld * mach.mr-fixoh,2)
       app-voh = ROUND(ld * mach.mr-varoh,2).
    ELSE
      ASSIGN
       app-lab = ROUND(ld * pc-prdd.crew * (mach.run-rate / vrun-crusiz),2)
       app-foh = ROUND(ld * mach.run-fixoh,2)
       app-voh = ROUND(ld * mach.run-varoh,2).

    {jc/jcglcrt.i prod.wip-lab 0 app-lab}
    {jc/jcglcrt.i prod.wip-fo  0 app-foh}
    {jc/jcglcrt.i prod.wip-vo  0 app-voh}
    {jc/jcglcrt.i prod.aa-lab app-lab 0}
    {jc/jcglcrt.i prod.aa-fo  app-foh 0}
    {jc/jcglcrt.i prod.aa-vo  app-voh 0}
  END.

  v-recid = ?.
  for each job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq pc-prdd.job
        and job-hdr.job-no  eq pc-prdd.job-no
        and job-hdr.job-no2 eq pc-prdd.job-no2
        and job-hdr.frm     eq pc-prdd.frm
      no-lock
      by job-hdr.blank-no desc:
              
    v-recid = recid(job-hdr).
              
    if job-hdr.blank-no eq pc-prdd.blank-no then leave.
  end.
  find job-hdr where recid(job-hdr) eq v-recid no-lock no-error.
  
  release itemfg.
  if avail job-hdr then
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq job-hdr.i-no
      no-lock no-error.

  display pc-prdd.m-code     when first-of(pc-prdd.m-code)
          mach.m-dscr        when first-of(pc-prdd.m-code)
          mach.dept[1]       when first-of({1})
          pc-prdd.op-date when first-of(pc-prdd.op-date)
          pc-prdd.shift
          pc-prdd.job-no
          pc-prdd.job-no2
          pc-prdd.frm
          pc-prdd.blank-no
          pc-prdd.pass
          job-hdr.i-no when avail job-hdr
          itemfg.i-name when avail itemfg
          pc-prdd.code
          pc-prdd.hours
          string(pc-prdd.start,"HH:MM") @ v-start
          string(pc-prdd.stopp,"HH:MM") @ v-stopp
          pc-prdd.crew
          pc-prdd.qty
          pc-prdd.waste
          "Y" when pc-prdd.complete @ v-comp.
  down.

   IF tb_excel THEN                                                 /*Task# 02061402*/
          PUT STREAM excel UNFORMATTED
            '"' pc-prdd.m-code '",' 
            '"' mach.m-dscr '",'
            '"' mach.dept[1]   '",'
            '"' pc-prdd.op-date '",' 
            '"' pc-prdd.shift                                    '",'
            '"' (pc-prdd.job-no + "-" + STRING(pc-prdd.job-no2)) '",'
            '"' pc-prdd.frm                                      '",'
            '"' pc-prdd.blank-no                                 '",'
            '"' pc-prdd.pass                                     '",'
            '"' IF avail job-hdr THEN job-hdr.i-no ELSE ""       '",'
            '"' IF avail itemfg THEN itemfg.i-name ELSE ""       '",'
            '"' pc-prdd.code                                     '",'
            '"' pc-prdd.hours                                    '",' 
            '"' string(pc-prdd.start,"HH:MM")                    '",' 
            '"' string(pc-prdd.stopp,"HH:MM")                    '",' 
            '"' pc-prdd.crew                                     '",' 
            '"' pc-prdd.qty                                      '",' 
            '"' pc-prdd.waste                                    '",' 
            '"' IF pc-prdd.COMPLETE THEN "Y" ELSE "N"            '",' 
            SKIP.                                                       
                                                                        
  assign                                                                
   tothour = tothour + pc-prdd.hours                                    
   totqty  = totqty  + pc-prdd.qty
   totwst  = totwst  + pc-prdd.waste.

  if last-of({1}) then do:
    if v-toth then do:
      put uline at 89 skip
          totchar at 72
          tothour at 94
          totqty  to 126
          totwst  to 133 skip(1).
      down.
    end.
    
    assign
     tothour = 0
     totqty  = 0
     totwst  = 0.
  end.    
    
  if "{1}" eq "pc-prdd.dept" then page.
end.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
