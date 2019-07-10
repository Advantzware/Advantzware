/***********************************************************************
 jcrep/tickrrpt.i
 ***********************************************************************/

FOR EACH tt-keyst:
  DELETE tt-keyst.
END.
FOR EACH tt-key2:
  DELETE tt-key2.
END.
FOR EACH tt-prem:
  DELETE tt-prem.
END.
FOR EACH tt-pallet:
    DELETE tt-pallet.
END.

EMPTY TEMP-TABLE tt-sample-ctn.
spec-list = "".
DO li = 1 TO NUM-ENTRIES(spec_codes):
  spec-list = TRIM(ENTRY(li,spec_codes)) + "," + TRIM(spec-list).
END.

/*FibreFC,*/
IF tb_fold  AND CAN-DO("Interpac,Dayton,Livngstn,CentBox,Frankstn,Colonial,Unipak,OTTPkg,MWFibre,Shelby,CCC,Indiana-XL,PPI,PackRite,Rosmar,Accord,Knight,MidYork,Badger,Carded",lv-format-f) THEN 
  lines-per-page = 50. /*55*/
ELSE IF tb_fold AND CAN-DO("FibreFC,HPB,METRO,Dee",lv-format-f) THEN 
  lines-per-page = 70 /* 58 lines-per-page*/.
ELSE 
  lines-per-page = 99.

IF tb_freeze-note THEN
   for each job-hdr 
        where job-hdr.company               eq cocode
          and (production OR job-hdr.ftick-prnt eq reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
          and job-hdr.job-no                ge substr(fjob-no,1,6)
          and job-hdr.job-no                le substr(tjob-no,1,6)
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  ge fjob-no
          and fill(" ",6 - length(trim(job-hdr.job-no))) +
              trim(job-hdr.job-no) +
              string(job-hdr.job-no2,"99")  le tjob-no
              NO-LOCK,
        first job
        where job.company                   eq cocode
          and job.job                       eq job-hdr.job
          and job.job-no                    eq job-hdr.job-no
          and job.job-no2                   eq job-hdr.job-no2
          and job.stat                      ne "H"
          AND (job.pr-printed EQ reprint OR NOT production)
          AND (tb_app-unprinted EQ NO OR
              (tb_app-unprinted AND job.pr-printed = NO AND
               job.opened = YES AND job.cs-to-pr = YES))
        EXCLUSIVE-LOCK:
        RUN jc\jobnotes.p(BUFFER job).

        IF NOT CAN-FIND(FIRST b-reftable-freeze WHERE
           b-reftable-freeze.reftable EQ "FREEZENOTE" AND
           b-reftable-freeze.company  EQ cocode AND
           b-reftable-freeze.loc      EQ job-hdr.job-no AND
           b-reftable-freeze.CODE     EQ STRING(job-hdr.job-no2,"99")) THEN
           DO:
              CREATE b-reftable-freeze.
              ASSIGN
                b-reftable-freeze.reftable = "FREEZENOTE"
                b-reftable-freeze.company  = cocode
                b-reftable-freeze.loc      = job-hdr.job-no
                b-reftable-freeze.CODE     = STRING(job-hdr.job-no2,"99").
              
              RELEASE b-reftable-freeze.
           END.
   END.

FIND CURRENT job NO-LOCK NO-ERROR.

for each job-hdr NO-LOCK
    where job-hdr.company               eq cocode
      and (production OR job-hdr.ftick-prnt eq reprint OR
          PROGRAM-NAME(2) MATCHES "*r-tickt2*")
      and job-hdr.job-no                ge substr(fjob-no,1,6)
      and job-hdr.job-no                le substr(tjob-no,1,6)

      and fill(" ",6 - length(trim(job-hdr.job-no))) +
          trim(job-hdr.job-no) +
          string(job-hdr.job-no2,"99")  ge fjob-no

      and fill(" ",6 - length(trim(job-hdr.job-no))) +
          trim(job-hdr.job-no) +
          string(job-hdr.job-no2,"99")  le tjob-no
    USE-INDEX job-no,
    first job
        where job.company                   eq cocode
          and job.job                       eq job-hdr.job
          and job.job-no                    eq job-hdr.job-no
          and job.job-no2                   eq job-hdr.job-no2
          and job.stat                      ne "H"
          AND (job.pr-printed EQ reprint OR NOT production)
          AND (tb_app-unprinted EQ NO OR
              (tb_app-unprinted AND job.pr-printed = NO AND
               job.opened = YES AND job.cs-to-pr = YES))
          NO-LOCK:

    IF tb_prompt-ship THEN
    DO:
       IF NOT CAN-FIND(FIRST b-reftable-split WHERE
          b-reftable-split.reftable EQ "splitshp" AND
          b-reftable-split.company  EQ cocode AND
          b-reftable-split.loc      EQ TRIM(job-hdr.job-no) AND
          b-reftable-split.code     EQ STRING(job-hdr.job-no2,"9999999999")) THEN
          DO:
             CREATE b-reftable-split.
             ASSIGN
                b-reftable-split.reftable = "splitshp"
                b-reftable-split.company  = cocode
                b-reftable-split.loc      = TRIM(job-hdr.job-no)
                b-reftable-split.code     = STRING(job-hdr.job-no2,"9999999999").
             RELEASE b-reftable-split.
          END.
    END.
    ELSE
    DO:
       FIND FIRST b-reftable-split WHERE
            b-reftable-split.reftable EQ "splitshp" AND
            b-reftable-split.company  EQ cocode AND
            b-reftable-split.loc      EQ TRIM(job-hdr.job-no) AND
            b-reftable-split.code     EQ STRING(job-hdr.job-no2,"9999999999")
            NO-ERROR.

       IF AVAIL b-reftable-split THEN
          DELETE b-reftable-split.
    END.
END.

IF tb_fold  AND lv-format-f = "FibreFC" THEN 
  lines-per-page = 67.

IF tb_corr  AND lv-format-c = "Badger" THEN 
  lines-per-page = 56.


IF ip-industry EQ "Fold" AND tb_fold AND CAN-DO("Frankstn,Keystone,FibreFC,METRO,HPB,MWFibre,PPI,PackRite,Rosmar,Knight,MidYork,Dee",lv-format-f) THEN
  {cerep/jobkeyst.i NO-LOCK}
  , EACH job-mat WHERE job-mat.company = job-hdr.company
                   AND job-mat.job     = job-hdr.job
                 NO-LOCK
    BREAK BY job-hdr.job
          BY job-hdr.job-no
          BY job-hdr.job-no2
          BY job-hdr.frm
          BY job-mat.frm
          BY job-hdr.i-no
  TRANSACTION:
  
    IF CAN-DO("Keystone,MWFibre",lv-format-f) THEN DO:

     /* IF FIRST-OF(job-hdr.frm) THEN 
        RUN cerep/d-keyst.w (job-hdr.job-no, job-hdr.job-no2, job-hdr.frm).*/
  
      IF FIRST-OF(job-hdr.i-no) THEN DO:
  
        FIND FIRST tt-key2
             WHERE tt-key2.tt-job-no  EQ job-hdr.job-no
               AND tt-key2.tt-job-no2 EQ job-hdr.job-no2
               AND tt-key2.tt-frm     EQ job-hdr.frm
               AND tt-key2.tt-i-no    EQ job-hdr.i-no NO-LOCK NO-ERROR.
  
        IF NOT AVAIL tt-key2 THEN DO:
  
          CREATE tt-key2.
  
          ASSIGN
            tt-key2.tt-job-no  = job-hdr.job-no
            tt-key2.tt-job-no2 = job-hdr.job-no2
            tt-key2.tt-frm     = job-hdr.frm
            tt-key2.tt-i-no    = job-hdr.i-no
            tt-key2.tt-rowid   = ROWID(job-hdr).
        END. /* not avail tt-key2 */
  
       /* RUN cerep/d-keys2.w (job-hdr.job-no, job-hdr.job-no2, job-hdr.frm, job-hdr.i-no).*/
      END. /* FIRST-OF(job-hdr.i-no) */
    END. /* CAN-DO("Keystone,MWFibre",lv-format-f) */

    ELSE
    IF FIRST-OF(job-hdr.frm) THEN
      IF CAN-DO("FrankstnFrankstn,METROMETRO,HPBHPB,PPIPPI,RosmarRosmar",lv-format-f) THEN 
          RUN cerep/d-fibre.w (ROWID(job-hdr)).
      ELSE
          IF CAN-DO("PackRitePackRite",lv-format-f) THEN
              RUN cerep/d-packrite.w(ROWID(job-hdr)).
          ELSE
          /*
         IF CAN-DO("FibreFC",lv-format-f) THEN
            RUN cerep/d-fibr1.w (ROWID(job-hdr), job-mat.frm).
         ELSE */
             /* gdm - 11030807 (ROWID(job-hdr))*/
             IF CAN-DO("Deeeee",lv-format-f) THEN
                    RUN cerep/d-dee1.w (ROWID(job-hdr)).
             ELSE 
                 IF CAN-DO("Knight,MidYork",lv-format-f) THEN DO:
                     FOR EACH eb WHERE eb.company = est.company
                                   AND eb.est-no = est.est-no 
                                   AND eb.form-no = job-hdr.frm NO-LOCK:
                         /*RUN cerep/d-knight.w(INPUT ROWID(eb),
                                              INPUT job-hdr.job-no,
                                              INPUT job-hdr.job-no2,
                                              INPUT job-hdr.frm,
                                              INPUT job-hdr.i-no).*/
                     END.
                 END.
    DEF BUFFER bf-est FOR est.
    FIND FIRST bf-est WHERE bf-est.company   EQ job-hdr.company
                     AND bf-est.est-no    EQ job-hdr.est-no
                     AND (bf-est.est-type EQ 2)
                   NO-LOCK NO-ERROR.
  IF  ( (avail(bf-est) AND first-of(job-mat.frm))
        OR FIRST-OF(job-hdr.frm) )
      AND CAN-DO("FibreFC",lv-format-f) THEN
    RUN cerep/d-fibr1.w (ROWID(job-hdr), job-mat.frm).

END.

IF ip-industry EQ "Corr" AND tb_corr and tb_prompt-ship              AND
   lookup(lv-format-c,"ARTIOS,Hughes,Protagon,Delta,CapCity,Allwest,PEACHTREE") > 0 THEN
   {cecrep/jc-fibr1.i}
   /*RUN cecrep/d-fibre.w (ROWID(job-hdr),ROWID(reftable)).*/
END.   

/*IF tb_corr AND lv-format-c EQ "ARTIOS" THEN
   {cecrep/jc-artios.i}
   RUN cecrep/d-artios.w(ROWID(job-hdr),ROWID(reftable)).
END.*/
/*
IF ip-industry EQ "Corr" AND tb_corr = TRUE AND 
   (lv-format-c EQ "Protagon" OR lv-format-c EQ "Hughes" OR lv-format-c EQ "Allwest") AND 
   TB_sample_req = TRUE THEN DO:
       {cecrep/jc-hughs.i}
       END.
END.*/

IF ip-industry EQ "Corr" AND tb_corr                                         AND
   CAN-DO("Premier,Xprint,Suthrlnd,United,Oklahoma,Spectrum,Michcor,PQP,RFC2",lv-format-c) AND
   s-prt-set-header                                     THEN
  {cecrep/jobprem.i NO-LOCK} 
  break by job.job-no
        by job.job-no2:

  IF FIRST-OF(job.job-no2) AND 
      est.est-type    = 6 THEN do:

      i = 0.

      FOR EACH eb WHERE eb.company = est.company
                    AND eb.est-no = est.est-no 
                    AND eb.form-no > 0 NO-LOCK:
          i = i + 1.
      END.

      /*IF i > 1 THEN 
        RUN cecrep/d-prem.w (job-hdr.job-no, job-hdr.job-no2).*/
  END.
END.

/* CentBox */
IF ip-industry EQ "Fold" AND tb_fold AND 
   s-committed-board-only       AND 
   (lv-format-f EQ "CentBox"    or
    lv-format-f eq "Indiana-XL" OR
    lv-format-f EQ "Accord") THEN
  RUN cerep/jobcbox3.p.

DEF VAR v-sample-on-ct AS LOG NO-UNDO.

IF ip-industry EQ "Fold" AND tb_fold AND lv-format-f EQ "Colonial" THEN
DO:
   for each job-hdr WHERE
       job-hdr.company               eq cocode AND
       job-hdr.job-no                ge substr(fjob-no,1,6) AND
       job-hdr.job-no                le substr(tjob-no,1,6) AND
       fill(" ",6 - length(trim(job-hdr.job-no))) +
           trim(job-hdr.job-no) +
           string(job-hdr.job-no2,"99")  ge fjob-no AND
       fill(" ",6 - length(trim(job-hdr.job-no))) +
           trim(job-hdr.job-no) +
           string(job-hdr.job-no2,"99")  le tjob-no AND
       (job-hdr.ftick-prnt           eq reprint OR
        PROGRAM-NAME(2) MATCHES "*r-tickt2*" ) AND
       can-find(first job where job.company eq cocode
                  and job.job     eq job-hdr.job
                  and job.job-no  eq job-hdr.job-no
                  and job.job-no2 eq job-hdr.job-no2
                  and job.stat    ne "H"),
       first est
        where est.company = job.company
          AND est.est-no                    eq job-hdr.est-no
          and est.est-type                  LE 4
        no-lock,
       first eb fields(company procat) WHERE
             eb.company = job-hdr.company AND
             eb.loc     =  job-hdr.loc AND
             eb.est-no  = job-hdr.est-no
             no-lock:

       IF CAN-FIND(FIRST prodl WHERE prodl.company EQ eb.company AND
          prodl.prolin EQ 'Printed' AND
          prodl.procat EQ eb.procat) THEN
          RUN cerep/d-cccpl.w(INPUT job-hdr.job-no,
                              INPUT job-hdr.job-no2,
                              INPUT job-hdr.frm,
                              INPUT job-hdr.i-no).
   END.
END.

{sys/inc/print1.i}
    
{sys/inc/outprint.i value(lines-per-page)} 

if td-show-parm then 
  run show-param.

SESSION:SET-WAIT-STATE ("general").

/*Change similar lines in jcrep\r-tickt2.w can-do ... in multiple places*/
is-xprint-form = (ip-industry EQ "Corr") OR 
                  CAN-DO("Interpac,FibreFC,Metro,HPB,Dayton,Livngstn,CentBox,Keystone,Frankstn,Colonial,Unipak,OTTPkg,MWFibre,Shelby,CCC,Indiana-XL,PPI,PackRite,Rosmar,Accord,Knight,MidYork,Dee,Badger,Carded",lv-format-f).
ASSIGN is-xprint-form = YES. 
IF is-xprint-form THEN DO:

  /*<PDF-LEFT=5mm><PDF-TOP=10mm>*/                              /* </PROGRESS>*/
  IF (ip-industry = "Corr" AND 
       (can-do ('PEACHTREE,PACIFIC,MWBox,Hughes,Protagon,ARTIOS,Suthrlnd,United,oklahoma,Spectrum,CapCity,Allwest,RFC2',lv-format-c))  OR 
       (can-do ("Xprint,TriLakes,TriLakes2,Michcor",lv-format-c) AND lv-ornt = "L")) OR
       (ip-industry = "FOLD" AND 
       can-do ('Interpac,Frankstn,OTTPkg,Colonial,CCC,Dayton,Livngstn,Shelby,HPB,METRO,FibreFC,PPI,PackRite,Rosmar,Knight,MidYork,Dee,Badger,Carded,Accord',lv-format-f)) THEN 
      PUT UNFORMATTED "<OLANDSCAPE>".
    
    PUT "<PRINT=NO><PDF-LEFT=1mm><PDF-TOP=2mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(350)".
  
END.

/* FOLDING */
IF ip-industry EQ "Fold" THEN DO:
    /* Colonial */
   IF NOT CAN-DO('ASI,CentBox,UniPak,HPB,METRO,FibreFC,Indiana-XL,Accord,Dee,Colonial,Carded',lv-format-f) THEN spec-list = "".
   
   if  lv-format-f = 'Indiana-XL'      and 
       (logical (tb_RS) = true or
        logical (tb_PR) = true or
        logical (tb_DC) = true or
        logical (tb_GL) = true or
        logical (tb_SW) = true) then
   do:
     run cerep/jobindxl-rs.p (input   fjob-no,
                              input   tjob-no,
                              input   fjob-no2,
                              input   tjob-no2,
                              input   logical (tb_RS),
                              input   logical (tb_PR),
                              input   logical (tb_DC),
                              input   logical (tb_GL),
                              input   logical (tb_SW)).
   end.
   else IF lv-format-f EQ "HOP" THEN DO: 
      IF rd-dest = 6 THEN DO:
           PUT CONTROL CHR(27) CHR(38) CHR(107) CHR(50) CHR(83). 
           RUN ce/rep/jobtick.p.
           PUT CONTROL CHR(27) CHR(38) CHR(107) CHR(50) CHR(83). 
      END.
      ELSE RUN ce/rep/jobtick.p.
   END.
   ELSE IF lv-format-f EQ "Interpac" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobintpk.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "Frankstn" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobfrank.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "PPI" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobppi.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "PackRite" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobpackrite.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "Rosmar" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobRosmar.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "OTTPkg" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobottpk.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "Colonial" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobcolnl.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "CCC" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobccc.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "Dayton" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobdaytn.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "Livngstn" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/joblbox.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "Shelby" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobshlby.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "Carded" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobcarded.p (lv-format-f).
   END.

   ELSE IF lv-format-f = "FibreFC" THEN do:  
       /* 10x14 works but too much margin, paper size is 11x14 2790,3550
       <FORMAT=10x14> */
       
       /*PUT UNFORMATTED "<OLANDSCAPE><P12></PROGRESS>".*/

       /*PUT UNFORMATTED "<FORMAT=10x14><OLANDSCAPE><P12></PROGRESS>".*/

       PUT UNFORMATTED "<FORMAT=CUSTOM,2794,3556><OLANDSCAPE><P12></PROGRESS>".
       /* gdm - 07130906*/
       RUN cerep/jobfibre.p (lv-format-f,fl-jobord,tb_app-unprinted).
   END.
   ELSE IF lv-format-f = "Metro" THEN do:  
       PUT UNFORMATTED "<FORMAT=10x14><OLANDSCAPE><P12></PROGRESS>" .
       RUN cerep/jobmetro.p (lv-format-f).
   END.
   ELSE IF lv-format-f = "HPB" THEN do:  
       /* 10x14 works but too much margin paper size is 11x14 2790,3550*/             
       /*PUT UNFORMATTED "<FORMAT=CUSTOM,100,100><P12></PROGRESS>" .        */

       /*PUT UNFORMATTED "<FORMAT=10x14><OPORTRAIT><P12></PROGRESS>" .*/

       PUT UNFORMATTED "<FORMAT=CUSTOM,2920,3682><OLANDSCAPE><P12></PROGRESS>".

       RUN cerep/jobhpb.p (lv-format-f). 
   END.    

   ELSE
   IF lv-format-f EQ "CentBox"    or 
      lv-format-f eq "Indiana-XL" THEN
     IF lv-int-f EQ 1 THEN
       RUN cerep/jobcbox2.p (lv-format-f).
     ELSE
       RUN cerep/jobcbox.p (lv-format-f).

   ELSE IF lv-format-f EQ "Accord" THEN
      IF lv-int-f EQ 1 THEN
       RUN cerep/jobacc2.p (lv-format-f).
     ELSE
       RUN cerep/jobacc.p (lv-format-f).

   ELSE
   IF lv-format-f EQ "Unipak" THEN
     RUN cerep/jobunipk.p (lv-format-f).

   ELSE IF lv-format-f EQ "Keystone" THEN do:  
    PUT UNFORMATTED "<P10></PROGRESS>" .    
    RUN cerep/jobkeyst.p (lv-format-f).
   END.
   ELSE IF lv-format-f = "MWFibre" THEN do: 
    PUT UNFORMATTED "<P10></PROGRESS>" .    
    RUN cerep/jobmwfbr.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "Knight" THEN DO:  /* rtc */
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobknight.p (lv-format-f).
   END.
   ELSE IF lv-format-f EQ "MidYork" THEN DO: 
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobmidyork.p (lv-format-f).
   END.
   /* gdm - 11030807 */
   ELSE IF lv-format-f = "Dee" THEN do:  
       /* 10x14 works but too much margin, paper size is 11x14 2790,3550
       <FORMAT=10x14> */
       
       /*PUT UNFORMATTED "<OLANDSCAPE><P12></PROGRESS>".*/

       /*PUT UNFORMATTED "<FORMAT=10x14><OLANDSCAPE><P12></PROGRESS>".*/

/*        PUT UNFORMATTED "<FORMAT=CUSTOM,2794,3556><OLANDSCAPE><P12></PROGRESS>". */
       PUT UNFORMATTED "<FORMAT=LEGAL><OLANDSCAPE><P12></PROGRESS>".

       RUN cerep/jobdee.p (lv-format-f).
   END.
   /* gdm - 08270909 */
   ELSE IF lv-format-f EQ "Badger" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobbadgr.p (lv-format-f).                
   END.
   /* gdm - 08270909 end */
   ELSE 
     RUN cerep/jobtick.p (lv-format-f).


END.

/* CORRUGATED */
ELSE IF ip-industry EQ "Corr" THEN DO:    
  IF lv-format-c = "PACIFIC" THEN DO:
     PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" .
     RUN cecrep/jobpacif.p (lv-format-c).
  END.
  IF lv-format-c = "MWBox" THEN do:
     PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" .
     RUN cecrep/jobmwbox.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "Hughes" THEN do:
     PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" .
     RUN cecrep/jobhughs.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "Protagon" THEN do:
      
     PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" .
     RUN cecrep/jobprogn.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "MWFIBRE" THEN do: 
     RUN cecrep/jobmidw.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "BlueRidg" THEN do:
     RUN cecrep/jobblur.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "P&P" THEN do:       
     RUN cecrep/jobpnp.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "CSC" THEN do:       
     RUN cecrep/jobcsc.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "CSC-GA" THEN do:       
     RUN cecrep/jobtickcsc2.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "ARTIOS" THEN do:   /* For Fibre */    
     
     IF tb_tray-2 THEN
     DO:
        DO i = 1 TO 3:
           IF i = 1 THEN
              PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS><BIN=261>".
           ELSE /*HP Laserjet 4100 Universal Print Driver PCL 6*/
              PUT UNFORMATTED "<P10><BIN=260>".
          
           RUN cecrep/jobfibre.p (lv-format-c,i,3,tb_app-unprinted).
        END.
     END.
     ELSE
     DO:
        PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
        RUN cecrep/jobfibre.p (lv-format-c,1,1,tb_app-unprinted).
     END.
  END.
  ELSE IF lv-format-c = "CapCity" THEN do:   /* For Fibre */    
     PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" .
     RUN cecrep/jobcapcity.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "HPB" THEN do:   /* For Hamilton */   
     PUT UNFORMATTED "<FORMAT=10x14><OLANDSCAPE><P12></PROGRESS>" .    
     RUN cecrep/jobhpb.p (lv-format-c). 
  END.    
  ELSE IF lv-format-c = "Premier" THEN do:
      PUT UNFORMATTED "</PROGRESS><P7>" skip.
      RUN cecrep/jobprem.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "Suthrlnd" THEN do:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>". 
      RUN cecrep/jobsuthr.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "Spectrum" THEN do:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>". 
      RUN cecrep/jobspect.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "United" THEN do:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" skip.
      RUN cecrep/jobuntd.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "MulticellGA" THEN do:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" skip.
      RUN cecrep/jobmcga.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "Oklahoma" THEN do:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" skip.
      RUN cecrep/joboklma.p (lv-format-c).
  END.
  ELSE IF lv-format-c EQ "TriLakes2" THEN do:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" skip.
      RUN cecrep/jobtri2.p (lv-format-c).
  END.
  ELSE IF lv-format-c EQ "Michcor" THEN
  DO:
     PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" skip.
     RUN cecrep/jobmich.p (lv-format-c).
  END.
  ELSE IF lv-format-c EQ "ColonialPL" THEN DO:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobcolnlpl.p (lv-format-c). 
  END.
  ELSE IF lv-format-c EQ "LoyLang" THEN DO:
     PUT UNFORMATTED "<OLANDSCAPE><DUPLEX=H><P10></PROGRESS>" skip.
     RUN cecrep/jobloy.p (lv-format-c).
  END.
  ELSE IF lv-format-c EQ "Badger" THEN DO:      
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
      RUN cerep/jobbadgrC.p (lv-format-c).                          
  END.
  ELSE IF lv-format-c = "Delta" THEN do:
     PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" .
     RUN cecrep/jobDelta.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "Allwest" THEN do:
     PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" .
     RUN cecrep/joballws.p (lv-format-c).
  END. 
  ELSE IF lv-format-c = "PQP" THEN do:
      PUT UNFORMATTED "</PROGRESS><P13>" skip.
      RUN cecrep/jobpqp.p (lv-format-c).
  END.
  ELSE IF lv-format-c = "RFC2" THEN do:
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>". 
      RUN cecrep/jobrfc2.p (lv-format-c,tb_app-unprinted).
  END.
  ELSE IF lv-format-c = "PEACHTREE" THEN do:    
     PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>".
     RUN cecrep/jobptree.p (lv-format-c,1,1,tb_app-unprinted,tb_make_hold).
  END.
  ELSE IF lv-ornt = "P" THEN do:
      PUT UNFORMATTED "</PROGRESS><P7>" skip.
      RUN cecrep/jobtick.p (lv-format-c).
  END.
  ELSE do:   
      PUT UNFORMATTED "<OLANDSCAPE><P10></PROGRESS>" skip.
      RUN cecrep/jobtickL.p (lv-format-c).
  END.
END.
