/* ----------------------------------------------------- gl/gl-fs2.i 8/94 rd  */
/* g/l report -                                                               */
/* -------------------------------------------------------------------------- */

DEF BUFFER b-period{1} FOR period.


find first gl-ctrl where gl-ctrl.company eq account.company no-lock no-error.

find first w-account
    where w-account.company eq account.company
      and w-account.actnum  eq account.actnum
    no-error.

find first period
    where period.company eq company.company 
      and period.pstat   eq yes
    no-lock no-error.
current-yr = if avail period then period.yr else year(today).
   
find first period
    where period.company eq company.company
      and period.pst     le udate
      and period.pend    ge udate
    no-lock.

IF all-per THEN DO:
  DO li = 1 TO v-no-col - 1:
    ASSIGN
     all-per-tot{1}[li] = tot{1}[li].
  END.

  ASSIGN
   fisc-yr     = period.yr
   hld-period  = uperiod
   hld-date    = udate.
  
  FOR EACH period
      WHERE period.company EQ company.company
        AND period.yr      EQ fisc-yr
        AND period.pnum    LE hld-period
      NO-LOCK
      BY period.pnum:
    ASSIGN
     tot{1}  = 0
     uperiod = period.pnum
     udate   = period.pend.

    RUN gl/gl-tot (RECID(account), INT("{1}")).

    IF pre-close THEN RUN gl/gl-pre.p (RECID(account), INT("{1}")).

    all-per-tot{1}[period.pnum] = all-per-tot{1}[period.pnum] + tot{1}[1].
  END.

  ASSIGN
   tot{1}  = 0
   uperiod = hld-period
   udate   = hld-date.

  DO li = 1 TO v-no-col - 1:
    ASSIGN
     tot{1}[li]       = all-per-tot{1}[li]
     tot{1}[v-no-col] = tot{1}[v-no-col] + tot{1}[li].
  END.
END.

ELSE DO:
  fisc-yr = current-yr - int(not company.yend-per).

  tot{1}[3] = tot{1}[3] + (if period.yr gt fisc-yr then account.cyr-open   else
                           if period.yr eq fisc-yr then account.lyr-open   else
                           if avail w-account      then w-account.lyr-open else 0).

  IF INDEX("ALCT",account.type) GT 0 THEN DO:
    do i = 1 to company.num-per:
      assign
       tot{1}[3] = tot{1}[3] + (if period.yr gt fisc-yr then account.cyr[i]   else
                                if period.yr eq fisc-yr then account.lyr[i]   else
                                if avail w-account      then w-account.lyr[i] else 0).
    end.
  END.

  run gl/gl-tot.p (recid(account), INT("{1}")).

  tot{1}[4] = tot{1}[4] + account.bud[uperiod].

  if uperiod ge 1 and uperiod le 3 then
  do i = 1 to uperiod:
    tot{1}[5] = tot{1}[5] + account.bud[i].
  end.

  if uperiod ge 4 and uperiod le 6 then
  do i = 4 to uperiod:
    tot{1}[5] = tot{1}[5] + account.bud[i].
  end.

  if uperiod ge 7 and uperiod le 9 then
  do i = 7 to uperiod:
    tot{1}[5] = tot{1}[5] + account.bud[i].
  end.

  if uperiod ge 10 and uperiod le 13 then
  do i = 10 to uperiod:
    tot{1}[5] = tot{1}[5] + account.bud[i].
  end.

  do pp = 1 to uperiod:
    tot{1}[6] = tot{1}[6] + account.bud[pp].
  end.

  tot{1}[7] = tot{1}[7] + (if period.yr gt fisc-yr then account.cyr[uperiod]   else
                           if period.yr eq fisc-yr then account.lyr[uperiod]   else
                           if avail w-account      then w-account.lyr[uperiod] else 0).

  if uperiod ge 1 and uperiod le 3 then
  do i = 1 to uperiod:
    tot{1}[8] = tot{1}[8] + (if period.yr gt fisc-yr then account.cyr[i]   else
                             if period.yr eq fisc-yr then account.lyr[i]   else
                             if avail w-account      then w-account.lyr[i] else 0).
  end.

  if uperiod ge 4 and uperiod le 6 then
  do i = 4 to uperiod:
    tot{1}[8] = tot{1}[8] + (if period.yr gt fisc-yr then account.cyr[i]   else
                             if period.yr eq fisc-yr then account.lyr[i]   else
                             if avail w-account      then w-account.lyr[i] else 0).
  end.

  if uperiod ge 7 and uperiod le 9 then
  do i = 7 to uperiod:
    tot{1}[8] = tot{1}[8] + (if period.yr gt fisc-yr then account.cyr[i]   else
                             if period.yr eq fisc-yr then account.lyr[i]   else
                             if avail w-account      then w-account.lyr[i] else 0).
  end.

  if uperiod ge 10 and uperiod le 13 then
  do i = 10 to uperiod:
    tot{1}[8] = tot{1}[8] + (if period.yr gt fisc-yr then account.cyr[i]   else
                             if period.yr eq fisc-yr then account.lyr[i]   else
                             if avail w-account      then w-account.lyr[i] else 0).
  end.

  tot{1}[9] = tot{1}[9] + (if period.yr gt fisc-yr then account.cyr-open   else
                           if period.yr eq fisc-yr then account.lyr-open   else
                           if avail w-account      then w-account.lyr-open else 0).

  do pp = 1 to uperiod:
    tot{1}[9] = tot{1}[9] + (if period.yr gt fisc-yr then account.cyr[pp]   else
                             if period.yr eq fisc-yr then account.lyr[pp]   else
                             if avail w-account      then w-account.lyr[pp] else 0).
  end.

  if pre-close then run gl/gl-pre.p (recid(account), INT("{1}")).

  if v-vcol[1] ne 0 and v-vcol[2] ne 0 and v-vcol[3] ne 0 then 
    tot{1}[10] = tot{1}[10] + tot{1}[lookup(v-ct[v-vcol[2]],c-t-dscr)]  -
                              tot{1}[lookup(v-ct[v-vcol[3]],c-t-dscr)].
                      
  if v-vcol[4] ne 0 and v-vcol[5] ne 0 and v-vcol[6] ne 0 then 
    tot{1}[11] = tot{1}[11] + tot{1}[lookup(v-ct[v-vcol[5]],c-t-dscr)]  -
                              tot{1}[lookup(v-ct[v-vcol[6]],c-t-dscr)].
END.
 
/* end ---------------------------------- Copr. 1994  Advanced Software, Inc. */
