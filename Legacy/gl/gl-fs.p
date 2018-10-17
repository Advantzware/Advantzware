/* ------------------------------------------------------ gl/gl-fs.p 8/94 rd  */
/* g/l income statement report - w/variance                                   */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{gl/gl-fs.i}

DEFINE INPUT PARAMETER ip-excel AS LOG NO-UNDO.

def var skip-line as logical init no.
def var v-outer-cnt as int init 0.
def var zero_bal as logical  no-undo.           /* 9507 CAH */
def var current-yr as int init 0 no-undo.
def var v-first as log no-undo.

def var v-year as int.
def var fisc-yr as int.
DEF VAR li AS INT NO-UNDO.
DEF VAR print-tot LIKE tot NO-UNDO.
DEF VAR print-amt AS DEC NO-UNDO.

def workfile w-account like account.
def buffer   b-w-acc for w-account.

DEF VAR hld-period LIKE uperiod NO-UNDO.
DEF VAR hld-date LIKE udate NO-UNDO.
DEF VAR excel-str-2 AS CHAR FORMAT "X(500)" NO-UNDO.
DEF SHARED STREAM excel.

form prt-detail format "x(200)"
    with frame glinc no-labels no-box width 200 STREAM-IO.

DEF TEMP-TABLE tt-rpt LIKE gl-rpt
                      FIELD seq       AS   INT
                      FIELD amt       LIKE tot
                      FIELD pct       LIKE stot
                      FIELD pdscr     LIKE gl-rpt.dscr EXTENT 12
                      FIELD spct      LIKE tot
                      INDEX seq seq.

DEF BUFFER b-tt-rpt FOR tt-rpt.

DEF TEMP-TABLE tt-account FIELD actnum LIKE account.actnum
                          FIELD company LIKE account.company
                          INDEX actnum actnum company.


if keyfunction(lastkey) eq "end-error" then undo, leave.
else do:
  if not consolidate                    and
     (index(company-list,",") gt 0 or
      company-list eq "")               then company-list = cocode.
      
ASSIGN
 stot = 0
 tot3 = 0.

  /* Create a workfile that contains the data prior to closing the year */   
  for each company where lookup(company.company,company-list) gt 0 no-lock:
    for each account where account.company eq company.company no-lock:
      create w-account.
      buffer-copy account to w-account.
    end.
    
    find first gl-ctrl where gl-ctrl.company eq company.company no-lock.
    
    find first period
        where period.company eq company.company
          and period.pstat   eq yes
        no-lock no-error.
    fisc-yr = (if avail period then period.yr else year(today)) -
              int(not company.yend-per) - 1.
              
    {util/reopenyr.i company.company "w-"}

    for each account
        where account.company eq company.company
          and account.type    eq "R"
        no-lock:
      {gl/gl-fs2.i 3}
    end.
  end.

  DO jj = 1 TO EXTENT(tot3):
    tot3[jj] = - tot3[jj].
  END.

  /* Create Temp Table for REPORT */
  for each gl-rpt
      where gl-rpt.company eq cocode
        and gl-rpt.rpt     eq v-rpt
        and gl-rpt.line    ge 100
      break by gl-rpt.line:

    FIND LAST tt-rpt USE-INDEX seq NO-ERROR.
    i = (IF AVAIL tt-rpt THEN seq ELSE 0) + 1.
    CREATE tt-rpt.
    BUFFER-COPY gl-rpt EXCEPT rec_key TO tt-rpt
    ASSIGN
     tt-rpt.seq = i.

    do i = 1 to 14:
      assign
       tot[i]  = 0
       tot2[i] = 0.
    end.

    /* DESCRIPTION ONLY */
    if gl-rpt.type eq {gl/gl-type.i 90} then next.

      /* ACCOUNT DETAIL LINES */
    if gl-rpt.type ge {gl/gl-type.i 21} and
       gl-rpt.type le {gl/gl-type.i 24} then do:
      /* multiple accounts - multiple lines  also calculate for
                             multiple accounts single line */

      /* Create Temp Table with the list of GL Account#'s for this line */
      FOR EACH tt-account:
        DELETE tt-account.
      END.

      DO v-outer-cnt = 1 TO EXTENT(gl-rpt.actnum):
        IF gl-rpt.actnum[v-outer-cnt] NE "" THEN DO:
          CREATE tt-account.
          tt-account.actnum = gl-rpt.actnum[v-outer-cnt].
        END.
      END.

      DO v-outer-cnt = 1 TO EXTENT(gl-rpt.acrange2):
        IF gl-rpt.acrange2[v-outer-cnt] NE "" THEN DO:
          CREATE tt-account.
          tt-account.actnum = gl-rpt.acrange2[v-outer-cnt].
        END.
      END.

      RELEASE tt-account.

      DO v-outer-cnt = 1 TO NUM-ENTRIES(company-list):
        FOR EACH account
            WHERE account.company  EQ ENTRY(v-outer-cnt,company-list)

              AND ((account.actnum GE gl-rpt.acrange[1]  AND
                    account.actnum LE gl-rpt.acrange[2])
               OR  (account.actnum GE gl-rpt.acrange1[1] AND
                    account.actnum LE gl-rpt.acrange1[2])
               OR  (account.actnum GE gl-rpt.acrange1[3] AND
                    account.actnum LE gl-rpt.acrange1[4])
               OR  (account.actnum GE gl-rpt.acrange1[5] AND
                    account.actnum LE gl-rpt.acrange1[6])
               OR  (account.actnum GE gl-rpt.acrange1[7] AND
                    account.actnum LE gl-rpt.acrange1[8])
               OR  CAN-FIND(FIRST tt-account
                            WHERE tt-account.company EQ ""
                              AND account.actnum     MATCHES tt-account.actnum))

              AND ((subac-lvl       EQ 1      AND
                    account.n1      GE fsubac AND
                    account.n1      LE tsubac)
               OR  (subac-lvl       EQ 2      AND
                    account.n2      GE fsubac AND
                    account.n2      LE tsubac)
               OR  (subac-lvl       EQ 3      AND
                    account.n3      GE fsubac AND
                    account.n3      LE tsubac)
               OR  (subac-lvl       EQ 4      AND
                    account.n4      GE fsubac AND
                    account.n4      LE tsubac)
               OR  (subac-lvl       EQ 5      AND
                    account.n5      GE fsubac AND
                    account.n5      LE tsubac))
            NO-LOCK:
          CREATE tt-account.
          BUFFER-COPY account TO tt-account.
        END.
      END.

      FOR EACH tt-account BREAK BY tt-account.company BY tt-account.actnum:
        IF NOT FIRST-OF(tt-account.actnum) OR
           tt-account.company EQ ""        THEN DELETE tt-account.
      END.

      FOR EACH tt-account,
          FIRST account
          WHERE account.company EQ tt-account.company
            AND account.actnum  EQ tt-account.actnum
          NO-LOCK
          BREAK BY tt-account.actnum:

        RUN detail-line (FIRST-OF(tt-account.actnum),
                         LAST-OF(tt-account.actnum)).
      END.

      /* MULTIPLE ACCOUNTS - SINGLE LINE */
      if gl-rpt.type eq {gl/gl-type.i 21} or
         gl-rpt.type eq {gl/gl-type.i 23} then do: 

        if v-vcol[1] ne 0 and v-vcol[2] ne 0 and v-vcol[3] ne 0 then
         tot[10]  = tot[lookup(v-ct[v-vcol[2]],c-t-dscr)]  -
                    tot[lookup(v-ct[v-vcol[3]],c-t-dscr)].
        if v-vcol[4] ne 0 and v-vcol[5] ne 0 and v-vcol[6] ne 0 then
         tot[11]  = tot[lookup(v-ct[v-vcol[5]],c-t-dscr)]  -
                    tot[lookup(v-ct[v-vcol[6]],c-t-dscr)].
        do jj = 1 to 12:
          if substr(gl-rpt.flag,jj ,1) = "Y" then
          do i = 1 to 14:
            stot[jj * 14 - 14 + i]  = stot[jj * 14 - 14 + i] + tot[i].
          end.
        end.
        if gl-rpt.type eq {gl/gl-type.i 21} then do jj = 1 to 14:
          tot[jj] = - tot[jj].
        end.

        DO i = 1 TO v-no-col:
          tt-rpt.amt[i] = IF v-onevar EQ i THEN tot[10]
                          ELSE
                          IF v-twovar EQ i THEN tot[11]
                          ELSE
                          IF all-per       THEN tot[i]
                          ELSE
                          IF LOOKUP(v-ct[i],c-t-dscr) NE 0
                          THEN tot[LOOKUP(v-ct[i],c-t-dscr)]
                          ELSE 0.
        END.
        
        if gl-rpt.type eq {gl/gl-type.i 21} then do jj = 1 to 14:
          tot[jj] = - tot[jj].
        end.
      end.
    end. /* account detail lines */

    release account.

    /* WE HIT A SUB-TOTAL LINE */
    if gl-rpt.type eq {gl/gl-type.i 60} or
       gl-rpt.type eq {gl/gl-type.i 61} then do with frame glinc:

      if v-vcol[1] ne 0 and v-vcol[2] ne 0 and v-vcol[3] ne 0 then
       stot[gl-rpt.level * 14 - 14 + 10]  =
            stot[gl-rpt.level * 14 - 14 + lookup(v-ct[v-vcol[2]],c-t-dscr)] -
            stot[gl-rpt.level * 14 - 14 + lookup(v-ct[v-vcol[3]],c-t-dscr)].

      if v-vcol[4] ne 0 and v-vcol[5] ne 0 and v-vcol[6] ne 0 then
       stot[gl-rpt.level * 14 - 14 + 11]  =
            stot[gl-rpt.level * 14 - 14 + lookup(v-ct[v-vcol[5]],c-t-dscr)] -
            stot[gl-rpt.level * 14 - 14 + lookup(v-ct[v-vcol[6]],c-t-dscr)].

      if gl-rpt.type eq 61 then do jj = 1 to 14:
        stot[gl-rpt.level * 14 - 14 + jj] =
             - stot[gl-rpt.level * 14 - 14 + jj].
      end.

      DO i = 1 TO v-no-col:
        tt-rpt.amt[i] = IF v-onevar EQ i THEN stot[gl-rpt.level * 14 - 14 + 10]
                        ELSE
                        IF v-twovar EQ i THEN stot[gl-rpt.level * 14 - 14 + 11]
                        ELSE
                        IF all-per       THEN stot[gl-rpt.level * 14 - 14 + i]
                        ELSE
                        IF LOOKUP(v-ct[i],c-t-dscr) NE 0
                                         THEN stot[gl-rpt.level * 14 - 14 + lookup(v-ct[i],c-t-dscr)]
                        ELSE 0.
      END.

      if gl-rpt.type eq {gl/gl-type.i 61} then do jj = 1 to 14:
        stot[gl-rpt.level * 14 - 14 + jj] = - stot[gl-rpt.level * 14 - 14 + jj].
      end.
    end. /* end subtotal */
  end. /* each gl-rpt */

  /*release gl-rpt.*/

  /* Calculate Percentages of Subtotals */
  FOR EACH tt-rpt
      WHERE tt-rpt.type EQ {gl/gl-type.i 60} OR
            tt-rpt.type EQ {gl/gl-type.i 61}:

    DO jj = 1 TO 12:
      IF SUBSTR(tt-rpt.flag,jj,1) EQ "Y"  THEN
      FOR EACH b-tt-rpt
          WHERE b-tt-rpt.type              GE {gl/gl-type.i 21}
            AND b-tt-rpt.type              LE {gl/gl-type.i 24}
            AND SUBSTR(b-tt-rpt.flag,jj,1) EQ "Y":

        b-tt-rpt.pdscr[jj] = tt-rpt.dscr.

        DO i = 1 TO 14:
          IF b-tt-rpt.amt[i] NE 0 AND tt-rpt.amt[i] NE 0 THEN
            b-tt-rpt.pct[jj * 14 - 14 + i] = b-tt-rpt.amt[i] /
                                             tt-rpt.amt[i] * 100.
        END.
      END.
    END.
  END.

  /* Print REPORT */
  for each tt-rpt break by tt-rpt.line by tt-rpt.seq with frame glinc:
    if tt-rpt.type ge 100 and not first(tt-rpt.line) then page.

    if skip-line = yes then do:
      skip-line = no.
      if tt-rpt.type ne {gl/gl-type.i 60} and
         tt-rpt.type ne {gl/gl-type.i 61} and
         tt-rpt.type ne {gl/gl-type.i 73} and
         tt-rpt.type ne {gl/gl-type.i 71} then put skip(1).
    end.

    /* DESCRIPTION ONLY */
    if tt-rpt.type eq {gl/gl-type.i 90} then do with frame glinc:
      
      display tt-rpt.dscr @ prt-detail with no-labels no-box.
      down with frame glinc.

      IF ip-excel THEN
        PUT STREAM excel
          tt-rpt.dscr SKIP.
    end.

    ELSE
      /* ACCOUNT DETAIL LINES */
    if tt-rpt.type ge {gl/gl-type.i 21} and
       tt-rpt.type le {gl/gl-type.i 24} then RUN display-line.

    ELSE
    /* WE HIT A SUB-TOTAL LINE */
    if tt-rpt.type eq {gl/gl-type.i 60} or
       tt-rpt.type eq {gl/gl-type.i 61} then do with frame glinc:

      prt-detail = fill(" ",v-d-wid).

      IF ip-excel THEN
        excel-str-2 = ",".

      do i = 1 to v-no-col:
        prt-detail = prt-detail + sul-format + (IF v-per[i] THEN sul-formats ELSE "").

        IF ip-excel THEN
          excel-str-2 = excel-str-2 + '"' + sul-format
                      + (IF v-per[i] THEN sul-formats ELSE "") + '",'.
      end.
      
      display prt-detail with frame glinc.
      down with frame glinc.

      IF ip-excel THEN
        PUT STREAM excel excel-str-2 SKIP.

      RUN display-line.

      skip-line = yes.
    end. /* end subtotal */

    ELSE
    /* SINGLE UNDERLINE */
    if tt-rpt.type eq {gl/gl-type.i 71} then do with frame glinc:
      assign
       prt-detail = fill(" ",200)
       prt-detail = substr(prt-detail,1,v-d-wid).

      if ip-excel then
        excel-str-2 = ",".

      do i = 1 to v-no-col:
        prt-detail = prt-detail + sul-format + (IF v-per[i] THEN sul-formats ELSE "").

        IF ip-excel THEN
          excel-str-2 = excel-str-2 + '"' + sul-format
                      + (IF v-per[i] THEN sul-formats ELSE "") + '",'.
      end.
      
      display prt-detail with frame glinc.
      down with frame glinc.

      IF ip-excel THEN
        PUT STREAM excel excel-str-2 SKIP.

    end. /* end single underline */

    ELSE
    /* DOUBLE UNDERLINE */
    if tt-rpt.type eq {gl/gl-type.i 73} then do with frame glinc:
      assign
       prt-detail = fill(" ",200)
       prt-detail = substr(prt-detail,1,v-d-wid).

      IF ip-excel THEN
        excel-str-2 = ",".

      do i = 1 to v-no-col:
        prt-detail = prt-detail + dul-format + (IF v-per[i] THEN dul-formats ELSE "").

        IF ip-excel THEN
          excel-str-2 = excel-str-2 + '"' + dul-format
                      + (IF v-per[i] THEN dul-formats ELSE "") + '",'. 
      end.
      
      display prt-detail with frame glinc.
      down with frame glinc.

      IF ip-excel THEN
        PUT STREAM excel excel-str-2 SKIP.
    end. /* end double underline */
    
    DELETE tt-rpt.
  end. /* each tt-rpt */
end.

PAUSE 0.

RETURN.

PROCEDURE detail-line.
  DEF INPUT PARAM ip-frst-of AS LOG NO-UNDO.
  DEF INPUT PARAM ip-last-of AS LOG NO-UNDO.
    

  FIND FIRST company WHERE company.company EQ account.company NO-LOCK NO-ERROR.

  IF gl-rpt.type EQ {gl/gl-type.i 22} OR
     gl-rpt.type EQ {gl/gl-type.i 24} THEN DO:
    IF ip-frst-of THEN tot = 0.
    tot2 = 0.
  END.

  {gl/gl-fs2.i}

  DO i = 1 TO 14:
    /*if gl-ctrl.ret eq account.actnum and i eq 9 then
      tot2[i] = -1 * (if period.yr gt fisc-yr then account.cyr-open   else
                      if period.yr eq fisc-yr then account.lyr-open   else
                      if avail w-account      then w-account.lyr-open else 0).
    
    else */
    if gl-rpt.type eq {gl/gl-type.i 22} or
       gl-rpt.type eq {gl/gl-type.i 21} then
      tot2[i] = tot2[i] - tot[i].
    
    else
      tot2[i] = tot2[i] + tot[i].
  END.

  RELEASE gltrans.
      
  IF ip-last-of THEN DO:
    IF gl-rpt.type EQ {gl/gl-type.i 22} OR
       gl-rpt.type EQ {gl/gl-type.i 24} THEN
    DO jj = 1 TO 12:
      IF SUBSTR(gl-rpt.flag,jj,1) EQ "Y" THEN
      DO i = 1 TO 14:
        stot[jj * 14 - 14 + i] = stot[jj * 14 - 14 + i] + tot[i].
      END.
    END.

    IF gl-rpt.type EQ {gl/gl-type.i 22} OR
       gl-rpt.type EQ {gl/gl-type.i 24} THEN DO:

      FIND LAST b-tt-rpt USE-INDEX seq NO-ERROR.
      i = (IF AVAIL b-tt-rpt THEN b-tt-rpt.seq ELSE 0) + 1.
      CREATE b-tt-rpt.
      BUFFER-COPY gl-rpt EXCEPT rec_key TO b-tt-rpt
      ASSIGN
       b-tt-rpt.seq  = i
       b-tt-rpt.dscr = (IF ll-acct# THEN TRIM(account.actnum) + " " ELSE "") + account.dscr.

      DO i = 1 TO v-no-col:
        b-tt-rpt.amt[i] = IF v-onevar EQ i THEN tot2[10]
                          ELSE
                          IF v-twovar EQ i THEN tot2[11]
                          ELSE
                          IF all-per       THEN tot2[i]
                          ELSE
                          IF LOOKUP(v-ct[i],c-t-dscr) NE 0
                                           THEN tot2[LOOKUP(v-ct[i],c-t-dscr)]
                          ELSE 0.
      END.
    END.
  END.

END PROCEDURE.

PROCEDURE display-line.

  DEF VAR pct_printed AS LOG INIT NO NO-UNDO.
  DEF VAR excel-str AS CHAR FORMAT "X(500)" NO-UNDO.

  ASSIGN
   prt-detail = tt-rpt.dscr + FILL(" ",200)
   prt-detail = SUBSTR(prt-detail,1,v-d-wid)
   zero_bal   = YES.

  IF ip-excel THEN 
    excel-str = '"' + tt-rpt.dscr + '",'.
  

  DO i = 1 TO v-no-col:
    IF tt-rpt.amt[i] NE 0 THEN zero_bal = NO.

    IF tt-rpt.amt[i] EQ 0 AND supp_zero THEN DO:
      prt-detail = prt-detail + FILL(" ",LENGTH(tot-format)).

      IF ip-excel THEN
        excel-str = excel-str + ",".
    END.
    ELSE DO:
      prt-detail = prt-detail + STRING(tt-rpt.amt[i],tot-format).
      IF ip-excel THEN
        excel-str = excel-str + '"' + STRING(tt-rpt.amt[i],tot-format) + '",'.
    END.

    IF v-per[i] THEN DO:
      tt-rpt.spct[i] = ROUND(tt-rpt.amt[i] / (IF v-onevar EQ i THEN tot3[10]
                                              ELSE
                                              IF v-twovar EQ i THEN tot3[11]
                                              ELSE
                                              IF all-per       THEN tot3[i]
                                              ELSE
                                              IF LOOKUP(v-ct[i],c-t-dscr) NE 0
                                                               THEN tot3[LOOKUP(v-ct[i],c-t-dscr)]
                                              ELSE 0) * 100,2).

      IF tt-rpt.spct[i] EQ ? THEN tt-rpt.spct[i] = 0.

      IF tt-rpt.spct[i] EQ 0 AND supp_zero THEN DO:
        prt-detail = prt-detail + FILL(" ",LENGTH(pct-formats)).
        IF ip-excel THEN
          excel-str = excel-str + ",".
      END.
      ELSE DO:
        prt-detail = prt-detail + STRING(tt-rpt.spct[i],pct-formats).
        IF ip-excel THEN
          excel-str = excel-str + '"' + STRING(tt-rpt.spct[i],pct-formats)
                    + '",'.
      END.
    END.
  END.

  IF NOT zero_bal OR NOT skip_zero THEN DO:

    DISPLAY prt-detail WITH FRAME glinc.
    DOWN WITH FRAME glinc.

    IF ip-excel THEN
      PUT STREAM excel excel-str SKIP.

    /* Print Percentages */
    RELEASE reftable.

    IF tt-rpt.type NE {gl/gl-type.i 60} AND
       tt-rpt.type NE {gl/gl-type.i 61} THEN

      IF tt-rpt.pct-subtotal = yes THEN
    DO jj = 1 TO 12:
      IF SUBSTR(tt-rpt.flag,jj,1) EQ "Y" THEN DO:
        ASSIGN
         prt-detail = "    % of " + TRIM(tt-rpt.pdscr[jj]) + FILL(" ",200)
         prt-detail = SUBSTR(prt-detail,1,v-d-wid)
         zero_bal   = YES.

        IF ip-excel THEN
          excel-str = '"' + "    % of " + TRIM(tt-rpt.pdscr[jj]) + '",'.

        DO i = 1 TO v-no-col:
          zero_bal = NO.

          IF tt-rpt.pct[jj * 14 - 14 + i] EQ 0 AND supp_zero THEN DO:
            prt-detail = prt-detail + FILL(" ",LENGTH(pct-format)).
            IF ip-excel THEN
              excel-str = excel-str + ",,".
          END.
          ELSE DO:
            prt-detail = prt-detail + STRING(tt-rpt.pct[jj * 14 - 14 + i],
                                             pct-format).
            IF ip-excel THEN
              excel-str = excel-str + '"' + STRING(tt-rpt.pct[jj * 14 - 14 + i],
                                             pct-format) + '",,'.
          END.
        END.

        IF NOT zero_bal THEN DO:
          DISPLAY prt-detail WITH FRAME glinc.
          DOWN WITH FRAME glinc.

          IF ip-excel THEN
            PUT STREAM excel excel-str SKIP.

          pct_printed = YES.
        END.
      END.
    END.

    IF pct_printed THEN PUT SKIP(1).
  END.
END.

/* End ---------------------------------- Copr. 1994  Advanced Software, Inc. */
