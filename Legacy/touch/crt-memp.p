 /*addon/touch/crt-memp.p  */
DEF INPUT PARAMETER ip-company_code AS cha NO-UNDO.
DEF INPUT PARAM ip-machine_code AS cha NO-UNDO.
DEF INPUT PARAM ip-machtran-rowid AS ROWID NO-UNDO.
DEF BUFFER bf-machtran FOR machtran .
DEF BUFFER buf-machemp FOR machemp.
DEF BUFFER buf-mach FOR mach.

DEF VAR v-start-date-1 AS CHAR NO-UNDO.
DEF VAR v-start-date-2 AS CHAR NO-UNDO.

{custom/emprate.i}

FIND bf-machtran WHERE ROWID(bf-machtran) = ip-machtran-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL bf-machtran THEN RETURN.

DEF VAR lv-mach-list AS cha NO-UNDO.
FIND FIRST mach NO-LOCK WHERE mach.company = ip-company_code
                                    AND mach.m-code = ip-machine_code NO-ERROR.
IF mach.sch-m-code <> "" THEN DO:
  FOR EACH buf-mach FIELDS(m-code) WHERE
      buf-mach.company EQ mach.company AND
      buf-mach.loc     EQ mach.loc AND
      buf-mach.sch-m-code = mach.sch-m-code
      NO-LOCK:
      lv-mach-list = lv-mach-list + buf-mach.m-code + ",".
  END.
END.

FOR EACH emplogin WHERE
    emplogin.company = ip-company_code AND
    (emplogin.machine = ip-machine_code OR LOOKUP(emplogin.machine,lv-mach-list) > 0) AND
    emplogin.end_date = ? AND
    emplogin.end_time = 0 AND
    emplogin.total_time = 0
    NO-LOCK:

    ASSIGN
       v-start-date-1 = STRING(YEAR(emplogin.START_date),"9999") +
                        STRING(MONTH(emplogin.START_date),"99")  +
                        STRING(DAY(emplogin.START_date),"99")    +
                        STRING(emplogin.START_time,"9999999")
       v-start-date-2 = STRING(YEAR(bf-machtran.start_date),"9999") +
                        STRING(MONTH(bf-machtran.start_date),"99")  +
                        STRING(DAY(bf-machtran.start_date),"99")    +
                        STRING(bf-machtran.start_time,"9999999").

    IF v-start-date-1 GT v-start-date-2 THEN NEXT.

    FIND FIRST employee WHERE
         employee.company EQ emplogin.company and
         employee.employee EQ emplogin.employee
         NO-LOCK.

    IF NOT CAN-FIND(FIRST buf-machemp WHERE
       buf-machemp.table_rec_key = bf-machtran.rec_key AND
       buf-machemp.employee = emplogin.employee AND
       buf-machemp.start_date = bf-machtran.start_date AND
       buf-machemp.start_time = bf-machtran.start_time) THEN
       DO:
          CREATE buf-machemp.
          ASSIGN
             buf-machemp.table_rec_key = bf-machtran.rec_key
             buf-machemp.employee = emplogin.employee
             buf-machemp.start_date = bf-machtran.start_date
             buf-machemp.start_time = bf-machtran.start_time
             buf-machemp.end_date = bf-machtran.end_date
             buf-machemp.end_time = bf-machtran.end_time
             buf-machemp.shift = bf-machtran.shift
             buf-machemp.ratetype = 'Standard'
             buf-machemp.rate_usage = employee.rate_usage.
           RUN Employee-Rate(ip-company_code,buf-machemp.employee,buf-machemp.shift,ip-machine_code,
                             buf-machemp.rate_usage,buf-machemp.ratetype,OUTPUT buf-machemp.rate).
           {custom/calctime.i &file="buf-machemp"}

           RELEASE buf-machemp.
       END.
END. /* each emplogin */
