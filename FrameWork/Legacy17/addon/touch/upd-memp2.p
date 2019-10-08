 /*addon/touch/upd-memp2.p  */
DEF INPUT PARAM ip-machtran-rowid AS ROWID NO-UNDO.

DEF BUFFER bf-machtran FOR machtran.
DEF BUFFER buf-machemp FOR machemp.
DEF BUFFER buf-mach FOR mach.

DEF VAR lv-mach-list AS cha NO-UNDO.
DEF VAR v-date-time-1 AS CHAR NO-UNDO.
DEF VAR v-date-time-2 AS CHAR NO-UNDO.

FIND bf-machtran WHERE ROWID(bf-machtran) = ip-machtran-rowid NO-LOCK NO-ERROR.
IF NOT AVAIL bf-machtran THEN RETURN.

FIND FIRST mach WHERE
     mach.company = bf-machtran.company AND
     mach.m-code = bf-machtran.machine
     NO-LOCK NO-ERROR.

IF AVAIL mach AND mach.sch-m-code <> "" THEN
   FOR EACH buf-mach FIELDS(m-code) WHERE
       buf-mach.company EQ mach.company AND
       buf-mach.loc     EQ mach.loc AND
       buf-mach.sch-m-code = mach.sch-m-code
       NO-LOCK:
       lv-mach-list = lv-mach-list + buf-mach.m-code + ",".
   END.

FOR EACH buf-machemp WHERE
    buf-machemp.TABLE_rec_key = bf-machtran.rec_key AND
    buf-machemp.shift = bf-machtran.shift,
    FIRST emplogin WHERE
          emplogin.company = bf-machtran.company AND
          emplogin.employee = buf-machemp.employee AND
          (emplogin.machine = bf-machtran.machine OR LOOKUP(emplogin.machine,lv-mach-list) > 0) AND
          emplogin.end_date = ? AND
          emplogin.end_time = 0 AND
          emplogin.total_time = 0
          NO-LOCK:

          ASSIGN
             buf-machemp.end_date = bf-machtran.end_date
             buf-machemp.end_time = bf-machtran.end_time.
              
          {custom/calctime.i &file="buf-machemp"}
END.

FOR EACH buf-machemp WHERE buf-machemp.TABLE_rec_key EQ bf-machtran.rec_key AND
    buf-machemp.end_date NE ?:
    ASSIGN
       v-date-time-1 = STRING(YEAR(buf-machemp.end_date),"9999") +
                       STRING(MONTH(buf-machemp.end_date),"99")  +
                       STRING(DAY(buf-machemp.end_date),"99")    +
                       STRING(buf-machemp.end_time,"9999999")
       v-date-time-2 = STRING(YEAR(bf-machtran.end_date),"9999") +
                       STRING(MONTH(bf-machtran.end_date),"99")  +
                       STRING(DAY(bf-machtran.end_date),"99")    +
                       STRING(bf-machtran.end_time,"9999999").

    IF v-date-time-1 GT v-date-time-2 THEN
    DO:
       ASSIGN buf-machemp.END_date = bf-machtran.end_date
              buf-machemp.END_time = bf-machtran.END_time.

       {custom/calctime.i &file="buf-machemp"}

       ASSIGN
          v-date-time-1 = STRING(YEAR(buf-machemp.end_date),"9999") +
                          STRING(MONTH(buf-machemp.end_date),"99")  +
                          STRING(DAY(buf-machemp.end_date),"99")    +
                          STRING(buf-machemp.end_time,"9999999")
          v-date-time-2 = STRING(YEAR(buf-machemp.start_date),"9999") +
                          STRING(MONTH(buf-machemp.start_date),"99")  +
                          STRING(DAY(buf-machemp.start_date),"99")    +
                          STRING(buf-machemp.start_time,"9999999").

       IF v-date-time-1 LT v-date-time-2 THEN
          DELETE machemp.
    END.
END.
