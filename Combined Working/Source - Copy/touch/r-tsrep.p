DEFINE SHARED VARIABLE LvWeekending AS DATE NO-UNDO.
DEFINE SHARED VARIABLE LvEmpNum AS CHAR NO-UNDO.
DEFINE SHARED VARIABLE LvTSFolder AS CHAR NO-UNDO.

{methods/defines/hndldefs.i}
{custom/gcompany.i}
{custom/getcmpny.i}

DEFINE SHARED TEMP-TABLE tt-tsrep
    FIELD tt-date AS DATE
    FIELD tt-time-in1 AS CHAR 
    FIELD tt-time-out1 AS CHAR
    FIELD tt-time-in2 AS CHAR
    FIELD tt-time-out2 AS CHAR
    FIELD tt-wrk-hrs AS CHAR
    FIELD tt-ot-hrs AS CHAR
    FIELD tt-dt-hrs AS CHAR
    INDEX pi-tsrep tt-date.

DEFINE SHARED TEMP-TABLE tt-note NO-UNDO
  FIELD employee LIKE emplogin.employee
  FIELD rec_key LIKE notes.rec_key
  FIELD note_date LIKE notes.note_date
  FIELD note_title LIKE notes.note_title
  FIELD note_text LIKE notes.note_text
  FIELD note_src AS CHARACTER.

  DEFINE VARIABLE li-overtime AS INTEGER LABEL "OT @x1.5" NO-UNDO.
  DEFINE VARIABLE li-2time AS INTEGER LABEL "OT @x2" NO-UNDO.
  DEFINE VARIABLE li-day-time AS INTEGER NO-UNDO.
  
  DEFINE VARIABLE li-start-time AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-end-time AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-start-time2 AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-end-time2 AS INTEGER NO-UNDO.

  DEFINE VARIABLE li-emp-over AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-emp-2time AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-emp-tot AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-emp-tot-hr AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-emp-tot-min AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-over-tot-hr AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-over-tot-min AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-2time-tot-hr AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-2time-tot-min AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-overtime-to-apply AS INTEGER NO-UNDO.
  DEFINE VARIABLE li-tmp-wrk-hrs AS INT NO-UNDO.
  DEFINE VARIABLE li-tmp-ot-hrs AS INT NO-UNDO.
  DEFINE VARIABLE li-ot-extra AS INT NO-UNDO.
  DEFINE VARIABLE v-total-time-hr-min AS CHAR NO-UNDO.
  DEFINE VARIABLE v-total-time-sec AS INT NO-UNDO.

  DEFINE BUFFER bf-employee for employee.
  DEFINE BUFFER bf-machemp for machemp.
  DEFINE BUFFER bf-emplogin FOR emplogin.

  DEFINE VARIABLE li-reg AS INT NO-UNDO.
  DEF VAR v-index AS INT NO-UNDO.

  EMPTY TEMP-TABLE tt-note.
  EMPTY TEMP-TABLE tt-tsrep.
  
  for each emplogin FIELDS(rec_key employee) no-lock where
      emplogin.company = gcompany AND
      emplogin.employee = LvEmpNum ,
      each notes where
           notes.rec_key = emplogin.rec_key and 
           notes.note_date >= ( LvWeekEnding - 6 ) and
           notes.note_date <= LvWeekEnding
           no-lock:     

       create tt-note.
       assign tt-note.employee = emplogin.employee
              tt-note.rec_key = notes.rec_key
              tt-note.note_date = notes.note_date
              tt-note.note_title = notes.note_title
              tt-note.note_text = notes.note_text 
              tt-note.note_src = "Log In/Out".
       RELEASE tt-note.
   end.

   for each bf-employee FIELDS(rec_key employee) where
       bf-employee.company EQ gcompany AND
       bf-employee.employee = LvEmpNum
       no-lock,
       each notes where
            notes.rec_key = bf-employee.rec_key and
            notes.note_date >= ( LvWeekEnding - 6 ) and
            notes.note_date <= LvWeekEnding
            NO-LOCK:

         CREATE tt-note.
         assign tt-note.employee = bf-employee.employee
              tt-note.rec_key = notes.rec_key
              tt-note.note_date = notes.note_date
              tt-note.note_title = notes.note_title
              tt-note.note_text = notes.note_text
              tt-note.note_src = "Employee".
         RELEASE tt-note.
   end.    
   for each bf-machemp FIELDS(rec_key employee) where
       bf-machemp.employee = LvEmpNum
       no-lock,
       each notes where
            notes.rec_key = bf-machemp.rec_key and
            notes.note_date >= ( LvWeekEnding - 6 ) and
            notes.note_date <= LvWeekEnding
            NO-LOCK:

            create tt-note.
            assign tt-note.employee = bf-machemp.employee
                   tt-note.rec_key = notes.rec_key
                   tt-note.note_date = notes.note_date
                   tt-note.note_title = notes.note_title
                   tt-note.note_text = notes.note_text
                   tt-note.note_src = "Emp. Transaction".
            RELEASE tt-note.
   end.    
  
   for each emplogin no-lock where
       emplogin.company EQ gcompany AND
       emplogin.employee = LvEmpNum  and
       ((emplogin.start_date >= ( LvWeekEnding - 6 ) and
       emplogin.start_date <= ( LvWeekEnding )))
       break by emplogin.employee by emplogin.start_date by start_time:

      if first-of(emplogin.start_date) then do:
         
         find employee where
              employee.company EQ gcompany AND
              employee.employee = emplogin.employee
              no-lock no-error.

         if not avail employee then next.

         ASSIGN
           li-start-time = if li-start-time = 0 then emplogin.start_time else li-start-time
           li-end-time = emplogin.end_time
           li-start-time2 = 0 
           li-end-time2 = 0
           li-day-time = 0.
      end.                     
      
      ASSIGN /*round to nearest minute instead of second*/
        v-total-time-hr-min = STRING(TRUNCATE(emplogin.total_time / 3600,0),">>>>9") + ":" + 
                              STRING(((emplogin.total_time MOD 3600) / 60),"99")

        v-index = INDEX(v-total-time-hr-min,":")
        v-total-time-sec = (INT(SUBSTRING(v-total-time-hr-min,1,v-index - 1)) * 3600) +
                           (INT(SUBSTRING(v-total-time-hr-min,v-index + 1,2)) * 60)
        li-day-time = li-day-time + v-total-time-sec
        li-overtime = 0
        li-2time = 0.

      IF li-start-time2 EQ 0 AND
         NOT first-of(emplogin.start_date) THEN
         li-start-time2 = emplogin.start_time.
      
      if last-of(emplogin.start_date) then do:
          IF emplogin.start_time <> li-start-time THEN
             li-end-time2   = emplogin.end_time.
         
         /*double time after 12 hours*/
         if li-day-time > 43200  THEN /*12 Hr  (x 3600) */ 
             assign li-2time = li-day-time - 43200
                    li-overtime = 14400. /*4 hr*/
         else if li-day-time > 28800  and 
                 li-day-time <= 43200 THEN
              assign li-2time = 0
                     li-overtime = li-day-time - 28800. /* 8 Hr */

         li-reg = 0.
                          /*8 hrs*/
         IF li-day-time > 28800 THEN
             ASSIGN li-reg = 28800.
         ELSE IF li-day-time > 0 THEN
             ASSIGN li-reg = li-day-time.
         
         create tt-tsrep.
         ASSIGN li-emp-over = li-emp-over + li-overtime 
                li-emp-2time = li-emp-2time + li-2time
                tt-tsrep.tt-date    = emplogin.start_date
                tt-tsrep.tt-time-in1 = string(li-start-time,"HH:MM AM")
                tt-tsrep.tt-time-out1 = string(li-end-time,"HH:MM AM")
                tt-tsrep.tt-time-in2 = IF li-start-time2 <> 0 THEN string(li-start-time2,"HH:MM AM") ELSE ""
                tt-tsrep.tt-time-out2 = IF li-end-time2 <> 0 THEN string(li-end-time2,"HH:MM AM") ELSE ""
                tt-tsrep.tt-wrk-hrs = STRING(li-reg + li-overtime,"HH:MM") 
                tt-tsrep.tt-ot-hrs = string(li-overtime, "HH:MM")
                tt-tsrep.tt-dt-hrs = string(li-2time, "HH:MM")
                li-start-time = 0.
      end. 

      if last-of(emplogin.employee) then do:
         
         assign li-emp-tot = if li-day-time <> 0 then li-day-time else 0 
                li-emp-tot-hr = truncate(li-emp-tot / 3600,0)
                li-emp-tot-min = truncate((li-emp-tot mod 3600) / 60,0)
                li-over-tot-hr = if li-emp-over > 0 then truncate(li-emp-over / 3600,0) else 0
                li-over-tot-min = if li-emp-over > 0 then truncate((li-emp-over mod 3600) / 60,0) else 0
                li-2time-tot-hr = if li-emp-2time > 0 then truncate(li-emp-2time / 3600,0) else 0
                li-2time-tot-min = if li-emp-2time > 0 then truncate((li-emp-2time mod 3600) / 60,0) else 0.

         /*More than 40 hrs in a week, it is overtime ...
           Put overtime in starting with last date and go backward
           Overtime hours cannot exceed total working hours for that day*/

         IF li-emp-tot GT 144000 THEN
         DO:
            li-overtime-to-apply = li-emp-tot - li-emp-over - 144000.

            IF li-overtime-to-apply GT 0 THEN
               FOR EACH tt-tsrep
                   BY tt-tsrep.tt-date DESC:
              
                   IF li-overtime-to-apply EQ 0 THEN
                      LEAVE.
              
                   ASSIGN
                      li-tmp-wrk-hrs = (INT(SUBSTRING(tt-tsrep.tt-wrk-hrs,1,2)) * 3600) +
                                       (INT(SUBSTRING(tt-tsrep.tt-wrk-hrs,4)) * 60)
              
                      li-tmp-ot-hrs = (INT(SUBSTRING(tt-tsrep.tt-ot-hrs,1,2)) * 3600) +
                                      (INT(SUBSTRING(tt-tsrep.tt-ot-hrs,4)) * 60).
                   
                   IF li-tmp-wrk-hrs - li-tmp-ot-hrs GT 0 THEN
                   DO:
                      IF li-overtime-to-apply GT li-tmp-wrk-hrs - li-tmp-ot-hrs THEN
                         li-ot-extra = li-tmp-wrk-hrs - li-tmp-ot-hrs.
                      ELSE
                         li-ot-extra = li-overtime-to-apply.
              
                      ASSIGN
                         li-overtime-to-apply = li-overtime-to-apply - li-ot-extra
                         tt-tsrep.tt-ot-hrs = string(li-tmp-ot-hrs + li-ot-extra, "HH:MM").
                   END.
               END.
         END.
      end.           
  end.                        

