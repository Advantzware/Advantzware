/* emplogi_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'emplogi_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="emplogin" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="emplogin" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="emplogin" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="emplogin" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/
/*  defined in emplogi_.p
def temp-table tt-note field employee like emplogin.employee
                       field rec_key like notes.rec_key
                       field note_date like notes.note_date
                       field note_title like notes.note_title
                       .
                       
*/   
def var li-overtime as int label "OT @x1.5" no-undo.
def var li-2time as int label "OT @x2" no-undo.
def var li-day-time as int no-undo.
def var li-lunch-time as int no-undo.
def var li-lunch-emp as int no-undo.
def var li-start-time as int no-undo.
def var li-end-time as int no-undo.
def var li-emp-over as int no-undo.
def var li-emp-2time as int no-undo.
def var li-emp-tot as int no-undo.
def var li-emp-tot-hr as int no-undo.
def var li-emp-tot-min as int no-undo.
def var li-over-tot-hr as int no-undo.
def var li-over-tot-min as int no-undo.
def var li-2time-tot-hr as int no-undo.
def var li-2time-tot-min as int no-undo.

def buffer bf-employee for employee.
def buffer bf-machemp for machemp.
     
if lv-note-only then do:
   for each emplogin no-lock where emplogin.employee >= begin_employee and
                                  emplogin.employee <= end_employee,
       each notes where notes.rec_key = emplogin.rec_key and 
                        notes.note_date >= begin_note-date and
                        notes.note_date <= end_note-date
                        no-lock:     
        create tt-note.
        assign tt-note.employee = emplogin.employee
               tt-note.rec_key = notes.rec_key
               tt-note.note_date = notes.note_date
               tt-note.note_title = notes.note_title
               tt-note.note_src = "Log In/Out".
               .
    end.
    for each bf-employee where bf-employee.employee >= begin_employee 
                           and bf-employee.employee <= end_employee 
                            no-lock,
        each notes where notes.rec_key = bf-employee.rec_key and
                          notes.note_date >= begin_note-date and
                          notes.note_date <= end_note-date
                          no-lock .
          create tt-note.
          assign tt-note.employee = bf-employee.employee
               tt-note.rec_key = notes.rec_key
               tt-note.note_date = notes.note_date
               tt-note.note_title = notes.note_title
               tt-note.note_src = "Employee".
    end.    
    for each bf-machemp where bf-machemp.employee >= begin_employee
                          and bf-machemp.employee <= end_employee
                            no-lock,
        each notes where notes.rec_key = bf-machemp.rec_key and
                          notes.note_date >= begin_note-date and
                          notes.note_date <= end_note-date
                      no-lock .
             create tt-note.
             assign tt-note.employee = bf-machemp.employee
                    tt-note.rec_key = notes.rec_key
                    tt-note.note_date = notes.note_date
                    tt-note.note_title = notes.note_title
                    tt-note.note_src = "Emp. Transaction".
    end.    

    
end.

for each emplogin no-lock where emplogin.employee >= begin_employee and
                                emplogin.employee <= end_employee and
                                ((emplogin.start_date >= begin_end_date and
                                emplogin.start_date <= end_end_date)
                               /* or
                                (lv-note-only and emplogin.start_date >= begin_note-date and
                                 emplogin.start_date <= end_note-date
                                ) */ )
                          break by emplogin.employee by emplogin.start_date by start_time:
                          
    if first-of(emplogin.employee) then do:
       find employee where employee.employee = emplogin.employee  no-lock no-error.
       if not avail employee then next.
       put "Employee: " employee.employee "  " employee.first_name employee.last_name skip
           "====================================================================" skip.           
       if lv-labor-only then 
       put            "                 Login      Logout     Hrs                            " skip  
           "Start Date Shift Time       Time       Worked     OT @x1.5   OT @x2" skip
           "---------- ----- ---------- ---------- ---------  ---------- -------" skip           
           .                       
    end.                      
    /*
    if lv-note-only then 
    for each notes where notes.rec_key = emplogin.rec_key and 
                        notes.note_date >= begin_note-date and
                        notes.note_date <= end_note-date
                        no-lock:     
        create tt-note.
        assign tt-note.employee = emplogin.employee
               tt-note.rec_key = notes.rec_key
               tt-note.note_date = notes.note_date
               tt-note.note_title = notes.note_title
               .
    end.
    */
    assign li-day-time = emplogin.total_time
           li-overtime = 0
           li-2time = 0
           li-start-time = if li-start-time = 0 then emplogin.start_time else li-start-time.
           li-end-time = emplogin.end_time
           .
   /*
    if li-day-time > 43200  /*12 Hr  (x 3600) */ 
             then  assign li-2time = li-day-time - 43200
                          li-overtime = 14400.
    else if li-day-time > 28800  and li-day-time <= 43200
             then assign li-2time = 0
                         li-overtime = li-day-time - 28800  /* 8 Hr */
                         .
    */
    accumulate li-day-time (total by emplogin.employee by emplogin.start_date).
    /*accumulate li-overtime (total by emplogin.employee by emplogin.start_date).
      accumulate li-2time (total by emplogin.employee by emplogin.start_date).    
    */
    /* ======== display every transaction         
    disp emplogin.start_date
         emplogin.shift
         string(emplogin.start_time,"HH:MM AM") /*column-label "Login!Time" form "x(10)" */
         string(emplogin.end_time,"HH:MM AM")  /*column-label "Logout!Time" form "x(10)" */
         string(emplogin.total_time,"HH:MM") /*column-label "Hrs!Worked" form "x(10)"*/ @ li-day-time
         string(li-overtime,"HH:MM") @ li-overtime
         string(li-2time,"HH:MM") @ li-2time
         with frame det stream-io down no-label .
   ================*/
/* ============ not for fiber
    for each machemp where machemp.employee = emplogin.employee and
                           machemp.start_date = emplogin.start_date
                           no-lock:
        if machemp.start_date = machemp.end_date 
        then do:
           if machemp.ratetype begins "Double" 
              then  li-2time = li-2time + machemp.total_time 
           else if machemp.ratetype begins "over" then li-overtime = li-overtime + machemp.total_time
           else /*standard */ li-day-time = li-day-time + machemp.total_time.
        end.
           
    end.
 ===============================*/
    /* ===== daily total display */
    if last-of(emplogin.start_date) then do:
       /*
       find first rate where rate.company = emplogin.company and
                             rate.employee = emplogin.employee and
                             rate.shift = emplogin.shift and                             
       */     
       find shifts where shifts.company = emplogin.company and
                        shifts.shift = emplogin.shift
                        no-lock no-error.
       if avail shifts and not shifts.lunch_paid then do:
          if  emplogin.end_time <= shifts.lunch_start then li-lunch-time = 0.  
          else if emplogin.start_time >= shifts.lunch_end then li-lunch-time = 0.
          else if emplogin.start_time <= shifts.lunch_start and
                  emplogin.end_time >= shifts.lunch_start and
                  emplogin.end_time <= shifts.lunch_end 
               then li-lunch-time = emplogin.end_time - shifts.lunch_start.
          else if emplogin.start_time >= shifts.lunch_start and
                  emplogin.start_time <= shifts.lunch_end and
                  emplogin.end_time >= shifts.lunch_end 
               then li-lunch-time = shifts.lunch_end - emplogin.start_time .
          else if emplogin.start_time >= shifts.lunch_start and
                  emplogin.end_time <= shifts.lunch_end 
               then li-lunch-time = emplogin.end_time - emplogin.start_time.                    
          else li-lunch-time = shifts.lunch_end - shifts.lunch_start .          
       end.
       else li-lunch-time = 0.
       if employee.lunch_paid then li-lunch-time = 0. 
       
       li-lunch-emp = li-lunch-emp + li-lunch-time.
       if (accum total by emplogin.start_date li-day-time) - li-lunch-time > 43200  /*12 Hr  (x 3600) */ 
             then  assign li-2time = (accum total by emplogin.start_date li-day-time) - li-lunch-time - 43200
                          li-overtime = 14400.
       else if ((accum total by emplogin.start_date li-day-time) - li-lunch-time) > 28800  and 
               ((accum total by emplogin.start_date li-day-time) - li-lunch-time)  <= 43200
             then assign li-2time = 0
                         li-overtime = (accum total by emplogin.start_date li-day-time) - li-lunch-time - 28800  /* 8 Hr */
                         .        
       assign li-emp-over = li-emp-over + li-overtime 
              li-emp-2time = li-emp-2time + li-2time .
       if lv-labor-only then        
       disp emplogin.start_date
            emplogin.shift form "x(5)"
            string(li-start-time,"HH:MM AM") /* column-label "Login!Time" */ form "x(10)" 
            string(li-end-time,"HH:MM AM")  /*column-label "Logout!Time" */ form "x(10)"
            if (accum total by emplogin.start_date li-day-time) <> 0 then 
            string((accum total by emplogin.start_date li-day-time) - li-lunch-time, "HH:MM")
            else "0"      @ li-day-time
            string(li-overtime, "HH:MM") when li-overtime <> 0 @ li-overtime
            string(li-2time, "HH:MM") when li-2time <> 0 @ li-2time
         with frame det stream-io  down no-label.
       li-start-time = 0.  
    end. 

    if last-of(emplogin.employee) then do:
     
       if lv-labor-only then do:
          down with frame det.
          assign li-emp-tot = if (accum total by emplogin.employee li-day-time) <> 0 then ((accum total by emplogin.employee li-day-time) - li-lunch-emp ) else 0 
                 li-emp-over = if li-emp-over <> 0 then (li-emp-over /*- li-lunch-emp*/) else 0
                 li-emp-2time = if li-emp-2time <> 0 then (li-emp-2time /*- li-lunch-emp*/ ) else 0.
          assign       
                 li-emp-tot-hr = truncate(li-emp-tot / 3600,0)
                 li-emp-tot-min = truncate((li-emp-tot mod 3600) / 60,0)
                 li-over-tot-hr = if li-emp-over > 0 then truncate(li-emp-over / 3600,0) else 0
                 li-over-tot-min = if li-emp-over > 0 then truncate((li-emp-over mod 3600) / 60,0) else 0
                 li-2time-tot-hr = if li-emp-2time > 0 then truncate(li-emp-2time / 3600,0) else 0
                 li-2time-tot-min = if li-emp-2time > 0 then truncate((li-emp-2time mod 3600) / 60,0) else 0
                 .
                 
          disp "Total" @ emplogin.start_date
          /*  string((accum total by emplogin.employee li-day-time) - li-lunch-emp, "HH:MM") @ li-day-time
            string(li-emp-over - li-lunch-emp, "HH:MM") when li-emp-over <> 0 @ li-overtime
            string(li-emp-2time - li-lunch-emp, "HH:MM") when li-emp-2time <> 0 @ li-2time
          */
            string(li-emp-tot-hr,">99") + ":" + string(li-emp-tot-min,"99") when li-emp-tot <> 0 @ li-day-time
            string(li-over-tot-hr,">99") + ":" + string(li-over-tot-min,"99") when li-emp-over <> 0 @ li-overtime
            string(li-2time-tot-hr,">99") + ":" + string(li-2time-tot-min,"99") when li-emp-2time <> 0 @ li-2time
            with frame det down.
          put skip(1).  
       end.  
       assign li-lunch-emp = 0
              li-emp-over = 0
              li-emp-2time = 0  
              .
       if lv-note-only then do:
          put "Notes:" skip .
          for each tt-note where tt-note.employee = emplogin.employee
                              by tt-note.note_date:
             
              put "     " tt-note.note_date " " tt-note.note_title 
                     /*tt-note.note_src form "x(15)"*/  skip.                   
          end.                       
          put skip(1).
       end.
    end.           
end.                        


{methods/lstlogic/shownote.i &db_table="emplogin" &col="5" &frame-name="f-notes"}
/*
{methods/lstlogic/showmisc.i &db_table="emplogin" &col="5" &frame-name="f-miscflds"}
*/
