/* getshift.i */
/* Get a count of the number of matching shifts */
counter = 0.
FOR EACH {&file} NO-LOCK {&where}:
  IF (op-shift = '' AND
      ip-time GE {&file}.start_time AND
      ip-time LE {&file}.end_time) OR
     (op-shift = '' AND {&file}.start_time GT {&file}.end_time AND
      ip-time GE {&file}.start_time AND
      ip-time LE 86400) OR
     (op-shift = '' AND {&file}.start_time GT {&file}.end_time AND
      ip-time GE 0 AND
      ip-time LE {&file}.end_time) THEN
  DO:
    ASSIGN counter = counter + 1
           tmp-shift = {&file}.shift.
/*
    op-shift = {&file}.shift.
    RETURN.
*/
  END.
END. /* each {&file} */

/* If only one matching shift was found, use it */
IF counter EQ 1 THEN 
DO:
    
  ASSIGN op-shift = tmp-shift.

  OUTPUT TO logs/get-shift.LOG APPEND.
  EXPORT DELIMITER "|" "get-shift1" "Day" TODAY "cTime" TIME "Mach " ip-machine " time " ip-time
                  " optype " ip-optype " op-shift " op-shift SKIP.
  OUTPUT CLOSE.

  RETURN.
END.
ELSE DO:
ASSIGN op-shift = ""
       tmp-shift = "".
       

/* Look specifically for shifts that are defined for yesterday */
/* that are ending this morning */
IF tmp-shift EQ "" THEN DO:

  /* Find of earliest shift defined for today */
  iEarliestShiftStart = 86399.
  FOR EACH shifts NO-LOCK WHERE shifts.company = ip-company,
        FIRST reftable WHERE reftable.reftable = "ShiftDays"
                  AND reftable.CODE = shifts.rec_key
                  AND reftable.loc = "1" /* Specific Days */
                  AND ENTRY(WEEKDAY(TODAY),reftable.code2) EQ "YES"
        BY shifts.START_time:
    iEarliestShiftStart = shifts.START_time.
    LEAVE.
  END.

  /* Select shifts from today - 1 which end before today's earliest shift */
  FOR EACH shifts NO-LOCK WHERE shifts.company = ip-company
           AND shifts.END_time LT iEarliestShiftStart,
        FIRST reftable WHERE reftable.reftable = "ShiftDays"
                  AND reftable.CODE = shifts.rec_key
                  AND reftable.loc = "1" /* Specific Days */
                  AND ENTRY(WEEKDAY(TODAY - 1),reftable.code2) EQ "YES":
  
  
  
      IF  op-shift = '' AND
  
          /* Shift Ends Next Day and time given is less than end_time */
          (op-shift = '' AND shifts.start_time GT shifts.end_time AND
          ip-time GE 0 AND
          ip-time LE shifts.end_time) THEN
      DO:

          OUTPUT TO logs/get-shift.LOG APPEND.
          EXPORT DELIMITER "|" "get-shift1.1" "Day" TODAY "cTime" TIME "Mach " ip-machine " time " ip-time
                          " optype " ip-optype " op-shift " op-shift SKIP.
          OUTPUT CLOSE.

          op-shift = shifts.shift.
          tmp-shift = shifts.shift.      
  
      END.
      
  END. /* each {&file} */
END. /* if tmp shift blank */


  /* Take the first shift where ip-optype is specified and the start/end times are equal, */
  /* or take the last shift that matches the criteria, sorted by primary index which      */
  /* would be by shift code for the shifts table or machine/shift for the machshft table */
  
  
  FOR EACH {&file} NO-LOCK {&where},
        FIRST reftable WHERE reftable.reftable = "ShiftDays"
                  AND reftable.CODE = {&file}.rec_key
                  AND reftable.loc = "1" /* Specific Days */
                  AND ENTRY(WEEKDAY(TODAY),reftable.code2) EQ "YES":
        
      IF (op-shift = '' AND
          ip-time GE {&file}.start_time AND
          ip-time LE {&file}.end_time) OR
          (op-shift = '' AND {&file}.start_time GT {&file}.end_time AND
          ip-time GE {&file}.start_time AND
          ip-time LE 86400) OR
          (op-shift = '' AND {&file}.start_time GT {&file}.end_time AND
          ip-time GE 0 AND
          ip-time LE {&file}.end_time) THEN
      DO:
          IF (ip-optype BEGINS 'START' AND {&file}.start_time EQ ip-time) OR 
              (ip-optype BEGINS 'END' AND {&file}.end_time EQ ip-time) THEN
          DO:
            
              op-shift = {&file}.shift.
              OUTPUT TO logs/get-shift.LOG APPEND.
              EXPORT DELIMITER "|" "get-shift3" "Day" TODAY "cTime" TIME "Mach " ip-machine " time " ip-time
                  " optype " ip-optype " op-shift " op-shift SKIP.            
  
              OUTPUT CLOSE.
              RETURN.
          END.
          
          tmp-shift = {&file}.shift.
      END.
      
  END. /* each {&file} */


  IF tmp-shift EQ "" THEN DO:
      FOR EACH {&file} NO-LOCK {&where}:
        IF CAN-FIND(FIRST reftable WHERE reftable.reftable = "ShiftDays"
                  AND reftable.CODE = {&file}.rec_key
                  AND reftable.loc = "1" /* Specific Days */) THEN 
          NEXT.
        IF (op-shift = '' AND
            ip-time GE {&file}.start_time AND
            ip-time LE {&file}.end_time) OR
           (op-shift = '' AND {&file}.start_time GT {&file}.end_time AND
            ip-time GE {&file}.start_time AND
            ip-time LE 86400) OR
           (op-shift = '' AND {&file}.start_time GT {&file}.end_time AND
            ip-time GE 0 AND
            ip-time LE {&file}.end_time) THEN
        DO:
          IF (ip-optype BEGINS 'START' AND {&file}.start_time EQ ip-time) OR 
             (ip-optype BEGINS 'END' AND {&file}.end_time EQ ip-time) THEN
          DO:

            op-shift = {&file}.shift.

            OUTPUT TO logs/get-shift.LOG APPEND.
            EXPORT DELIMITER "|" "get-shift2" "Day" TODAY "cTime" TIME "Mach " ip-machine " time " ip-time
                  " optype " ip-optype " op-shift " op-shift SKIP.            
            OUTPUT CLOSE.


            RETURN.

          END.
          tmp-shift = {&file}.shift.
        END.
          
      END. /* each {&file} */
  END.


  op-shift = tmp-shift .  /* get last shift within range */


END.  /* else */
