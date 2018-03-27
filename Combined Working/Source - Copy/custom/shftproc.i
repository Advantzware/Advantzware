/* shftproc.i */

PROCEDURE Get-Shift:
  DEFINE INPUT PARAMETER ip-company AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-machine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-time AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ip-optype AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-shift AS CHARACTER NO-UNDO.

def var counter as int no-undo.
def var tmp-shift as char no-undo.
DEF VAR iEarliestShiftStart AS INT NO-UNDO.
  {custom/getshift.i &file="machshft" &where="WHERE machshft.company = ip-company
                                              AND machshft.machine = ip-machine"}
  OUTPUT TO logs/get-shift.LOG APPEND.
  
   EXPORT DELIMITER "|" "get-shiftp" "Day" TODAY "cTime" TIME "Mach " ip-machine " time " ip-time
                    " optype " ip-optype " op-shift " op-shift SKIP.
  OUTPUT CLOSE.  


  if op-shift eq '' THEN DO:



    {custom/getshift.i &file="shifts" &WHERE="WHERE shifts.company = ip-company"}

    OUTPUT TO logs/get-shift.LOG APPEND.
    
     EXPORT DELIMITER "|" "get-shiftp1" "Day" TODAY "cTime" TIME "Mach " ip-machine " time " ip-time
                      " optype " ip-optype " op-shift " op-shift SKIP.
    OUTPUT CLOSE.



  END.

END PROCEDURE.

PROCEDURE Missing-Shift:
  DEFINE INPUT PARAMETER ip-company AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-machine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-startshift AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-endshift AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-missingshift AS CHARACTER NO-UNDO.

  {custom/mssgshft.i &file="machshft" &where="AND machshft.machine = ip-machine"}
  {custom/mssgshft.i &file="shifts"}
  OUTPUT TO logs/missing-shift.LOG APPEND.
  PUT UNFORMATTED "missing-shift " "Mach " ip-machine " start " ip-startshift
                  " endshift " ip-endshift " op-missingshift " op-missingshift SKIP.
  OUTPUT CLOSE.
END PROCEDURE.

PROCEDURE Shift-Data:
  DEFINE INPUT PARAMETER ip-company AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-machine AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ip-shift AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-starttime AS INTEGER NO-UNDO.
  DEFINE OUTPUT PARAMETER op-endtime AS INTEGER NO-UNDO.

  {custom/shftdata.i &file="machshft"
    &where="machshft.company = ip-company AND machshft.machine = ip-machine AND "}
  IF op-starttime NE 0 OR op-endtime NE 0 THEN
  RETURN.
  {custom/shftdata.i &file="shifts" &WHERE="shifts.company = ip-company AND "}
  OUTPUT TO logs/shift-data.LOG APPEND.
  PUT UNFORMATTED "shift-data " "Mach " ip-machine " start " ip-shift
                  " startshift " op-starttime " op-endshift " op-endtime SKIP.
  OUTPUT CLOSE.
END PROCEDURE.
