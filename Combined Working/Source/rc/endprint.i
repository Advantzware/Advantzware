/*
06.09.96 by CAH on Ricky@812<rprodemo> Log#0000:
1.  Added export_open test and close if open.
*/
IF export_open THEN
DO:
  OUTPUT STREAM s-export close.
END.
OUTPUT {1} close.
IF printfid = 'V'
  AND (IF PROGRAM-NAME(1) <> ?
  THEN NOT PROGRAM-NAME(1) matches '*rc/printscr*'
  ELSE TRUE)
  THEN
DO:
  RUN rc/printscr.p (INPUT printdest).
END.
IF printfid matches "FAX*" THEN
DO:
  RUN rc/sendfax.p (printdest, ws_fax, OUTPUT ws_erc).
END.
if export_open then do:
  export_open = FALSE.
  ws_logical = false.
  message color value (c_add)
    "Would you like to view export file" export_fid "?" update ws_logical
        format "Y/N" auto-return.
  if ws_logical then do:
    run rc/viewexpf.p (input export_fid).
  end.
end.
IF CAN-DO('V', printfid) THEN
PAUSE 0.
IF CAN-DO('S', printfid) THEN
HIDE ALL.
ASSIGN
  batch_capable = FALSE
  export_capable = FALSE
  hdg_text = ''
  hdg_left = ''
  hdg_right = ''.
