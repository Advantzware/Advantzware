/* repltarget.p
 *
 */

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

subscribe to "replTargetLastTRX" anywhere run-procedure "replTargetLastTRX".

return.


procedure replTargetLastTRX:

  define output parameter targetTRX      as integer no-undo.
  define output parameter targetBlksRecv as integer no-undo.
  define output parameter targetBlksProc as integer no-undo.

  define variable i as integer no-undo.

  /***
  do i = 1 to num-dbs:
    message i pdbname(i) ldbname(i).
    pause.
  end.

  find replTarget._db no-lock no-error.

  for each replTarget._dbStatus no-lock:
    message replTarget._dbStatus._dbStatus-StartTime.
    pause.
  end.
   ***/

  find replTarget._dbStatus   no-lock no-error.

  if available replTarget._dbStatus then
    assign
      targetTRX      = replTarget._dbStatus._dbStatus-LastTran
    .

  find first replTarget._Repl-Agent no-lock no-error.

  if available replTarget._Repl-Agent then
    assign
      targetBlksRecv = replTarget._Repl-Agent._ReplAgt-BlocksReceived
      targetBlksProc = replTarget._Repl-Agent._ReplAgt-BlocksProcessed
    .

  /***
  find replTarget._Repl-Agent no-lock no-error.

  message  "  db start:" replTarget._dbStatus._dbStatus-StartTime
                "agent:" replTarget._Repl-Agent._ReplAgt-AgentName
          "  targetTRX:" replTarget._dbStatus._dbStatus-LastTran  
               "  TRID:" replTarget._Repl-Agent._ReplAgt-LastTRID
    view-as alert-box
  .
   ***/

  return.

end.
