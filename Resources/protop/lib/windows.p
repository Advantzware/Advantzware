/* lib/windows.p
 *
 * select the correct PP to support windows kernel calls
 *
 */

if opsys begins "win" then
  do:

  &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 11.3 &THEN
    run lib/windows32.p persistent.
  &ELSE
    if process-architecture = 64 then
      run lib/windows64.p persistent.
     else
      run lib/windows32.p persistent.
  &ENDIF

  end.

return.
