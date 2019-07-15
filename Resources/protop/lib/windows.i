/* lib/windows.i
 *
 * windows kernel calls
 *
 */

if opsys begins "win" then
  do:

    if process-architecture = 64 then
      run lib/windows64.p persistent.
     else
      run lib/windows32.p persistent.

  end.
