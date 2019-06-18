/* zprof_flush.p
 *
 * simple stand-alone non-persistent profiler data flush
 *
 * run lib/zprof_flush.p.
 *
 */

define new global shared variable profilerStart        as datetime  no-undo.
define new global shared variable profilerDescription  as character no-undo.
define new global shared variable profilerBaseName     as character no-undo.
define new global shared variable dbgMode              as integer   no-undo initial 3.

define variable profilerOutfile as character format "x(50)" no-undo.

if profiler:enabled <> yes then							/* not enabled	*/
  do:
    if dbgMode >= 5 then message now "zprofiler was not enabled".
    return.
  end.

/* append the date and time to the output file name so that we can easily keep a history
 */

profilerOutFile =
  session:temp-directory +
  profilerBaseName +
  substitute(
    ".&1.&2.&3.&4",
    year( today ),
    string( month( today ), "99" ),
    string( day( today ), "99" ),
    replace( string( time, "hh:mm:ss" ), ":", "." )
  )
.

assign
  profiler:description = profilerDescription + " [" + string( absolute( interval( profilerStart, now, "seconds" )), "hh:mm:ss" ) + "]"
  profiler:file-name   = profilerOutFile + ".prf"
.

profiler:write-data().
if dbgMode >= 5 then message now "zprofiler [" + profiler:description + "] flushed:" profiler:file-name.

profilerStart = now.

return.
