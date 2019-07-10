/* zprof_check.p
 *
 * simple stand-alone non-persistent profiler control
 *
 * run lib/zprof_check.p ( "baseName", "description", zFlush ).
 *
 * check for baseName.zprof in session:temp-directory -- if it exists start profiling, if it does not then end profiling
 *
 * if baseName is blank or unknown then check for "zprof.zprof"
 *
 */

define input parameter zBaseName    as character no-undo.
define input parameter zDescription as character no-undo.
define input parameter zFlush       as logical   no-undo.

define new global shared variable profilerStart        as datetime  no-undo.
define new global shared variable profilerDescription  as character no-undo.
define new global shared variable profilerBaseName     as character no-undo.
define new global shared variable dbgMode              as integer   no-undo initial 3.

define variable flagFile as character no-undo.

if zBaseName = "" or zBaseName = ? then
  flagFile = "zprof".
 else
  flagFile = zBaseName.

file-info:file-name = session:temp-directory + "/" + flagFile + ".zprof".
if file-info:full-pathname = ? then
  run lib/zprof_off.p.
 else
  run lib/zprof_on.p ( zBaseName, zDescription, zFlush ).
  

return.
