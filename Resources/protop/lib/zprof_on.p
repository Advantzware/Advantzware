/* zprof_on.p
 *
 * simple stand-alone non-persistent profiler startup and control
 *
 * a persistent-procedure version of this code is in zprofiler.p
 *
 *******************************************************************************
 *
 * run lib/zprof_on.p ( "baseName", "description", zFlush ).
 *
 * if baseName is blank or ? then a GUID will be generated and used for the output file name
 * the zFlush parameter controls flushing of the output if the profiler is already enabled: yes = flush, no = don't flush
 *
 * there are 6 stand-alone programs in this package:
 *
 *    zprof_on.p    - initializes profiling
 *    zprof_off.p   - ends profiling and writes the oputput file
 *    zprof_topx.p  - produces a report on the results
 *
 *    zprof_flush.p - flushes profiling output, this is optional but necessary if your program does not terminate cleanly
 *
 *    zprof_check.p - checks for a basename.zprof "flag" file and starts profiling if it exists, stops profiling if it does not
 *
 * to use:
 *
 *    run lib/zprof_on.p ( "baseName", "description", no ).
 *    run interestingStuff.p
 *    run lib/zprof_off.p.
 *
 * this will create a single basename*.prf file that can be loaded into a profiler analysis tool such as zprof_topx.p
 *
 *    pro -p lib/zprof_topx.p -param tmp/basename.2016.12.18.11.12.14
 *
 * note: there is no ".prf" at the end of the file name!
 *
 * if you call zprof_on.p when it is already running no harm will be done -- if the zFlush parameter = yes
 * then the profiler will just flush the output.  this can be very useful when you have limited control over
 * starting and stopping the profiler.
 *
 * zprof_check is handy if you want to embed the zprof tools and selectively turn profiling on and off from
 * outside the context of the application.
 *
 * perhaps you have a loop similar to this:
 *
 *   do while true:
 *     run lib/zprof_check( "baseName", "description", yes ).
 *     run interestingStuff.p
 *     pause 10 no-message.
 *   end.
 *
 * this approach allows you to enable and disable profiling externally -- just create baseName.zprof to
 * enable profiling and remove it to disable profiling.  with the zFlush flag set to "yes" each iteration
 * of the loop will create a basename.YY.MM.DD.HH.MM.SS.prf output file.
 *
 */

define input parameter zBaseName    as character no-undo.
define input parameter zDescription as character no-undo.
define input parameter zFlush       as logical   no-undo.

define new global shared variable profilerStart        as datetime  no-undo.
define new global shared variable profilerDescription  as character no-undo.
define new global shared variable profilerBaseName     as character no-undo.
define new global shared variable dbgMode              as integer   no-undo initial 3.

define variable profilerOutfile as character format "x(50)" no-undo.

if profiler:enabled = yes then							/* already enabled, flush the output	*/
  do:
    if zFlush = no then
      do:
        if dbgMode >= 5 then message now "zprofiler [" + profiler:description + "] is already enabled:" profiler:file-name ", NOT flushing profiler data.".
        return.
      end.
     else
      do:
        if dbgMode >= 5 then message now "zprofiler [" + profiler:description + "] is already enabled:" profiler:file-name ", flushing profiler data.".
        run lib/zprof_flush.p.
        return.
      end.
  end.

if zBaseName = "" or zBaseName = ? then zBaseName = guid( generate-uuid ).

assign
  profilerStart       = now
  profilerBaseName    = zBaseName
  profilerOutFile     = zBaseName
  profilerDescription = zDescription
.

/* If the "listings" or "coverage" attributes will be used then you must set them to yes BEFORE setting ENABLED to yes...
 *
 * profiler:listings = yes  /* results in wierd names -- it is much better to explicitly compile with debug-list... */
 * profiler:coverage = yes  /* not really relevant... */
 */

assign
  profiler:enabled     = yes
  profiler:description = profilerDescription
  profiler:profiling   = yes
  profiler:file-name   = profilerOutFile + ".prf"
.

if dbgMode >= 5 then message now "zprofiler [" + profiler:description + "] enabled:" profiler:file-name.

return.
