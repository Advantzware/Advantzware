/* zprofiler.p
 *
 * persistent procedure version of the profiler library
 *
 * an example procedure showing how to use the PP version of the library is at the end of this file
 *
 * the stand-alone, non PP, version is documented in zprof_on.p
 *
 */

define input parameter zBaseName    as character no-undo.
define input parameter zDescription as character no-undo.

define temp-table tt_profile
  field id          as integer format ">>>>9"
  field pdate       as date format "99/99/99"
  field description as character format "x(30)"
  index profile-idx is unique primary
    id
  index profile-date
    pdate
.

define temp-table tt_source
  field id          as integer format ">>>>9"
  field pid         as integer format ">>>>>9"
  field pname       as character format "x(40)"
  field debug_name  as character format "x(40)"
  index source-idx is unique primary
    id pid
  index source-name
    pname
.

define temp-table tt_tree
  field id          as integer format ">>>>9"
  field caller      as integer format ">>>>>9"
  field src_line    as integer format ">>>>>9"
  field callee      as integer format ">>>>>9"
  field call_count  as integer format ">>>>>9"
  index tree-idx is primary
    id caller src_line callee
.

define temp-table tt_ptime
  field id          as integer format ">>>>9"
  field pid         as integer format ">>>>>9"
  field src_line    as integer format ">>>>>9"
  field exec_count  as integer format ">>>>>>>>>9"
  field exe_time    as decimal format ">>>>>9.999999"
  field tot_time    as decimal format ">>>>>9.999999"
  field avg_time    as decimal format ">>>>>9.999999"
  index ptime-idx is unique primary
    id pid src_line
  index avg-idx
    avg_time descending
  index line-idx
    src_line
  index ptime-pid-t1
    id pid exe_time
  index ptime-pid-t3
    id pid avg_time
  index ptime-t1
    id exe_time
  index ptime-t3
    id avg_time
.

define temp-table tt_code_line
  field pid         as integer   format ">>>>>9"		/* program id#				*/
  field src_line    as integer   format ">>>>9"			/* source line#				*/
  field pname       as character format "x(30)"			/* procedure or class name		*/
  field ipname      as character format "x(40)"			/* internal procedure or method name	*/
  field t1          as decimal   format ">>>>>9.999999"		/* execution time			*/
  field t2          as integer   format ">>>>>>>>>9"		/* calls				*/
  field t3          as integer   format ">>9"			/* sessions				*/
  field t4          as decimal   format ">>>>>9.999999"		/* average time				*/
  index bad-idx1 is unique primary
    pid pname src_line
  index bad-idx2
    t2 t3
  index bad-idx3
    t1
  index avg-idx
    t4
.

define variable profilerOutfile      as character format "x(50)" no-undo.
define variable profilerDescription  as character format "x(50)" no-undo.
define variable profilerStart        as datetime no-undo.

assign
  profilerDescription = zDescription
.


/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

message "zprofiler super-procedure added.".

return.


/*********************************************************/


procedure zprofiler_on:

  /* append the date and time to the output file name so that we can easily keep a history
   */

  profilerOutFile = substitute( "&1&2.&3.&4.&5.&6", session:temp-directory, zBaseName, year( today ), string( month( today ), "99" ), string( day( today ), "99" ), replace( string( time, "hh:mm:ss" ), ":", "." )).

  assign
    profilerStart = now
    /*
     * If these will be used then you must set them to yes BEFORE setting ENABLED to yes...
     *
     * profiler:listings = yes	/* results is wierd names -- it is much better to explicitly compile with debug-list... */
     * profiler:coverage = yes	/* not really relevant... */
     */
    profiler:enabled     = yes
    profiler:description = profilerDescription
    profiler:profiling   = yes
    profiler:file-name   = profilerOutFile + ".prf"
  .

  message "zprofiler enabled" profilerOutFile. /* pause. */

  return.

end.


procedure zprofiler_off:

  message "turning zprofiler off". /* pause. */

  assign
    profiler:description = profilerDescription + " [" + string( absolute( interval( profilerStart, now, "seconds" )), "hh:mm:ss" ) + "]"
    profiler:enabled     = no
    profiler:profiling   = no
  .

  message "writing profiler data:" profilerOutFile + ".prf".
  profiler:write-data().
  message "profiler data written".

  return.

end.


define stream inStrm.

procedure zprofiler_load:

  define variable i   as integer   no-undo.
  define variable v   as integer   no-undo.
  define variable dt  as date      no-undo.
  define variable dsc as character no-undo.

  define variable profile_id as integer no-undo.

  empty temp-table tt_profile.
  empty temp-table tt_source.
  empty temp-table tt_tree.
  empty temp-table tt_ptime.

  file-info:file-name = profilerOutFile + ".prf".

  if file-info:full-pathname = ? then
    do:
      message "Cannot find profiler .prf data file:" profilerOutFile.
      pause.
      return.
    end.

  message "loading from:" file-info:full-pathname. /* session:date-format. */ /* pause. */

  input stream inStrm from value( file-info:full-pathname ).

  i = 1.

  repeat:				/* in theory there could be more than 1?  that would probably break a lot of stuff...	*/

    import stream inStrm v /* dt */ ^ dsc no-error.				/* the profiler apparently ignores session:date-format...	*/

    if v <> 1 then
      do:
        input stream inStrm close.
        message "Invalid version:" v.
        pause.
        return.
      end.

    /* message v dt dsc. pause.	*/				/* the profiler apparently ignores session:date-format...	*/

    profile_id = i.

    create tt_profile.
    assign
      tt_profile.id          = profile_id
      tt_profile.pdate       = today /* dt */
      tt_profile.description = dsc
    .

    i = i + 1.

  end.

  /* message "profile id:" profile_id. pause. */

  i = 1.

  repeat:

      create tt_source.
      tt_source.id = profile_id.
      import stream inStrm tt_source.pid tt_source.pname tt_source.debug_name no-error.

      i = i + 1.

  end.

/*  message i "tt_source loaded". pause.
 *  message "creating tt_source session record". pause.
 */

  /* create tt_source. */		/* don't CREATE -- an extra will be left over from the REPEAT logic	*/
  assign
    tt_source.id = profile_id
    tt_source.pid = 0
    tt_source.pname = "Session"
    tt_source.debug_name = "Session"
  .

  /* message "tt_source session record created". pause. */

  i = 1.

  repeat:

    create tt_tree.
    tt_tree.id = profile_id.
    import stream inStrm tt_tree.caller tt_tree.src_line tt_tree.callee tt_tree.call_count no-error.

    i = i + 1.

  end.

  delete tt_tree.

  /* message i "tt_tree loaded". pause. */

  i = 1.

  repeat:

    create tt_ptime.
    tt_ptime.id = profile_id.
    import stream inStrm tt_ptime.pid tt_ptime.src_line tt_ptime.exec_count tt_ptime.exe_time tt_ptime.tot_time no-error.
    tt_ptime.avg_time = tt_ptime.exe_time / tt_ptime.exec_count.

    i = i + 1.

  end.

  delete tt_ptime.

  /* message i "tt_ptime loaded". pause. */

  input stream inStrm close.

  return.

end.


procedure zprofiler_proc:

  define variable c  as integer no-undo.
  define variable i  as integer no-undo.
  define variable t1 as decimal no-undo format ">>>>>9.999999".
  define variable t2 as integer no-undo format ">>>>>>>>>9".
  define variable t3 as integer no-undo format ">>9".

  define variable srcName    as character no-undo.
  define variable iprocName  as character no-undo.

  empty temp-table tt_code_line.

  message "processing".

  for each tt_ptime no-lock by tt_ptime.avg_time descending:     

    /*  if exec_count < 1 /* or src_line = 0 */ then next. */

    find tt_source where
         tt_source.id =  tt_ptime.id and
         tt_source.pid = tt_ptime.pid no-error.

    if not available( tt_source ) then
      srcName = "session".
     else
      srcName = tt_source.pname.

    if srcName begins "lib/zprofiler/" then next.		/* don't include the profiler */

    find tt_code_line where
         tt_code_line.pid      = tt_ptime.pid and
         tt_code_line.src_line = tt_ptime.src_line and
         tt_code_line.pname    = srcName /* tt_source.pname */ no-error.

    if not available tt_code_line then
      do:
        create tt_code_line.
        assign
          i = i + 1
          tt_code_line.pid      = tt_ptime.pid
          tt_code_line.src_line = tt_ptime.src_line
          tt_code_line.pname    = srcName
        .
      end.

  end.

  message i "entries processed". /* pause. */

  for each tt_code_line:

    assign
      tt_code_line.t1 = 0
      tt_code_line.t2 = 0
    .

    for
      each tt_source where
        tt_source.pname = tt_code_line.pname,
      each tt_ptime where
        tt_ptime.id       = tt_source.id  and
        tt_ptime.pid      = tt_source.pid and
        tt_ptime.src_line = tt_code_line.src_line:      

      assign
        tt_code_line.t1 = tt_code_line.t1 + tt_ptime.exe_time
        tt_code_line.t2 = tt_code_line.t2 + tt_ptime.exec_count
        tt_code_line.t3 = tt_code_line.t3 + 1
      .

      if tt_ptime.pid = 0 and tt_ptime.src_line = 0 then tt_code_line.t1 = tt_ptime.tot_time.

    end.

  end.

  for each tt_code_line:

    tt_code_line.t4 = ( tt_code_line.t1 / tt_code_line.t2 ).	/* calculate the average time... */

    if num-entries( tt_code_line.pname, " " ) > 1 then
      assign
        tt_code_line.ipname = entry( 1, tt_code_line.pname, " " )
        tt_code_line.pname  = entry( 2, tt_code_line.pname, " " )
      .

  end.

  return.

end.


procedure zprofiler_topx:

  define input parameter toTTY as logical no-undo.

  define variable c  as integer no-undo.
  define variable i  as integer no-undo.
  define variable t1 as decimal no-undo format ">>>>>9.999999".
  define variable t2 as integer no-undo format ">>>>>>>>>9".
  define variable t3 as integer no-undo format ">>9".

  define variable t9 as integer no-undo.

  find first tt_profile no-lock no-error.	/* assuming that they're all the same date... */

  for each tt_code_line no-lock where tt_code_line.pname <> "session":
    t9 = t9 + tt_code_line.t1.
  end.

  if toTTY = no then output to value( profilerOutFile + ".rpt" ).
  
  display
    tt_profile.description  label "Description" format "x(70)" skip
    "Session Total Execution Time  " string( t9, "hh:mm:ss" )  skip
    "Line 0 = initialization, line -1 = cleanup"               skip
   with frame prof-hdr
     title " Profiler: Top 20 Results "
     width 120
     centered
     overlay
     side-labels
     row 4
  .

  i = 0.

  for each tt_code_line no-lock by tt_code_line.t1 descending:

    if tt_code_line.pname = "session" then next.

    i = i + 1.

    if i <= 20 then
      do:
        display
          tt_code_line.pname    label "Program/Class"
          tt_code_line.src_line label "Line"
          tt_code_line.t1       label "Time"
          tt_code_line.t4       label "Avg Time"
          tt_code_line.t2       label "Calls"
          tt_code_line.ipname   label "Internal Procedure/Method"
         with frame prof-rpt
           title " Top 20 Lines: Total Execution Time "
           width 120
           centered
           overlay
           20 down
           row 9
        .
      end.

    if i > 20 then leave.		/* Top 20... */

  end.

  if toTTY = no then
    output close.
   else
    do:
      pause.
      hide frame prof-rpt.
      hide frame prof-hdr.
    end.

  return.

end.

/******/

/*** example usage
 ***
 ***

/* use the profiler to help debug performance issues
 */

define variable zprofilerState as logical initial ?.

procedure myProfiler:

  if zprofilerState = yes then				/* the profiler is already running				*/
    do:

      run zprofiler_off.				/* flip the state of the profiler to "off"			*/
      zprofilerState = no.
      run zprofiler_load.				/* load profiler data into temp-tables to analyze		*/
      run zprofiler_proc.				/* process the data						*/
      run zprofiler_topx( no ).				/* report on the top 20 execution time lines -- to file		*/
      run zprofiler_topx( yes ).			/* report on the top 20 execution time lines -- to TTY		*/

      return.						/* do not continue after this -- return				*/

    end.

  if zprofilerState = ? then				/* we need to launch the profiler				*/
    do:
      run lib/zprofiler.p persistent (			/* launch the PP						*/
        "zprofiler",					/* output file basename						*/
        "Execution Profile"				/* description							*/
      ).
      zprofilerState = yes.				/* enable the profiler at initial launch			*/
    end.

  if zprofilerState = no then				/* profiler has previously been launched (state <> ?)		*/
    zprofilerState = yes.				/* so the user is just starting another set of data		*/

  if zprofilerState = yes then				/* a profiling dataset is being requested			*/
    do:							/* either from the initial launch or a subsequent request	*/
      run zprofiler_on.
      zprofilerState = yes.
    end.

  return.

end.

 ***
 ***
 ***/

