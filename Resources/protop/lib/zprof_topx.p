/* zprof_topx.p
 *
 * process a .prf file and produce a report
 *
 * pro -p lib/zprof_topx.p -param tmp/f03e40c9-8043-0aba-e611-3cc5eac467bd.2016.12.18.11.12.14
 *
 * note: there is no ".prf" at the end of the file name!
 *
 */

define variable zprofData as character no-undo.
define variable topLines  as integer no-undo initial 20.

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


/*********************************************************/

define stream inStrm.

procedure zprofiler_load:

  define input parameter zprofData as character no-undo.

  define variable i   as integer   no-undo.
  define variable v   as integer   no-undo.
  define variable dt  as date      no-undo.
  define variable dsc as character no-undo.

  define variable profile_id as integer no-undo.

  empty temp-table tt_profile.
  empty temp-table tt_source.
  empty temp-table tt_tree.
  empty temp-table tt_ptime.

  file-info:file-name = zprofData + ".prf".

  if file-info:full-pathname = ? then
    do:
      message "Cannot find profiler .prf data file:" zprofData.
      pause.
      return.
    end.

  /* message "loading from:" file-info:full-pathname. /* session:date-format. */ pause. */

  input stream inStrm from value( file-info:full-pathname ).

  i = 1.

  repeat:				/* in theory there could be more than 1?  that would probably break a lot of stuff...	*/

    import stream inStrm v /* dt */ ^ dsc no-error.		/* the profiler apparently ignores session:date-format...	*/

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

  for each tt_ptime no-lock by tt_ptime.avg_time descending:     

    /*  if exec_count < 1 /* or src_line = 0 */ then next. */

    find tt_source where
         tt_source.id =  tt_ptime.id and
         tt_source.pid = tt_ptime.pid no-error.

    if not available( tt_source ) then
      srcName = "session".
     else
      srcName = tt_source.pname.

    if srcName begins "lib/zprof" then next.			/* don't include the profiler */

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

  /* message i "entries processed". pause. */

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

  define input parameter zprofData as character no-undo.
  define input parameter toTTY     as logical   no-undo.
  define input parameter topLines  as integer   no-undo.

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

  if toTTY = no then output to value( zprofData + ".rpt" ).
  
  display
    tt_profile.description  label "Description" format "x(70)" skip
    "Session Total Execution Time  " string( t9, "hh:mm:ss" )  skip
    "Line 0 = initialization, line -1 = cleanup"               skip
   with frame prof-hdr
     title " Profiler: Top Results "
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

    display
      tt_code_line.pname    label "Program/Class"
      tt_code_line.src_line label "Line"
      tt_code_line.t1       label "Time"
      tt_code_line.t4       label "Avg Time"
      tt_code_line.t2       label "Calls"
      tt_code_line.ipname   label "Internal Procedure/Method"
     with frame prof-rpt
       title " Top Lines: Total Execution Time "
       width 120
       centered
       overlay
       down
       row 9
    .

    if i > topLines then leave.

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


/* main body
 *
 */

zprofData = entry( 1, session:parameter, "|" ).
if num-entries( session:parameter, "|" ) = 2 then topLines = integer( entry( 2, session:parameter, "|" )).

run zprofiler_load( zprofData ).			/* load profiler data into temp-tables to analyze		*/
run zprofiler_proc.					/* process the data						*/
run zprofiler_topx( zprofData, no,  topLines ).		/* report on the top X execution time lines -- to file		*/

if session:batch = no then
  run zprofiler_topx( zprofData, yes, topLines ).	/* report on the top X execution time lines -- to TTY		*/

quit.
