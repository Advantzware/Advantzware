/* readprobe2017.p
 *
 * Probe maximum record read rate.
 *
 * Determine how it changes with number of CPUs and number of processes etc.
 *
 ***************************************************************************
 ***************************************************************************
 *
 * There is an automated cleanup that uses "kill" commands at the end.
 * REVIEW this!  Do not run as root...
 *
 ***************************************************************************
 ***************************************************************************
 *
 * Adjust the number of loops & sampling delay if necessary.  As is it
 * will start 100 background jobs.  The default sample interval is 5
 * seconds so the whole process will take about 40 minutes to run.
 *
 * _progres MUST be in the PATH, for version 8 you must have the character
 * client license installed (it's free but not a default option).
 *
 * Start the sports database with:
 *
 *     proserve sports -n 120 -spin 10000 -B 1000
 *
 * (or whatever settings you'd like to test -- just make sure that -n is
 * somewhat larger than nproc below...)
 * 
 * If you have a single processor box use -spin 1.  Or don't use spin (or
 * try it both ways...)
 * 
 * Start the probe like so:
 *
 *     mpro sports -p ./readprobe10.p \
 *        -param "RedHat Linux 7.1, 1x300MHz AMD K6, 598 BogoMIPS"
 *
 * Try to provide OS, # of CPUs, speed & type on command line as shown.
 * BogoMIPS or SPECInts are interesting too...
 *
 * If you run this against a v8 database VSTs must be enabled.
 *
 *
 */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 10.0 &THEN
&global-define	NOW	substitute( "&1 &2", today, string( time, "hh:mm:ss" ))
&global-define	LNGCR	character
&global-define	DTZ	integer
&ELSE
&global-define	OE10	"yes"
&global-define	NOW	now
&global-define	LNGCR	longchar
&global-define	DTZ	datetime-tz
&ENDIF

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
&global-define	BIGINT		int64
&ELSE
&global-define	BIGINT		decimal
&ENDIF

define variable rpDB as character no-undo.

rpDB = os-getenv( "DB" ).
if rpDB = ? then rpDB = "sports".
 
assign		/* so I don't have to convert the resulting data file... */
  session:date-format    = "mdy"
  session:numeric-format = "american"
.

{windows.i}
{zippy.i}

define temp-table tt_pid
  field pid as integer.

define variable dlc as character no-undo.
define variable cmd as character no-undo.

define variable uname    as character no-undo format "x(50)".

/***
dlc = os-getenv( "DLC" ).
dlc = trim( dlc, '"' ).
 ***/

/* _progres MUST be in the PATH, for version 8 you must have the character
 * client license installed (it's free but not a default option).
 *
 */

if opsys = "unix" then
  do:
    cmd = '_progres -b ' + rpDB + ' -p ./readalot.p -rand 2 -pf rp.pf > /dev/null 2>&1 &'.
  end.
 else
  do:
    cmd = '_progres.exe -b ' + rpDB + ' -p readalot.p -rand 2 -pf rp.pf'.
  end.

&global-define SAMPLE-DELAY 5
&global-define NITER        5

define variable i      as integer no-undo format ">>>>9".
define variable step   as integer no-undo format ">>>>9" initial 1.

define variable u      as integer no-undo format ">>>>9".
define variable nproc  as integer no-undo format ">>>>9".
define variable jump   as integer no-undo format ">>>>9".
define variable ramp   as integer no-undo format ">>>>9".

define variable et     as decimal no-undo format ">>>9.9999".

define variable loadAvg as character no-undo extent 16.
define variable xLdAvg  as decimal   no-undo.

define variable best1  as {&BIGINT}   no-undo.
define variable bestr  as {&BIGINT}   no-undo.
define variable bestu  as {&BIGINT}   no-undo.

define variable rr     as {&BIGINT}   no-undo.
define variable old-rr as {&BIGINT}   no-undo.

define variable br     as {&BIGINT}   no-undo.
define variable old-br as {&BIGINT}   no-undo.

define variable bht-lk as {&BIGINT}   no-undo.
define variable bht-by as {&BIGINT}   no-undo.
define variable bht-sp as {&BIGINT}   no-undo.
define variable bht-wt as {&BIGINT}   no-undo.

define variable lru-lk as {&BIGINT}   no-undo.
define variable lru-by as {&BIGINT}   no-undo.
define variable lru-sp as {&BIGINT}   no-undo.
define variable lru-wt as {&BIGINT}   no-undo.

define variable lru2-lk as {&BIGINT}  no-undo.
define variable lru2-by as {&BIGINT}  no-undo.
define variable lru2-sp as {&BIGINT}  no-undo.
define variable lru2-wt as {&BIGINT}  no-undo.

define variable latWt   as {&BIGINT}  no-undo.
define variable lruWt   as {&BIGINT}  no-undo.
define variable lru2Wt  as {&BIGINT}  no-undo.

define variable xLatWt     as {&BIGINT}  no-undo.
define variable oldlatWt   as {&BIGINT}  no-undo.
define variable oldlruWt   as {&BIGINT}  no-undo.
define variable oldlru2Wt  as {&BIGINT}  no-undo.

define variable maxlatWt   as {&BIGINT}  no-undo.
define variable maxlruWt   as {&BIGINT}  no-undo.
define variable maxlru2Wt  as {&BIGINT}  no-undo.

define variable loop   as integer no-undo.

define variable cntStr   as character no-undo.
define variable ofname   as character no-undo.

define variable startTime as {&DTZ} no-undo.
define variable runTime   as integer no-undo.

define variable userExperience as integer no-undo.

define variable statFile as {&LNGCR}  no-undo.
define variable inLine   as character no-undo.

define variable instat    as character no-undo extent 16 format "x(10)".
define variable cpustat   as character no-undo extent 16 format "x(10)".
define variable vmstat    as character no-undo extent 32.
define variable scenario  as character no-undo.

define variable xSpin as integer no-undo.
define variable xCPU  as integer no-undo.
define variable xMHz  as integer no-undo.

define stream out-data.
define stream osStats.
define stream CPUstat.
define stream inStrm.

run environment.p ( input "all", output uname, output cntStr, output ofName, output nProc, output jump, output ramp, output scenario, output xSpin, output xCPU, output xMHz ).

if opsys = "unix" and search( "./usrexp.p" ) <> ? then
  os-command silent value( substitute( "_progres " + rpDB + " -b -pf rp.pf -p ./usrexp.p -param &1 && > /dev/null 2>&&1", cntStr )).

output to "readprobe.flg".
put unformatted {&NOW} " " cntStr skip.
output close.

if uname begins "AIX" then
  do:

    /* run mpstat in the background so that we can later review scheduling affinity
     */

    os-command silent value( substitute( "mpstat -d {&SAMPLE-DELAY} &2 > results/&1.mpstat &&", cntStr, {&NITER} * nproc )).

    /* use iostat as the timer and gather some OS info along the way
     */

    input stream osStats through value( substitute( "iostat -tT {&SAMPLE-DELAY} &1", {&NITER} * nproc )).

    import stream osStats ^.
    import stream osStats ^.
    import stream osStats ^.
    import stream osStats ^.

    output stream CPUstat to value( substitute( "results/&1.cpustat", cntStr )).

  end.
 else if uname begins "Linux" then
  do:

    /*
    $ sar 5 5
    Linux 3.10.0-123.20.1.el7.x86_64 (traxportal01.intcomex.com)    03/23/2015      _x86_64_        (2 CPU)

    02:31:27 PM     CPU     %user     %nice   %system   %iowait    %steal     %idle
    02:31:32 PM     all      0.10      0.00      0.10      0.00      0.00     99.80
    02:31:37 PM     all      2.71      0.00      0.50      0.00      0.00     96.79
    02:31:42 PM     all      0.10      0.00      0.30      0.00      0.00     99.60
    02:31:47 PM     all      2.81      0.00      0.30      0.00      0.00     96.89
    02:31:52 PM     all      0.20      0.00      0.20      0.00      0.00     99.60
    Average:        all      1.18      0.00      0.28      0.00      0.00     98.54
     */

    os-command silent value( substitute( "sar -u {&SAMPLE-DELAY} &2 > results/&1.sar-u &&", cntStr, {&NITER} * nproc )).

    output stream CPUstat to value( substitute( "results/&1.cpustat", cntStr )).

  end.

pause 0 before-hide.

&IF DEFINED( OE10 ) &THEN
startTime = {&NOW}.
&ENDIF

output stream out-data to value( ofname ) unbuffered append.

/* ^d or q terminate the loop
 *
 */

loop: do while lastkey <> 4 and lastkey <> 113:

  readkey pause 0.
  if lastkey = 4 or lastkey = 113 or lastkey = 81 then leave loop.

  file-info:file-name = "readprobe.flg".
  if file-info:full-pathname = ? then leave loop.

  if u > nproc then
    leave loop.
   else if ( loop modulo {&NITER} = 0 ) then
    do:
      if u >= ramp then step = jump.
      do i = 1 to step:
        u = u + 1.
        if u > nproc then leave loop.
        if opsys <> "unix" then
          do:
            create tt_pid.
            run spawn( input cmd, input "", output tt_pid.pid ).
          end.
         else
          do:
            os-command silent value( cmd + " > /tmp/rp." + string( u, "99999" ) + ".err 2>&1" ).
          end.
      end.
      pause 1 no-message.
    end.

  loop = loop + 1.

  find first _ActSummary no-lock.

  find first _ActBuffer  no-lock.
  
  find _Latch where _Latch._Latch-Name = "MTL_BHT" no-lock.

  assign
    rr = _ActSummary._Summary-RecReads
    br = _ActBuffer._Buffer-LogicRds
    bht-lk = _Latch._Latch-Lock
    bht-by = _Latch._Latch-Busy
    bht-sp = _Latch._Latch-Spin
    bht-wt = _Latch._Latch-Wait
  .

  find _Latch where _Latch._Latch-Name = "MTL_LRU" no-lock no-error.
  if available( _Latch ) then
    assign
      lru-lk = _Latch._Latch-Lock
      lru-by = _Latch._Latch-Busy
      lru-sp = _Latch._Latch-Spin
      lru-wt = _Latch._Latch-Wait
    .

  find _Latch where _Latch._Latch-Name = "MTL_LRU2" no-lock no-error.
  if available( _Latch ) then
    assign
      lru2-lk = _Latch._Latch-Lock
      lru2-by = _Latch._Latch-Busy
      lru2-sp = _Latch._Latch-Spin
      lru2-wt = _Latch._Latch-Wait
    .

  /* start the timer
   */

  etime( yes ).

  pause {&SAMPLE-DELAY} no-message.					/* 10.0 had a broken PAUSE :(			*/
  if lastkey = 4 or lastkey = 113 or lastkey = 81 then leave loop.

  if uname begins "AIX" then
    do:
      cpustat = "".
      do while cpustat[1] = "tty:" or cpustat[3] = "":			/* every 24 lines iostat ouputs new headers... */
        cpustat = "".
        import stream osStats cpustat.
      end.
      put stream CPUstat cpustat skip.
    end.
   else if uname begins "Linux" then
    do:

/* os-command silent value( substitute( "sar -u {&SAMPLE-DELAY} &2 > results/&1.sar-u &&", cntStr, {&NITER} * nproc )). */
/* 02:31:27 PM     CPU     %user     %nice   %system   %iowait    %steal     %idle */
/* 02:31:37 PM     all      2.71      0.00      0.50      0.00      0.00     96.79 */

&IF DEFINED( OE10 ) &THEN

      copy-lob from file substitute( "results/&1.sar-u", cntStr ) to statFile.

      cpuStat = "".
      inLine = string( entry( num-entries( statFile, "~n" ) - 1, statFile, "~n" )).
      /* message length( statFile ) num-entries( statFile, "~n" ) inLine. */
      assign
        cpustat[1] = substring( inLine,  1, 11 )
        cpustat[2] = substring( inLine, 23,  7 )
        cpustat[3] = substring( inLine, 43,  7 )
        cpustat[4] = substring( inLine, 73 )
        no-error
      .
      put stream CPUstat cpustat skip.

&ENDIF

    end.

  et = ( etime / 1000 ).
  if et < 1 or et= ? then next loop.

  find first _ActSummary no-lock.
  find first _ActBuffer  no-lock.

  latWt = 0.
  for each _Latch no-lock:

    latWt = latWt + _Latch._Latch-Wait.
  
    if _Latch._Latch-Name = "MTL_LRU" then
      assign
        LRUWt    = ( _Latch._Latch-Wait - oldLRUWt )
        oldLRUWt = LRUWt
        LRUWt    = LRUWt / et
        maxLRUWt = max( maxLRUWt, LRUwt )
      .

    if _Latch._Latch-Name = "MTL_LRU2" then
      assign
        maxLRU2Wt = max( LRU2Wt, _Latch._Latch-Wait / et )
        LRU2Wt    = _Latch._Latch-Wait / et
      .

  end.

  assign
    xLatWt   = oldLatWt
    oldLatWt = latWt
    latWt    = latWt - xLatWt
    latWt    = latWt / et
    maxLatWt = max( maxLatWt, latWt )
  .

  find _Latch where _Latch._Latch-Name = "MTL_BHT" no-lock.

  bestr = max( bestr, integer(( _ActSummary._Summary-RecReads - rr ) / et )).
  if ( bestr = integer(( _ActSummary._Summary-RecReads - rr ) / et )) then bestu = u.

  if u = 1 then best1 = max( best1, bestr ).

  userExperience = zippy().

  if opsys = "unix" then
    do:

      input stream inStrm through value( "uptime" ).
      import stream inStrm loadAvg.
      input stream inStrm close.

      /*   09:10AM   up  16:21,  3 users,  load average: 3.86, 17.36, 23.94
       *   10:38AM   up 3 days,  14:57,  33 users,  load average: 2.83, 3.59, 3.66
       */

      do i = 1 to 10:
        if loadAvg[i] = "average:" then leave.
      end.

      assign xLdAvg = decimal( entry( 1, loadAvg[i + 1] )) no-error.

    end.

  if session:batch = no then
    do:

      display
          "        Loop: "  loop  to 25 "              Best Single: "     best1 to 70 " " skip
          "    Sessions: "     u  to 25 "             Best Rec/sec: "     bestr to 70 skip
          "        Time: "    et  to 25 "               Best Users: "     bestu to 70 skip
	  skip(1)
          " Latch Waits: " latWt  to 25 "        Worst Latch Waits: "  maxLatWt to 70 skip
          "   LRU Waits: " lruWt  to 25 "          Worst LRU Waits: "  maxLRUWt to 70 skip
          "  LRU2 Waits: " lru2Wt to 25 "         Worst LRU2 Waits: " maxLRU2Wt to 70 skip
          skip(1)
          "     Rec/sec: " (( _ActSummary._Summary-RecReads - rr ) / et )        format ">>>>>>>>>9" to 25
	     "     %usr: " at 43 decimal( cpustat[2] ) format ">>>>>>9.99" to 70 skip
	  "    Rec/User: " ((( _ActSummary._Summary-RecReads - rr ) / et ) / u ) format ">>>>>>>9"   to 25
	     "     %sys: " at 43 decimal( cpustat[3] ) format ">>>>>>9.99" to 70 skip
          "     Blk/sec: " (( _ActBuffer._Buffer-LogicRds - br ) / et )          format ">>>>>>>>>9" to 25
	     "    %idle: " at 43 decimal( cpustat[4] ) format ">>>>>>9.99" to 70 skip
          "  User Exper: " userExperience format ">>>>>>>>>9" to 25
	     "%Entitled: " at 43 decimal( cpustat[8] ) format ">>>>>>9.99" to 70 skip
             " Load Avg: " at 43 xLdAvg               to 70 skip
/*
          skip(1)
          "                    type q or ^d to abort benchmark " skip
          skip(1)
 */
	with no-box no-labels width 80.
    end.

  if (( loop modulo {&NITER} ) = ( {&NITER} - 1 )) then
    do:

      find _Latch where _Latch._Latch-Name = "MTL_BHT" no-lock no-error.

      put stream out-data
        u et 
        (( _ActSummary._Summary-RecReads - rr ) / et ) format ">>>>>>>>>9"
        (( _ActBuffer._Buffer-LogicRds - br ) / et )   format ">>>>>>>>>9"
        (( _ActSummary._Summary-RecReads - rr ) / ( u * et )) format ">>>>>>>>>9"
        latWt format ">>>>>>>>9"
        (( _Latch._Latch-Wait - bht-wt ) / et ) format ">>>>>>>>9"
      .
      bht-wt = _Latch._Latch-Wait.
  
      find _Latch where _Latch._Latch-Name = "MTL_LRU" no-lock no-error.
      if not available( _Latch ) then
        put stream out-data "        0".
       else
        put stream out-data
          (( _Latch._Latch-Wait - lru-wt ) / et ) format ">>>>>>>>9".
        .

      lru-wt = _Latch._Latch-Wait.

      find _Latch where _Latch._Latch-Name = "MTL_LRU2" no-lock no-error.
      if not available( _Latch ) then
        put stream out-data "        0".
       else
        put stream out-data
          (( _Latch._Latch-Wait - lru2-wt ) / et ) format ">>>>>>>>9".
        .

      if available( _Latch ) then
        lru2-wt = _Latch._Latch-Wait.

      put stream out-data
        xLdAvg format ">>>>9.99"
        decimal( cpustat[2] ) format ">>>>9.99"
        decimal( cpustat[3] ) format ">>>>9.99"
        decimal( cpustat[4] ) format ">>>>9.99"
        decimal( cpustat[8] ) format ">>>>9.99"
	userExperience format ">>>>>>>9"
      .

      put stream out-data skip.

    end.

  readkey pause 0.

  if lastkey = 4 or lastkey = 113 or lastkey = 81 then leave loop.

end.

put stream out-data skip(1).

&IF DEFINED( OE10 ) &THEN
runTime = interval( {&NOW}, startTime, "seconds" ).
&ENDIF

display stream out-data
  "        Loop: "  loop  to 25 "         Best Single: "     best1 to 65 skip
  "    Sessions: "     u  to 25 "        Best Rec/sec: "     bestr to 65 skip
  "        Time: "    et  to 25 "          Best Users: "     bestu to 65 skip
  skip(1)
  " Latch Waits: " latWt  to 25 "   Worst Latch Waits: "  maxLatWt to 65 skip
  "   LRU Waits: " lruWt  to 25 "     Worst LRU Waits: "  maxLRUWt to 65 skip
  "  LRU2 Waits: " lru2Wt to 25 "    Worst LRU2 Waits: " maxLRU2Wt to 65 skip
  skip(1)
  "    Run Time: " string( runTime, "hh:mm:ss" ) to 25 skip
 with
  frame summary
  no-labels
.

output stream out-data close.

/* append to summary
 */

output stream out-data to value( "results/summary.rpt" ) append.

file-info:file-name = "results/summary.rpt".
if file-info:file-size < 10 then
  put stream out-data
    "Test           Spin  #CPU      MHz           Single        Best Recs  Users  Scenario         " skip
    "====  =============  ====  =======  ===============  ===============  =====  =================" skip
  .

put stream out-data
  cntStr   format "x(4)"            "  "
  xSpin    format ">>>>>>>>>>>>9"   "  "
  xCPU     format ">>>9"            "  "
  xMHz     format ">>>>>>9"         "  "
  best1    format ">>>>>>>>>>>>>>9" "  "
  bestr    format ">>>>>>>>>>>>>>9" "  "
  bestu    format ">>>>9" "  "
.

put stream out-data unformatted
  scenario "; "
  session:parameter
  skip
.

output stream out-data close.

/* shutdown
 */

file-info:file-name = "readprobe.flg".
if file-info:full-pathname = ? then
  do:
    message "Benchmark externally stopped.".
  end.
 else
  do:
    os-delete value( "readprobe.flg" ).
    message "Stopping benchmark.".
  end.

pause.

hide all no-pause.

/***
do on error undo, leave
   on stop undo, leave
   on quit undo, leave:

  os-command no-wait value( "proshut -by " + rpDB ).

end.
 ***/

quit.
