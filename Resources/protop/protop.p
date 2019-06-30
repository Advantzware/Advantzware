/* protop.p
 *
 * start interactive protop client
 *
 * mpro -p protop -param "friendlyName|startupDataCollectorList|connectString|options"
 *
 */

do on stop   undo, retry						/* drop out on any untrapped fatal errors               */
   on error  undo, retry
   on endkey undo, retry:

  if retry then								/* this should never fire				*/
    do:
      message "Unhandled fatal error:" error-status:get-number(1) error-status:get-message(1).
      pause.
      quit.
    end.

/* the end of this block is at the end of protop.p... indenting the whole program just seems silly */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) > 11.0 &THEN

if os-getenv( "TTDEBUG" ) = "yes" then
  do:
    Progress.Database.TempTableInfo:ArchiveTableStatistics = true no-error.
    Progress.Database.TempTableInfo:ArchiveIndexStatistics = true no-error.
  end.

&ENDIF

assign                                                          	/* a lazy american must have written this code...	*/
  session:date-format    = "ymd"
  session:numeric-format = "american"
.

{lib/protop.i}
{lib/dynscreen.i}

define new global shared variable pt_first as logical no-undo.
pt_first = yes.

define temp-table tt_protop no-undo
  field order    as integer
  field scrType  as character
  field evtName  as character
  field scrTitle as character
  field dcName   as character
  field ttName   as character
  field minRows  as integer
  field maxRows  as integer
  index pt-idx is primary unique order
.

dbgMode = 1.

define variable fast-start  as logical     no-undo initial yes.		/* if "no" then a pause will affect startup messages	*/

define new global shared variable monInt   as integer no-undo initial 10.	/* monitor sampling interval				*/
/* define variable monint      as integer     no-undo initial 10. */		/* monitor refresh interval				*/

define variable tick        as decimal     no-undo initial 0.5.		/* clock tick - 1/2 sec rounds up to 1 prior to OE11	*/
define variable monName     as character   no-undo initial "protop".
define variable dbgFileName as character   no-undo.			/* echo "3" > tmp/protop.s2k.debug			*/

define variable dcList      as character   no-undo.			/* list of active data collectors			*/
define variable result      as longchar    no-undo.			/* serialized XML or JSON from data collectors		*/

define variable ptCnxStr    as character   no-undo.			/* how to connect to remote data collectors		*/
define variable ptParams    as character   no-undo.			/* parameters for remote data collectors		*/

define variable ptStartup   as character   no-undo.			/* which data collectors should we start with?		*/

define variable keepRunning as logical     no-undo initial yes.		/* if false then quit ProTop				*/
define variable lastRefresh as datetime-tz no-undo.			/* when was the last refresh?				*/
define variable sampleMode  as character   no-undo initial "Auto".	/* are we sampling every X seconds?  or on-demand	*/
define variable connectMode as character   no-undo initial "Direct".	/* direct?  http?  stomp?				*/
define variable fetchStart  as datetime-tz no-undo.			/* when did we ask for data?				*/
define variable fetchTime   as integer     no-undo.			/* how long to fetch data?				*/

define variable userNum     as integer     no-undo.			/* Usr# for userInfo data collector			*/
define variable userPID     as integer     no-undo.			/* PID for userInfo data collector			*/

define variable userTblName as character   no-undo.			/* 							*/
define variable userTblNum  as integer     no-undo.			/* 							*/
define variable userIdxName as character   no-undo.			/* 							*/
define variable userIdxNum  as integer     no-undo.			/* 							*/

define variable rawMode     as integer     no-undo initial 5.		/* interval (5), cumulative (4) or "raw" (3) metrics	*/
define variable timeMode    as integer     no-undo initial 2.		/* rate (2) or "summary" (1) metrics			*/
define variable CSC         as logical     no-undo initial ?.		/* Client Statement Cache on/off			*/
define variable logMode     as logical     no-undo.			/* control the logging mode				*/

define variable b as handle  no-undo.					/* a buffer handle					*/
define variable i as integer no-undo.					/* an integer						*/

define new global shared variable dsSize         as integer  no-undo.	/* the shame!						*/
define new global shared variable dsRows         as integer  no-undo.	/* the shame!						*/
define new global shared variable programmerMode as logical  no-undo.

/* needed if the connection is to a remote db via http agents (connectMode = "http")
 */

define variable url  as character no-undo.			/* "localhost/cgi-bin/protop.cgi?type=pt3http&DB=s2k0&UID=tom"	*/
define variable host as character no-undo.
define variable port as character no-undo.
define variable path as character no-undo.

/* variables for the screen header (row 1): built in procedure paintScreen() below
 */

define variable headerFrame   as handle no-undo.
define variable modeIndicator as handle no-undo.
define variable protopVersion as handle no-undo.
define variable clockString   as handle no-undo.

/* row 2 of the header is built by dc/dbid.p
 */

/* a fake variable and frame that will be positioned on row 3, which is always blank, so as to be invisible.  this
 * gives the READKEY statement a widget for focus which fixes the problems of being able to move browsers with the
 * arrow keys as well as the hiding of a seemingly random browse title.
 *
 * all of which then provides a foundation for perhaps moving psuedo-focus among the frames to permit scrolling
 * once I get some time to mess around with such things.
 *
 */

define variable nothing   as character no-undo format "x".
define variable something as character no-undo format "x(50)".		/* useful for debugging what was pressed...		*/

form
  nothing something
 with
  frame notHere
  no-box
  no-labels
  overlay
  row 3
  column 1
  color display normal prompt normal			/* no underline for the field that we don't want anyone to see...	*/
.


/* event handlers and internal procedures
 *
 */

on any-printable of this-procedure do:
  message "eh?  that shouldn't have happened!".
  pause.
  publish "protop_command" ( chr( lastkey )).
  return. /* no-apply. */
end.

on window-close anywhere do:
  keepRunning = false.
  return.
end.

on any-printable of nothing in frame notHere do:
  something = something + chr( lastkey ).
  do while length( something ) > 8:
    something = substring( something, 2 ).
  end.
  display something with frame notHere.					/* help debug the vanishing keystroke problem...	*/
  publish "protop_command" ( chr( lastkey )).
  return. /* no-apply. */
end.


procedure logMsg:

  define input parameter logLevel as integer   no-undo.
  define input parameter msgTxt   as character no-undo.

  if logLevel <= 3 then
    do:
      message substitute( "[&1]", logLevel ) msgTxt view-as alert-box.
      /* pause. */
    end.

  return.

end.


/* ProTop might not be running as a stand-alone program.
 *
 */

procedure quitme:						/* If there is no parent procedure, we QUIT, otherwise drop	*/
								/* drop out of the main event loop & RETURN			*/
  keepRunning = FALSE.

  /* reminder to signup for the web portal might go here */

  if "{&window-system}" <> "tty" then				/* prowin?							*/
    apply "window-close" to this-procedure.
   else
    if program-name( 4 ) = ? then QUIT.

  return.

end.


procedure setSampleMode:

  if sampleMode = "OnDemand" then				/* this needs to be visible -- a header line is needed		*/
    sampleMode = "Auto".
   else
    sampleMode = "OnDemand".

  return.

end.


procedure userInfo2:						/* userInfo returns extra data - when it is choosen also	*/
								/* activate these						*/
  publish "protop_usrtblact".
  publish "protop_usridxact".
  publish "protop_usrstack".
  publish "protop_usrother".

  return.

end.


/* keep track of the available data collectors
 */

procedure register:

  define input parameter dcName as character no-undo.

  if dcName <> "" then
    dcList = dcList + ( if dcList = "" then "" else "," ) + dcName.

  return.

end.


procedure refresh:

  define variable reqString as character no-undo.

  /* only request the data collectors which are visible                        
   */
     
  publish "protop_getReqStr" ( input-output reqString ).
  
  /* /* --- currently only the userInformation data collector has an arglist --- */ a more sophisticated approach
   * might be needed now that that has changed
   */

  if lookup( "userInformation", reqString ) > 0 then
    do:

      if userNum <> ? then
        reqString = replace( reqString, "userInformation", substitute( "userInformation:USR=&1", userNum )).
       else if userPID <> ? then
        reqString = replace( reqString, "userInformation", substitute( "userInformation:PID=&1", userPID )).

    end.

  if lookup( "UsersOfTable", reqString ) > 0 then
    do:

      if userTblNum <> ? then
        reqString = replace( reqString, "usersOfTable", substitute( "usersOfTable:UTBLNUM=&1", userTblNum )).

    end.

  if lookup( "UsersOfIndex", reqString ) > 0 then
    do:

      if userIdxNum <> ? then
        reqString = replace( reqString, "usersOfIndex", substitute( "usersOfIndex:UIDXNUM=&1", userIdxNum )).

    end.

  run makeRequest ( reqString ).

  return.

end.


procedure makeRequest:

  define input parameter reqString as character no-undo.

  assign
    result     = ""
    fetchStart = now
  .
 
  if connectMode = "direct" then
    run doRequest( "|" + reqString, output result ).		/* we don't need userid for a direct connection 		*/

  /*** @@@ if length( result ) > 0 then @@@ ***/ run paintScreen.

  return.

end.


define variable rt as character no-undo.

procedure paintScreen:

  define variable s  as character no-undo.

  if true then /*** @@@ length( result ) > 0 then @@@ ***/
    do:

      fetchTime = abs( interval( now, fetchStart, "milliseconds" )).

      if index( result, "<?xml " ) = 1 then
        do:
          dynScreenUpdate( result ).
          rt = "XML".
        end.
       else if index( result, '~{"ProDataSet":' ) = 1 then
        do:
          dynScreenUpdate( result ).
          rt = "JSON".
        end.
       else
        do:
          dynScreenUpdate( result ).
          rt = "DS".
/*** @@@ eliminate prodataset2json etc conversion & reconversion
 *        s = substring( result, 1, 256 ).
 *        rt = "Unsupported".
 *        message "Unsupported return type:" s view-as alert-box.
 ***/
        end.
    end.

  assign							/* first line of header						*/
    modeIndicator:screen-value =
      replace(
        trim( substitute( "&1 &2 &3 &4 &5 &6 &7 &8 &9",
          trim( pt_shortname /* + " " + connectMode */ ),
          sampleMode,
          entry( rawMode, "Initial,Previous,Raw,Cumulative,Interval" ),
          entry( timeMode, "Summary,Rate" ),
          rt,
          dsRows,	/*** @@@ ***/
          dsSize,
          trim( string( fetchTime / 1000, ">>>9.999" )),
          ( if logMode then "Log=On" else "" )
        )),
        "  ",
        " "
      )
    clockString:screen-value   = string( now )
    headerFrame:hidden         = no
    headerFrame:visible        = yes
  .

  {ssg/sausage01.i}

  result = "".

  return.

end.


procedure dbanalys:

  define variable ok as logical no-undo.

  message
    skip(1)
    "  Running a dbanalys can be very IO intensive.  You might prefer to run  " skip
    "  this utility offline during a quiet period.                            " skip
    skip(1)
    "  To run it offline use a command similar to:                            " skip
    skip(1)
    substitute( "  proutil &1 -C dbanalys > dbanalys/&2.dba  ", ldbname(1), ( if pt_shortname <> "" then pt_shortname else ldbname(1) )) skip
    skip(1)
    "  Ideally you will routinely run a fresh dbanalys at least weekly.       " skip
    skip(1)
    view-as alert-box question buttons yes-no title " Are You Sure? "
    update ok
  .

  if ok = yes then run makeRequest ( "xcmd=dba" ).

  return.

end.


procedure statementCache:

  update
    skip(1)
    '  The client statement cache is a powerful feature that sometimes causes    ' dcolor 6 skip
    '  problems. If you are not comfortable with the potential issues, please    ' dcolor 6 skip
    '  do not enable it in Production.                                           ' dcolor 6 skip
    skip(1)
    '  Rather than globally enabling CSC, ProTop can enable or disable the       ' skip
    '  client statement cache for specific users by using the "#" command and    ' skip
    '  entering a usr#.                                                          ' skip
    skip(1)
    '  You can also use the menu at PROMON R&D, 1, 18 for fine-grained control   ' skip
    '  over individual sessions.                                                 ' skip
    skip(1)
    '  Global Client Statement Cache status:' CSC format "On/Off" '   On, Off, ? = no change  ' skip
    skip(1)
   with
    frame getCSC
    title " Global Client Statement Cache "
    row 3
    centered
    /* width 80 */
    no-labels
    overlay
  .

  hide frame getCSC no-pause.

  if csc = yes then
    do:
      run makeRequest ( substitute( "xcmd=csc_&1", string( CSC, "on/off" ))).
    end.
   else if csc = no then
    do:
      run makeRequest ( substitute( "xcmd=csc_&1", string( CSC, "on/off" ))).
    end.

  return.

end.


procedure setRawMode:
  run makeRequest ( substitute( "xcmd=raw_&1",  entry( rawMode,  "Initial,Previous,Raw,Cumulative,Interval" ))).
  run makeRequest ( substitute( "xcmd=time_&1", entry( timeMode, "Summary,Rate" ))).
  return.
end.


procedure setRowLimit:
  run makeRequest ( substitute( "xcmd=rowLimit+&1", rowLimit )).
  return.
end.


procedure setSendType:

  define input parameter sendType as character no-undo.

  if lookup( sendType, "XML,JSON" ) > 0 then
    run makeRequest ( substitute( "xcmd=sendType+&1", sendType )).

  return.

end.


procedure protopPause:
  pause.
  return.
end.


/* when a programmers is checking a bit of code in isolation it is
 * helpful to use this combination of settings
 */

procedure programmerMode:

  if programmerMode = no then
    do:
      assign
        sampleMode = "OnDemand"
        rawMode    = 5
        timeMode   = 1
      .
      run makeRequest ( substitute( "xcmd=raw_&1",  "Interval" )).
      run makeRequest ( substitute( "xcmd=time_&1", "Summary" )).
      programmerMode = yes.
    end.
   else
    do:
      assign
        sampleMode = "Auto"
        rawMode    = 5
        timeMode   = 2
      .
      run makeRequest ( substitute( "xcmd=raw_&1",  "Interval" )).
      run makeRequest ( substitute( "xcmd=time_&1", "Rate" )).
      programmerMode = no.
    end.

  return.

end.


/* use the profiler to help debug internal ProTop performance issues
 */

define variable zprofilerState as logical initial ?.

procedure zprofiler:

  /*** message "Profiler state:" zprofilerState. ***/

  if zprofilerState = yes then				/* the profiler is already running				*/
    do:

      run zprofiler_off.				/* flip the state of the profiler to "off"			*/
      zprofilerState = no.

      run zprofiler_load.				/* load profiler data into temp-tables to analyze		*/
      run zprofiler_proc.				/* process the data						*/
      run zprofiler_topx( no ).				/* report on the top 20 execution time lines -- to file		*/
      run zprofiler_topx( yes ).			/* report on the top 20 execution time lines -- to TTY		*/

      message zprofilerState "run zprofiler_off".

      return.						/* do not continue after this -- return				*/

    end.

  if zprofilerState = ? then				/* we need to launch the profiler				*/
    do:
      run lib/zprofiler.p persistent (			/* launch the PP						*/
        "protop3",					/* output file basename						*/
        "ProTop3 Execution Profile"			/* description							*/
      ).
    end.

  zprofilerState = no.					/* default to no						*/

  message
    color value( "red" )
    skip(1)
    "  The Profiler capability is used to track down performance issues   " skip
    "  within the ProTop client.  It is very unusual for an end-user to   " skip
    "  need to run this for that purpose.                                 " skip
    skip(1)
    "  Aside from debugging ProTop this code is also a useful example of  " skip
    "  emebdding the profiler within an application.  The source can be   " skip
    "  found in lib/zprof*.p                                              " skip
    skip(1)
    "  It is fine to run this code in order to get a feel for how useful  " skip
    "  embedded profiling can be (IMHO it is *VERY* useful).              " skip
    skip(1)
    "  But be aware that profiling can very quickly create very large     " skip
    "  temp files (gigabytes in minutes) so do not run this just for      " skip
    "  giggles and do not leave it running unattended.                    " skip
    skip(1)
    view-as alert-box question buttons yes-no title " Are You Sure? "
    update zprofilerState
  .

  if zprofilerState = yes then
    do:

      run zprofiler_on.

      message
        color value( "red" )
        skip(1)
        '  The Profiler is now enabled.  Press "y"  ' skip    
        '  when you are ready to view the results.  ' skip   
        skip(1)
        view-as alert-box title " Profiler Enabled "
      .

    end.

  return.

end.


/* the contents of the CASE statement could probably be managed dynamically
 * by adding the keystroke mapping to the viewer/browser initialization
 *
 * that could also be used to build the help screen contents
 *
 */

procedure command:

  define input parameter c as character no-undo case-sensitive.

  define variable k as character no-undo.

  case c:

    when "e" then publish "protop_show" ( "appActivity" ).		/* aka appmon					*/
    when "j" then publish "protop_show" ( "pasoe" ).

    when "c" then publish "protop_show" ( "configuration" ).
    when "d" then publish "protop_show" ( "dashboard" ).
    when "U" then publish "protop_show" ( "userinfo" ).
    when "O" then publish "protop_show" ( "osinfo" ).

    when "l" then publish "protop_show" ( "latches" ).

    when "r" then publish "protop_show" ( "replagent" ).

    when "u" then publish "protop_show" ( "userio" ).
    when "t" then publish "protop_show" ( "tblact" ).
    when "i" then publish "protop_show" ( "idxact" ).
    when "/" then publish "protop_show" ( "seqact" ).
    when "a" then publish "protop_show" ( "areas" ).
    when "b" then publish "protop_show" ( "blocked" ).
    when "s" then publish "protop_show" ( "serveract" ).
    when "x" then publish "protop_show" ( "activetrx" ).
    when "f" then publish "protop_show" ( "fileio" ).
    when "k" then publish "protop_show" ( "chkpt" ).
    when "m" then publish "protop_show" ( "tenantinfo" ).
    when "w" then publish "protop_show" ( "who" ).
    when "Q" then publish "protop_show" ( "sql" ).

    when "B" then publish "protop_show" ( "bigb" ).

/*  when "T" then publish "protop_show" ( "txe" ).  */			/* obsolete - now part of latches & resources	*/
    when "T" then publish "protop_showRangeData".

    when "L" then publish "protop_show" ( "BrokerConfig" ).
    when "M" then run protopMail.

    when "2" then publish "protop_show" ( "b2" ).			/* also control-b				*/

    when "3" then run util/serverrpt.p.					/* move to control-s ? probably a bad idea	*/

    when "6" then publish "protop_getUserTblName" ( input-output userTblName, input-output userTblNum ).
    when "7" then publish "protop_getUserIdxName" ( input-output userIdxName, input-output userIdxNum ).

    when "8" then publish "protop_show" ( "usersoftbl" ).
    when "9" then publish "protop_show" ( "usersofidx" ).

    when "@" then publish "protop_show" ( "apsvstat" ).
    when "&" then publish "protop_properties".

    when "C" then publish "protop_clearCSC".

    when "D" then publish "protop_show" ( "df" ).
    when "N" then publish "protop_show" ( "netstat" ).

    when "I" then publish "protop_getMonInt"  ( input-output monInt ).
    when "#" then publish "protop_getUserNum" ( input-output userNum, input-output userPID ).
    when "P" then publish "protop_getUserPID" ( input-output userPID, input-output userNum ).

    when "R" then
      do:
        publish "protop_getRawMode" ( input-output rawMode, input-output timeMode ).
        publish "protop_setRawMode".
      end.

    when "S" then publish "protop_setSampleMode".

/*
 *  when "X" then
 *    do:
 *      define variable sendType as character no-undo.
 *      publish "protop_getSendType" ( input-output sendType ).
 *      publish "protop_setSendType" ( sendType ).
 *    end.
 */

    when "y" then run zprofiler.
    when "Y" then run lib/showustats.p.

    when "z" then run protopSort.

    when "Z" then
      do:
        publish "protop_getRowLimit" ( input-output rowLimit ).
        publish "protop_setRowLimit".
      end.

    when "^" then
      do:
        publish "protop_getLockLimit" ( input-output pt_lktbllim ).
      end.

    when "*" then publish "protop_statementCache".
    when "A" then publish "protop_dbanalys".

    when "p" then run protopPause.

    when " " then
      do:
        /* if sampleMode = "Auto" then publish "protop_refresh". */
      end.

    when "h" or when "H" or when "?" then
      do:
        publish "protop_help".
        k = keylabel( lastkey ).
        if k <> "h" and k <> "?" then run command( k ).
      end.

    when "q" then publish "protop_quit".

    {ssg/sausage04.i}

  end.

  return.

end.


on ctrl-a of nothing in frame notHere do:
  something = something + "^a".
  publish "protop_show" ( "aiinfo" ).
  return.
end.

on ctrl-b of nothing in frame notHere do:
  something = something + "^b".
  publish "protop_show" ( "b2" ).
  return.
end.

on ctrl-d of nothing in frame notHere do:
  something = something + "^d".
  publish "dump_load".
  return.
end.

on ctrl-l of nothing in frame notHere do:
  something = something + "^l".
  run lib/clife.p.
  return.
end.

on ctrl-p of nothing in frame notHere do:
  something = something + "^p".
  run programmerMode.
  return.
end.

on ctrl-r of nothing in frame notHere do:
  something = something + "^r".
  publish "dba_report".
  return. /* no-apply. */
end.

on ctrl-t of nothing in frame notHere do:
  something = something + "^t".
  if os-getenv( "TTDEBUG" ) = "yes" then
    publish "protop_showTT".
   else
    do:
      message
        skip(1)
        '  Temp table statistics require OpenEdge 11 or higher  ' skip
        '  and must be enabled by setting:                      ' skip
        skip(1)
        '        export TTDEBUG=yes                             ' skip
        skip(1)
        '  in $PROTOP/bin/localenv                              ' skip
        skip(1)
        '  You must also uncomment the -tt* parameters in       ' skip
        '  etc/protop.pf:                                       ' skip
        skip(1)
        '      -ttbaseindex 1                                   ' skip
        '      -ttbasetable 1                                   ' skip
        '      -ttindexrangesize 1000                           ' skip
        '      -tttablerangesize 1000                           ' skip
        skip(1)
       view-as alert-box
      .
    end.
  return.
end.

on ctrl-u of nothing in frame notHere do:
  something = something + "^u".
  run util/genupdsql.p.
  return.
end.

on DELETE-WORD OF nothing in frame notHere do:
  something = something + "esc-d".
  publish "getDbgMode".
  return.
end.

{ssg/sausage02.i}


/* initialization
 *
 */

if connected( "dictdb" ) then
  ptStartup = "dashboard,tblact,idxact,userio".				/* dynamic browses don't come to ChUI until 10.1C	*/
 else
  ptStartup = "".

if num-entries( session:parameter, "|" ) >= 1 then pt_shortname = entry( 1, session:parameter, "|" ).
if num-entries( session:parameter, "|" ) >= 2 then ptStartup    = entry( 2, session:parameter, "|" ).

if num-entries( session:parameter, "|" ) >= 3 then ptParams     = entry( 3, session:parameter, "|" ).	/* currently unused	*/
if num-entries( session:parameter, "|" ) >= 4 then ptCnxStr     = entry( 4, session:parameter, "|" ).	/* currently unused	*/

if "{&window-system}" <> "tty" then					/* look reasonable if someone fires up prowin32		*/
  assign
    current-window:hidden       = true
    session:v6display           = true
    current-window:height-chars = 60
    current-window:width-chars  = 160
    current-window:bgcolor      = 15
    current-window:hidden       = false
    current-window:visible      = true
  .

/* current-window:message-area = no.					/* no such luck :(					*/
 * current-window:status-area  = no.
 */

run lib/chkscreen.p.							/* is the screen big enough?				*/

connectMode = "Direct".

subscribe to "logMsg"                anywhere run-procedure "logMsg".
subscribe to "protop_quit"           anywhere run-procedure "quitme".
subscribe to "protop_command"        anywhere run-procedure "command".
subscribe to "protop_refresh"        anywhere run-procedure "refresh".
subscribe to "protop_register"       anywhere run-procedure "register".
subscribe to "protop_setRawMode"     anywhere run-procedure "setRawMode".
subscribe to "protop_setRowLimit"    anywhere run-procedure "setRowLimit".
subscribe to "protop_setSendType"    anywhere run-procedure "setSendType".
subscribe to "protop_setSampleMode"  anywhere run-procedure "setSampleMode".
subscribe to "protop_userinfo2"      anywhere run-procedure "userInfo2".
subscribe to "protop_statementCache" anywhere run-procedure "statementCache".
subscribe to "protop_dbanalys"       anywhere run-procedure "dbanalys".

run lib/ptheader.p ( input-output headerFrame, input-output modeIndicator, input-output protopVersion, input-output clockString ).

if fast-start then pause 0 before-hide.					/* set fast-start = no to preserve startup messages	*/

message "Starting ProTop...".

run lib/protoplib.p  persistent.					/* load protop infrastructure library           	*/

if connected( "dictdb" ) then						/* needs to be loaded prior to protop-cfg because	*/
  do:									/* isReplTarget() is potentially called by setptname	*/
    message "Loading vstlib...".
    run lib/vstlib.p persistent.					/* load vst function library                    	*/
  end.

run lib/protop-cfg.p persistent.					/* initialize protop environment                	*/

file-info:file-name = pt_tmpdir.					/* make certain that we have a temp directory!		*/
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_tmpdir ).

file-info:file-name = pt_logdir.					/* make certain that we have a log directory!		*/
if file-info:full-pathname = ? then
  os-command silent value( "mkdir " + pt_logdir ).

/* chkDbgMode() is in lib/protoplib.p
 */

dbgFileName = substitute( "&1/&2.&3.&4", pt_tmpdir, monName, ( if pt_shortname <> "" then pt_shortname else ldbname(1)), "dbg" ).
run chkDbgMode( dbgFileName, input-output dbgMode ).

if connected( "dictdb" ) then
  do:

    run lib/ptsetvardb.p persistent.					/* set variables that affect behavior and need a cnx	*/

    message "Checking table and index ranges...".
    run lib/xrange.p persistent.

    message "Initializing _userTable and _userIndex stats...".
    run lib/usertablestats.p persistent.				/* some additional useful debugging support		*/

  end.

/*** temporary !!! *** use only if "sports" is connected!!!
 ***
 *** this is just to generate some usertablestats for illustration purposes 
 ***
 ***

define variable j as integer no-undo.

do while true:

  case random( 1, 9 ):
    when   1 then for each dictdb.Invoice       no-lock: end.
    when   2 then for each dictdb.Customer      no-lock: end.
    when   3 then for each dictdb.Item          no-lock: end.
    when   4 then for each dictdb.Order         no-lock: end.
    when   5 then for each dictdb.Order-Line    no-lock: end.
    when   6 then for each dictdb.Salesrep      no-lock: end.
    when   7 then for each dictdb.State         no-lock: end.
    when   8 then for each dictdb.Local-Default no-lock: end.
    when   9 then for each dictdb.Ref-Call      no-lock: end.
  end.

  j = j + 1.
  if j > 20 then leave.

end.

 ***
 ***
 *** ^^^^^^^^^^^^^ ***/


if "{&window-system}" = "tty" then
  run lib/ptsplash.p.
 else
  .									/* we should do something with the fancy logo!		*/

if connected( "dictdb" ) then run lib/chkcs.p.

if os-getenv( "TTDEBUG" ) = "yes" then
  run lib/ttinfo.p persistent.						/* some useful debugging support			*/

run ssg/sausage42.p persistent.

{ssg/sausage05.i}

run lib/mailx.p     persistent.						/* handle mail						*/
run lib/ptsetvar.p  persistent.						/* set variables that affect protop.p behavior		*/
run lib/pthelp.p    persistent.						/* the help screen					*/
run lib/ptprops.p   persistent.						/* properties controlled by environment variables	*/

run ssg/sausage02.p persistent.

publish "protop_checkRangeData".

message "Configuring UI...".

run lib/dynscreen.p persistent.						/* dynamic screen painting routines			*/

message "Launching data collectors...".

if connectMode = "direct" then
  do:
    run lib/ptsrv.p persistent.						/* launch data collectors in the srv directory		*/
    run initProTopServer( "direct", "", "" ).				/* no arguments needed for direct connections		*/
  end.

if dbgMode >= 3 then
  message "Refresh data collectors...".

run refresh.		/*** this is a cheat -- it causes the data collectors to be instantiated which has the side-effect	***/
			/*** of regenerating the .xsd files that are needed to configure the viewers & browsers.  in this	***/
			/*** way they xsd is always current and up to date for direct connections.				***/
			/*** for indirect connections the .xsd still has to be distributed to the client as part of a		***/
			/*** release.												***/

if dbgMode >= 3 then
  message "Setting up data collector UI...".

run getConfig( "tt_protop.xml", output result ).

buffer tt_protop:read-xml( "longchar", result, ?, ?, ? ).
result = "".

/* create the UI objects for each data collector
 */

for each tt_protop by tt_protop.order:
  if dbgMode >= 3 then message "creating:" order evtName scrTitle dcName ttName.
  run value( substitute( "lib/&1.p", scrType )) persistent ( tt_protop.order, evtName, scrTitle, dcName, ttName, minRows, maxRows ).
  if dbgMode >= 3 then message "created:" evtName ttName.
end.

/* ok, we're ready for business!
 */

if dbgMode >= 3 then
  message "Initializing header...".

publish "protop_dbid".							/* line #2 of the header - dbId data collector		*/

if index( ptStartup, "dbid" ) <= 0 then ptStartup = "dbId," + ptStartup.
ptStartup = right-trim( ptStartup, "," ).

do i = 1 to num-entries( ptStartup ):
  publish "protop_show" ( entry( i, ptStartup )).			/* startup with the specified data collectors		*/
end.

lastRefresh = now.
publish "protop_refresh".

if dbgMode >= 3 then
  message "Ready to start...".

if fast-start = no or dbgMode >= 3 then					/* set fast-start = no to preserve startup messages	*/
  pause.								/* slow start, pause to read startup messages		*/

hide message no-pause.


/* main block
 *
 */

define variable keyPressed as logical no-undo.

nothing = "".
enable nothing with frame notHere.

main_block: do while keepRunning on error undo, retry:

  /* PAUSE can take fractions of a second starting with OE11, prior to that 0.5 rounds up to 1
   */

  /* wait-for "window-close" of this-procedure pause 0.5. */

  /* priority #1 is keystrokes...
   */

  keyPressed = no.

  /*** if keystrokes mysteriously vanish it is likely that some code somewhere
   *** is doing INPUT FROM or INPUT THROUGH without a STREAM -- track down that
   *** code and add a STREAM
   ***
   ***/

  readkey pause tick. 					/* This works much better than a WAIT-FOR PAUSE N			*/

  do while lastkey > 0:					/* a key was pressed! execute the appropriate command			*/
    keyPressed  = yes.
    if keyfunction( lastkey ) = "end-error" then
      leave main_block.
    apply lastkey to nothing in frame notHere.
    readkey pause tick.					/* keep reading keystrokes						*/
  end.

  if keyPressed then
    do:
      lastRefresh = now.
      publish "protop_refresh".
      next main_block.
    end.

  /*
   * if connectMode <> "direct" then			/* obsolete and no longer supported...					*/
   *   do:
   *     process events.				/* maybe a non-readkey event (stomp or http response) has occured?	*/
   *     if length( result ) > 0 then
   *       do:
   *         lastRefresh = now.
   *         run paintScreen.
   *         next main_block.
   *       end.
   *   end.
   */

  /* timeouts only matter if nothing else happens
   */

  if lastkey <= 0 then					/* timeout...								*/
    do:
      if sampleMode = "Auto" and abs( interval( now, lastRefresh, "seconds" )) >= ( monint - ( if connectMode = "direct" then 0 else 1 )) then
        do:

          if pdbname( 1 ) <> ? then
            run checkDBX.				/* check to see if a new DBAnalys is available				*/

          run chkDbgMode( dbgFileName, input-output dbgMode ).

          lastRefresh = now.
          publish "protop_refresh".

          next main_block.

      end.
    end.

end.

end.	/* on stop etc ... */

publish "protop_clearCSC".				/* if the user set any client statement cache options clear them	*/

if program-name( 4 ) = ? then				/* was protop started via -p?  or called from something else?		*/
  quit.							/* do not drop out to the "tram lines"					*/
 else
  return.						/* someone called us so return there					*/

/* the end */
