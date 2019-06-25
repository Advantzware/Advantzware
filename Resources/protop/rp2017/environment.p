/* environment.p
 *
 */

define input  parameter stuff  as character no-undo.

define output parameter uname    as character no-undo format "x(60)".
define output parameter cntStr   as character no-undo.
define output parameter ofName   as character no-undo.
define output parameter nProc    as integer   no-undo format ">>>>9" initial 100.
define output parameter jump     as integer   no-undo format   ">>9" initial  10.
define output parameter ramp     as integer   no-undo format   ">>9" initial  50.
define output parameter scenario as character no-undo format "x(250)". /* view-as fill-in size 60 by 1. */
define output parameter xSpin    as integer   no-undo.
define output parameter xCPU     as integer   no-undo.
define output parameter xMHz     as integer   no-undo.


define variable contrib  as character no-undo format "x(50)".
define variable email    as character no-undo format "x(50)".

define variable cpu#     as integer   no-undo format ">>>9".
define variable clock    as integer   no-undo format ">>>>>9".
define variable spin     as integer   no-undo format ">>>>>9".
define variable xtext    as character no-undo format "x(250)" view-as fill-in size 60 by 1.
define variable xdata    as character no-undo format "x(250)" view-as fill-in size 60 by 1.
/* define variable scenario as character no-undo format "x(250)" view-as fill-in size 60 by 1. */
define variable vfile    as character no-undo.
define variable vstr     as character no-undo.
define variable pver     as character no-undo.
define variable xtag     as character no-undo.
define variable ofcnt    as integer no-undo.

define stream inStrm.
define stream out-data.

{lib/v9.i}
{lib/osinfo.i}

form
  skip(1)
  "    # of Sessions: " nproc to 28 "  After:" ramp "  Jump by:" jump skip
  skip(1)
  " CPU info: " cpu#  to 28 "x" clock "MHz" skip
  skip(1)
  "  Comment: " xtext    ">>" skip
  " Scenario: " scenario view-as fill-in size 60 by 1 ">>" skip
  " uname -a: " uname         skip
  "  Ext Sys: " xdata    ">>" skip
  skip(1)
  "      Contributor: " contrib     skip
  "           e-mail: " email       skip
  " Progress Version: " pver        skip
  skip(1)
 with
  frame sys-data
  width 80
  no-labels
  no-box
.

vfile = search( "version" ).

pver = proversion.	/* yup, I'm a wimp...	*/

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.0
&THEN
        xtag = "Release".
&ELSE
        xtag = "Version".
&ENDIF

if vfile <> ? then
  do:
    input stream inStrm from value( vfile ).
    import stream inStrm unformatted vstr.
    input stream inStrm close.
    pver = entry( 1, substring( vstr, index( vstr, xtag ) + length( xtag ) + 1 ), " " ).
  end.

/* i.e.
 *
 * -param "RedHat Linux 7.1, 1x300MHz AMD K6, 598 BogoMIPS"
 *
 */

xtext = session:parameter.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 12.0
&THEN
find _DbParams no-lock where _DbParams-name = "-spin" no-error.
spin = integer( _dbParams-Value ).
&ELSE
find _startup no-lock.
spin = _Startup._Startup-spin.
&ENDIF

find first _BuffStatus no-lock.

contrib = userid( "dictdb" ).

scenario = os-getenv( "SCENARIO" ).

if opsys = "unix" then
  do on endkey undo, leave:

    input stream inStrm through value( "uname -a" ).
    import stream inStrm unformatted uname.
    input stream inStrm close.

    input stream inStrm through value( "grep ^$LOGNAME: /etc/passwd | awk -F : '~{print $5~}'" ).
    import stream inStrm unformatted contrib.
    input stream inStrm close.
    if contrib = "" then contrib = os-getenv( "LOGNAME" ).

    input stream inStrm through value( "echo $LOGNAME@`hostname`" ).
    import stream inStrm unformatted email.
    input stream inStrm close.

  end.

run osInfo.

/*
cpu#  = integer( os-getenv( "NCORE" )).
clock = integer( os-getenv( "CPUMHZ" )).
 */

assign
  cpu#  = integer( tt_osInfo.numCPU )
  clock = decimal( entry( 1, tt_osInfo.clkCPU, "G" )) * 1000
no-error.

if cpu#  = 0 or cpu#  = ? then cpu#  = integer( os-getenv( "NCORE" )).
if clock = 0 or clock = ? then clock = integer( os-getenv( "CPUMHZ" )).

xCPU = cpu#.
xMHz = clock.

os-create-dir value( "results" ).				/* create a "results" directory if it does not already exist	*/

ofcnt = 0.
file-info:file-name = "results/counter.txt".
if file-info:full-pathname <> ? then
  do:
    input stream inStrm from "results/counter.txt".
    import stream inStrm ofcnt.
    input stream inStrm close.
    ofcnt = ofcnt + 1.
  end.

cntStr = string( ofcnt, "999" ).

output to "results/counter.txt".
put cntStr skip.
output close.

os-copy value( "rp.pf" )            value( substitute( "results/&1.rp.pf", cntStr )).
os-copy value( ldbname(1) + ".pf" ) value( substitute( "results/&1.&2.pf", cntStr, ldbname(1))).

os-command silent value( substitute( "tail -100 &2.lg > results/&1.&2.lg", cntStr, ldbname(1))).

output to value( substitute( "results/&1.osinfo", cntStr )).
display tt_osInfo with side-labels 1 column width 132.
output close.

if opsys = "unix" then
  do:
    os-command silent value( substitute( "ipcs -a     > results/&1.ipcs     2>&&1", cntStr )).
  end.

if uname begins "AIX" then
  do:

    /* should be available without privileges */

    os-command silent value( substitute( "lsattr -E -l sys0 > results/&1.lsattr   2>&&1", cntStr )).
    os-command silent value( substitute( "lssrad -va        > results/&1.lssrad   2>&&1", cntStr )).
    os-command silent value( substitute( "lsmcode -c        > results/&1.lsmcode  2>&&1", cntStr )).
    os-command silent value( substitute( "oslevel -r        > results/&1.oslevel  2>&&1", cntStr )).
    os-command silent value( substitute( "lparstat -i       > results/&1.lparstat 2>&&1", cntStr )).
    os-command silent value( substitute( "prtconf           > results/&1.prtconf  2>&&1", cntStr )).
    os-command silent value( substitute( "vmstat -v         > results/&1.vmstat_v 2>&&1", cntStr )).

    /* may require "root" privileges :( */

    os-command silent value( substitute( "vmo -a            > results/&1.vmo      2>&&1", cntStr )).
    os-command silent value( substitute( "ioo -a            > results/&1.ioo      2>&&1", cntStr )).
    os-command silent value( substitute( "no -a             > results/&1.no       2>&&1", cntStr )).

    input stream inStrm through value( "lparstat | grep lcpu | awk '~{print $6~}' | awk -F= '~{print $2~}'" ).
    import stream inStrm cpu#.
    input stream inStrm close.

    input stream inStrm through value( "lparstat | grep lcpu" ).
    import stream inStrm unformatted xdata.
    input stream inStrm close.
    xdata = trim( entry( 2, xdata, ":" )).
  
    input stream inStrm through value( substitute( "grep 'Processor Clock Speed:' results/&1.prtconf | awk -F= '~{print $4~}'", cntStr )).
    import stream inStrm clock.
    input stream inStrm close.

  end.

ofname = substitute( "results/&1.readprobe", cntStr ).

if session:batch = no then
  do on error undo, retry
     on endkey undo, retry:

    if retry then quit.

    display
      nproc ramp jump
      contrib email pver cpu# clock uname xtext scenario xdata
     with
      frame sys-data
    .

    if stuff = "all" then
      do:
        update
          nproc ramp jump
	  cpu# clock
          contrib email pver
	  xtext scenario uname xdata
         with
          frame sys-data
        .
      end.
     else if stuff = "config" then
      do:
        update
	  cpu# clock
          contrib email pver
	  xtext scenario uname xdata
         with
          frame sys-data
        .
      end.

  end.

/* I don't see how this can ever happen?  So it seems pointless to convert it to post oe12 no _startup flavor
 *
 *  if spin <> _Startup._Startup-spin then
 *    do:
 *      find _startup exclusive-lock no-error.
 *      if available _Startup then
 *        _Startup._Startup-spin = spin.
 *       else
 *        message "Unable to modify -spin.".
 *    end.
 *
 */

xSpin = spin.

output stream out-data to value( ofname ) unbuffered.

put stream out-data
  "Progress Version " pver string( today ) + " " + string( time, "hh:mm am") format "x(17)" to 78 skip
  skip(1)
  xtext    skip
  scenario skip
  xdata    skip
  skip(1)
  " Number of CPUs: " cpu#  to 30   skip
  "    Clock Speed: " clock to 30   skip
  "          -Spin: " spin  to 30   skip
  "             -B: " _BuffStatus._BfStatus-TotBufs to 30 skip
  "          uname: " uname         skip
  "    Contributor: " contrib + " " + email format "x(60)" skip
  "         dbName: " ldbname(1)    skip
  skip(1)
  "Sessions  Time     Rec/s     Blk/s UsrRec/s    LatW/s   BHTW/s   LRUW/s   LR2W/s   LdAvg    %Usr    %Sys   %Idle    %Ent  UsrExp" skip
  "===== ======== ========= ========= ======== ========= ======== ======== ======== ======= ======= ======= ======= ======= =======" skip.

output stream out-data close.

return.
