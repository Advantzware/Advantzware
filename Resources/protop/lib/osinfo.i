/* lib/osinfo.i
 *
 * minimalist example:
 *
 *	{lib/v9.i}
 *
 *      define stream inStrm.		/* be wary of possible duplicates */
 *
 *	{lib/osinfo.i}
 *
 *	run osInfo.
 *
 *	for each tt_osInfo:
 *	  display tt_osInfo with side-labels 1 column.
 *	end.
 *
 */

define temp-table tt_OSInfo no-undo

  field hostName  as character label "Host"      format "x(30)"
  field osName    as character label "OS"        format "x(100)"
  field osVersion as character label "osVersion" format "x(30)"
  field keVersion as character label "keVersion" format "x(30)"
  field osUpTime  as character label "Up Time"   format "x(30)"

  field maxFileSz  as character label "Max File Size"   format "x(16)"
  field maxOpenFl  as character label "Max Open Files"  format "x(16)"
  field maxProcMem as character label "Max Proc Memory" format "x(16)"
  field maxProcs   as character label "Max Processes"   format "x(16)"

  field maxStack   as character label "Max Stack"       format "x(16)"
  field maxDataSeg as character label "Max Data Seg"    format "x(16)"

  field SEMMNS     as character initial "n/a" format "x(16)"
  field SEMMNI     as character initial "n/a" format "x(16)"
  field SEMMSL     as character initial "n/a" format "x(16)"
  field SEMOPM     as character initial "n/a" format "x(16)"
  field SHMMAX     as character initial "n/a" format "x(16)"
  field SHMMNI     as character initial "n/a" format "x(16)"
  field SHMALL     as character initial "n/a" format "x(16)"
  field osPageSize as integer

  field modelCPU   as character format "x(30)"
  field clkCPU     as character format "x(16)"
  field numCPU     as character format "x(16)"
  field memTotal   as character format "x(16)"

  field memBufs    as decimal format ">>>>>>>>>>>>>>>>>>>>9"
  field memCached  as decimal format ">>>>>>>>>>>>>>>>>>>>9"
  field memShared  as decimal format ">>>>>>>>>>>>>>>>>>>>9"
  field memActive  as decimal format ">>>>>>>>>>>>>>>>>>>>9"
  field memInAct   as decimal format ">>>>>>>>>>>>>>>>>>>>9"
  field memFree    as decimal format ">>>>>>>>>>>>>>>>>>>>9"

  index hostName-idx is unique primary hostName
.

define variable linuxPatch as logical initial yes.

procedure getAIX:

  define variable inLine as character no-undo extent 16.
  define variable xLine  as character no-undo.

  find tt_OSInfo no-error.

  return.

end.

procedure getSunOS:

  define variable inLine as character no-undo extent 16.
  define variable xLine  as character no-undo.

  find tt_OSInfo no-error.

  /* $ uname -X
   * System = SunOS
   * Node = traxlive
   * Release = 5.10
   * KernelID = Generic_142900-03
   * Machine = sun4u
   * BusType = <unknown>
   * Serial = <unknown>
   * Users = <unknown>
   * OEM# = 0
   * Origin# = 1
   * NumCPU = 48
   */

  input stream inStrm through value( "uname -X" ).
  repeat:

     xLine = "".
     import stream inStrm unformatted xLine.

     if       index( xLine, "NumCPU" )   > 0 then numCPU    = trim( entry( 2, xLine, "=" )).
      else if index( xLine, "Release" )  > 0 then osVersion = trim( entry( 2, xLine, "=" )).
      else if index( xLine, "KernelID" ) > 0 then keVersion = trim( entry( 2, xLine, "=" )).

  end.
  input stream inStrm close.

  /* $ nslookup traxlive
   * Server:         10.1.8.21
   * Address:        10.1.8.21#53
   * 
   * Name:   traxlive.intcomex.com
   * Address: 10.1.10.178
   */

  xLine = "".
  input stream inStrm through value( "nslookup " + hostName + " | grep Name" ).
  import stream inStrm unformatted xLine.
  if index( xLine, hostName ) > 0 then hostName = trim( entry( 2, xLine, ":" )).
  input stream inStrm close.

  /* $ /usr/sbin/prtconf | more
   * System Configuration:  Sun Microsystems  sun4u
   * Memory size: 131072 Megabytes
   * System Peripherals (Software Nodes):
   * 
   * SUNW,SPARC-Enterprise
   * ...
   */

  xLine = "".
  input stream inStrm through value( '/usr/sbin/prtconf | grep "Memory size:"' ).		/* reported in Megabytes...	*/
  import stream inStrm unformatted xLine.
  memTotal = trim( entry( 3, xLine, " " )).
  input stream inStrm close.

  memTotal = trim( string( decimal( memTotal ) / 1024, ">>>>>>>>9.99" )) + " GB" no-error.

  /*                            Old             Old       New         New
   *  Resource control          tunable         default   Max value   default
   *  ----------------------    -------------   -------   ---------   ----------
   *  process.max-sem-ops       seminfo_semopm  10        INT_MAX     512
   *  process.max-sem-nsems     seminfo_semmsl  25        SHRT_MAX    512
   *  project.max-sem-ids       seminfo_semmni  10        2**24       128
   *
   *  project.max-shm-memory    shminfo_shmmax  0x800000  UINT64_MAX  1/4 physmem
   *  project.max-shm-ids       shminfo_shmmni  100       2**24       128
   *
   *  $ prctl -n process.max-sem-ops $$
   *  process: 21592: -bash
   *  NAME    PRIVILEGE       VALUE    FLAG   ACTION                       RECIPIENT
   *  process.max-sem-ops
   *          privileged        512       -   deny                                 -
   *          system          2.15G     max   deny                                 -
   *
   *  $ prctl -n process.max-sem-ops $PPID | grep privileged
   *  semopm         privileged        512       -   deny 
   *
   *  $ prctl -n process.max-sem-ops -P $PPID | grep privileged
   *  process.max-sem-ops privileged 512 - deny -
   *
   */

  inLine = "".
  input stream inStrm through value( 'prctl -n process.max-sem-ops -P $PPID | grep privileged' ).
  import stream inStrm inLine.
  if inLine[2] <> "failed" then SEMOPM = trim( inLine[3] ).
  input stream inStrm close.

  inLine = "".
  input stream inStrm through value( 'prctl -n process.max-sem-nsems -P $PPID | grep privileged' ).
  import stream inStrm inLine.
  if inLine[2] <> "failed" then SEMMSL = trim( inLine[3] ).
  input stream inStrm close.

  inLine = "".
  input stream inStrm through value( 'prctl -n process.max-sem-ids $PPID | grep privileged' ).
  import stream inStrm inLine.
  if inLine[2] <> "failed" then SEMMNI = trim( inLine[3] ).
  input stream inStrm close.

  inLine = "".
  input stream inStrm through value( 'prctl -n process.max-shm-memory $PPID | grep privileged' ).
  import stream inStrm inLine.
  if inLine[2] <> "failed" then SHMMAX = trim( inLine[3] ).
  input stream inStrm close.

  inLine = "".
  input stream inStrm through value( 'prctl -n process.maxx-shm-ids $PPID | grep privileged' ).
  import stream inStrm inLine.
  if inLine[2] <> "failed" then SHMMNI = trim( inLine[3] ).
  input stream inStrm close.

  osPageSize = 4096.

  /* $ psrinfo -v | grep "operates at" | head -1
   *  The sparcv9 processor operates at 2530 MHz,
   */

  inLine = "".
  input stream inStrm through value( 'psrinfo -v | grep "operates at" | head -1' ).
  import stream inStrm inLine.
  if inLine[4] = "operates" then
    assign
      modelCPU = inLine[2]
      clkCPU   = inLine[6] + " " + inline[7]
    .
  input stream inStrm close.

  return.

end.

procedure getHPUX:

  define variable inLine as character no-undo extent 16.
  define variable xLine  as character no-undo.

  find tt_OSInfo no-error.

/* "machinfo"... */



/* HP-UX just has to be different :(
 *
 * $ uname -a
 * 
 * HP-UX uslvsheila02 B.11.31 U ia64 1827409880 unlimited-user license
 * 
 * $ ulimit -a
 * time(seconds)        unlimited
 * file(blocks)         unlimited
 * data(kbytes)         3932160
 * stack(kbytes)        262140
 * memory(kbytes)       unlimited
 * coredump(blocks)     4194303
 * 
 */

/* In HP/UX v 11, the command to display kernel parameters is kctune and we can grep to see the semaphore settings.
 *
 * $ /usr/sbin/kconfig -v
 * Configuration last_install
 * Title         Created by last OS install
 * Created       Tue Dec 14 12:29:49 2010 by root
 *               by saving the running configuration
 * Modified      Tue Dec 14 12:29:50 2010 by root
 * Kernel Path   /stand/last_install/vmunix
 * 
 * $ /usr/sbin/kctune -c last_install | grep sem
 *
 * sema                 1
 * semaem               16384
 * semmap               (SEMMNI+2)
 * semmni               200
 * semmns               800
 * semmnu               30
 * semume               10
 * semvmx               32767
 */

  return.

end.

procedure getWIN32:

  define variable inLine as character   no-undo extent 16.
  define variable xLine  as character   no-undo.

  define variable i      as integer     no-undo.

  define variable dtz    as {&DTZ} no-undo.

  find tt_OSInfo no-error.

  xLine = "".
  input stream inStrm through value( 'wmic os get csname' ).
  import stream inStrm unformatted xLine.
  import stream inStrm unformatted hostName.
  input stream inStrm close.

  xLine = "".
  input stream inStrm through value( 'wmic os get caption' ).
  import stream inStrm unformatted xline.
  import stream inStrm unformatted osName.
  input stream inStrm close.
  osName = substring( osName, 11 ).

  xLine = "".
  input stream inStrm through value( 'wmic os get version' ).
  import stream inStrm unformatted xline.
  import stream inStrm unformatted osVersion.
  input stream inStrm close.

  xLine = "".
  input stream inStrm through value( 'wmic cpu get name' ).
  import stream inStrm unformatted xline.
  import stream inStrm unformatted modelCPU.
  input stream inStrm close.
  modelCPU = replace( modelCPU, "(R)", "" ).
  modelCPU = replace( modelCPU, "(TM)", "" ).
  i = index( modelCPU, "CPU" ).
  if i > 0 then modelCPU = substring( modelCPU, 1, i - 2 ).
  i = index( modelCPU, "processor" ).
  if i > 0 then modelCPU = substring( modelCPU, 1, i - 2 ).

  xLine = "".
  input stream inStrm through value( 'wmic cpu get maxclockspeed' ).
  import stream inStrm unformatted xline.
  import stream inStrm unformatted clkCPU.
  input stream inStrm close.
  clkCPU = trim( string( ( decimal( clkCPU ) / 1000.0 ), ">>>>>>9.99" )) + " Ghz".

  xLine = "".
  input stream inStrm through value( 'wmic cpu get numberoflogicalprocessors' ).
  import stream inStrm unformatted xline.
  import stream inStrm unformatted numCPU.
  input stream inStrm close.

  xLine = "".
  input stream inStrm through value( 'wmic computersystem get totalphysicalmemory' ).
  import stream inStrm unformatted xline.
  import stream inStrm unformatted memTotal.
  input stream inStrm close.
  memTotal = trim( string( ( decimal( memTotal ) / ( 1024 * 1024 * 1024 )), ">>>>>>>9.99" )) + " GB".

  xLine = "".
  input stream inStrm through value( 'wmic os get lastbootuptime' ).
  import stream inStrm unformatted xline.
  import stream inStrm unformatted osUpTime.
  input stream inStrm close.

&IF DEFINED( OE10 ) &THEN

  dtz = datetime-tz(
    integer( substring( osUpTime,  5, 2 )),		/* month	*/
    integer( substring( osUpTime,  7, 2 )),		/* day		*/
    integer( substring( osUpTime,  1, 4 )),		/* year		*/
    integer( substring( osUpTime,  9, 2 )),		/* hours	*/
    integer( substring( osUpTime, 11, 2 )),		/* minutes	*/
    integer( substring( osUpTime, 13, 2 )),		/* seconds	*/
    integer( integer( substring( osUpTime, 16, 6 )) / 1000 ),	/* milliseconds	*/
    integer( substring( osUpTime, 22 ))			/* TZ		*/
  ).

  osUpTime =
    string( abs ( interval( dtz, now, "days" )), ">>>>>>" ) + " " +
    string( abs ( interval( dtz, now, "seconds" )), "hh:mm:ss" )
  .

  osUpTime = trim( osUpTime ).

&ENDIF

  return.

end.

procedure getLinux:

  define variable inLine as character no-undo extent 16.
  define variable xLine  as character no-undo.

  find tt_OSInfo no-error.

  if search( "/etc/SuSE-release" ) <> ? then                 /* SuSE */
    /* sample:
    SUSE Linux Enterprise Server 10 (x86_64)
    VERSION = 10
    PATCHLEVEL = 2
    */
    do:
      input stream inStrm from value( "/etc/SuSE-release" ).
      import stream inStrm unformatted xLine.
      osName = xline. /* substring( xLine, 1, 30 ). */
      xLine = "".
      import stream inStrm unformatted xLine.
      osVersion = entry( 3, xLine, " " ) + ".".
      xLine = "".
      import stream inStrm unformatted xLine.
      osVersion = osVersion + entry( 3, xLine, " " ).
      input stream inStrm close.
    end.
   else if search( "/etc/centos-release" ) <> ? then          /* CentOS */
    /* sample:
    CentOS release 6.5 (Final)
    */
    do:
      input stream inStrm through value( 'cut -b 1-25 "/etc/centos-release"' ).
      import stream inStrm unformatted osName.
      input stream inStrm close.
      osVersion = entry( 3, osName, " " ).
    end.
   else if search( "/etc/redhat-release" ) <> ? then          /* RH (or a clone?) */
    /* sample:
    Red Hat Enterprise Linux Server release 6.3 (Santiago)
    */
    do:
      input stream inStrm from value( "/etc/redhat-release" ).
      import stream inStrm unformatted xLine.
      input stream inStrm close.
      osName = xLine. /* substring( xLine, 1, 25 ). */
      osVersion = entry( 2, substring( xLine, index( xLine, "release" ) ), " " ).
    end.
   else                                                       /* some other Linux */
    do:
      input stream inStrm through value( 'uname' ).
      import stream inStrm osName.
      input stream inStrm close.
    end.

  input stream inStrm from value( "/proc/sys/kernel/sem" ).
  import stream inStrm SEMMSL SEMMNS SEMOPM SEMMNI.
  input stream inStrm close.

  input stream inStrm from value( "/proc/sys/kernel/shmmax" ).
  import stream inStrm SHMMAX.
  input stream inStrm close.

  input stream inStrm through value( "getconf PAGESIZE" ).
  import stream inStrm osPageSize.   /* in bytes */
  input stream inStrm close.

  SHMMAX = string( decimal( SHMMAX ) / ( 1024 * 1024 * 1024 ), ">>>>>>>>>>>>>>>>>>>>9" ) + " GB".

  input stream inStrm from value( "/proc/sys/kernel/shmmni" ).
  import stream inStrm SHMMNI.
  input stream inStrm close.

  input stream inStrm from value( "/proc/sys/kernel/shmall" ).
  import stream inStrm SHMALL.
  input stream inStrm close.

  SHMALL = string(( decimal( SHMALL ) / ( 1024 * 1024 * 1024 )) * osPageSize, ">>>>>>>>>>>>>>>>>>>>9" ) + " GB".

  input stream inStrm from value( "/proc/cpuinfo" ).
  repeat:
    xLine = "".
    import stream inStrm unformatted xLine.
    if       xLine begins "model name" then modelCPU = entry( 2, xLine, ":" ).
     else if xLine begins "cpu MHz"    then clkCPU   = trim( string( decimal( trim( entry( 2, xLine, ":" ))) / 1024, ">>>>>9.99" )).
     else if xLine begins "core id"    then leave.
  end.
  input stream inStrm close.

  input stream inStrm through value( 'grep "processor" /proc/cpuinfo | wc -l' ).
  import stream inStrm numCPU.
  input stream inStrm close.

  clkCPU = clkCPU + " GHz".

  if modelCPU <> "" then
    do:
      assign
        modelCPU = entry( 1, modelCPU, "@" )
        modelCPU = replace( modelCPU, "(R)", "" )
        modelCPU = replace( modelCPU, "Intel", "" )
        modelCPU = replace( modelCPU, "CPU", "" )
      .
      do while index( modelCPU, "  " ) > 0:
        modelCPU = replace( modelCPU, "  ", " " ).
      end. 
    end.

  input stream inStrm from value( "/proc/meminfo" ).
  repeat:
    xLine = "".
    import stream inStrm unformatted xLine.
    if       xLine begins "MemTotal"   then memTotal  = trim( string( decimal( entry( 1, trim( entry( 2, xLine, ":" )), " " )) / ( 1024 * 1024 ), ">>>>>>>>>>>>9.99 GB" )).
  /*** this is more of a realtime dashboard -- nmon or top are much better for this
   ***
     else if xLine begins "MemFree"    then memFree   = decimal( entry( 1, trim( entry( 2, xLine, ":" )), " " )) / ( 1024 * 1024 ).
     else if xLine begins "Buffers"    then memBufs   = decimal( entry( 1, trim( entry( 2, xLine, ":" )), " " )) / ( 1024 * 1024 ).
     else if xLine begins "Cached"     then memCached = decimal( entry( 1, trim( entry( 2, xLine, ":" )), " " )) / ( 1024 * 1024 ).
     else if xLine begins "Shmem"      then memShared = decimal( entry( 1, trim( entry( 2, xLine, ":" )), " " )) / ( 1024 * 1024 ).
     else if xLine begins "Active:"    then memActive = decimal( entry( 1, trim( entry( 2, xLine, ":" )), " " )) / ( 1024 * 1024 ).
     else if xLine begins "Inactive:"  then memInAct  = decimal( entry( 1, trim( entry( 2, xLine, ":" )), " " )) / ( 1024 * 1024 ).
   ***
   ***/
     else if xLine begins "Slab" then leave.
  end.
  input stream inStrm close.

  input stream inStrm through value( "nslookup " + hostName + " | grep canonical" ).	/* Linux only?	*/
  import stream inStrm xLine.
  if index( xLine, hostName ) > 0 then hostName = xLine.
  input stream inStrm close.

  return.

end.

procedure osInfo:

  define variable inLine as character no-undo extent 16.
  define variable xLine  as character no-undo.

  find tt_OSInfo no-error.
  if not available( tt_osinfo ) then create tt_osinfo.

  if opsys begins "WIN" then
    assign
      osName = "Please apply the Linux patch ;)"
      linuxPatch = yes
    .
   else
    do:
      input stream inStrm through value( "uname -a" ).
      import stream inStrm osName hostName keVersion.
      input stream inStrm close.
    end.

  if opsys <> "WIN32" then
    do:

      input stream inStrm through value( "ulimit -a" ).		/* this should be portable across all unix implementations...	*/
								/* except, of course, for HP-UX :(				*/
      repeat:

        /* $ ulimit -a
         * time(seconds)        unlimited
         * file(blocks)         unlimited
         * data(kbytes)         3932160
         * stack(kbytes)        262140
         * memory(kbytes)       unlimited
         * coredump(blocks)     4194303
         */

        xLine = "".
        import stream inStrm unformatted xLine.

        if index( xLine, "-f)" ) > 0 or xLine begins "file(blocks)" then
          do:
            maxFileSz  = trim( entry( 2, xLine, ")" )).											/* ulimit -f	*/
            if maxFileSz <> "unlimited" then 
              maxFileSz  = string( decimal( maxFileSz ) / ( 1024 * 1024 * 2 )) + " GB" no-error.					/* ulimit value is in "blocks"	*/
          end.
         else if index( xLine, "-n)" ) > 0 then maxOpenFl  = trim( entry( 2, xLine, ")" )) no-error.					/* ulimit -n	*/
         else if index( xLine, "-u)" ) > 0 then maxProcs   = trim( entry( 2, xLine, ")" )) no-error.					/* ulimit -u	*/
         else if index( xLine, "-s)" ) > 0 or xLine begins "stack(kbytes)"  then maxStack   = trim( entry( 2, xLine, ")" )) no-error.	/* ulimit -s	*/
         else if index( xLine, "-d)" ) > 0 or xLine begins "data(kbytes)"   then maxDataSeg = trim( entry( 2, xLine, ")" )) no-error.	/* ulimit -d	*/
         else if index( xLine, "-m)" ) > 0 or xLine begins "memory(kbytes)" then maxProcMem = trim( entry( 2, xLine, ")" )) no-error.	/* ulimit -m	*/

/***
 *      if       inLine[1] = "file"  and inLine[2] = "size"   then maxFileSz  = inLine[5].	/* ulimit -f	*/
 *       else if inLine[1] = "open"  and inLine[2] = "files"  then maxOpenFl  = inLine[4].	/* ulimit -n	*/
 *       else if inLine[1] = "max"   and inLine[2] = "memory" then maxProcMem = inLine[6].	/* ulimit -m	*/
 *       else if inLine[1] = "max"   and inLine[2] = "user"   then maxProcs   = inLine[5].	/* ulimit -u	*/
 *       else if inLine[1] = "stack" and inLine[2] = "size"   then maxStack   = inLine[5].	/* ulimit -s	*/
 *       else if inLine[1] = "data"  and inLine[2] = "seg"    then maxDataSeg = inLine[6].	/* ulimit -d	*/
 *       /*
 *       else if inLine[1] = "max"   and inLine[2] = "user"   then maxProcs   = inLine[5].	/* ulimit -	*/
 *       else if inLine[1] = "max"   and inLine[2] = "user"   then maxProcs   = inLine[5].	/* ulimit -	*/
 *        */
 ***/

      end.
      input stream inStrm close.

    end.

  if       osName = "Linux" then run getLinux.
   else if osName = "AIX"   then run getAIX.
   else if osName = "SunOS" then run getSunOS.
   else if osName = "HPUX"  then run getHPUX.
   else if opsys  = "WIN32" then run getWIN32.

  return.

end.

