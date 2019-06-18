/*******************************************************************************
 *******************************************************************************
 **                                                                           **
 **                                                                           **
 **  Copyright 2003-2006 Tom Bascom, Greenfield Technologies                  **
 **  http://www.greenfieldtech.com                                            **
 **                                                                           **
 **  ProTop is free software; you can redistribute it and/or modify it        **
 **  under the terms of the GNU General Public License (GPL) as published     **
 **  by the Free Software Foundation; either version 2 of the License, or     **
 **  at your option) any later version.                                       **
 **                                                                           **
 **  ProTop is distributed in the hope that it will be useful, but WITHOUT    **
 **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or    **
 **  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License     **
 **  for more details.                                                        **
 **                                                                           **
 **  See TERMS.TXT for more information regarding the Terms and Conditions    **
 **  of use and alternative licensing options for this software.              **
 **                                                                           **
 **  A copy of the GPL is in GPL.TXT which was provided with this package.    **
 **                                                                           **
 **  See http://www.fsf.org for more information about the GPL.               **
 **                                                                           **
 **                                                                           **
 *******************************************************************************
 *******************************************************************************
 *
 * protoplib.p
 *
 * Library of common infrastructure functions for the ProTop family of programs
 *
 *
 * Known Bugs & Issues:
 *
 *
 * To Do:
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	August 28, 2003
 *
 *
 * History:
 *
 *	Accepted changes from Sam Paakki regarding "quit" function and PROPATH
 *	September 26, 2003
 *
 *      Accepted changes from Patrick Tingen to set html output dir,   
 *      eliminating the nasty curr-page shared variable, simplifying the
 *	release# and adding protop-url 
 *      October 30, 2003
 * 
 */

{lib/protop.i}

/* {lib/windows.i} */

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

return.


/** Common functions
 **
 **/


/* Thanks to Wim van der Ham -- the very first community contribution!
 *
 * Now directly supported in 4gl but I keep it around for the memories :)
 *
 *
 *
 * function LOGICAL returns logical ( input p_text as character ):
 *   return ( p_text = "true" or p_text = "yes" ).
 * end.
 *
 */

/* Thanks to Sam Paakki!
 *
 * The built-in search() won't find a directory on the propath -- this does (it also finds ordinary files)
 *
 */

function searchDir returns character ( input xDir as character ):

  define variable i as integer no-undo.

  do i = 1 to num-entries( PROPATH ):
    file-info:file-name = entry( i, PROPATH ) + '/' + xDir.
    if file-info:full-pathname <> ? then return file-info:full-pathname.
  end.

  return ?.	/* didn't find it :-(	*/

end.

/* This one's for you Pete!
 *
 * "Just do it!"
 *
 */

function uDateTime returns integer:			/* BTW -- it doesn't account for leap seconds... */
  return ((( today - 1/1/1970 ) * 86400 ) + time ).
end.

function string2uDateTime returns integer( input p_text as character ):

  /* i.e. "Thu Oct  4 21:05:02 2007" */

  return
    (( date(
      lookup(  substring( p_text,  5, 3 ), "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec" ),
      integer( substring( p_text,  9, 2 )),
      integer( substring( p_text, 21, 4 ))
      ) - 1/1/1970 ) * 86400 ) +

    (
      integer( substring( p_text, 12, 2 )) * 3600 +
      integer( substring( p_text, 15, 2 )) * 60   +
      integer( substring( p_text, 18, 2 ))
    ).

end.

function tmText2uDateTime returns integer( input p_text as character ):

  define variable tmText as character no-undo.
  define variable numDay as integer   no-undo.
  define variable numHr  as integer   no-undo.
  define variable numMin as integer   no-undo.
  define variable numSec as integer   no-undo.

  assign
    tmText = trim( p_text )
    numDay = 0
    numHr  = 0
    numMin = 0
    numSec = 0
  .
  if num-entries( tmText, "d" ) > 1 then
    assign
      numDay = integer( entry( 1, tmText, "d" ))
      tmText = trim( entry( 2, tmText, "d" ))
    .

  if num-entries( tmText, ":" ) > 1 then
    assign
      numHr  = integer( entry( 1, tmText, ":" ))
      numMin = integer( entry( 2, tmText, ":" ))
      numSec = integer( entry( 3, tmText, ":" ))
    no-error.

  return ( 86400 * numDay ) + ( 3600 * numHr  ) + ( 60 * numMin ) + numSec.

end.

/* return a corrected decimal on the assumption that the sign bit should not have been flipped
 *
 */

function unsignMe returns decimal ( input s as decimal, i as integer ):

  if s < 0 then s = s + exp( 2, i ).

  return s.

end.

/* Variations on the hit ratio theme -- Gus' method:
 *
 *	http://www.peg.com/lists/dba/history/200209/msg00057.html
 *
 */

define variable warned as logical no-undo.	/*** debugging code ***/

/***
function hr returns decimal ( input lr as integer, input osr as integer, output hr-str as character, output hr as decimal, output mr as decimal ):
 ***/

function hr returns decimal ( input lr as decimal, input osr as decimal, output hr-str as character, output hr as decimal, output mr as decimal ):

  define variable lrx  as decimal no-undo.
  define variable osrx as decimal no-undo.

  define variable hr-top as decimal no-undo.
  define variable hr-bot as decimal no-undo.

  assign
    lrx       = lr
    osrx      = osr
    hr-top    = lrx / osrx
    hr-bot    = 1.0
    hr        = 100.0 * (( lrx - osrx ) / lrx )
    mr        = 100.0 - hr
  .

  if hr < 0 and warned = no then
    do:

	hr = 0.
	warned = yes.

	/** Debugging -- this is very bad!!!
	 **

        display
	   skip(1)
           "  BOGUS HIT RATIO Calculation!!!  " skip
	   skip(1)
           "       lr:" lr     skip
           "      osr:" osr    skip
           "      lrx:" lrx    skip
           "     osrx:" osrx   skip
           "   hr-top:" hr-top skip
           "   hr-bot:" hr-bot skip
           "       hr:" hr     skip
           "       mr:" mr    skip
           skip(1)
           "   Please send this data to:" skip
           "       tom@greenfieldtech.com" skip
           skip(1)
         with no-labels.

	 **
	 **/

    end.

  if hr < 0 then
    do:
      hr = 0.
      hr-str = "".
      return hr.
    end.

  if hr < 1 then		  /* deal with pathologically bad ratios...	*/
    assign
      hr-bot = 1.0 / hr-top
      hr-top = 1.0
    .

  hr-str = string( truncate( hr-top, 0 )) + ":" + string( truncate( hr-bot, 0 )).
  if hr-str = ? then hr-str = "0:0".
  if length( hr-str ) < 10 then hr-str = fill( " ", 10 - length( hr-str )) + hr-str.

  return hr.

end.

function do-SumSample returns logical ( output p_index as integer, output p_time  as integer ):

  define variable r as character no-undo case-sensitive initial "r".
  define variable s as character no-undo case-sensitive initial "s".

  publish "get-RateRaw" ( output r ).		/*** I'm missing something...	***/
  publish "get-SumSample" ( output s ).

  if s = "S" then
    assign
      p_index = 4
      p_time  = xtime.
   else
    assign
      p_index = 5
      p_time  = itime.

  return true.

end.	/* do-SumSample */

/* useful when there is no db conection and thus no _myconnection
 */

procedure GetCurrentProcessId external "kernel32":
  define return parameter pid as long.
end.

function myPID returns character ():

  define variable pid as integer no-undo initial ?.

  if opsys begins "WIN" then
    do:
      run GetCurrentProcessId( output pid ).
    end.
   else		/* calling the libc getpid function would be nice but finding libc.so is a pain... */
    do:
      input stream inStrm through value( "echo $PPID" ).
      import stream inStrm pid.
      input stream inStrm close.
    end.

  return string( pid ).

end.


/* myCustId
 *
 */

procedure myCustId:

  define output parameter custId as character no-undo.

  custId = "".

  file-info:file-name = "etc/custid.cfg".
  if file-info:full-pathname <> ? then
    do:
      input stream inStrm from value( file-info:full-pathname ).
      import stream inStrm unformatted custId.
      input stream inStrm close.
    end.

  return.

end.


/* chkDbgMode
 *
 * check for changes to the debug level
 *
 * i.e.
 *
 *	echo 5 > $TMPDIR/monName.friendlyName.dbg
 *
 */

define variable dbgChgDate   as date        no-undo.
define variable dbgChgTime   as integer     no-undo.

procedure chkDbgMode:

  define input        parameter dbgFileName as character no-undo.
  define input-output parameter dbgMode     as integer   no-undo.

  file-info:file-name = dbgFileName.
  if file-info:full-pathname <> ? and ( file-info:file-mod-date <> dbgChgDate or file-info:file-mod-time <> dbgChgTime ) then
    do:
      assign								/* remember when we last looked!			*/
        dbgChgDate = file-info:file-mod-date
        dbgChgTime = file-info:file-mod-time
      .
      input stream inStrm from value( file-info:full-pathname ).
      import stream inStrm dbgMode.
      input stream inStrm close.
      message {&NOW} "dbgMode:" dbgMode.
    end.

  return.

end.


/* monName
 *
 */

procedure getMonName:

  define input-output parameter monName as character.

  define variable i as integer no-undo.

  do i = 1 to num-entries( session:startup-parameters ):
    if entry( i, session:startup-parameters ) begins "-p " then
      do:

/*      monName = entry( 1, substring( entry( i, session:startup-parameters ), 4 ), "." ). */

        monName = substring( entry( i, session:startup-parameters ), 4 ).		/* remove the "-p "			*/

        leave.

      end.
  end.

  monName = substring( monName, r-index( monName, "/" ) + 1 ).		/* take the part from the last "/" onward		*/
  monName = substring( monName, r-index( monName, "~\" ) + 1 ).

  monName = right-trim( monName, ".p" ).				/* get rid of the ".p"					*/

  return.

end.


procedure mkFlag:

  define input parameter flgFileName as character no-undo.

  define variable osPID     as character no-undo.			/* process id from "ps"					*/
  define variable myPID     as character no-undo.			/* the current process' PID				*/

  define variable flgPID    as character no-undo.			/* process id in .flg file				*/
  define variable flgDate   as character no-undo.			/* the flag file date					*/
  define variable flgTime   as character no-undo.			/* the flag file time					*/

  define variable monName   as character no-undo.			/* monitor name						*/

  define variable ph        as integer   no-undo.			/* process handle					*/
  define variable retVal    as integer   no-undo.

  define variable wmicLine  as character no-undo.			/* wmic output line					*/
  define variable hdrLine   as character no-undo.			/* wmic header line					*/
  define variable posCmdLn  as integer   no-undo.			/* position of the CommandLine field			*/
  define variable posName   as integer   no-undo.			/* position of the Name field				*/
  define variable posPID    as integer   no-undo.			/* position of the ProcessID field			*/

  myPID = myPID().

  file-info:file-name = flgFileName.
  if file-info:full-pathname <> ? then					/* the flag file exists -- but it might be stale	*/
    do:

      input stream inStrm from value( file-info:full-pathname ).	/* what PID created it?					*/
      import stream inStrm flgDate flgTime flgPID.			/* date time PID					*/
      input stream inStrm close.

      flgPID = trim( flgPID ).

      if myPID = flgPID then						/* if the PID in the flag file = my PID...		*/
        do:
          message {&NOW} "WTF?  myPID = flgPID?" flgDate flgTime flgPID "this should not be possible".
          return.
        end.
       else 								/* if the PID in the flag file <> my PID...		*/
        do:								/* then I didn't create it				*/

          if dbgMode >= 2 then message {&NOW} "Flag file exists.  Checking for stale flag file...".

          ph = 0.

          /* check if the indicated PID is still running...
           *... and if it is an _progres process (missing 4 windows)...
           */

          if opsys = "unix" then					/* try to find the monitor that created the .flg file	*/
            do:

              monName = entry( 1, substring( flgFileName, r-index( flgFileName, "/" ) + 1 ), "." ).

              if monName <> "" and monName <> ? then
                monName = "util/" + monName.				/* something useful to grep for				*/
               else
                do:
                  message {&NOW} "monName is [" monName "]".		/* this shouldn't happen...				*/
                  monName = "util/".					/* something useful to grep for				*/
                end.

              /* FYI, "ps -ef" is surprisingly slow on some systems -- 30 secs + on one particular Solaris system...
               */

              input stream inStrm through value( substitute( 'ps -p &1 -f | grep _progres | grep &2', flgPID, monName )).
              repeat:
                osPID = ?.
                import stream inStrm ^ osPID.
                osPID = trim( osPID ).
                if osPID = flgPID and osPID <> ? then
                  do:
                    ph = -1.
                    leave.
                  end.
              end.
              input stream inStrm close.

            end.
           else
            do:

              monName = entry( 1, substring( flgFileName, r-index( flgFileName, "~\" ) + 1 ), "." ).

              if monName <> "" and monName <> ? then
                monName = "util~\" + monName.				/* something useful to grep for				*/
               else
                do:
                  message {&NOW} "monName is [" monName "]".		/* this shouldn't happen...				*/
                  monName = "util~\".					/* something useful to grep for				*/
                end.

              input stream inStrm through value( "wmic process get commandline,name,processid" ).

              /* looking for _progres and monName
               */

              /* this would have been too easy -- but Windows doesn't support /format and "where" everywhere...
               *
               * if monName = "" then
               *   input stream inStrm through value( substitute( "wmic process where ~"commandline like '%_progres%'~" get commandline,name,processid /format:csv" )).
               *  else
               *   input stream inStrm through value( substitute( "wmic process where ~"commandline like '%_progres%'~" get commandline,name,processid /format:csv | findstr -i &1", monName )).
               *
               * repeat:
               *   osPID = ?.
               *   import stream inStrm delimiter "," ^ ^ ^ osPID.	/* 4th field -- CSV format has the computername prefixed to each line... */
               *   osPID = trim( osPID ).
               *   if osPID = flgPID and osPID <> ? then
               *     do:
               *       ph = -1.						/* a non-zero result means that we found it		*/
               *       leave.
               *     end.
               * end.
               *
               */

              import stream inStrm unformatted hdrLine.
              if hdrLine = "" then import stream inStrm unformatted hdrLine.

              if hdrLine <> "" then
                assign
                  posCmdLn = index( hdrLine, "CommandLine" )
                  posName  = index( hdrLine, "Name" )
                  posPID   = index( hdrLine, "ProcessId" )
                .

              repeat:
                import stream inStrm unformatted wmicLine.
                if index( wmicLine, " _progres.exe " ) > 0 and index( wmicLine, " " + flgPID + " " ) > 0 then
                  do:
                    if monName <> "" and index( wmicLine, monName ) < 0 then next.
                    osPID = flgPID.
                    ph = -1.
                    leave.
                  end.
              end.

              input stream inStrm close.

              /* last chance!
               */

              if ph = 0 then
                do:
                  /* 
                   * run OpenProcess( 1024, 0, integer( flgPID ), output ph ).	/* checks if *something* with that PID is running -- but not more, no _progres or friendlyName	check :(	*/
		   * /* need a command line check...				*/
                   * run CloseHandle( ph, output retVal ).
                   */
                end.

	    end.

          if ph <> 0 then
            do:
              if dbgMode >= 2 then
                do:
                  message {&NOW} "Flag file:" flgFileName "exists and is owned by PID:" flgPID.
                  message {&NOW} "It would appear that there is already a monitor running.".
                  message {&NOW} "Cowardly refusing to start another.".
                end.
              quit.
            end.
           else
            do:
              message {&NOW} "Flag file:" flgFileName "exists but appears to be stale.  PID:" flgPID "is not running." osPID ph.
              message {&NOW} "Deleting" flgFileName.
              os-delete value( flgFileName ).				/* just in case this is a mistake -- give the other guy	*/
              pause 30 no-message.					/* a chance to notice and exit...			*/
            end.					

        end.

    end.

  if myPID = ? or myPID = "?" or integer( myPID ) < 2 then
    do:

      message {&NOW} "I don't know my own PID:" myPID.
      message {&NOW} "Refusing to create a bogus flag file.".
      if opsys = "unix" then message {&NOW} "There may be a problem with $PPID -- check bin/protopenv.".
      quit.

    end.
   else
    do:

      /* the flag file did not exist or it was stale, create a new one and carry on.
       */

      message {&NOW} "Creating new flag file:" flgFileName myPID skip.

      output to value( flgFileName ) unbuffered append.
      put unformatted today space(1) string( time, "hh:mm:ss" ) space(1) myPID skip.
      output close.

    end.

  return.

end.


procedure searchCfg:

  define input  parameter cfgRoot   as character no-undo.
  define input  parameter cfgOption as character no-undo.
  define output parameter cfgFile   as character no-undo.

  cfgFile = ?.

  if cfgOption <> "" then
    do:
      cfgFile = substitute( "&1/&2.&3.&4", "etc", cfgRoot, cfgOption, "cfg" ).
      if dbgMode >= 5 then message {&NOW} "looking for:" cfgFile.
      file-info:file-name = cfgFile.
      if file-info:full-pathname <> ? then
        do:
          if dbgMode >= 4 then message {&NOW} "xfound:" cfgFile.
        end.
      cfgFile = file-info:full-pathname.
    end.

  return.

end.


procedure findCfgName:

  define input parameter cfgRoot as character no-undo.
  define input-output parameter cfgFile as character no-undo.

  define variable custId as character no-undo.

  run myCustId( output custId  ).

  /* from most specific to least specific look for:
   *
   * 1) etc/cfgRoot.pt_shortName.cfg
   * 2) etc/cfgRoot.pt_server.pt_resrcType.cfg
   * 3) etc/cfgRoot.custId.pt_resrcType.cfg
   * 4) etc/cfgRoot.pt_resrcType.cfg
   * 5) etc/cfgRoot.pt_server.cfg
   * 6) etc/cfgRoot.custId.cfg
   * 7) etc/cfgRoot.cfg
   *
   */

  run searchCfg( cfgRoot, pt_shortName, output cfgFile ).
  if cfgFile <> ? then return.

  if pt_resrcType <> "" then
    do:

      run searchCfg( cfgRoot, pt_server + "." + pt_resrcType, output cfgFile ).
      if cfgFile <> ? then return.

      run searchCfg( cfgRoot, custId + "." + pt_resrcType, output cfgFile ).
      if cfgFile <> ? then return.

      run searchCfg( cfgRoot, pt_resrcType, output cfgFile ).
      if cfgFile <> ? then return.

    end.

  run searchCfg( cfgRoot, pt_server, output cfgFile ).
  if cfgFile <> ? then return.

  run searchCfg( cfgRoot, custId, output cfgFile ).
  if cfgFile <> ? then return.

  /* naked -- no cfgOption
   */

  cfgFile = substitute( "&1/&2.&4",    "etc", cfgRoot, "", "cfg" ).
  if dbgMode >= 5 then message {&NOW} "looking for:" cfgFile.
  file-info:file-name = cfgFile.
  if file-info:full-pathname <> ? then
    do:
      if dbgMode >= 4 then message {&NOW} "found:" cfgFile.
      return.
    end.

end.


/* end protoplib.p */
