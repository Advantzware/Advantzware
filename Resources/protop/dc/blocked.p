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
 * blocked.p
 *
 *
 * Blocked clients.
 *
 *
 * To Do:
 *
 *	Handle other sorts of blocked clients
 *	Ought to break up "blkstr" into it's components
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 16, 2003
 *
 */

{lib/protop.i}

define output parameter dcDescription as character no-undo initial "Blocked".

&IF DEFINED( OE11 ) &THEN
  &GLOBAL-DEFINE NWIDTH 74
&ELSE
  &GLOBAL-DEFINE NWIDTH 87
&ENDIF

define temp-table tt_blocked no-undo
  field xid       as integer
  field xvalid    as logical
  field usrnum    as integer   format ">>>>9"     label "Usr"
&IF DEFINED( OE11 ) &THEN
  field tId       as integer   format "->>>>9"    label "Tenant"
&ENDIF
  field userName  as character format "x(15)"     label "Name"
  field userPID   as character format "x(8)"      label "PID"
  field userFlags as character format "x(5)"      label "Flags"
  field blkDura   as character format "x(8)"      label "Duration"
  field blkWait   as character format "x(8)"      label "Wait"
  field blkResrc  as character format "x(12)"     label "Resrc Id"
  field blkNote   as character format "x({&NWIDTH})" label /* "Note" */ "Table Blocker-Usr#:Device:PID Blocker-StatementCache WaitList"
  field blkBy     as integer   format ">>>>9"     label "BlkBy"      {&NOSERIALIZE}
  field xtime     as integer   format ">>>>>>>>9" label "Blkd Since" {&NOSERIALIZE}
  field ztime     as integer   format ">>>>>>>>9" label "Duration"   {&NOSERIALIZE}

  index ztime-idx   ztime
  index xid-idx     is unique xid
  index usrnum-idx  is unique usrnum
  index blkDura-idx is primary blkDura descending

&IF DEFINED( OE11 ) &THEN
  index tId-idx   tId
&ENDIF

.

{lib/dumpTT.i tt_blocked}

define temp-table tt_blocker no-undo
  field xid      as integer
  field xvalid   as logical
  field stackNum as integer   format ">>>>>>>9"  label "Depth"
  field lineNum  as integer   format ">>>>>>>9"  label "Line#"
  field procName as character format "x(137)"    label "Procedure"
  index stackNum-idx is primary /* usrNum */ stackNum ascending
  index xid-idx     is unique xid
.

{lib/dumpTT.i tt_blocker}

define temp-table tt_blocker_Info no-undo
  field infoString as character
  index infoString-idx is primary unique infoString
.

{lib/dumpTT.i tt_blocker_Info}

define temp-table tt_lock no-undo like _lock
  field qflag as logical
  index id-idx is unique primary _lock-id
  index recid-idx _lock-recid
  index usr-idx _lock-usr
  index tbl-idx _lock-table
.

define new global shared variable dbgMode  as integer no-undo initial 1.	/* 1 = errors, 5 = info, 9 = response to file	*/
define new global shared variable rowLimit as integer no-undo.


function getUserInfo returns character ( usrNum as integer, x as integer ):

  define buffer xConnect for _Connect.

  define variable ci as character no-undo.
  define variable cl as integer   no-undo.

  define variable blkInfo    as character no-undo.
  define variable xCacheInfo as character no-undo.

  find xConnect no-lock where _Connect-id = usrNum + 1 no-error.

  if available xConnect then
    do:
      blkInfo = "::".

&IF {&CONNECTX} = "yes" &THEN

      ci =  buffer xconnect:handle:buffer-field( "_connect-cacheInfo" ):buffer-value( 1 ) no-error.
      cl =  buffer xconnect:handle:buffer-field( "_connect-cacheLineNumber" ):buffer-value( 1 ) no-error.

      blkInfo = substitute( "&1:&2:&3 &4 ", _Connect-usr, _Connect-device, _Connect-PID, cl ).
      if ci <> ? then
        do:
          xCacheInfo = ci.
          /* xCacheInfo = entry( num-entries( xCacheInfo, " " ), xCacheInfo, " " ). */
          if length( blkInfo + xCacheInfo ) >= x then
            xCacheInfo = ">" + substring( xCacheInfo, length( xCacheInfo ) - ( x - length( blkInfo )) + 1 ).
          blkInfo = blkInfo + xCacheInfo.
        end.

&ELSE

      blkInfo = substitute( "&1:&2:&3", _Connect-usr, _Connect-device, _Connect-PID ).

&ENDIF

    end.

  return blkInfo.

end.

function snapshotLock returns logical ():

  define variable i       as integer   no-undo.
  define variable j       as integer   no-undo.
  define variable k       as integer   no-undo.
  define variable estart  as integer   no-undo.
  define variable lkscan  as integer   no-undo.
  define variable foundIt as logical   no-undo.
  define variable xline   as character no-undo extent 16.

  empty temp-table tt_Lock.
  lkscan = 0.

  find dictdb._DbStatus no-lock.

  if {&FASTLOCK} then
    do:

      estart = etime.

/***
      for each dictdb._Lock
        while _Lock._Lock-usr <> ?
          and _Lock._Lock-recid <> ?
          and lkScan < _DbStatus._DbStatus-NumLocks
          and lkScan < pt_lktbllim					/* user defined limit					*/
          and ( estart - etime ) < 5000:				/* hard-code for now, but it should be user defined	*/
 ***/

      /* 11.4+ have a much improved _Lock VST
       */

      for each dictdb._Lock where _Lock._Lock-usr <> ? and _Lock._Lock-recid <> ?:

        create tt_lock.
        buffer-copy _lock to tt_lock no-error.
        if index( _Lock._Lock-flags, "Q" ) > 0 then tt_lock.qflag = yes.
        lkscan = lkscan + 1.

        if ( etime - estart ) > 500 and j = 0 and session:batch = no then		/* if the snapshot is fast don't bother the user	*/
          do:
            j = 1.
            message {&NOW} "Making snapshot of _Lock...".
          end.

      end.

      if ( dbgMode > 3 or session:batch = no ) and ( /* lkScan >= pt_lktbllim or */ ( etime - estart ) >= 5000 ) then
        message {&NOW} "_Lock scan is too slow, abandonded, scan:" lkScan "  etime:" ( etime - estart ).

    end.
   else if _DbStatus._DbStatus-NumLocks < 5000 and session:batch = no then
    do:

      if opsys begins "win" then
        input stream inStrm through value( "promon " + pdbname( 1 ) + " < etc/promon.dump_locks 2> /nul" ).
       else
        input stream inStrm through value( "promon " + pdbname( 1 ) + " < etc/promon.dump_locks 2> /dev/null" ).

      repeat:

        xline = "".

        import stream inStrm xline.

        if xline[1] = "" then
          next.
         else if foundIt = yes and xline[1] = "OpenEdge" and xline[2] = "Monitor" and xline[3] = "Session" and xline[4] = "End." then
          leave.

        if j = 1 then	/* oe10	*/
          do:

            if xline[4] <> "REC" then next.

            i = i + 1.

            create tt_lock.
            assign
              tt_lock._lock-id    = i
              tt_lock._lock-usr   = integer( xline[1] )
              tt_lock._lock-recid = {&BIGINT}(   xline[6] )
              tt_lock._lock-table = integer( xline[5] )
            .

            do k = 7 to 16:
              if index( xline[k], "Q" ) > 0 then tt_lock.qflag = yes.
              if tt_lock.qflag then leave.
            end.

          end.
         else if j = 2 then	/* oe11 */
          do:

            if xline[5] <> "REC" then next.

            i = i + 1.

            create tt_lock.
            assign
              tt_lock._lock-id    = i
              tt_lock._lock-usr   = integer( xline[1] )
              tt_lock._lock-recid = {&BIGINT}(   xline[7] )
              tt_lock._lock-table = integer( entry( 1, xline[6], ":" ))
            .

            do k = 8 to 16:
              if index( xline[k], "Q" ) > 0 then tt_lock.qflag = yes.
              if tt_lock.qflag then leave.
            end.

          end.

        if       xline[1] = "Usr"     and xline[2] = "Name" then j = 1.
         else if xline[1] = "Usr:Ten" and xline[2] = "Name" then j = 2.

      end.

      input stream inStrm close.

    end.

  if false then
    do:
      output to value( "log/locks.txt" ).
      message {&NOW} i "locks found in promon".
      for each tt_lock:
        display tt_lock.
      end.
      output close.
    end.

  return true.

end.

procedure mon-init:

  empty temp-table tt_blocked.

  return.

end.

/* update
 *
 */

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable u_name  as character no-undo.
  define variable u_flags as character no-undo.
  define variable u_pid   as character no-undo.
  define variable u_csc   as character no-undo.
  define variable bstr    as character no-undo.

  define variable xusr    as character no-undo.
  define variable uInfo   as character no-undo.

  define variable lkScan  as integer   no-undo.
  define variable i       as integer   no-undo.
  define variable j       as integer   no-undo.
  define variable t       as integer   no-undo.

  define variable ci as character no-undo.
  define variable cl as integer   no-undo.

  for each tt_blocked:
    tt_blocked.xvalid = no.
  end.

  empty temp-table tt_blocker.

  empty temp-table tt_Lock.
  lkscan = 0.

  /* get a list of tt_blocked sessions
   *
   */

  for each dictdb._Connect no-lock
    where _Connect-usr <> ?
      and _Connect-wait <> " -- ":					/* not perfect -- but works for record locks 		 */

    i = i + 1.

    if i = 1 then snapshotLock().			/* there is at least one blocked user... grab a snapshot of _Lock	*/

    find tt_blocked where tt_blocked.usrnum = _Connect-usr no-error.

    if not available tt_blocked then
      do:
        create tt_blocked.
        assign
          tt_blocked.userPID   = string( _connect-PID )			/* UDF has to be before indexed fields in old releases	*/
          tt_blocked.userFlags = connectFlags( _Connect-Id )
          tt_blocked.userName  = connectName( _Connect-Id, tt_blocked.userFlags )
          tt_blocked.xid      = _Connect-usr 
          tt_blocked.usrnum   = _Connect-usr 
          tt_blocked.xtime    = time
       .
      end.

    /* if wait1 has changed it is a new event! 
     */

    if tt_blocked.blkResrc <> string( _Connect-wait1 ) then
      tt_blocked.xtime = time.

    /*** bstr = get-blocked( _Connect-usr ). ***/		/* build a description of the reason	*/

    assign
      tt_blocked.xvalid    = yes
      tt_blocked.ztime     = time - tt_blocked.xtime
/*    tt_blocked.blkSince  = string( tt_blocked.xtime, "hh:mm:ss" ) */
      tt_blocked.blkDura   = string( tt_blocked.ztime, "hh:mm:ss" )
      tt_blocked.blkWait   = _Connect-Wait
      tt_blocked.blkResrc  = string( _Connect-wait1 )
      tt_blocked.blkBy     = ?
    .

    /* use the snapshot of _Lock that was made above -- _Lock is not 100% reliable and records may be missing etc.
     *
     * RECIDs are not unique -- so the FIND /could/ find more than one lock meeting the criteria and thus be unable to
     * determine what we are actually waiting for :( and who else might be holding it.
     *
     * "FIRST" would "fix" that in that the query would succeed but that would be misleading (at best) or just plain wrong.
     *
     * IF we find a unique record (available = true) then we can be reasonably confident that it is the one that the user
     * is waiting for so we show the table name and proceed to find out more about the holder of the record lock with getUserInfo() 
     *
     */

    find /* first */ tt_lock where tt_lock._lock-usr = _connect-usr and tt_lock._lock-recid = _connect-wait1 no-error.
    if available tt_lock then
      do:

        t = tt_lock._lock-table.

        find tt_tbl no-lock where tt_tbl.xid = t no-error.

        assign
          tt_blocked.blkWait = _Connect-Wait + " " + ( if tt_lock._lock-flags = ? then "" else replace( tt_lock._lock-flags, " ", "" ))
          tt_blocked.blkNote = ( if available tt_tbl then tt_tbl.tblName else substitute( "[&1]", t ))
        .

        for each tt_lock
           where tt_lock._lock-table = t
             and tt_lock.qflag       = no			/* not a queued lock */
             and tt_lock._lock-recid = _connect-wait1
             and tt_lock._lock-usr  <> _connect-usr:

          uInfo = getUserInfo( tt_lock._lock-usr, {&NWIDTH} - length( tt_blocked.blkNote )).
          if not( uInfo begins  ":" ) then tt_blocked.blkBy = integer( entry( 1, uInfo, ":" )).
          tt_blocked.blkNote = tt_blocked.blkNote + " " + uInfo.

        end.    

        /* for the queued users we just want a list...
         */

        j = 0.

        for each tt_lock
           where tt_lock._lock-table = t
             and tt_lock.qflag       = yes
             and tt_lock._lock-recid = _connect-wait1
             and tt_lock._lock-usr  <> _connect-usr:

          xusr = string( tt_lock._lock-usr ).

          tt_blocked.blkNote = tt_blocked.blkNote + " " + xusr.

          j = j + 1.
          if j > 5 then leave.

        end.    


      end.

    if ( rowLimit > 0 ) and ( i >= rowLimit ) then leave.

  end.

  for each tt_blocked where tt_blocked.xvalid = no:
    delete tt_blocked.
  end.

  /* just capture the oldest duration /* blocker */ blocked user -- the *blocker* is shown in the "blocker-statementCache"
   */

  for each tt_blocked by ztime:
    leave.
  end.

/***

  if available tt_blocked and tt_blocked.blkBy <> ? then
    do:
      find _connect no-lock where ( _connect-id = /* tt_blocked.blkBy */ tt_blocked.usrNum + 1 ) and _connect-usr <> ? no-error.
      empty temp-table tt_blocker_Info.
      create  tt_blocker_Info.
      tt_blocker_Info.infoString = substitute( "Oldest Blocked (usr: &1, &2) Stack Trace", _connect-usr, _connect-name ).

&IF {&CONNECTX} = "yes" &THEN

     getStack: do i = 1 to 32:

        assign
          ci = ?
          ci =  buffer _connect:handle:buffer-field( "_connect-cacheInfo" ):buffer-value( i )
          cl =  buffer _connect:handle:buffer-field( "_connect-cacheLineNumber" ):buffer-value( i )
        no-error.

        if ci = ? then leave getStack.
        create tt_blocker.
        assign
          tt_blocker.xid      = i
          tt_blocker.stackNum = i
          tt_blocker.lineNum  = cl
          tt_blocker.procName = ci
        .      
      end.

&ENDIF

    end.

 ***/

  add2ds( temp-table tt_blocked:default-buffer-handle ).

/***
  add2ds( temp-table tt_blocker:default-buffer-handle ).
  add2ds( temp-table tt_blocker_Info:default-buffer-handle ).
 ***/

  return.

end.

{ssg/blocked.i}

return.
