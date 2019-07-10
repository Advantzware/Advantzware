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
 * vstlib.p
 *
 * Functions that deal with VSTs
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

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

define variable i as integer no-undo.

define variable hasClstrSz as logical no-undo.
define variable dbRecid    as recid   no-undo.

find first dictdb._DB no-lock.
dbRecid = recid( _DB ).

hasClstrSz = no.
find dictdb._File  no-lock where _File-Name = "_Area".
find dictdb._Field no-lock where _Field._File-recid = recid( _File ) and _Field-Name = "_Area-ClusterSize" no-error.
if available _Field then hasClstrSz = yes.

/*** initialize tt cache for various schema tables
 ***
 ***/

empty temp-table tt_tbl.
empty temp-table tt_idx.
empty temp-table tt_areaExtent.
empty temp-table tt_area.

for each dictdb._File no-lock:

  find first tt_tbl where tt_tbl.tblName = _file._file-name no-error.
  if available tt_tbl then next.						/* don't build the caches twice...		*/

  find dictdb._storageObject no-lock 
    where _storageObject._Db-recid = dbRecid
      and _Object-type   = 1
      and _Object-number = _file._file-num no-error.

  find _area no-lock where _area._area-number = _storageObject._Area-Number no-error.

  create tt_tbl.
  assign
    tt_tbl.xid = _File._File-num
    tt_tbl.areaNum = _storageObject._Area-Number when available _storageObject
    tt_tbl.tblPool = "b1"	/* default */
    tt_tbl.tblPool = ( if max( get-bits( _object-attrib, 7, 1 ), get-bits( _area-attrib, 7, 1 )) = 0 then "B1" else "B2" ) when available _storageObject
    tt_tbl.tblname = _File._File-name
  .

  for each dictdb._Index no-lock of dictdb._File:

    find _storageObject no-lock
      where _storageObject._Db-recid = dbRecid
        and _Object-type   = 2
        and _Object-number = _index._idx-num no-error.

    find _area no-lock where _area._area-number = _storageObject._Area-Number no-error.

    create tt_idx.
    assign
      tt_idx.xid      = _Index._Idx-num
      tt_idx.idxname  = _Index._Index-name
      tt_idx.tblnum   = _File._File-num
      tt_idx.tblname  = _File._File-name
      tt_idx.idxnote  = 
        ( if dictdb._file._prime-index = recid( _index ) then "P" else "" ) +
        ( if _index._unique then "U" else "" )
      tt_idx.idxRoot  = {&BIGINT}( _Object-root ) when available _storageObject
      tt_idx.areaNum  = {&BIGINT}( _storageObject._Area-Number ) when available _storageObject
      tt_idx.idxPool = "b1"	/* default */
      tt_idx.idxPool =  ( if max( get-bits( _object-attrib, 7, 1 ), get-bits( _area-attrib, 7, 1 )) = 0 then "B1" else "B2" ) when available _storageObject
    .

  end.

end.

for each dictdb._areaExtent no-lock:
  find tt_areaExtent
    where tt_areaExtent.areaNum = _areaExtent._Area-Number and tt_areaExtent.extNum  = _areaExtent._Extent-Number
    no-error.
  if available tt_areaExtent then next.						/* don't build the caches twice...		*/
  create tt_areaExtent.
  assign
    tt_areaExtent.areaNum = _areaExtent._Area-Number
    tt_areaExtent.extNum  = _areaExtent._Extent-Number
    tt_areaExtent.extSize = _areaExtent._Extent-Size
    tt_areaExtent.extType = _areaExtent._Extent-Type
    tt_areaExtent.extPath = _areaExtent._Extent-Path
  .
end.

for each dictdb._Area no-lock:
  if _Area._Area-number <= 3 or _Area._Area-type = 7 then next.		/* skip control area, bi area and after image areas		*/
  find tt_area where tt_area.SANum = _Area._Area-Number no-error.
  if not available tt_area then
    do:
      find first _AreaStatus no-lock where _AreaStatus-areaNum = _Area._Area-Number.
      create tt_area.
      assign
        tt_area.xid      = _AreaStatus._AreaStatus-Id		/* need an xid field for context	*/
        tt_area.SANum    = _Area._Area-Number
        tt_area.areaPool = "b1"		/* default */
        tt_area.areaPool = ( if get-bits( _area-attrib, 7, 1 ) = 0 then "B1" else "B2" )
        tt_area.SAName   = string( _Area._Area-Number )		/* temporary	*/
      .
    end.
end.

/* end caching schema tables
 *
 */


run lib/parsedba.p persistent.
run lib/getdba.p persistent.


/*** _TableStat-Id mapping to _File-num is "quirky" when _statbase._tablebase <> 1
 ***
 ***/

i = 1.

do while true:
  find _tablestat no-lock where _tablestat-id = i no-error.
  if not available _tablestat then
    leave.
   else
    do:
      /***
      find _file no-lock where _file-num = _tablestat-id no-error.
      display i _tablestat-id with down.
      if available _file then display _file-num _file-name with down.
      down.
       ***/
      find tt_tbl where tt_tbl.xid = _tablestat-id no-error.
      if available tt_tbl then tt_tbl.tstatid = i.	/* NOT _tablestat-id!!!  -- it changes magically */
    end.
  i = i + 1.
end.

/*** so is _IndexStat...
 ***
 ***/

i = 1.

do while true:
  find _indexstat no-lock where _indexstat-id = i no-error.
  if not available _indexstat then
    leave.
   else
    do:
      find tt_idx where tt_idx.xid = _indexstat-id no-error.
      if available tt_idx then tt_idx.istatid = i.	/* NOT _indexstat-id!!!  -- it changes magically */
    end.
  i = i + 1.
end.

return.



/** VST functions
 **
 **/


function connectFlags returns character( cnxId as integer ):

  define variable u_flags as character no-undo.
  define variable xflag   as character no-undo.
  define variable cc      as character no-undo.
  define variable um      as character no-undo.

  find dictdb._Connect no-lock where _Connect-Id = cnxId.

  if       _Connect-type = "self" then
    u_flags = "S".
   else if _Connect-type = "remc" then
    u_flags = "R".
   else if _Connect-type = "brok" then
    u_flags = "L".
   else if _Connect-type = "serv" then
    u_flags = "@".
   else
    u_flags = "O".	/* "Other" -- I'm too lazy to dream up codes right now...	*/

/*** future fun...
 ***
 ***

    when "serv"  then
      do:
        find _Servers no-lock where _Server-id = _Connect-usr + 1 no-error.
        if available _Servers then
          do:
            if _Server-type = "Login" then
              tt_Dashboard.con_brok = tt_Dashboard.con_brok  + 1.
             else
              tt_Dashboard.con_4glServ = tt_Dashboard.con_4glServ + 1.
          end.
      end.

 ***
 ***/

  if lookup( trim( _connect-type ), "APW,BIW,AIW,WDOG" ) > 0 then u_flags = "H".

&IF {&CONNECTX} = "yes" &THEN

  cc =  buffer _connect:handle:buffer-field( "_connect-clientType" ):buffer-value no-error.

  if       cc = "abl" then     u_flags = u_flags + "4".
   else if cc = "wta" then     u_flags = u_flags + "W".
   else if cc = "apsv" then    u_flags = u_flags + "A".
   else if cc = "pasn" then    u_flags = u_flags + "P".
   else if cc begins "SQ" then u_flags = u_flags + "Q".
   else                        u_flags = u_flags + "X".

&ENDIF

  /* why, why is it so difficult to determine if a session is a batch session?
   * why is _connect-batch a /character/ field?!?
   * what is the _connect-device matches "*batch*" logic seen elsewhere all about?
   *
   */

  if _Connect-device = "batch" or _connect-batch = "yes" then
    u_flags = u_flags + "B".
   else
    u_flags = u_flags + " ".

  /* identify the current process as ProTop 3
   */

  find _myconnection no-lock.
  if _myconn-pid = _connect-pid then u_flags = "PT3".

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 11.7 &THEN
  um = "".
  um =  buffer _connect:handle:buffer-field( "_connect-usermisc" ):buffer-value no-error.
  if um begins "pt3:" then
    u_flags = "PT3".
&ENDIF

  xflag = "".
  if _Connect-TransId    > 0 then xflag = xflag + "*".
  if _Connect-Disconnect = 1 then xflag = xflag + "d".
  if _Connect-Resync     = 1 then xflag = xflag + "r".
  if _Connect-Interrupt  = 1 then xflag = xflag + "i".

  return u_flags + xflag.

end.

function connectName returns character( cnxId as integer, cnxFlags as character ):

  define variable u_name  as character no-undo.
  define variable cc      as character no-undo.
  define variable um      as character no-undo.

  find dictdb._Connect no-lock where _Connect-Id = cnxId.

  if cnxFlags begins "L" or cnxFlags begins "O" or cnxFlags begins "H" or cnxFlags begins "@" then
    u_name = trim( substitute( "&1 &2", _connect-name, _connect-type )).
   else
    u_name = _Connect-name.

&IF {&CONNECTX} = "yes" &THEN

  cc =  buffer _connect:handle:buffer-field( "_connect-client" ):buffer-value no-error.

  if cc = "apsv" and u_name = "" then				/* use the server name or IP for app server connections with no name	*/
    u_name = _connect-device.

&ENDIF

  if _connect-type = "remc" and u_name = "" then		/* use the server name or IP for remote connections with no name	*/
    u_name = _connect-device.

  if u_name = "batch" then u_name = "local".

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 11.7 &THEN
  um = "".
  um =  buffer _connect:handle:buffer-field( "_connect-usermisc" ):buffer-value no-error.
  if um begins "pt3:" then
    u_name = trim( substitute( "&1 &2", u_name, entry( 2, um, ":" ))).
&ENDIF

  /* if there is no name, use the usr# (login with blank id)
   *
   * used to be _connect-PID but that is less useful than usr#
   *
   */

  return max( u_name, string( _connect-usr )).

end.

function lastStatement returns character( cnxId as integer, output lineNum as integer, output procName as character ):

  define variable ct as character no-undo.
  define variable ci as character no-undo.
  define variable cl as integer   no-undo.

&IF {&CONNECTX} = "yes" &THEN

  find dictdb._Connect no-lock where _Connect-Id = cnxId.

  ct =  buffer _connect:handle:buffer-field( "_connect-clientType" ):buffer-value no-error.
  ci =  buffer _connect:handle:buffer-field( "_connect-cacheinfo" ):buffer-value( 1 ) no-error.
  cl =  buffer _connect:handle:buffer-field( "_connect-cachelinenumber" ):buffer-value( 1 ) no-error.

  assign
    procName  = ( if ci = ? then "" else ci )
    lineNum   = ( if cl = ? then 0  else cl )
  .

  /* Note: lineNum -1 = "cleanup" at the end of a procedure.	*/

  /* eliminate optional internal procedure name if it is not a SQL connection
   *
   * IPName = trim( entry( 1, _Connect-CacheInfo[1], " " ))
   *
   */

  if not ( ct begins "SQ" ) and num-entries( ci, " " ) > 1 then
    assign
      procName  = trim( entry( 2, ci, " " ))
    .

  return ( if ct begins "SQ" then procName else substitute( "&1 &2", lineNum, procName )).

&ELSE

  return "".

&ENDIF

end.


/* Thanks to George Potemkin!
 *
 */

function aiInfo returns character ( input vAiFile as character, output vAiGenNum as integer ):

  DEFINE VARIABLE vAiBlkSize AS INTEGER   NO-UNDO.

  DEFINE VARIABLE vAiStatus  AS INTEGER   NO-UNDO.
  DEFINE VARIABLE vStatus    AS CHARACTER NO-UNDO.
  DEFINE VARIABLE vRaw       AS RAW       NO-UNDO.

  find first _logging no-lock.
  vAiBlkSize = _logging._logging-aiBlkSize.

  file-info:file-name = vAiFile.

  if file-info:full-pathname <> ? and index( file-info:file-type, "r" ) <> 0  and ( vAiBlkSize > 0 ) then
    do:

      /* Offset Len  Description
       * ------ ---  -----------
       * 24-27    4  AiGenNum      AI extent sequence number (mb_aigennbr, Seqno)
       * 40-43    4  AiStatus      1=Busy, 2=Empty, 4=Full
       *
       * v10 moved everything 65 bytes...
       * ( if integer( dbversion(1)) > 9 then 65 else 0 ).
       */

      INPUT stream inStrm FROM VALUE( vAiFile ).

      ASSIGN LENGTH( vRaw ) = 4.
      SEEK stream inStrm TO 24 + vAiBlkSize + ( if integer( dbversion(1)) > 9 then 64 else 0 ).
      IMPORT  stream inStrm UNFORMATTED vRaw.
      ASSIGN vAiGenNum = GET-LONG( vRaw, 1 ).
            
      SEEK  stream inStrm TO 40 + vAiBlkSize + ( if integer( dbversion(1)) > 9 then 64 else 0 ).
      IMPORT stream inStrm UNFORMATTED vRaw.
      ASSIGN vAiStatus = GET-LONG(vRaw, 1).

      ASSIGN LENGTH(vRaw) = 0.
      INPUT stream inStrm CLOSE.

      vStatus =
        IF       vAiStatus EQ 0 THEN "None"		/* Msg 694	*/
         ELSE IF vAiStatus EQ 1 THEN "Busy"		/* Msg 3792	*/
         ELSE IF vAiStatus EQ 2 THEN "Empty"		/* Msg 3793	*/
         ELSE IF vAiStatus EQ 4 THEN "Full"		/* Msg 3794	*/
         ELSE IF vAiStatus EQ 8 THEN "Locked"		/* ????		*/
         ELSE "Unknown".				/* Msg 737	*/

    end.
   else
    do:
      vStatus = "Can not open".
    end.

  return vstatus.

end.	/* aiInfo */


/* get consolidated info about a storage area
 *
 */

procedure initAreaInfo:

  define variable seqnum     as integer no-undo.

  define variable area_bh    as handle  no-undo.
  define variable area_bf    as handle  no-undo.

  for each dictdb._areaStatus:

    if    _AreaStatus-AreaName begins "Control Area"
       or _AreaStatus-AreaName begins "Primary Recovery Area"
       or _AreaStatus-AreaName begins "After Image Area" then next.

    find tt_area where tt_area.SANum = _AreaStatus-AreaNum.

    find dictdb._Area no-lock where _Area._Area-number = tt_area.SANum.

    assign
      tt_area.SAName        = _AreaStatus-Areaname
  /*  tt_area.areaStatus-Id = _areaStatus-id */
      tt_area.rpb           = exp( 2, _Area._Area-recbits )			/* thanks to Dmitri Levin!			*/
      tt_area.numExts       = _AreaStatus-Extents
    .

    /* count the number of storage objects (tables & indexes) in this storage area
     */

    assign
      tt_area.numTbls = 0
      tt_area.numIdxs = 0
      tt_area.numLOBs = 0
      tt_area.xnote = ""
    .

    for each _storageobject no-lock where _StorageObject._DB-Recid = dbRecid
         and _storageobject._area-number = tt_area.SANum
         and _storageobject._object-num > 0
         and _storageobject._object-associate > 0:

      if       _storageobject._object-type = 1 then tt_area.numTbls = tt_area.numTbls + 1.
       else if _storageobject._object-type = 2 then tt_area.numIdxs = tt_area.numIdxs + 1.
       else if _storageobject._object-type = 3 then tt_area.numLOBs = tt_area.numLOBs + 1.

    end.

    /* if there are data tables or indexes in the schema area (area 6) note it with a "*"
     */

    if tt_area.SANum = 6 and ( tt_area.numTbls + tt_area.numIdxs + tt_area.numLOBs ) > 0 then tt_area.xnote = tt_area.xnote + "*".

    if _areaStatus-Areaname matches "*After Image Area*" then tt_area.xNote = aiInfo( input _AreaStatus-LastExtent, output seqnum ).

    if hasClstrSz = false then
      tt_area.clstrSz = 0.
     else
      do:
        assign        
          area_bh = buffer _Area:handle
          area_bf = area_bh:buffer-field( "_Area-ClusterSize" )
        no-error.
        tt_area.clstrSz = area_bf:buffer-value.
      end.

    tt_area.blkSzKB = _Area-blocksize / 1024.					/* _area-blocksize is in bytes		*/

  end.

  return.

end.


procedure getAreaVarInfo:

  define variable blksAlloc   as decimal no-undo.
  define variable blksVar     as decimal no-undo.
  define variable blksFree    as decimal no-undo.
  define variable extBlks     as decimal no-undo.
  define variable lastFixedSz as decimal no-undo.

  define variable x as decimal no-undo.

  x = ( 1024 * 1024 ).

  for each tt_area:

    assign
      blksAlloc = 0
      blksVar   = 0
    .

    /* _AreaExtent._Extent-size is in KB, it is the *planned* size from the .st file,
     *  variable extents are reported as zero.
     */

    /* functions in WHERE clauses are not fun so use a pre-processor for modern releases and
     * save the kludge for ancient, obsolete and unsupported releases...
     */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1C" &THEN
    find dictdb._AreaStatus no-lock where _AreaStatus-Id = tt_area.xid.
&ELSE
    find dictdb._AreaStatus no-lock where {&BIGINT}( _AreaStatus-Id ) = tt_area.xid.
&ENDIF

    lastFixedSz = 0.

    for each tt_AreaExtent no-lock where tt_AreaExtent.areaNum = tt_area.SANum:

      extBlks = ( extSize / tt_area.blkSzKB ).
      if extBlks = ? then extBlks = 0.

      if extType >= 4 and extType <= 7 then
        assign
          tt_area.hasVar = yes
          blksVar = max( extBlks, ( _AreaStatus-totblocks - blksAlloc ))
        .
       else
        assign
          lastFixedSz = ( extBlks * tt_area.blkSzKB ) / x
          blksAlloc = blksAlloc + extBlks
        .

    end.

    blksFree = _AreaStatus-Totblocks - _AreaStatus-Hiwater.
    if ( _AreaStatus-Freenum = ? ) then blksFree = blksFree + _AreaStatus-Freenum.

    if blksFree = ? then blksFree = _AreaStatus-totblocks.

    assign
      /* lastFixedSz     = ( extBlks   * tt_area.blkSzKB ) / x */
      tt_area.allocGB = ( blksAlloc * tt_area.blkSzKB ) / x
      tt_area.varGB   = ( blksVar   * tt_area.blkSzKB ) / x
      tt_area.freeGB  = ( blksFree  * tt_area.blkSzKB ) / x
      tt_area.totGB   = tt_area.allocGB + tt_area.varGB
    .

    assign
      tt_area.hiGB     = max( 0, ( tt_area.totGB - tt_area.freeGB ))
      tt_area.pctAlloc = (( tt_area.hiGB / tt_area.allocGB ) * 100 )
    .

    if tt_area.pctAlloc = ? then tt_area.pctAlloc = 0.

    if lastFixedSz <= 0 then
      tt_area.pctLastX = 0.			/* there is only a variable length extent - so pctLastX makes no sense	*/
     else
      do:
        if _AreaStatus-Hiwater >= blksAlloc then
          tt_area.pctLastX = 100.		/* we have overflowed and we are using the variable extent		*/
          else
          do:
            if tt_area.freeGB >= lastFixedSz then
              tt_area.pctLastX = 0.		/* we are not in the last fixed extent yet				*/
             else
              tt_area.pctLastX = ( 1 - ( tt_area.freeGB / lastFixedSz )) * 100.
/* (( /* 1 - */ ( lastFixedSz / ( tt_area.allocGB - tt_area.freeGB ))) * 100 ). */
          end.
      end.

    /* if we are within 80% of the 32/64 bit boundry that kbase 000081166 references:
     *   http://knowledgebase.progress.com/articles/Critical_Alert/Critical-Alert-Index-manager-defect-can-corrupt-large-indexes-where-dbkeys-straddle-32-64-bit-boundary
     */

    /***
    if ( tt_area.numIdxs > 0 ) then
      do:
        message
          "area name:"    tt_area.SAName
          " hwm:"         _areaStatus-hiWater
          " rpb:"         tt_area.rpb
          " hwm/rpb:"     ( _areaStatus-hiWater / tt_area.rpb )
          " (hwm/rpb)/2^31:"     ( _areaStatus-hiWater / tt_area.rpb ) / exp( 2, 31 )
        .
        pause.
      end.
     ***/

    if ( tt_area.numIdxs > 0 ) then
      assign
        idx3264 = (( _areaStatus-hiWater / tt_area.rpb ) / exp( 2, 31 )) * 100
        xNote = xNote + "!" when idx3264 > 80
      .

    /***
    if idx3264 > 50 then
      do:
        message "idx3264" SANum _areaStatus-areaName _areaStatus-hiwater tt_area.rpb idx3264.
        pause.
      end.
     ***/

    /* if a type1 area is approaching the 32 bit limit then its max area size may be an issue
     * t2 areas seem unlikely to get there anytime soon -- but we may as well calculate it
     */

    if /* ( tt_area.clstrSz < 2 ) and */ ( SANum >= 6 ) and (( _areaStatus-Areaname matches "*After Image Area*" ) = no ) then
      assign
/*      areaMaxPct = (( _areaStatus-hiWater / exp( 2, (( if tt_area.clstrsz < 2 then 31 else 63 ) - ( lookup( string( rpb ), "1,2,4,8,16,32,64,128,256" ) - 1 )))) * 100 ) */
        areaMaxPct = (( _areaStatus-hiWater / tt_area.rpb ) / exp( 2, ( if tt_area.clstrsz < 2 then 31 else 63 ))) * 100
        xNote = xNote + "1" when areaMaxPct > 20
      .

    /* my programming license should be revoked for that LOOKUP() stunt	-- if you really must ask it is answering "how many bits?"	*/

    /***
    if areamaxpct > 50 then
      do:
        message "areamaxpct" SANum _areaStatus-areaName _areaStatus-hiwater tt_area.rpb areaMaxPct.
        pause.
      end.
     ***/

  end.

  return.

end.


/* check ai extent status
 *
 */

function chkai returns integer ( output ai_exts as integer, output ai_full as integer, output ai_empty as integer ):

  define variable ai_seq  as integer.
  define variable ai_busy as integer.

  assign
    ai_exts  = 0
    ai_busy  = 0
    ai_full  = 0
    ai_empty = 0
    ai_seq   = 0
  .

  for each _AreaStatus no-lock where ( _areaStatus-Areaname matches "*After Image Area*" ):

    ai_exts = ai_exts + 1.

    case aiInfo( input _AreaStatus-LastExtent, output ai_seq ):
      when "full"  then ai_full  = ai_full  + 1.
      when "empty" then ai_empty = ai_empty + 1.
      when "busy"  then ai_busy  = ai_exts.
    end.

  end.

  return ai_busy.

end.


/* chkptNum
 *
 */

function chkptNum returns integer ( input-output oldbi as integer ):

  /* Adjust to align with checkpoint numbers -- if we can.  _Trans-counter is the
   * checkpoint# since the last "truncate bi".  Unfortunately the only way to map
   * from that number to _BfStatus-LastCkpNum involves starting a transaction in
   * mon-init().  And that is optional since ProTop shouldn't require the user to
   * write to a monitored database.  So if that option isn't enabled (chkp-base = ?)
   * we have to live with the fact that we can't map them.  Which means that we're
   * showing 0 at times when the real active checkpoint is probably something else.
   * That's not as bad as it sounds though because it should only happen when there
   * are no active transactions which means that, while we don't know what the
   * actual current checkpoint # is, we also only have one active checkpoint so
   * there's nothing to be excited about in these two numbers.  (The whole point
   * of this metric is to show growth in the number of active bi clusters.)
   *
   */

  define variable currbi as integer no-undo.

  assign
    oldbi  = 0
    currbi = 0
  .

  /***
  find first dictdb._BuffStatus no-lock no-error.
  if available _BuffStatus then currbi = _BfStatus-LastCkpNum.
  if oldbi = 0 and currbi <> 0 then oldbi = currbi.
   ***/

  for each dictdb._Trans no-lock where _Trans-usrnum <> ? and _Trans-state <> "allocated":
    if _Trans-counter <> ? and _Trans-counter > 0 then
      do:
        currbi = max( currbi, _Trans-counter ).
        if oldbi = 0 or _Trans-counter < oldbi then oldbi = _Trans-counter.
      end.
  end.

  return currbi.

/*
  for each dictdb._Trans no-lock where _Trans-usrnum <> ? and _Trans-state <> "allocated":
    if _Trans-counter <> ? and _Trans-counter > 0 then
      do:
        if oldbi = 0 or _Trans-counter < oldbi then oldbi = _Trans-counter.
        currbi = max( currbi, _Trans-counter ).
      end.
  end.

  find first dictdb._BuffStatus no-lock.

  if chkp-base <> ? then
    do:
      currbi = _BfStatus-LastCkpNum.
      if oldbi = 0 then
        oldbi = currbi.
       else
        oldbi = oldbi - chkp-base.
    end.
   else
    do:
      if oldbi = 0 then oldbi = currbi.
    end.

  if oldbi > currbi then oldbi = currbi.

  return currbi.
 */

end.


/* is after-imaging enabled?
 *
 */

function isAIEnabled returns logical ():

  find dictdb._Logging no-lock.

  return( if _Logging-AIBegin begins "-" then no else yes ).	         /* _Logging-AIJournal is apparently broken :( 		*/

end.


/* is this db a replication source?
 *
 */

function isReplSource returns logical ():

  find first dictdb._repl-server no-lock no-error.

  return available( _repl-server ).

end.


/* is this db a replication target?
 *
 */

function isReplTarget returns logical ():

  find first dictdb._repl-agent no-lock no-error.

  return available( _repl-agent ).

end.


/* check to see if a backup is running
 *
 * also: if the backup completed recently (within the sample interval) consider it to be active and set backupStatus = yes
 */

function isBackupRunning returns logical ():

  define variable backupStatus as logical no-undo initial no.

  define variable currDT  as integer no-undo.
  define variable fBackUp as integer no-undo.
  define variable iBackUp as integer no-undo.

  /* is an online backup running?
   */

  for each dictdb._userStatus no-lock where _userStatus-operation <> ?:
    if _userStatus-operation = "online backup" then backupStatus = yes.
  end.

  /* if the backup completed within the sample interval consider it to be active and set backupStatus = yes
   */

  find dictdb._DbStatus no-lock.

  assign
    currDT =  uDateTime()
    fBackUp = ( currDT - string2uDateTime( _dbStatus-fbDate ))
    iBackUp = ( currDT - string2uDateTime( _dbStatus-ibDate ))
  .

  if fBackup <> ? and fBackup < 300 then backupStatus = yes.
  if iBackup <> ? and iBackup < 300 then backupStatus = yes.

  return backupStatus.

end.


/* is this db a workgroup db?
 *
 */

function isWorkgroup returns logical ():

  return ( integer( getStartUpX( "_Startup-spin", "(-spin)", "" )) <= 1 ).

end.


/* try to get startup parameter values that are missing from VSTs from PROMON
 *
 * see dc/promon.txt for sample output
 *
 */

define variable startProMon  as character /* longchar */ /* no-undo */.

procedure loadProMonStartUp:

  define variable promonCmd  as character no-undo.
  define variable inLine     as character no-undo.

  if opsys = "unix" then
    promonCmd = "$DLC/bin/promon &1 < $PROTOP/etc/promon.startup 2> /dev/null".
   else
    promonCmd = "%DLC%~\bin~\promon &1 < %PROTOP%~\etc~\promon.startup 2> nul".

  startProMon = "".
  input stream inStrm through value( substitute( promonCmd, pdbname( 1 ))).
  repeat:
    import stream inStrm unformatted inLine.
    startProMon = startProMon + inLine + "~n".
  end.
  input stream inStrm close.

  return.

end.


/* gets a startup parameter from promon screen scraping
 *
 * p1 = parameter name in parenthesis
 * p2 = unique text
 *
 */

define variable promonLoaded as logical.

function getProMonParam returns character ( input p1 as character, input p2 as character ):

  define variable v as character no-undo.
  define variable t as character no-undo.

  define variable i as integer   no-undo.
  define variable n as integer   no-undo.

  if promonLoaded = no then
    do:
      run loadProMonStartUp.
      promonLoaded = yes.
    end.

  n = num-entries( startProMon, "~n" ).

  do i = 1 to n:

    t = entry( i, startProMon, "~n" ).

    if num-entries( t, ":" ) < 2 then next.

    if index( t, p1 ) > 0 then								/* try the parameter name first, i.e. "(-Mxs)"	*/
      do:
        v = trim( entry( 2, t, ":" )).
        leave.
      end.
     else if p2 <> "" and index( t, p2 ) > 0 then					/* try message text				*/
      do:
        v = trim( entry( 2, t, ":" )).
        leave.
      end.

  end.

  /*** message substitute( "getProMonParam looking for: [&1] [&2] in &3 lines; found: [&4]", p1, p2, n, v ). pause. ***/

  /* pica is specified in KB but reported in MB in the .lg and in PROMON - and reported in bytes in _dbParams, return bytes
   */

  if index( p1, "pica" ) > 0 then v = string( decimal( v ) * 1024 * 1024 ) no-error.

  return v.

end.


define variable hasDBParams  as logical no-undo.
define variable hasStartup   as logical no-undo.

define variable bdbp as handle no-undo.					/* buffer handle for _dbParams (preferred starting with 11.5)	*/
define variable qdbp as handle no-undo.					/* query handle for _dbParams (preferred starting with 11.5)	*/

define variable bsu  as handle no-undo.					/* buffer handle for _startup (removed in oe12)			*/
define variable qsu  as handle no-undo.					/* query handle for _startup (removed in oe12)			*/

procedure chkStartup:

  hasDBParams   = no.
  hasStartup    = no.

  find dictdb._File no-lock where _File-Name = "_DbParams" no-error.
  if available _File then
    do:
      hasDBParams = yes.
      create buffer bdbp for table "dictdb._DbParams".
      create query qdbp.
      qdbp:set-buffers( bdbp ).
    end.

  find dictdb._File no-lock where _File-Name = "_Startup" no-error.
  if available _File then
    do:
      hasStartup = yes.
      create buffer bsu for table "dictdb._Startup".
      create query qsu.
      qsu:set-buffers( bsu ).
    end.

  return.

end.


/* get db startup parameters - try _dbParams 1st, _startup 2nd, and screen scraping last
 *
 * tt_configuration.excessSHM = integer( getStartUpX( "_Startup-MemOverflow", "(-Mxs)", "Excess shared memory" )) no-error.
 *
 * v  = _startUp field name
 * p1 = parameter name in parenthesis
 * p2 = unique text to find in .lg file or promon output
 *
 */

function getStartupX returns character ( input v as character, input p1 as character, input p2 as character ):

  define variable s as character no-undo.
  define variable p as character no-undo.
  define variable x as decimal   no-undo.
  define variable f as decimal   no-undo.

  define variable p1x as character no-undo case-sensitive.

  p1x = p1.

  /* if we have _dbParams go for that first!
   */

  if s = "" and hasDbParams = yes then
    do:

      qdbp:query-prepare( substitute( 'preselect each _DbParams where _DbParams-name = "&1"', trim( p1x, "()" ))).
      qdbp:query-open.
      if qdbp:get-next( no-lock ) then
        do:
          s = string( bdbp:buffer-field( "_dbParams-Value" ):buffer-value ) no-error.
        end.
      qdbp:query-close.

      /*** message substitute( "looking for: [&1] in _dbParams; found [&2]", trim( p1x, "()" ), s ). pause. ***/

    end.

  /* use _startup if it is available
   */

  if s = "" and hasStartup then
    do:
      qsu:query-prepare( 'for each _Startup' ).
      qsu:query-open.
      if qsu:get-first( no-lock ) then
        do:
          s = string( bsu:buffer-field( v ):buffer-value ) no-error.
        end.
      qsu:query-close.
    end.

  /* screen scraping -- last resort! :(
   */

  if s = "" then
    do:  

      /*** message substitute( "did not find: [&1] [&2] [&3]", v, p1x, p2 ). pause. ***/

      /* try promon output
       */

      s = trim( getProMonParam( p1x, p2 )).

      p = s.
      s = entry( 1, s, " " ).

      if num-entries( p, " " ) = 2 then
        do:
          if       entry( 2, p, " " ) begins "KB" then f = 1024.
           else if entry( 2, p, " " ) begins "MB" then f = 1024 * 1024.
           else if entry( 2, p, " " ) begins "GB" then f = 1024 * 1024 * 1024.
           else if entry( 2, p, " " ) begins "TB" then f = 1024 * 1024 * 1024 * 1024.
          if f > 0 then
            do:
              x = ?.
              x = decimal( s ) no-error.
              if x <> ? then s = string( x * f ).
            end.
        end.

    end.

  s = trim( s ).

  if s = "" or s = ? then s = "N/A".

  return s.

end.


/* end vstlib.p */
