/*******************************************************************************
 *******************************************************************************
 **                                                                           **
 **                                                                           **
 **  Copyright 2003-2009 Tom Bascom, Greenfield Technologies                  **
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
 * userinfo.p
 *
 */

{lib/protop.i}
{lib/tick.i}

define output parameter dcDescription as character no-undo initial "UserInformation".

define variable hasOSRd    as logical   no-undo.

{lib/tt_xstat.i}
{lib/tt_utable.i}
{lib/tt_uindex.i}

define temp-table tt_sstat no-undo                                      /* tt to sample statistics                      */
  field xId          as integer                                         /* metric id                                    */
  field sum-stat     as decimal extent 5 format ">>>>>>9"               /* init, prev, curr, cumulative & interval      */
  index xid-idx is unique primary xId
.

define temp-table tt_UserInfo no-undo
  field usrNum      as integer   format ">>>9"
  field usrId       as integer   format ">>>9"
  field usrName     as character format "x(30)"
  field usrFullName as character format "x(30)"
  field usrPhone    as character format "x(30)"
  field usrEMail    as character format "x(30)"
  field usrIPAddr   as character format "x(30)"
  field usrPID      as integer   format ">>>>>>>>9"
  field usrServ     as integer   format ">>>>>>>>>"
  field usrLoginTm  as character format "x(24)"
  field usrDBA      as decimal   format ">>>>>>>>9"
  field usrDBR      as decimal   format ">>>>>>>>9"
  field usrHR       as decimal   format ">>9.99%"
  field usrDBW      as decimal   format ">>>>>>>>9"
  field usrBIRd     as decimal   format ">>>>>>>>9"
  field usrBIWr     as decimal   format ">>>>>>>>9"
  field usrAIRd     as decimal   format ">>>>>>>>9"
  field usrAIWr     as decimal   format ">>>>>>>>9"
  field usrRecLks   as integer   format ">>>>>>>>9"
  field SessionInfo as character format "x(30)"
  field TrxInfo     as character format "x(30)"

  index usrNum-idx  is unique primary usrNum
.

{lib/dumpTT.i tt_UserInfo}

define temp-table tt_uOther no-undo
  field xid         as integer   format "->>>>9"
  field usrNum      as integer   format ">>>>9"      label "USR#"
  field usrPID      as integer   format ">>>>>>>9"   label "PID"
  field usrType     as character format "x(5)"       label "Flags"
  field usrServ     as integer   format ">>>>>9"     label "Server"
  field usrIPAddr   as character format "x(16)"      label "Device/IP Address"
  field usrLoginTm  as character format "x(15)"      label "Login Time"
/*
  field usrDBA      as decimal   format ">>>>>>>>9"  label "Blk Access"
  field usrDBR      as decimal   format ">>>>>>>>9"  label "OS Reads"
  field usrDBW      as decimal   format ">>>>>>>>9"  label "OS Writes"
  field usrHR       as decimal   format ">>9.99%"    label "Hit%"
 */
  field lineNum      as integer   format "->>>>9"    label "Line#"
  field procName     as character format "x(86)"     label "Program Name"
/*
  field IPName       as character format "x(20)"     label "Routine (IP)"
 */
  index usrNum-idx  is unique primary usrNum
  index xid-idx     is unique xid
.

/*
define temp-table tt_tblAct no-undo
  field xid        as integer   format "->>>>9"
  field tblNum     as integer   format "->>>>9"          label "Tbl#"
  field tblAreaNum as integer   format ">>>>9"           label "Area#"
  field tblName    as character format "x(30)"           label "Table Name"
  field dbaRM      as decimal   format ">>>>>>>>>>>>"    label "RM Chain"
  field dbaRecs    as decimal   format ">>>>>>>>>>>>"    label "#Records"
  field dbaFragPct as decimal   format ">>>>>9.99%"      label "Frag%"
  field dbaScatter as decimal   format ">>>>"            label "Scat"
  field tblTurn    as decimal   format ">>>>>>9.99"      label "Turns"
  field dbaAvgRow  as decimal   format ">>>>>>>"         label "AvgRow"
  field tblCr      as decimal   format ">>>>>>>>9"       label "Create"
  field tblRd      as decimal   format ">>>>>>>>>9"      label "Read"
  field tblUp      as decimal   format ">>>>>>>>9"       label "Update"
  field tblDl      as decimal   format ">>>>>>>>9"       label "Delete"
  field tblOSRd    as decimal   format ">>>>>>>>9"       label "OS Read"

 */

define temp-table tt_uTbl no-undo
  field xid        as integer   format "->>>>9"
  field tblNum     as integer   format "->>>>9"          label "Tbl#"
  field areaNum    as integer   format ">>>>9"           label "Area#"

  field tblName    as character format "x(30)"           label "Table Name"
  field dbaRM      as decimal   format ">>>>>>>>>>>>"    label "RM Chain"
  field dbaRecs    as decimal   format ">>>>>>>>>>>>"    label "#Records"
  field dbaFragPct as decimal   format ">>>>>9.99%"      label "Frag%"
  field dbaScatter as decimal   format ">>>>"            label "Scat"
  field tblTurn    as decimal   format ">>>>>>9.99"      label "Turns"
  field dbaAvgRow  as decimal   format ">>>>>>>"         label "AvgRow"
  field tblCreate  as decimal   format ">>>>>>>>9"       label "Create"
  field tblRead    as decimal   format ">>>>>>>>>9"      label "Read"
  field tblUpdate  as decimal   format ">>>>>>>>9"       label "Update"
  field tblDelete  as decimal   format ">>>>>>>>9"       label "Delete"
  field tblOSRd    as decimal   format ">>>>>>>>9"       label "OS Read"

  index tblRead-idx is primary tblRead descending
  index tblNum-idx  is unique tblNum
  index tblname-idx is unique tblName
  index xid-idx     is unique xid
  index dbaRecs-idx   dbaRecs   descending
  index tblTurn-idx   tblTurn   descending
  index tblCreate-idx tblCreate descending
  index tblUpdate-idx tblUpdate descending
  index tblDelete-idx tblDelete descending
  index tblOSRd-idx   tblOSRd   descending
.

/*
  field xid        as integer   format "->>>>9"
  field idxNum     as integer   format "->>>>9"       label "Idx#"
  field idxAreaNum as integer   format ">>>>9"        label "Area#"
  field idxName    as character format "x(44)"        label "Index Name"

  field idxBlks    as {&BIGINT} format ">>>>>>>>>>9"  label "Blocks"
  field idxUtil    as decimal   format ">>>>>9.99%"   label "Util"
  field idxLvls    as decimal   format ">>>9"         label "Lvls"

  field idxRoot    as {&BIGINT} format ">>>>>>>>>9"   label "Idx Root"
  field idxNote    as character format "   x(4)"      label "  Note "
  field idxCr      as decimal   format ">>>>>>>>9"    label "Create"
  field idxRd      as decimal   format ">>>>>>>>>9"   label "Read"
  field idxSp      as decimal   format ">>>>>>>>9"    label "Split"
  field idxDl      as decimal   format ">>>>>>>>9"    label "Delete"
  field idxBlkDl   as decimal   format ">>>>>>>>9"    label "BlkDl"

 */

define temp-table tt_uIdx no-undo
  field xid        as integer   format "->>>>9"
  field idxNum     as integer   format "->>>>9"       label "Idx#"
  field areaNum    as integer   format ">>>>9"        label "Area#"
  field idxName    as character format "x(44)"        label "Index Name"

  field idxBlks    as {&BIGINT} format ">>>>>>>>>>9"  label "Blocks"
  field idxUtil    as decimal   format ">>>>>9.99%"    label "Util"
  field idxLvls    as decimal   format ">>>9"         label "Lvls"

  field idxRoot    as {&BIGINT} format ">>>>>>>>>9"   label "Idx Root"
  field idxNote    as character format "   x(4)"      label "  Note "
  field idxCreate  as decimal   format ">>>>>>>>9"    label "Create"
  field idxRead    as decimal   format ">>>>>>>>>9"   label "Read"
  field idxSplit   as decimal   format ">>>>>>>>9"    label "Split"
  field idxDelete  as decimal   format ">>>>>>>>9"    label "Delete"
  field idxBlkDel  as decimal   format ">>>>>>>>9"    label "BlkDl"

  index id-idx      is unique idxNum
  index idxRead-idx is unique primary idxRead descending idxNum
  index xid-idx     is unique xid
.

define temp-table tt_uStack no-undo
  field xid          as integer   format "->>>>9"
  field depth        as integer   format ">>>>9"   label "Depth"
  field lineNum      as integer   format "->>>>9"  label "Line#"
  field procName     as character format "x(142)"   label "Program Name"
/*
  field IPName       as character format "x(49)"   label "Routine (IP)"
 */
  index depth-idx is unique primary depth descending
  index xid-idx   is unique xid
.

{lib/dumpTT.i tt_uOther}
{lib/dumpTT.i tt_uTbl}
{lib/dumpTT.i tt_uIdx}
{lib/dumpTT.i tt_uStack}

define variable pt_trimStack as character no-undo.	/* a list of items to automatically remove from the program stack	*/

define variable bh  as handle no-undo.
define variable bf  as handle no-undo.
define variable bf2 as handle no-undo.
define variable qh  as handle no-undo.
define variable qut as handle no-undo.
define variable qui as handle no-undo.
define variable qx  as handle no-undo.
define variable qy  as handle no-undo.
define variable qz  as handle no-undo.
define variable qo  as handle no-undo.

define variable but as handle no-undo.
define variable bui as handle no-undo.

define variable hasCnxCache  as logical no-undo.
define variable hasUsrStats  as logical no-undo.
define variable hasUsrExt    as logical no-undo.

procedure chkCnx:
  hasCnxCache  = no.
  find dictdb._File  no-lock where _File-Name = "_Connect" no-error.
  if not available _File then return.
  find dictdb._Field no-lock where _Field._File-recid = recid( _File ) and _Field-Name = "_Connect-CacheInfo" no-error.
  if available _Field then hasCnxCache = yes.
  return.
end.

procedure chkUsrStats:
  hasUsrStats  = no.
  find dictdb._File  no-lock where _File-Name = "_UserTableStat" no-error.
  if available _File then
    do:
      hasUsrStats = yes.
      create buffer but for table "dictdb._userTableStat".
      create query qut.
      qut:set-buffers( but ).
      create buffer bui for table "dictdb._userIndexStat".
      create query qui.
      qui:set-buffers( bui ).
    end.
  return.
end.

procedure chkUsrExt:
  hasUsrExt  = no.
  find dictdb._File  no-lock where _File-Name = "_User" no-error.
  if not available _file then return.
  find dictdb._Field no-lock where _Field._File-recid = recid( _File ) and _Field-Name = "_Telephone" no-error.
  if available _Field then hasUsrExt = yes.
  return.
end.

procedure chkOSRd:
  hasOSRd = no.
  find _file no-lock where _file._file-name = "_usertablestat" no-error.
  if available _file then find _field no-lock of _file where _field._field-name = "_usertablestat-osread" no-error.
  hasOSRd = available( _field ).
  return.
end.

procedure mon-init:

  run chkCnx.
  run chkUsrStats.
  run chkUsrExt.
  run chkOSRd.

/* */
  create tt_userInfo.

/***
  create tt_uTbl.
  create tt_uIdx.
  create tt_uStack.
 ***/

/***
  create query qh.
  qh:set-buffers( buffer tt_userInfo:handle ).
  qh:query-prepare( "preselect each tt_userInfo" ).
  qh:query-open.

  create query qx.
  qx:set-buffers( buffer tt_uTbl:handle ).
  qx:query-prepare( "preselect each tt_uTbl" ).
  qx:query-open.

  create query qy.
  qy:set-buffers( buffer tt_uIdx:handle ).
  qy:query-prepare( "preselect each tt_uIdx" ).
  qy:query-open.

  create query qz.
  qz:set-buffers( buffer tt_uStack:handle ).
  qz:query-prepare( "preselect each tt_uStack" ).
  qz:query-open.
 ***/

  create query qo.
  qo:set-buffers( buffer tt_uOther:handle ).
  qo:query-prepare( "preselect each tt_uOther" ).
  qo:query-open.

&IF DEFINED( OE10 ) &THEN
  prevTick = mtime.
&ELSE
  prevTick = etime.
&ENDIF

  return.

end.

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable i   as integer no-undo.
  define variable j   as integer no-undo.
  define variable k   as integer no-undo.
  define variable rLk as integer no-undo.

  define variable qs      as character no-undo.

  define variable firstId as integer no-undo.
  define variable lastId  as integer no-undo.

  define variable stackEntry as character no-undo.

  define variable xUsr as integer no-undo.
  define variable xPID as integer no-undo initial ?.

  find tt_userInfo no-error.

  if argList <> "" then
    do:
      if argList begins "USR" then
        assign xUsr = integer( entry( 2, argList, "=" )) no-error.
       else if argList begins "PID" then
        do:
          assign xPID = integer( entry( 2, argList, "=" )) no-error.
          find first _Connect no-lock where _Connect._Connect-pid = xPID no-error.	/* _connect ought to be cached to a TT	*/
          if available _Connect then xUsr =  _Connect-usr.
        end.
    end.

  if xUsr <> tt_userInfo.usrNum then run set-user( xUsr ).

  empty temp-table tt_uStack.
  create tt_uStack.
  assign
    tt_uStack.xid      = 0
    tt_uStack.lineNum  = 0
    tt_uStack.procName = "4GL Stack Trace not available " + ( if hasCnxCache then "for this session." else "in this version of Progress." )
  .

  find dictdb._Connect no-lock where _Connect-id = tt_userInfo.usrNum + 1 and _Connect-usr <> ? no-error.
  if not available( _Connect ) then
    do:
      assign
        tt_userInfo.usrId       = 0
        tt_userInfo.usrName     = ""
        tt_userInfo.usrFullName = ""
        tt_userInfo.usrPhone    = ""
        tt_userInfo.usrEMail    = ""
        tt_userInfo.usrIPAddr   = ""
        tt_userInfo.usrPID      = 0
        tt_userInfo.usrLoginTm  = ""
        tt_userInfo.usrServ     = 0
        tt_userInfo.SessionInfo = ""
        tt_userInfo.TrxInfo     = ""
      .
    end.
   else
    do:

      assign
        tt_userInfo.usrId       = _Connect-Id
        tt_userInfo.usrName     = _Connect-Name
        tt_userInfo.usrPID      = _Connect-PID
        tt_userInfo.usrLoginTm  = _Connect-Time
        tt_userInfo.usrServ     = ( if _Connect-Server = ? then 0 else _Connect-Server )
        tt_userInfo.usrFullName = ""
        tt_userInfo.usrPhone    = ""
        tt_userInfo.usrEMail    = ""
        tt_userInfo.usrIPAddr   = ( if _Connect-Device = "batch" then "" else _Connect-Device )
      .

      empty temp-table tt_uOther.
      for each _Connect no-lock where _Connect-Usr <> ? and _Connect-Name = tt_userInfo.usrName:
        if _Connect-Usr <> tt_userInfo.usrNum then
          do:
            create tt_uOther.
            assign
              tt_uOther.usrType    = connectFlags( _connect-Id )	/* UDF has to be before indexed fields in old releases	*/
              tt_uOther.xid        = _Connect-Usr
              tt_uOther.usrNum     = _Connect-Usr
              tt_uOther.usrPID     = _Connect-PID
           /* tt_uOther.usrType    = _Connect-Type + ( if _Connect-Device = "batch" then " BAT" else "" ) */
              tt_uOther.usrServ    = ( if _Connect-Server = ? then 0 else _Connect-Server )
              tt_uOther.usrLoginTM = substring( _Connect-Time, 5 )
              tt_uOther.usrIPAddr  = ( if _Connect-Device = "batch" then "" else _Connect-device )
            .
            if hasCnxCache then
              do:
                assign
                  bh = buffer _Connect:handle
                  bf = bh:buffer-field( "_Connect-IPAddress" )
                  tt_uOther.usrIPAddr = ( if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then bf:buffer-value() else tt_uOther.usrIPAddr )
               /* bf = bh:buffer-field( "_Connect-ClientType" )
                * tt_uOther.usrType = ( if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then trim( bf:buffer-value()) + " " else "" ) + tt_uOther.usrType
                */
                  bf  = bh:buffer-field( "_Connect-CacheLineNumber" )
                  bf2 = bh:buffer-field( "_Connect-CacheInfo" )
                  stackEntry = bf2:buffer-value( 1 )
                .
                k = num-entries( pt_trimStack ).
                do j = 1 to k:
                  stackEntry = replace( stackEntry, entry( j, pt_trimStack ), "" ).
                end.
                if stackEntry <> ? and stackEntry <> "" then
                  assign
                    tt_uOther.lineNum  = bf:buffer-value( 1 )
                 /* tt_uOther.IPName   = ( if num-entries( stackEntry, " " ) > 1 then entry( 1, stackEntry, " " ) else "" )
                  * tt_uOther.procName = trim( stackEntry, tt_uOther.IPName )
                  * tt_uOther.procName = trim( tt_uOther.procName ) */
                    tt_uOther.procName = stackEntry
                  .
              end.
          end.
      end.
      qo:query-open.

      find dictdb._Connect no-lock where _Connect-id = tt_userInfo.usrNum + 1 and _Connect-usr <> ? no-error.

      find dictdb._User no-lock where _UserId = _Connect-Name no-error.
      if available _User then
        do:
          assign
            tt_userInfo.usrFullName = _User-Name
            tt_userInfo.usrPhone    = _User-Misc
            tt_userInfo.usrEMail    = _U-Misc2[1]
          .
          if hasUsrExt then
            do:
              bh = buffer _User:handle.
              bf = bh:buffer-field( "_Telephone" ).
              if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then tt_userInfo.usrPhone = bf:buffer-value().
              bf = bh:buffer-field( "_Email" ).
              if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then tt_userInfo.usrEMail = bf:buffer-value().
              bf = bh:buffer-field( "_Given_Name" ).
              if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then tt_userInfo.usrFullName = bf:buffer-value().
              bf = bh:buffer-field( "_Middle_Initial" ).
              if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then tt_userInfo.usrFullName = tt_userInfo.usrFullName + " " + bf:buffer-value() + ".".
              bf = bh:buffer-field( "_Surname" ).
              if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then tt_userInfo.usrFullName = tt_userInfo.usrFullName + " " + bf:buffer-value().
            end.
        end.

      find dictdb._UserIO no-lock where _UserIO-Id = tt_userInfo.usrId.

      run update_xstat(
        input 1,
        input "",
        input "m1",
        input "m2",
        input "m3",
        input "m4",
        input "m5",
        input _UserIO-dbaccess,
        input _UserIO-dbread,
        input _UserIO-dbwrite,
        input 0,
        input 0,
        input 0
      ).

      run update_xstat(
        input 2,
        input "",
        input "m1",
        input "m2",
        input "m3",
        input "m4",
        input "m5",
        input _UserIO-biread,
        input _UserIO-biwrite,
        input _UserIO-airead,
        input _UserIO-aiwrite,
        input 0,
        input 0
      ).

      run updTick.
      run age_xstat.

      if hasUsrStats then
        do:

          assign
            firstId = _Connect-Usr * tRange
            lastId  = firstId + min( tRange, tUsed )
            firstId = firstId + 1
            qs = substitute( "preselect each dictdb.&1 no-lock where &2 >= &3 and &2 <= &4", "_UserTableStat", "_UserTableStat-Id", string( firstId ), string( lastId ))
          .
          qut:query-prepare( qs ).
          qut:query-open.
          qut:get-first( no-lock ) no-error.
          do while ( qut:query-off-end = no ):
            run upd-tt_utable ( but:buffer-field( "_userTableStat-Id" ):buffer-value /* _UserTableStat._UserTableStat-id */ ).
            qut:get-next( no-lock ) no-error.
          end.

          run age_utable.

          empty temp-table tt_uTbl.

          for each tt_utable no-lock:
            /* i = i + 1. */
            create tt_uTbl.
            assign
              tt_uTbl.xid       = tt_utable.tblNum
              tt_uTbl.tblNum    = tt_utable.tblNum
              tt_uTbl.areaNum   = tt_utable.areaNum
              tt_uTbl.tblName   = tt_utable.tblName
              tt_uTbl.tblCreate = ( tt_utable.tbl-cre[x] / z )
              tt_uTbl.tblRead   = ( tt_utable.tbl-rd[x]  / z )
              tt_uTbl.tblUpdate = ( tt_utable.tbl-upd[x] / z )
              tt_uTbl.tblDelete = ( tt_utable.tbl-del[x] / z )
              tt_uTbl.tblOSRd   = ( tt_utable.tbl-osrd[x] / z )
            .
            run getTableInfo(
              tt_uTbl.tblName,
              output tt_uTbl.dbaRecs,
              output tt_uTbl.dbaRM,
              output tt_uTbl.dbaFragPct,
              output tt_uTbl.dbaScatter,
              output tt_uTbl.dbaAvgRow
            ).
            if tt_uTbl.dbaRecs > 0 then tt_uTbl.tblTurn = tt_uTbl.tblRead / tt_uTbl.dbaRecs.
          end.

          assign
            firstId = _connect-usr * iRange
            lastId  = firstId + min( iRange, iUsed )
            firstId = firstId + 1
            qs = substitute( "preselect each dictdb.&1 no-lock where &2 >= &3 and &2 <= &4", "_UserIndexStat", "_UserIndexStat-Id", string( firstId ), string( lastId ))
          .

          qui:query-prepare( qs ).
          qui:query-open.
          qui:get-first( no-lock ) no-error.
          do while ( qui:query-off-end = no ):
            run upd-tt_uindex ( bui:buffer-field( "_userIndexStat-Id" ):buffer-value ).
            qui:get-next( no-lock ) no-error.
          end.

          run age_uindex.

          empty temp-table tt_uIdx.

          for each tt_uindex no-lock:
            create tt_uIdx.
            assign
              tt_uIdx.xid       = tt_uindex.idxNum
              tt_uIdx.idxNum    = tt_uindex.idxNum
              tt_uIdx.areaNum   = tt_uindex.areaNum
              tt_uIdx.idxRoot   = tt_uindex.idxRoot
              tt_uIdx.idxName   = tt_uindex.idxName
              tt_uIdx.idxCreate = ( tt_uindex.idx-cre[x] / z )
              tt_uIdx.idxRead   = ( tt_uindex.idx-rd[x]  / z )
              tt_uIdx.idxSplit  = ( tt_uindex.idx-split[x] / z )
              tt_uIdx.idxDelete = ( tt_uindex.idx-del[x] / z )
              tt_uIdx.idxBlkDel = ( tt_uindex.idx-blkdel[x] / z )
              tt_uIdx.idxNote   = tt_uindex.idxNote
            .
            run getIndexInfo( tt_uIdx.idxName, output tt_uIdx.idxLvls, output tt_uIdx.idxBlks, output tt_uIdx.idxUtil ).
          end.

        end.

      /*** interesting but not currently used     
       *  i = 0.
       *  find dictdb._UserLock no-lock where _UserLock-Id = _Connect-Id.
       *  do while i < 512 and _UserLock-Recid[i + 1] <> ?:
       *    i = i + 1.
       *  end.
       ***/

      find tt_xstat where tt_xstat.xid = 1 no-error.
      assign
        tt_userInfo.usrDBA      = ( tt_xstat.stat1[x] / z )
        tt_userInfo.usrDBR      = ( tt_xstat.stat2[x] / z )
        tt_userInfo.usrHR       = ( if tt_xstat.stat-ratio = ? then 100.00 else tt_xstat.stat-ratio )
        tt_userInfo.usrDBW      = ( tt_xstat.stat3[x] / z )
        tt_userInfo.usrRecLks   = i
      no-error.
      find tt_xstat where tt_xstat.xid = 2 no-error.
      assign
        tt_userInfo.usrBIRd     = ( tt_xstat.stat1[x] / z )
        tt_userInfo.usrBIWr     = ( tt_xstat.stat2[x] / z )
        tt_userInfo.usrAIRd     = ( tt_xstat.stat3[x] / z )
        tt_userInfo.usrAIWr     = ( tt_xstat.stat4[x] / z )
      no-error.

      tt_userInfo.SessionInfo =
        _Connect-Type + " " + connectFlags( _connect-id ) + " " +
        ( if _Connect-Type = "SELF" or _Connect-type = "REMC" then ( if _Connect-Batch = "Yes" then "Batch " else "" ) else "Background" ) +
        ( if _Connect-Transid > 0 then "TRX " else "" ) +
        ( if _Connect-Wait <> " -- " then "Blocked on " + _Connect-Wait else "" )
      .

      find first dictdb._Trans no-lock where _Trans-UsrNum = _Connect-Usr no-error.

      if available _Trans then
        tt_userInfo.TrxInfo =
          _Trans-state + " " +
          _Trans-flags + " " +
          string( _Trans-Num ) + " " +
          /* ( if _Trans-txtime = ? then "" else _Trans-txtime ) + " " + */
          "Duration: " + ( if _Trans-duration = ? then "" else string( _Trans-duration, "hh:mm:ss" )) + " " +
          "Waiting On: " + ( if available _Connect then ( string( _Connect-wait ) + " " + string( _Connect-wait1 )) else "" ).
       else
        tt_userInfo.TrxInfo = "--None--".

      if hasCnxCache then
        do:

          bh = buffer _Connect:handle.

          /* enable the client statement cache for this user
           *
           * 1 = single statement, 2 = whole stack, 3 = one time
           *
           */
/*
 *        bf = bh:buffer-field( "_Connect-CachingType" ).
 *        if bf <> ? and bf:buffer-value() = ? then bf:buffer-value() = 2.
 */
          bf = bh:buffer-field( "_Connect-ClientType" ).
          if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then tt_userInfo.SessionInfo = trim( bf:buffer-value()) + " " + tt_userInfo.SessionInfo.

          bf = bh:buffer-field( "_Connect-IPAddress" ).
          if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then tt_userInfo.usrIPAddr = bf:buffer-value().

          bf = bh:buffer-field( "_Connect-CacheLineNumber" ).
          bf2 = bh:buffer-field( "_Connect-CacheInfo" ).

          if bf:buffer-value( 1 ) <> ? then empty temp-table tt_uStack.
          getStack: do i = 1 to 32:
            if bf:buffer-value( i ) = ? then
              leave getStack.
             else
              do:
                create tt_uStack.
                stackEntry = bf2:buffer-value( i ).
                k = num-entries( pt_trimStack ).
                do j = 1 to k:
                  stackEntry = replace( stackEntry, entry( j, pt_trimStack ), "" ).
                end.
                assign
                  tt_uStack.xid      = i
                  tt_uStack.depth    = i
                  tt_uStack.lineNum  = bf:buffer-value( i )
               /* tt_uStack.IPName   = ( if num-entries( stackEntry, " " ) > 1 then entry( 1, stackEntry, " " ) else "" )
                * tt_uStack.procName = trim( stackEntry, tt_uStack.IPName )
                * tt_uStack.procName = trim( tt_uStack.procName ) */
                  tt_uStack.procName = stackEntry
                .
              end.
          end.

        end.

    end.

  add2ds( temp-table tt_userInfo:default-buffer-handle ).
  add2ds( temp-table tt_uTbl:default-buffer-handle ).
  add2ds( temp-table tt_uIdx:default-buffer-handle ).
  add2ds( temp-table tt_uStack:default-buffer-handle ).
  add2ds( temp-table tt_uOther:default-buffer-handle ).

  return.

end.

procedure set-User:

  define input parameter p_usr as integer no-undo.

  find first tt_userInfo.
  tt_userInfo.usrNum = p_usr.
  empty temp-table tt_xstat.            /* changing the user invalidates the xstats...  */

  /* run mon-update. */

  return.

end.

return.
