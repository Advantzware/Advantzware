/*******************************************************************************
 *******************************************************************************
 **                                                                           **
 **                                                                           **
 **  Copyright 2003-2012 Tom Bascom, Greenfield Technologies                  **
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
 * b2.p
 *
 *
 * primary vs alternate buffer pool details
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 5, 2003
 *
 */

{lib/protop.i}
{lib/tick.i}

define output parameter dcDescription as character no-undo initial "B2".

{lib/tt_xstat.i}

define temp-table tt_b2 no-undo
  field xid        as integer   format ">9"               label "Id"
  field bpName     as character format "x(7)"             label "Pool"
  field bpTables   as integer   format ">>>9"             label "Tbls"
  field bpIndexes  as integer   format ">>>9"             label "Idxs"
  field bpLOBS     as integer   format ">>9"              label "LOB"
  field bplruStat  as character format "x(8)"             label "LRU Stat"
  field bpSize     as integer   format ">>>>>>>9"         label "Buffers"
  field bpUnused   as integer   format ">>>>>>>9"         label "Unused"

  field bpActive   as integer   format ">>>>>>>9"         label "Active"
  field bpData     as integer   format ">>>>>>>9"         label "DataBlk"
  field bpIdx      as integer   format ">>>>>>>9"         label "IdxBlk"

  field bpMstr     as integer   format ">>>9"             label "Mstr"
  field bpSeq      as integer   format ">>9"              label "SEQ"
  field bpArea     as integer   format ">>>9"             label "Area"
  field bpCtrl     as integer   format ">>>9"             label "Ctrl"
  field bpObj      as integer   format ">>>>9"            label "Obj"
  field bpOList    as integer   format ">>>9"             label "OLst"
  field bpOCList   as integer   format ">>9"              label "OCL"
  field bpCMap     as integer   format ">>>9"             label "CMap"
  field bpBMap     as integer   format ">>>9"             label "BMap"

  field bpLogRd    as integer   format "->>>>>>9"         label "logRd"
  field bpOSRd     as integer   format "->>>9"            label "OSRd"
  field bpLogWr    as integer   format "->>>>>9"          label "logWr"
  field bpOSWr     as integer   format "->>>9"            label "OSWr"
  field bpHitPct   as decimal   format ">>9.99"           label "Hit%"

  index bpName-idx is unique bpName descending
  index xid-idx    is unique xid
.

{lib/dumpTT.i tt_b2}

define new global shared variable bigB      as integer no-undo.
define new global shared variable bigB2     as integer no-undo.
define new global shared variable lruskips  as integer no-undo.
define new global shared variable lru2skips as integer no-undo.

define variable b1Tables  as integer no-undo.
define variable b1Indexes as integer no-undo.
define variable b1LOBs    as integer no-undo.
define variable b2Tables  as integer no-undo.
define variable b2Indexes as integer no-undo.
define variable b2LOBs    as integer no-undo.

define variable hasBPactive as logical.

procedure mon-init:

  find _file no-lock where _file._file-name = "_actBuffer" no-error.
  if available _file then
    do:
      find _field no-lock where _field._file-recid = recid( _file ) and _field._field-name = "_buffer-Active" no-error.
      if available( _field ) then hasBPactive = yes.
    end.

  empty temp-table tt_xstat.

  assign
    b1Tables  = 0
    b1Indexes = 0
    b1Lobs    = 0
    b2Tables  = 0
    b2Indexes = 0
    b2Lobs    = 0
  .

  for each tt_tbl:
    if tt_tbl.tblPool = "b1" then
      b1Tables = b1Tables + 1.
     else if tt_tbl.tblPool = "b2" then
      b2Tables = b2Tables + 1.
  end.

  for each tt_idx:
    if tt_idx.idxPool = "b1" then
      b1Indexes = b1Indexes + 1.
     else if tt_idx.idxPool = "b2" then
      b2Indexes = b2Indexes + 1.
  end.

  find first dictdb._DB no-lock.

  for each _storageObject no-lock where _object-type = 3:

    find _area no-lock where _area._area-number = _storageObject._area-number.

    if ( get-bits( _object-attrib, 7, 1 ) = 0 ) and ( get-bits( _area-attrib, 7, 1 ) = 0 ) then
      b1LOBs = b1LOBs + 1.
     else if ( get-bits( _object-attrib, 7, 1 ) = 1 ) or ( get-bits( _area-attrib, 7, 1 ) = 1 ) then
      b2LOBs = b2LOBs + 1.

  end.

  for each _actbuffer no-lock where _buffer-id > 1:
    create tt_b2.
    assign
      tt_b2.xid = _buffer-Id
      tt_b2.bpName = entry( _buffer-id, ",Primary,Alternate" )
    .
  end.

  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  run updTick.

  for each _actbuffer no-lock where _buffer-id > 1:

    run update_xstat (
      input _buffer-Id,
      input entry( _buffer-id, ",Primary,Alternate" ),
      input "m1",
      input "m2",
      input "m3",
      input "m4",
      input "m5",
      input _buffer-logicRds,
      input _buffer-OSrds,
      input _buffer-logicWrts,
      input _buffer-OSWrts,
      input 0,
      input 0
    ).

    find tt_b2 where tt_b2.xid = _buffer-id.
    assign
      tt_b2.bpActive = buffer _actBuffer:handle:buffer-field( "_buffer-Active" ):buffer-value
      tt_b2.bpData   = buffer _actBuffer:handle:buffer-field( "_buffer-RM" ):buffer-value
      tt_b2.bpIdx    = buffer _actBuffer:handle:buffer-field( "_buffer-Index" ):buffer-value
      tt_b2.bpMstr   = buffer _actBuffer:handle:buffer-field( "_buffer-Master" ):buffer-value
      tt_b2.bpSeq    = buffer _actBuffer:handle:buffer-field( "_buffer-Seq" ):buffer-value
      tt_b2.bpArea   = buffer _actBuffer:handle:buffer-field( "_buffer-Area" ):buffer-value
      tt_b2.bpCtrl   = buffer _actBuffer:handle:buffer-field( "_buffer-Control" ):buffer-value
      tt_b2.bpObj    = buffer _actBuffer:handle:buffer-field( "_buffer-Object" ):buffer-value
      tt_b2.bpOList  = buffer _actBuffer:handle:buffer-field( "_buffer-ObjList" ):buffer-value
      tt_b2.bpOCList = buffer _actBuffer:handle:buffer-field( "_buffer-ObjCList" ):buffer-value
      tt_b2.bpCMap   = buffer _actBuffer:handle:buffer-field( "_buffer-ClusterMap" ):buffer-value
      tt_b2.bpBMap   = buffer _actBuffer:handle:buffer-field( "_buffer-BlockMap" ):buffer-value
    no-error.

  end.

  run age_xstat.

  /*** empty temp-table tt_b2. ***/

  for each tt_xstat no-lock by tt_xstat.stat1[x] descending:

    find tt_b2 where tt_b2.xid = tt_xstat.xid.

    assign
      tt_b2.bpLogRd    = tt_xstat.stat1[x] / z
      tt_b2.bpOSRd     = tt_xstat.stat2[x] / z
      tt_b2.bpLogWr    = tt_xstat.stat3[x] / z
      tt_b2.bpOSWr     = tt_xstat.stat4[x] / z
      tt_b2.bpHitPct   = tt_xstat.stat-ratio
    no-error.

    if hasBPactive = no then
      assign
        tt_b2.bpActive = tt_b2.bpSize
        tt_b2.bpUnused = ?
      .

    if tt_xstat.xid = 2 then
      do:
        assign
          tt_b2.bpSize    = bigB
          tt_b2.bpUnused  = ( bpSize - bpActive ) when hasBPActive = yes
          tt_b2.bpTables  = b1Tables
          tt_b2.bpIndexes = b1Indexes
          tt_b2.bpLOBs    = b1LOBs
        .
        if true then 
          do:
            if lru2skips > 0 then 
              tt_b2.bplruStat = "Skip" + string( lruskips ).
             else
              tt_b2.bplruStat = "NotSet".
          end.
      end.

    if tt_xstat.xid = 3 then
      do:
        assign
          tt_b2.bpSize    = bigB2
          tt_b2.bpUnused  = ( bpSize - bpActive ) when hasBPActive = yes
          tt_b2.bpUnused  = max( 0, ( bpSize - tt_xstat.stat2[3] )) when hasBPActive = no
          tt_b2.bpTables  = b2Tables
          tt_b2.bpIndexes = b2Indexes
          tt_b2.bpLOBs    = b2LOBs
        .
        if tt_b2.bpSize = 0 or ( tt_b2.bpUnused > 0 and tt_b2.bpUnused <> ? ) then 
          tt_b2.bplruStat = "Bypassed".
         else
          do:
            if lru2skips > 0 then 
              tt_b2.bplruStat = "Skip" + string( lru2skips ).
             else
              tt_b2.bplruStat = "NotSet".
          end.
      end.

  end.

  publish "resizeBrowse" ( "B2", 2 ).

  add2ds( temp-table tt_b2:default-buffer-handle ).

  return.

end.

return.
