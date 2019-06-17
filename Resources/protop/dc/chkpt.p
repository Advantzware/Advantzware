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
 * chkpt.p
 *
 *
 * Checkpoint History.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	October 27, 2004
 *
 */

/***

Field name			Data type	Description
_Checkpoint-ApwQ		INT64		Number of buffers written by the APW queue and replaced on the least recently used (LRU) chain by APWs
_Checkpoint-Bi-Writes		INT64		The number of BI buffers written to disk at checkpoint end
_Checkpoint-Bi-Write-Time	DECIMAL		Time in seconds to write the BI buffers at checkpoint end
_Checkpoint-Buffers		INT64		The number of database buffers checkpointed
_Checkpoint-Cluster		INT64		The checkpoint current BI cluster number
_Checkpoint-CptQ		INT64		Number of buffers written from the checkpoint queue by the APWs
_Checkpoint-DB-Write-Time	DECIMAL		Time in seconds to write the buffers from the database buffer pool at checkpoint end
_Checkpoint-Dirty		INT64		Number of modified buffers scheduled to be written
_Checkpoint-Duration		DECIMAL		The time to do the housekeeping of the checkpoint. This includes the time to write the active recovery log buffers to disk, write any outstanding dirty buffers marked at the previous checkpoint, scan the database buffer pools for dirty buffers marking them as needing to be written to disk prior to the start of the next check point (putting them on the checkpoint queue) and to perform the file system synchronization (_Checkpoint-SyncTime)
_Checkpoint-Flush		INT64		Number of database buffers written at the end of the checkpoint
_Checkpoint-Len			CHARACTER	The time required to complete the checkpoint. The checkpoint is not considered completed until all dirty buffers on the checkpoint queue have been written out. This is either done over time by the APWs between checkpoints or is forced at the next checkpoint (buffers flushed at checkpoint)
_Checkpoint-Number		INT64		The number of this checkpoint
_Checkpoint-Scan		INT64		Number of buffers written by the APWs during the scan cycle
_Checkpoint-SyncTime		DECIMAL		The time to perform the file system synchronization of the checkpoint. (fdatasync() on UNIX systems, FlushFileBuffers() on Windows systems)
_Checkpoint-Time		CHARACTER	Time the checkpoint began

 ***/

{lib/protop.i}

define output parameter dcDescription as character no-undo initial "Checkpoints".

define temp-table tt_checkpt no-undo
  field xid       as integer
  field id        as integer   format "->>>>>>>9"     label "Chkpt#"
  field ckptClstr as decimal   format ">>>>>>>>9"     label "Cluster"
  field st        as character format "x(22)"         label " Start"
  field fini      as character format "x(8)"          label "  Finish"
  field cpLenSec  as integer						/* will not be displayed - dynscreen automatically supresses */
  field cpLen     as character format "x(8)"          label "  Length"
  field dirty     as decimal   format ">>>>>>9"       label "Dirty"
  field cptq      as decimal   format ">>>>>>9"       label "ChkPtQ"
  field scan      as decimal   format ">>>>>>9"       label "Scan"
  field apwq      as decimal   format ">>>>>>9"       label "APW Q"
  field flush     as decimal   format ">>>>>>>9"      label "Flushed"

  field duration  as decimal   format  ">>>>>9.99"    label "Duration"
  field syncTime  as decimal   format  ">>>>>9.99"    label "Sync Time"

  field biWrt     as decimal   format ">>>>>>9"       label "BIWr"
  field biWrtTm   as decimal   format ">>>>9.99"      label "BIWrTm"
  field DBWrt     as decimal   format ">>>>>>9"       label "DBWr"
  field DBWrtTm   as decimal   format ">>>>9.99"      label "DBWrTm"

  index  id-idx is unique primary id descending
.

{lib/dumpTT.i tt_checkpt}

define variable xseq#        as integer no-undo.

define variable bh           as handle no-undo.
define variable bf           as handle no-undo.

define variable hasSyncTime  as logical no-undo.

procedure chkSyncTime:

  hasSyncTime = no.
  find dictdb._File  no-lock where _File-Name = "_Checkpoint".
  find dictdb._Field no-lock where _Field._File-recid = recid( _File ) and _Field-Name = "_Checkpoint-SyncTime" no-error.
  if available _Field then hasSyncTime = yes.

  return.

end.

function fixDateString returns character ( input dx as character ):

  define variable d as character no-undo.
  define variable m as character no-undo.
  define variable y as character no-undo.
  define variable t as character no-undo.

  /* The date-time field in _checkpoint looks like: Mon Jan 14 03:33:08 2013 */

  assign
    m = string( lookup( substring( dx, 5, 3 ), "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec" ), "99" )
    d = string( integer( trim( substring( dx, 9, 2 ))), "99" )
    y = substring( dx, 21 )
    t = substring( dx, 12, 8 )
  .

  if       session:date-format = "ymd" then return substitute( "&1-&2-&3 &4", y, m, d, t ).
   else if session:date-format = "mdy" then return substitute( "&1-&2-&3 &4", m, d, y, t ).
   else if session:date-format = "dmy" then return substitute( "&1-&2-&3 &4", d, m, y, t ).
   else
    do:
      message "Unsupported session:date-format" session:date-format view-as alert-box.
      quit.
    end.

end.

procedure mon-init:

  empty temp-table tt_checkpt.

  xseq# = 0.

  run chkSyncTime.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

&IF DEFINED( OE10 ) &THEN

  define variable cpStart  as datetime no-undo.
  define variable cpFinish as datetime no-undo.
  define variable cpLength as integer  no-undo.

  empty temp-table tt_checkpt.			/* snapshot... it would be nice to capture the history, something to work on	*/

  find dictdb._ActSummary no-lock.
  find dictdb._BuffStatus no-lock.

  for each dictdb._Checkpoint no-lock where _Checkpoint-Time <> ?:

    find tt_checkpt where tt_checkpt.id = _checkpoint._checkpoint-id no-error.

    if not available tt_checkpt then
      do:
        create tt_checkpt.
        assign
          xseq# = xseq# + 1
          tt_checkpt.xid = xseq#
        .
      end.

    assign
      cpStart  = ?
      cpFinish = ?
      cpLength = 0
      cpStart  = datetime( fixDateString( _Checkpoint._Checkpoint-Time )) when _Checkpoint._Checkpoint-Time > ""
      cpFinish = datetime( fixDateString( _Checkpoint._Checkpoint-Len ))  when _Checkpoint._Checkpoint-Len > ""
      cpLength = abs( interval( cpFinish, cpStart, "seconds" )) when _Checkpoint._Checkpoint-Time > ""
/*    tt_checkpt.id    = _Summary-Chkpts - ( _Checkpoint._Checkpoint-Id - 2 ) */
      tt_checkpt.id    = _BfStatus-LastCkpNum - ( _Checkpoint._Checkpoint-Id - 1 )
      tt_checkpt.st    = " " + substring( _Checkpoint._Checkpoint-Time, 5, 20 ) when _Checkpoint._Checkpoint-Time > ""
      tt_checkpt.fini  = substring( _Checkpoint._Checkpoint-Len, 12, 8 ) when _Checkpoint._Checkpoint-Len > ""
      tt_checkpt.cpLenSec = cpLength
      tt_checkpt.cpLen = string( cpLength, "hh:mm:ss" )
      tt_checkpt.dirty = _Checkpoint._Checkpoint-Dirty
      tt_checkpt.cptq  = _Checkpoint._Checkpoint-CptQ
      tt_checkpt.scan  = _Checkpoint._Checkpoint-Scan
      tt_checkpt.apwq  = _Checkpoint._Checkpoint-ApwQ
      tt_checkpt.flush = _Checkpoint._Checkpoint-Flush
    .

    if hasSyncTime = yes then
      do:
        bh = buffer _Checkpoint:handle.
        bf = bh:buffer-field( "_Checkpoint-Duration" ).
        if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then tt_checkpt.duration = bf:buffer-value().
        bf = bh:buffer-field( "_Checkpoint-SyncTime" ).
        if bf <> ? and bf:buffer-value() <> "" and bf:buffer-value() <> ? then tt_checkpt.syncTime = bf:buffer-value().
      end.

    assign
      tt_checkpt.ckptClstr = buffer _Checkpoint:handle:buffer-field( "_Checkpoint-Cluster" ):buffer-value
      tt_checkpt.biWrt     = buffer _Checkpoint:handle:buffer-field( "_Checkpoint-bi-writes" ):buffer-value
      tt_checkpt.biWrtTm   = buffer _Checkpoint:handle:buffer-field( "_Checkpoint-bi-write-time" ):buffer-value
      tt_checkpt.dbWrt     = buffer _Checkpoint:handle:buffer-field( "_Checkpoint-buffers" ):buffer-value
      tt_checkpt.dbWrtTm   = buffer _Checkpoint:handle:buffer-field( "_Checkpoint-b-write-time" ):buffer-value
    no-error.

    if tt_checkpt.fini = ? then tt_checkpt.fini = "".

  end.

  add2ds( temp-table tt_checkpt:default-buffer-handle ).

&ENDIF

  return.

end.

return.
