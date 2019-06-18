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
 * aiinfo.p
 *
 * ai ainfo
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

define output parameter dcDescription as character no-undo initial "aiInfo".

define temp-table tt_aiinfo no-undo
  field xid        as integer   format ">>9"             label "Id"
  field aiName     as character format "x(60)"           label "Extent"
  field aiExtType  as character format "x(8)"            label "Extent Type"
  field aiSize     as integer   format ">>>>>>>>>>>"     label "Size"
  field aiSeqNum   as integer   format "->>>>>>>>>"      label "AI Seq#"
  field aiStatus   as character format "x(20)"           label "Status"
  field xtra       as character format "x(38)"           label " "

  index aiName-idx  is unique primary aiName
.

{lib/dumpTT.i tt_aiinfo}

define variable hasAIInfo as logical no-undo.
define variable useRFUtil as logical no-undo.

procedure chkAIInfo:

  hasAIInfo = no.
  find dictdb._File  no-lock where _File-Name = "_AreaStatus".
  find dictdb._Field no-lock where _Field._File-recid = recid( _File ) and _Field-Name = "_AreaStatus-AI-Seq" no-error.
  if available _Field then hasAIInfo = yes.

  return.

end.


procedure mon-init:

  run chkAIInfo.

  useRFUtil = ( if os-getenv( "USERFUTIL" ) = "yes" then yes else no ).

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable stuff as character no-undo extent 8.

  empty temp-table tt_aiinfo.

  /*
   * rfutil /home/tom/tmp/s2k -C aimage list
   * OpenEdge Release 11.7.2 as of Tue Oct 24 18:20:59 EDT 2017
   * 
   * Extent:  1
   * Status:  Empty
   *   Type:  Variable Length
   *   Path:  /home/tom/tmp/s2k.a1
   *   Size:  120
   *   Used:  0
   *  Start:  N/A
   * LastOp:  N/A
   *  Seqno:  0
   * 
   * Extent:  2
   * Status:  Empty
   *   Type:  Variable Length
   *   Path:  /home/tom/tmp/s2k.a2
   *   Size:  120
   *   Used:  0
   *  Start:  N/A
   * LastOp:  N/A
   *  Seqno:  0
   * 
   * Extent:  3
   * Status:  Busy
   *   Type:  Variable Length
   *   Path:  /home/tom/tmp/s2k.a3
   *   Size:  120
   *   Used:  7
   *  Start:  Thu Jun 14 14:51:42 2018
   * LastOp:  N/A
   *  Seqno:  15
   *
   * ...
   *
   */

  if hasAIInfo = no or useRFUtil = yes then
    do:

      if opsys = "unix" then
        input stream inStrm through value( substitute(      "rfutil &1 -C aimage list -cpinternal &2 -cpstream &3", pdbname(1), session:cpinternal, session:cpstream )).
       else
        input stream inStrm through value( substitute( "call rfutil &1 -C aimage list -cpinternal &2 -cpstream &3", pdbname(1), session:cpinternal, session:cpstream )).

      repeat:

        stuff = "".
        import stream inStrm stuff.

        if stuff[1] = "Extent:" then
          do:
            create tt_aiinfo.
            tt_aiinfo.xid = integer( trim( stuff[2] )) no-error.
          end.
         else if stuff[1] = "Status:" then tt_aiinfo.aiStatus = stuff[2].
         else if stuff[1] = "Type:"   then tt_aiinfo.aiExtType = stuff[2].
         else if stuff[1] = "Path:"   then tt_aiinfo.aiName = stuff[2].
         else if stuff[1] = "Size:"   then tt_aiinfo.aiSize = integer( trim( stuff[2] )) no-error.
         else if stuff[1] = "Seqno:"  then tt_aiinfo.aiSeqNum = integer( trim( stuff[2] )) no-error.

      end.

      input stream inStrm close.

    end.
   else
    do:

      for each _areaStatus no-lock where _areaStatus-areaName begins "after image area":

        file-info:file-name = _areaStatus-lastExtent.

        create tt_aiinfo.
        assign
          tt_aiinfo.xid    = _areaStatus-areaNum
          tt_aiinfo.aiName = _areaStatus-lastExtent
          tt_aiinfo.aiSize = file-info:file-size
        .

        find tt_AreaExtent no-lock where tt_areaExtent.extPath = _areaStatus-lastExtent no-error.
        if available tt_areaExtent then
          tt_aiinfo.aiExtType  = ( if tt_areaExtent.extType >= 4 and tt_areaExtent.extType <= 7 then "Variable" else "Fixed" ).

        if hasAIInfo = yes then
          assign
            tt_aiinfo.aiStatus = ( buffer _areaStatus:handle:buffer-field( "_areaStatus-State" ):buffer-value )
            tt_aiinfo.aiSeqNum = ( buffer _areaStatus:handle:buffer-field( "_areaStatus-AI-Seq" ):buffer-value )
          .

        end.

    end.


  add2ds( temp-table tt_aiinfo:default-buffer-handle ).

  return.

end.

return.
