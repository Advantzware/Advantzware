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
 * tt_uindex.i
 *
 */

define temp-table tt_uindex no-undo
  field xid        as integer
  field xvalid     as logical
  field idxNum     as integer
  field idxName    as character
  field tblName    as character
  field areaNum    as integer
  field idxRoot    as {&BIGINT}
  field idxnote    as character
  field usrNum     as integer
  field usrName    as character
  field idx-cre    as decimal extent 5
  field idx-rd     as decimal extent 5
  field idx-split  as decimal extent 5
  field idx-del    as decimal extent 5
  field idx-blkdel as decimal extent 5
  index xid-idx is unique primary xid.

define variable iBase   as integer no-undo.
define variable iRange  as integer no-undo.
define variable iUsed   as integer no-undo.

iBase = recid( _indexStat ).
find last  dictDB._indexStat no-lock.
iRange  = recid( _indexStat ).

iUsed = 0.
for each dictDB._index no-lock:
  iUsed = iUsed + 1.
end.

procedure upd-tt_uindex:

  define input parameter p_idx as integer no-undo.

  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-SumSample( output x, output z ).

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
  find dictdb._userIndexStat no-lock where _userIndexStat-id = p_idx no-error.
  find tt_idx no-lock where tt_idx.xid = _userIndexStat-num no-error.
&ENDIF

  if not available tt_idx then return.

  find dictdb._connect no-lock
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
    where _connect-id = _userIndexStat-conn + 1
&ENDIF
    no-error.

  if _connect-usr = ? then return.  /* it might be a good idea to zero out any existing tt_uindex 1st... */

  find tt_uindex exclusive-lock where tt_uindex.xid = p_idx no-error.

  if not available tt_uindex then
    do:
      create tt_uindex.
      assign
        tt_uindex.xid      = p_idx
        tt_uindex.xvalid   = yes
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
        tt_uindex.idxName  = ( if available( _userIndexStat ) then "" else "*" ) + tt_idx.tblname + "." + tt_idx.idxname
&ENDIF
        tt_uindex.idxnote  = tt_idx.idxnote
        tt_uindex.idxNum   = tt_idx.xid
        tt_uindex.idxroot  = tt_idx.idxroot
        tt_uindex.areanum  = tt_idx.areanum
      .
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
      if available _userIndexStat then		/* _userIndexStat range may not be active...	*/
        assign
          {lib/init-xrec.i tt_uindex.idx-cre    _userIndexStat-create}
          {lib/init-xrec.i tt_uindex.idx-rd     _userIndexStat-read}
          {lib/init-xrec.i tt_uindex.idx-split  _userIndexStat-split}
          {lib/init-xrec.i tt_uindex.idx-del    _userIndexStat-delete}
          {lib/init-xrec.i tt_uindex.idx-blkdel _userIndexStat-blockdelete}
        .
&ENDIF
    end.

  assign
    tt_uindex.xvalid   = yes
    tt_uindex.usrNum   = _connect-usr
    tt_uindex.usrName  = _connect-name
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
    {lib/upd-xrec.i tt_uindex.idx-cre    _userIndexStat-create}
    {lib/upd-xrec.i tt_uindex.idx-rd     _userIndexStat-read}
    {lib/upd-xrec.i tt_uindex.idx-split  _userIndexStat-split}
    {lib/upd-xrec.i tt_uindex.idx-del    _userIndexStat-delete}
    {lib/upd-xrec.i tt_uindex.idx-blkdel _userIndexStat-blockdelete}
&ENDIF
  .

  return.

end.

/* this will be called by the including procedure's mon-update procedure
 *
 */

procedure age_uindex:

  for each tt_uindex exclusive-lock:

    if tt_uindex.xvalid = no then
      delete tt_uindex.
     else
      assign
        tt_uindex.xvalid = no
      .

  end.

  return.

end.
