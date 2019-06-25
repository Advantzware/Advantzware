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
 * tt_utable.i
 *
 */

define temp-table tt_utable no-undo
  field xid      as integer
  field xvalid   as logical
  field tblNum   as integer
  field tblName  as character
  field areaNum  as integer
  field usrNum   as integer
  field usrName  as character
  field ttbl     as character
  field tbl-cre  as decimal extent 5
  field tbl-rd   as decimal extent 5
  field tbl-upd  as decimal extent 5
  field tbl-del  as decimal extent 5
  field tbl-osrd as decimal extent 5
  index xid-idx is unique primary xid.

define variable tBase   as integer no-undo.
define variable tRange  as integer no-undo.
define variable tUsed   as integer no-undo.

tBase = recid( _tableStat ).
find last  dictDB._tableStat no-lock.
tRange  = recid( _tableStat ).

tUsed = 0.
for each dictDB._file no-lock where _hidden = no:
  tUsed = tUsed + 1.
end.

procedure upd-tt_utable:

  define input parameter p_tbl as integer no-undo.

  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable bh  as handle no-undo.
  define variable bf  as handle no-undo.

  do-SumSample( output x, output z ).

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
  find dictdb._userTableStat no-lock where _userTableStat-id = p_tbl no-error.
  find tt_tbl no-lock where tt_tbl.xid = _userTableStat-num no-error.
&ENDIF

  if not available tt_tbl then return.

  find dictdb._connect no-lock
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
    where _connect-id = _userTableStat-conn + 1
&ENDIF
    no-error.

  if _connect-usr = ? then return.

  find tt_utable exclusive-lock where tt_utable.xid = p_tbl no-error.

  if not available tt_utable then
    do:
      create tt_utable.
      assign
        tt_utable.xid      = p_tbl
        tt_utable.xvalid   = yes
        tt_utable.tblName  =
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
         ( if available( _userTableStat ) then "" else "*" ) + 
&ENDIF
          tt_tbl.tblname
        tt_utable.tblNum   = tt_tbl.xid
        tt_utable.areanum  = tt_tbl.areanum
      .
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
      if available _userTableStat then		/* _userTableStat range may not be active...	*/
        do:
          assign
            {lib/init-xrec.i tt_utable.tbl-cre _userTableStat-create}
            {lib/init-xrec.i tt_utable.tbl-rd  _userTableStat-read}
            {lib/init-xrec.i tt_utable.tbl-upd _userTableStat-update}
            {lib/init-xrec.i tt_utable.tbl-del _userTableStat-delete}
          .
          if hasOSRd = no then
            tt_utable.tbl-osrd = ?.
           else
            assign
              bh = buffer _userTableStat:handle
              bf = bh:buffer-field( "_userTableStat-OSRead" )
              {lib/init-xrec.i tt_utable.tbl-osrd bf:buffer-value}
            .
        end.
&ENDIF
    end.

  assign
    tt_utable.xvalid   = yes
    tt_utable.usrNum   = _connect-usr
    tt_utable.usrName  = _connect-name
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
    {lib/upd-xrec.i tt_utable.tbl-cre _userTableStat-create}
    {lib/upd-xrec.i tt_utable.tbl-rd  _userTableStat-read}
    {lib/upd-xrec.i tt_utable.tbl-upd _userTableStat-update}
    {lib/upd-xrec.i tt_utable.tbl-del _userTableStat-delete}
  .
  if hasOSRd then
    assign
      bh = buffer _userTableStat:handle
      bf = bh:buffer-field( "_userTableStat-OSRead" )
      {lib/upd-xrec.i tt_utable.tbl-osrd bf:buffer-value}
&ENDIF
    .

  /* This uses the *base* statistics rather than the sampled data -- so it reveals
   * the historical pattern rather than current usage.
   */

  if ( tt_utable.tbl-cre[x] > 10 and tt_utable.tbl-del[x] > 10 ) and
     ( abs( tt_utable.tbl-cre[x] - tt_utable.tbl-del[x] ) < ( tt_utable.tbl-cre[x] * 0.2 )) then
    tt_utable.ttbl = "***".
   else
    tt_utable.ttbl = "".

  return.

end.

/* this will be called by the including procedure's mon-update procedure
 *
 */

procedure age_utable:

  for each tt_utable exclusive-lock:

    if tt_utable.xvalid = no then
      delete tt_utable.
     else
      assign
        tt_utable.xvalid = no
      .

  end.

  return.

end.
