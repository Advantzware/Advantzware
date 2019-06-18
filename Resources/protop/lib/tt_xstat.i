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
 * tt_xstat.i
 *
 */

define variable ratio-calc as integer no-undo.

define temp-table tt_xstat no-undo
  field xid       as integer
  field xvalid    as logical
  field xname     as character
  field misc1     as character
  field misc2     as character
  field misc3     as character
  field misc4     as character
  field misc5     as character
  field stat1     as decimal extent 5
  field stat2     as decimal extent 5
  field stat3     as decimal extent 5
  field stat4     as decimal extent 5
  field stat5     as decimal extent 5
  field stat6     as decimal extent 5
  field stat-ratio  as decimal
  field stat-ratio2 as decimal
  index xid-idx is unique primary xid.

/* this will be called by the including procedure's mon-update procedure
 *
 */

procedure update_xstat:

  define input parameter p_xid   as integer   no-undo.
  define input parameter p_xname as character no-undo.
  define input parameter p_misc1 as character no-undo.
  define input parameter p_misc2 as character no-undo.
  define input parameter p_misc3 as character no-undo.
  define input parameter p_misc4 as character no-undo.
  define input parameter p_misc5 as character no-undo.
  define input parameter p_this1 as decimal   no-undo.
  define input parameter p_this2 as decimal   no-undo.
  define input parameter p_this3 as decimal   no-undo.
  define input parameter p_this4 as decimal   no-undo.
  define input parameter p_this5 as decimal   no-undo.
  define input parameter p_this6 as decimal   no-undo.

  find tt_xstat exclusive-lock where tt_xstat.xid = p_xid no-error.

  if not available tt_xstat then
    do:

      create tt_xstat.
      assign
        tt_xstat.xvalid = yes
        tt_xstat.xid    = p_xid
        tt_xstat.xname  = p_xname
        tt_xstat.misc1  = p_misc1
        tt_xstat.misc2  = p_misc2
        tt_xstat.misc3  = p_misc3
        tt_xstat.misc4  = p_misc4
        tt_xstat.misc5  = p_misc5
        {lib/init-xrec.i tt_xstat.stat1 p_this1}
        {lib/init-xrec.i tt_xstat.stat2 p_this2}
        {lib/init-xrec.i tt_xstat.stat3 p_this3}
        {lib/init-xrec.i tt_xstat.stat4 p_this4}
        {lib/init-xrec.i tt_xstat.stat5 p_this5}
        {lib/init-xrec.i tt_xstat.stat6 p_this6}
      .

    end.
   else
    do:

      assign
        tt_xstat.xvalid   = yes				/* is this xid active?		*/
        tt_xstat.xname    = p_xname			/* reuse of ids is possible...	*/
        tt_xstat.misc1    = p_misc1
        tt_xstat.misc2    = p_misc2
        tt_xstat.misc3    = p_misc3
        tt_xstat.misc4    = p_misc4
        tt_xstat.misc5    = p_misc5
        tt_xstat.stat1[3] = p_this1
        tt_xstat.stat2[3] = p_this2
        tt_xstat.stat3[3] = p_this3
        tt_xstat.stat4[3] = p_this4
        tt_xstat.stat5[3] = p_this5
        tt_xstat.stat6[3] = p_this6
      .

      if tt_xstat.stat1[3] < tt_xstat.stat1[3] then	/* detect reuse of an id (stat rolling backwards...) 	*/
        assign
          tt_xstat.xname  = p_xname
          {lib/init-xrec.i tt_xstat.stat1 p_this1}
          {lib/init-xrec.i tt_xstat.stat2 p_this2}
          {lib/init-xrec.i tt_xstat.stat3 p_this3}
          {lib/init-xrec.i tt_xstat.stat4 p_this4}
          {lib/init-xrec.i tt_xstat.stat5 p_this5}
          {lib/init-xrec.i tt_xstat.stat6 p_this6}
        .

    end.

  return.

end.

/* this will be called by the including procedure's mon-update procedure
 *
 */

procedure age_xstat:

  define variable x as integer   no-undo.
  define variable z as integer   no-undo.
  define variable r as character no-undo case-sensitive initial "r".
  define variable s as character no-undo case-sensitive initial "s".

  publish "get-RateRaw" ( output r ).
  publish "get-SumSample" ( output s ).

  if s = "S" then
    assign
      x = 4
      z = xtime.
   else
    assign
      x = 5
      z = itime.

  for each tt_xstat exclusive-lock:

    if tt_xstat.xvalid = no then
      delete tt_xstat.
     else
      do:
        assign
          tt_xstat.xvalid = no
          {lib/upd-xrec.i tt_xstat.stat1 tt_xstat.stat1[3]}
          {lib/upd-xrec.i tt_xstat.stat2 tt_xstat.stat2[3]}
          {lib/upd-xrec.i tt_xstat.stat3 tt_xstat.stat3[3]}
          {lib/upd-xrec.i tt_xstat.stat4 tt_xstat.stat4[3]}
          {lib/upd-xrec.i tt_xstat.stat5 tt_xstat.stat5[3]}
          {lib/upd-xrec.i tt_xstat.stat6 tt_xstat.stat6[3]}
        .

        if ratio-calc = 0 then
          assign
            tt_xstat.stat-ratio  = 100 * (( tt_xstat.stat1[x] - tt_xstat.stat2[x] ) / tt_xstat.stat1[x] )
            tt_xstat.stat-ratio  = ( if tt_xstat.stat-ratio = ? then 0 else tt_xstat.stat-ratio )
            tt_xstat.stat-ratio2  = 100 * (( tt_xstat.stat3[x] - tt_xstat.stat4[x] ) / tt_xstat.stat3[x] )
            tt_xstat.stat-ratio2  = ( if tt_xstat.stat-ratio2 = ? then 0 else tt_xstat.stat-ratio2 )
          .
         else if ratio-calc = 1 then
          assign
            tt_xstat.stat-ratio  = 100 * ( tt_xstat.stat1[x] / tt_xstat.stat2[x] )
            tt_xstat.stat-ratio  = ( if tt_xstat.stat-ratio = ? then 0 else tt_xstat.stat-ratio )
            tt_xstat.stat-ratio2  = 100 * ( tt_xstat.stat3[x] / tt_xstat.stat4[x] )
            tt_xstat.stat-ratio2  = ( if tt_xstat.stat-ratio2 = ? then 0 else tt_xstat.stat-ratio2 )
          .
      end.

  end.

  return.

end.
