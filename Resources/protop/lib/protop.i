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
 * protop.i
 *
 * Header file for protop family of programs
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
 */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 11.4 &THEN
&global-define	FASTLOCK	true
&ELSE
&global-define	FASTLOCK	false
&ENDIF

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 11.0 &THEN
&global-define	OE11		"yes"
&global-define	xDEBUGTT	false
&ENDIF

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.2 AND PROVERSION >= "10.2B" &THEN
&global-define	NOSERIALIZE	serialize-hidden
&ENDIF

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
&global-define	BIGINT		int64
&ELSE
&global-define	BIGINT		decimal
&ENDIF

{lib/v9.i}

/* use extended _connect fields: -client, -cache*
 */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1C" &THEN
&global-define	CONNECTX	"yes"
&ELSE
&global-define	CONNECTX	"no"
&ENDIF

define stream inStrm.

define new global shared variable dbgMode as integer no-undo initial 1.

/* The values for these are defined in etc/protop.cfg and set by lib/protop-cfg.p
 *
 * someday everyone will have OO and I will replace these with a gsv class
 * or something of that ilk
 *
 */

define new global shared variable pt_shortname   as character no-undo.
define new global shared variable pt_uniqName    as character no-undo.
define new global shared variable pt_server      as character no-undo.
define new global shared variable pt_resrcType   as character no-undo.

define new global shared variable pt_tmpdir      as character no-undo initial "/tmp".
define new global shared variable pt_logdir      as character no-undo initial "/tmp".
define new global shared variable pt_rptdir      as character no-undo initial "/tmp".
define new global shared variable pt_logname     as character no-undo initial "&5.&2.&3".
define new global shared variable pt_mailcmd     as character no-undo initial 'mailx "-s &1" '.

define new global shared variable pt_votrx       as integer   no-undo initial 1800.
define new global shared variable pt_lktbllim    as integer   no-undo initial 0.
define new global shared variable pt_bkupstale   as integer   no-undo initial 26.
define new global shared variable pt_bogomips    as integer   no-undo initial 1000000.
define new global shared variable pt_ioresp      as integer   no-undo initial 100.
define new global shared variable pt_ioFileName  as character no-undo.
define new global shared variable pt_dfCmd       as character no-undo.

define new global shared variable pt_AICheckInterval   as integer no-undo initial 60.
define new global shared variable pt_PICACheckInterval as integer no-undo initial 60.
define new global shared variable pt_appsrvStuck       as integer no-undo initial 120.

define new global shared variable pt_bibkupAlert as integer   no-undo.		/* alert on long bi backup phase		*/
define new global shared variable pt_bibkupAlarm as integer   no-undo.		/* alarm on long bi backup phase		*/
define new global shared variable pt_bibkupPage  as integer   no-undo.		/* page  on long bi backup phase		*/

define new global shared variable pt_bkupAlert   as integer   no-undo.		/* alert on long db backup			*/
define new global shared variable pt_bkupAlarm   as integer   no-undo.		/* alarm on long db backup			*/
define new global shared variable pt_bkupPage    as integer   no-undo.		/* page  on long db backup			*/

define new global shared variable pt_userLock    as logical   no-undo.		/* enable _userLock  data?			*/
define new global shared variable pt_doZippy     as logical   no-undo.		/* enable "user experience" (aka "zippy")?	*/
define new global shared variable pt_useRFUtil   as logical   no-undo.		/* use rfutil to gather after-imaging status?	*/

define new global shared variable pt_updAreaData as integer   no-undo.		/* how often should we report storage area xref details from dbanalys? */

define new global shared variable pt_zoomTo      as integer no-undo.		/* new zoomed monInt				*/

define new global shared variable ptDBName     as character no-undo.			/* the db logical name		*/
define new global shared variable rowLimit     as integer   no-undo initial 100.	/* 50?				*/

/* do-sumSample() manipulates these -- ugly, ugly, ugly... (obsolete?)
 * 
 */

define new global shared variable stime as integer no-undo.			/* start time				*/
define new global shared variable ltime as integer no-undo.			/* last time				*/
define new global shared variable xtime as integer no-undo.			/* total time				*/
define new global shared variable itime as integer no-undo.			/* iteration time			*/

define new global shared variable chkp-base   as integer no-undo initial ?.	/* cover for the lack of a VST field	*/

										/* corresponding to base checkpoint#	*/
/** Global Shared Temp Table Definitions
 **
 ** Yup, they're shared.  But this stuff makes no sense across session boundaries anyway.
 ** And a shared temp-table is logically the same as a db table so who really cares?
 **
 **/

/* cache _File and _Index records so that we don't keep hitting the db to translate
 */

define new global shared temp-table tt_tbl no-undo
  field xid      as integer						/* _File._File-Num		*/
  field tstatid  as integer						/* _TableStat._TableStat-Id	*/
  field areaNum  as integer						/* _Storage-Object._Area-Number	*/
  field tblPool  as character						/* get-bits( _object-attrib, 7, 1 ) = 1	*/
  field tblname  as character						/* _File._File-Name		*/
  index xid-idx is unique primary xid.

define new global shared temp-table tt_idx no-undo
  field xid      as integer						/* _Index._Idx-Num		*/
  field istatid  as integer						/* _IndexStat._IndexStat-Id	*/
  field idxname  as character						/* _Index._Idx-Name		*/
  field idxnote  as character
  field idxRoot  as {&BIGINT}						/* _Storage-Object._Object-Root	*/
  field tblnum   as integer						/* _File._File-Num		*/
  field areaNum  as integer						/* _Storage-Object._Area-Number	*/
  field idxPool  as character						/* get-bits( _object-attrib, 7, 1 ) = 1	*/
  field tblname  as character						/* _File._File-Name		*/
  index xid-idx is unique primary xid.

define new global shared temp-table tt_areaExtent no-undo
  field areaNum  as integer						/* _areaExtent._Area-Number	*/
  field extNum   as integer						/* _areaExtent._Extent-Number	*/

  field extSize  as decimal						/* _areaExtent._Extent-Size	*/
  field extType  as integer						/* _areaExtent._Extent-Type	*/
  field extPath  as character						/* _areaExtent._Extent-Path	*/

  index ae-idx is unique primary areaNum extNum.

define new global shared temp-table tt_area no-undo
  field xid      as integer    format ">>>9"
  field SANum    as integer    format ">>>9"        label "#"
  field areaPool as character  format "x(2)"        label "BX"
/*field areaStatus-Id as {&BIGINT} format ">>>9"    label "Id" */
  field SAName   as character  format "x(30)"       label "Area Name"
  field allocGB  as decimal    format ">>>>>9.99"   label "Allocated"
  field varGB    as decimal    format ">>>>>9.99"   label "Variable"
  field totGB    as decimal    format ">>>>>>9.99"  label "Tot GB"
  field hiGB     as decimal    format ">>>>>9.99"   label "Hi Water" {&NOSERIALIZE}
  field freeGB   as decimal    format ">>>>>9.99"   label "Free GB"
  field pctAlloc as decimal    format ">>>>>9%"     label "%Alloc"
  field pctLastX as decimal    format ">>>>9%"      label "%LastX"

  field blkszkb as integer     format ">>9"         label "BSZ"
  field rpb     as integer     format ">>9"         label "RPB"
  field clstrsz as integer     format ">>9"         label "CSZ"

  field numTbls as integer     format ">>>>9"       label "#Tbls"
  field numIdxs as integer     format ">>>>9"       label "#Idxs"
  field numLOBs as integer     format ">>>>9"       label "#LOBs"

  field numExts as integer     format ">>>>9"       label "#Exts"
  field hasVar  as logical     format "Yes/No"      label "Var?"

  field xnote   as character   format "x"           label "*"

  field areaMaxPct as decimal  format ">>9.999%"    label "Max%"
  field idx3264    as decimal  format ">>9.999%"    label "Bug%"

  index pctAlloc-idx is primary pctAlloc descending
  index pctLastX-idx pctLastX pctAlloc descending
  index allocGB-idx allocGB descending
  index totGB-idx totGB descending
  index xid-idx is unique xid
  index SANum-idx is unique SANum
  index SAName-idx is unique SAName
.

{ssg/sausage16.i}
{lib/protoplib.i}
{lib/vstlib.i}

function add2ds returns logical ( input h as handle ) in super.
function getTempTableHandle returns handle ( input n as character ) in super.

/* end protop.i */
