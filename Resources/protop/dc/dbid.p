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
 * dbid.p
 *
 */

{lib/protop.i}
{lib/protoplib.i}

define output parameter dcDescription as character no-undo initial "DBId".

define temp-table tt_DBId no-undo

  field dbidLogName   as character label "DB Logical Name"
  field dbidPhysName  as character label "DB Physical Name"
  field dbidHost      as character label "DB Host Name"

  field pt_dbAccess   as {&BIGINT}
  field pt_dbRead     as {&BIGINT}

  index dbidLogName-idx is unique primary dbidLogName
.

{lib/dumpTT.i tt_DBId}

procedure mon-init:
  return.
end.

define variable dbidx as character no-undo.

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable dbCnxParam as character no-undo.

  find tt_dbid no-error.
  if not available( tt_dbid ) then create tt_dbid.

  if tt_dbid.dbidHost = "" or tt_dbid.dbidHost = ? then
    do:

      if connected( "dictdb" ) then
        dbCnxParam = dbParam( ldbName( "DICTDB" )).

      if index( dbCnxParam, "-H " ) > 0 then
        assign
          tt_dbid.dbidHost = entry( 2, entry( 1, substring( dbCnxParam, index( dbCnxParam, "-H " ))), " " )
        .
       else
        do:
          if       os-getenv( "PTSERVER" )     <> ? then tt_dbid.dbidHost = os-getenv( "PTSERVER" ).
           else if os-getenv( "HOST" )         <> ? then tt_dbid.dbidHost = os-getenv( "HOST" ).
           else if os-getenv( "HOSTNAME" )     <> ? then tt_dbid.dbidHost = os-getenv( "HOSTNAME" ).
           else if os-getenv( "COMPUTERNAME" ) <> ? then tt_dbid.dbidHost = os-getenv( "COMPUTERNAME" ).
        end.

      if ( tt_dbid.dbidHost = "" or tt_dbid.dbidHost = ? ) and opsys = "UNIX" then
        do:
          input stream inStrm through value( "uname -n" ).
          import stream inStrm tt_dbid.dbidHost.
          input stream inStrm close.
        end.

      if tt_dbid.dbidHost = "" or tt_dbid.dbidHost = ? then
        tt_dbid.dbidHost = "Unknown Host".

    end.

  if connected( "dictdb" ) then
    assign
      tt_dbid.dbidLogName    = ldbName( "DICTDB" )
      tt_dbid.dbidPhysName   = pdbName( "DICTDB" )
    .
   else
    assign
      tt_dbid.dbidLogName    = "n/a"
      tt_dbid.dbidPhysName   = "n/a"
    .

  dbidx = tt_dbid.dbidLogName.

  add2ds( temp-table tt_dbid:default-buffer-handle ).

  return.

end.

define new global shared variable programmerMode as logical  no-undo.

procedure updPTStats:

  define input parameter logRd as {&BIGINT} no-undo.
  define input parameter osRd  as {&BIGINT} no-undo.

  find tt_dbid no-error.
  if not available( tt_dbid ) then create tt_dbid.
  assign
    pt_dbAccess = logRd
    pt_dbRead   = osRd
    tt_dbid.dbidLogName = substitute( "&1 &2 &3 &4", dbidx, logRd, osRd, ( if programmerMode = yes then "programmerMode" else "" ))
  .

  return.

end.

subscribe to "updPTStats" anywhere run-procedure "updPTStats".

return.
