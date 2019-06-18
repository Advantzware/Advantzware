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
 * osinfo.p
 *
 */

{lib/protop.i}
{lib/protoplib.i}

define output parameter dcDescription as character no-undo initial "OSInfo".

function getNum returns decimal ( s as character ):

  s = trim( s ).

  if       substring( s, length( s ), 1 ) = "T" then return decimal( substring( s, 1, length( s ) - 1 )) * 1024 * 1024 * 1024 * 1024.
   else if substring( s, length( s ), 1 ) = "G" then return decimal( substring( s, 1, length( s ) - 1 )) * 1024 * 1024 * 1024.
   else if substring( s, length( s ), 1 ) = "M" then return decimal( substring( s, 1, length( s ) - 1 )) * 1024 * 1024.
   else if substring( s, length( s ), 1 ) = "K" then return decimal( substring( s, 1, length( s ) - 1 )) * 1024.
   else if substring( s, length( s ), 1 ) = "B" then return decimal( substring( s, 1, length( s ) - 1 )).
   else return decimal( s ).

end.

{lib/osinfo.i}


procedure mon-init:

  run osInfo.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable inLine as character no-undo extent 16.
  define variable xLine  as character no-undo.

  find tt_OSInfo no-error.
  if not available( tt_osinfo ) then create tt_osinfo.

  if opsys begins "WIN" then
    do:
      if linuxPatch then
        osName = "Please apply the Linux patch ;)".
       else
        run getWIN32.
      linuxPatch = not( linuxPatch ).
    end.
   else
    do:
      input stream inStrm through value( "uptime" ).
      import stream inStrm unformatted xLine.
      input stream inStrm close.
      osUpTime = entry( 1, substring( xLine, index( xLine, "up" ) + 3 )).
    end.

  add2ds( temp-table tt_osinfo:default-buffer-handle ).

  return.

end.

{lib/dumpTT.i tt_OSInfo}

return.
