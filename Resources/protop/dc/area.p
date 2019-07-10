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
 * area.p
 *
 *
 * Storage Area Statistics monitoring.
 *
 *
 * To Do:
 *
 *	Area specific notes:
 *	  - bi??? clusters in use?
 *	  Fixed/Variable extent indicator
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	August 28, 2003
 *
 *
 * History:
 *
 *	Implemented Dmitri Levin's RPB solution
 *	September 22, 2003
 *
 */

{lib/protop.i}

define output parameter dcDescription as character no-undo initial "StorageAreas".

/*** moved to lib/vstlib.p cache
 ***
define temp-table tt_area no-undo
 ...
 ***
 ***/

{lib/dumpTT.i tt_area}

/* initialize
 *
 */

define variable numAreas as integer no-undo.

procedure mon-init:

  run initAreaInfo.

  for each tt_area:
    numAreas = numAreas + 1.
  end.

  return.

end.

/* update
 *
 */

procedure mon-update:

  define input parameter argList as character no-undo.

  run getAreaVarInfo.

  publish "dba_trend".

  publish "resizeBrowse" ( "areas", numAreas ).

  add2ds( temp-table tt_area:default-buffer-handle ).

  return.

end.

/** Initialize PP
 **
 **/

subscribe to "mon-init"    anywhere run-procedure "mon-init".
subscribe to "mon-update"  anywhere run-procedure "mon-update".

return.
