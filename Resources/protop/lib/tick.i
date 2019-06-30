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
 * tick.i
 *
 *
 * common routine to handle "clock ticks"
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	January 21, 2012
 *
 */

{lib/v9.i}

define new global shared variable rawMode  as integer no-undo initial 5.
define new global shared variable timeMode as integer no-undo initial 2.

define new global shared variable x as integer no-undo initial 5.
define new global shared variable z as decimal no-undo.

define variable y as decimal no-undo.

define variable initTick as integer no-undo.
define variable prevTick as integer no-undo.


procedure updTick:

&IF DEFINED( OE10 ) &THEN
  if initTick = 0 then initTick = mtime.
  assign
    x = rawMode /* ( if rawMode then 4 else 5 ) */
    z = mtime - prevTick
    z = ( if z >= 0 then z else 86400000 - prevTick + mtime )   /* handle rolling over midnight */
    z = z / 1000
    y = mtime - initTick
    y = y / 1000
    prevTick = mtime
  .
&ELSE
  if initTick = 0 then initTick = etime.
  assign
    x = rawMode /* ( if rawMode then 4 else 5 ) */
    z = etime - prevTick
    z = ( if z >= 0 then z else 5000 )   /* etime doesn't rollover at midnite -- but a mistaken etime(yes) somewhere would be bad... */
    z = z / 1000
    y = etime - initTick
    y = y / 1000
    prevTick = etime
  .
&ENDIF

  /* if rawMode <> 5 then z = 1. */

  if timeMode = 1 then z = 1.

  return.

end.

/* end tick.i */
