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
 * protoplib.i
 *
 * ProTop infrastructure library definitions
 *
 */

function uDateTime returns integer () in super.
function string2uDateTime returns integer( input p_text as character ) in super.
function searchDir returns character ( input xDir as character ) in super.
function unsignMe returns decimal ( input s as decimal, input i as integer ) in super.
function hr returns decimal ( input lr as decimal, input osr as decimal, output hr-str as character, output hr as decimal, output mr as decimal ) in super.
function do-SumSample returns logical ( output p_index as integer, output p_time  as integer ) in super.
function myPID returns character () in super.
function hilite returns logical ( input b as handle, input p_metric as character, input p_value as character, output p_attr as character ) in super.

/* end protoplib.i */
