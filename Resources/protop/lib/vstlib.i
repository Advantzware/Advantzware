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
 * vstlib.i
 *
 * VST library definitions
 *
 */

function aiInfo returns character ( input vAiFile as character, output vAiGenNum as integer ) in super.
function chkai returns integer ( output ai_exts as integer, output ai_full as integer, output ai_empty as integer ) in super.
function chkarea returns integer ( input threshold as decimal, output worst as decimal ) in super.
function chkptNum returns integer ( input-output oldbi as integer ) in super.

function connectFlags returns character ( input cnxId as integer ) in super.
function connectName returns character ( input cnxId as integer, input cnxFlags as character ) in super.
function lastStatement returns character( input cnxId as integer, output lineNum as integer, output procName as character ) in super.

function isAIEnabled returns logical () in super.
function isReplSource returns logical () in super.
function isReplTarget returns logical () in super.
function isBackupRunning returns logical () in super.
function isWorkgroup returns logical () in super.

function getStartupX returns character ( input v as character, input p1 as character, input p2 as character ) in super.

/* end vstlib.i */
