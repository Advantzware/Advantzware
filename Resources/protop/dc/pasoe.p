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
 * pasoe.p
 *
 *
 * pasoe (aka "pacific") info
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 5, 2003
 *
 */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 12.0 &THEN

block-level on error undo, throw.

using Progress.Json.ObjectModel.JsonObject.
using Progress.Json.ObjectModel.JsonArray.
using Progress.Json.ObjectModel.JsonDataType.
using Progress.Json.ObjectModel.ObjectModelParser.
using OpenEdge.Core.JsonDataTypeEnum.

&ENDIF

{lib/protop.i}
{lib/tick.i}

define output parameter dcDescription as character no-undo initial "pasoeInfo".

define temp-table tt_pasoeInfo no-undo
  field xid       as integer  format ">>9" label "Id"
  field zLevel        as integer
  field zOrder        as integer
  field probeName    as character format "x(47)" label "Probe Name"
  field colorCode     as character                label "Color Code"
  field probeHealth   as decimal initial ?        label "Health"
  field probeValue    as decimal initial ?        label "Value"
  field probeMarginal as logical initial ?        label "Marginal"
  field probeCritical as logical initial ?        label "Critical"
  field probeFailOnCritical as logical initial ?  label "Fail"
  field probeIgnore   as logical initial ?        label "Ignore"
  field probeFailedBy as character format "x(40)" label "Failed By"
  index xid-idx is unique primary xid ascending
.

{lib/dumpTT.i tt_pasoeInfo}


define variable seqId     as integer     no-undo.
define variable probeTS   as datetime-tz no-undo.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 12.0 &THEN

procedure load_probe:

  define input parameter pProbe    as JsonObject.
  define input parameter pParentId as integer.
  define input parameter pIdx      as integer.

  define variable loop       as integer no-undo.
  define variable cnt        as integer no-undo.
  define variable valueData  as character no-undo.
  define variable valueType  as integer no-undo.

  define buffer lbProbe for tt_pasoeInfo.

  define variable probes     as JsonArray no-undo.
  define variable resultData as JsonObject no-undo.

  create lbProbe.
  assign
    seqId               = seqId + 1
    lbProbe.xid         = seqId
    lbProbe.zLevel      = pParentId
    lbProbe.zOrder      = pIdx
  .

  if pProbe:Has('name') and pProbe:GetType('name') eq JsonDataType:STRING then
    lbProbe.probeName = fill( " ", pParentId * 2 ) + pProbe:GetCharacter('name').

  if pProbe:Has('color') and pProbe:GetType('color') eq JsonDataType:STRING then
    lbProbe.colorCode = pProbe:GetCharacter('color').

  if pProbe:Has('timestamp') and pProbe:GetType('timestamp') eq JsonDataType:STRING then
    probeTS = pProbe:GetDatetimeTZ('timestamp').

  if  pProbe:Has('result') and pProbe:GetType('result') eq JsonDataType:OBJECT then
    do:

      assign
        resultData = pProbe:GetJsonObject('result')
        valueType  = resultData:GetType('value')
        valueData  = resultData:GetJsonText('value')
      .

      if resultData:Has('health') and resultData:GetType('health') eq JsonDataType:NUMBER then
        lbProbe.probeHealth = resultData:GetDecimal('health').

      if resultData:Has('value') and resultData:GetType('value') eq JsonDataType:NUMBER then
        lbProbe.probeValue = resultData:GetDecimal('value').

      if resultData:Has('marginal') and resultData:GetType('marginal') eq JsonDataType:BOOLEAN then
        lbProbe.probeMarginal = resultData:GetLogical('marginal').

      if resultData:Has('critical') and resultData:GetType('critical') eq JsonDataType:BOOLEAN then
        lbProbe.probeCritical = resultData:GetLogical('critical').

      if resultData:Has('ignore') and resultData:GetType('ignore') eq JsonDataType:BOOLEAN then
        lbProbe.probeIgnore = resultData:GetLogical('ignore').

      if resultData:Has('failOnCritical') and resultData:GetType('failOnCritical') eq JsonDataType:BOOLEAN then
        lbProbe.probefailOnCritical = resultData:GetLogical('failOnCritical').

      if resultData:Has('failedBy') and resultData:GetType('failedBy') eq JsonDataType:STRING then
        lbProbe.probeFailedBy = resultData:GetCharacter('failedBy').

    end.

  if pProbe:Has('probes') and pProbe:GetType('probes') eq JsonDataType:ARRAY then       
    do:

      assign
        probes = pProbe:GetJsonArray('probes')
        cnt    = probes:Length
      .

      do loop = 1 to cnt:
        run load_probe ( probes:GetJsonObject(loop), pParentId + 1, loop ).
      end.

    end.

  delete object resultData.

  return.

end.

&ENDIF


procedure mon-init:

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable jsonFile as character no-undo.
  define variable errFile  as character no-undo.

  empty temp-table tt_pasoeInfo.

  seqId = 0.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) < 12.0 &THEN

  create tt_pasoeInfo.
  assign
    tt_pasoeInfo.xid         = 1
    tt_pasoeInfo.zLevel      = 0
    tt_pasoeInfo.zOrder      = 0
    tt_pasoeInfo.probeName   = "PASOE is only supported with OE12 or greater".
  .

&ELSE

  define variable probeData as JsonObject no-undo.

  if opsys = "unix" then
    do:
      jsonFile = substitute( "&1/&2.&3", pt_tmpdir, pt_shortname, "pasoe.json" ).
      errFile  = substitute( "&1/&2.&3", pt_tmpdir, pt_shortname, "pasoe.err" ).
      os-command value( substitute( "pasoehc.sh > &1 2>&2", jsonFile, errFile )).
    end.
   else
    do:
      jsonFile = substitute( "&1~\&2.&3", pt_tmpdir, pt_shortname, "pasoe.json" ).
      errFile  = substitute( "&1~\&2.&3", pt_tmpdir, pt_shortname, "pasoe.err" ).
      os-command value( substitute( "pasoehc.bat > &1 2>&2", jsonFile, errFile )).
    end.

  probeData = cast(( new ObjectModelParser()):ParseFile( jsonFile ), JsonObject ).

  run load_Probe( probeData, 0, 0 ).

  delete object probeData.

&ENDIF

  add2ds( temp-table tt_pasoeInfo:default-buffer-handle ).

  return.

end.

return.
