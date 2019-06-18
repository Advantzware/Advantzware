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
 * tenant.p
 *
 *
 * Tenant info -- 11.0 +
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 5, 2003
 *
 */

{lib/protop.i}
{lib/tick.i}

define output parameter dcDescription as character no-undo initial "TenantInfo".

define temp-table tt_tenant no-undo
  field xid         as integer   format "->>>>9"
  field tid         as integer   format "->>>>9"  label "Id"
  field tenantName  as character format "x(18)"   label "Name"
  field tenantType  as character format "x(8)"    label "Type"
  field secDomName  as character format "x(50)"   label "Security Domain Names"
  field secDomType  as character format "x(30)"   label "Domain Types"
  field tenantAlloc as character format "x(10)"   label "Alloc"

  index tid-idx         is unique primary tid ascending
  index tenantName-idx  tenantName
  index tenantType-idx  tenantType
  index secDomName-idx  secDomName
  index secDomType-idx  secDomType
  index tenantAlloc-idx tenantAlloc
  index xid-idx  is unique xid
.

{lib/dumpTT.i tt_tenant}

procedure mon-init:

  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable tenTypeList as character no-undo initial "Default,Regular,Super".

  define variable c as character no-undo.

  run updTick.

  /* for now we'll just re-fetch every time... but this might be a reasonable place to use "valid"
   */

  empty temp-table tt_tenant.

&IF DEFINED( OE11 ) &THEN

  for each dictdb._Tenant no-lock:

    create tt_tenant.

    assign
      tt_tenant.xid         = _tenant._tenantId
      tt_tenant.tid         = _tenant._tenantId
      tt_tenant.tenantName  = _tenant._tenant-name
      tt_tenant.tenantType  = entry( _tenant._tenant-type + 1, tenTypeList )
      tt_tenant.tenantAlloc = _tenant._tenant-allocation-default
      tt_tenant.secDomName  = ""
      tt_tenant.secDomType  = ""
    .

    c = "".
    for each dictdb._sec-authentication-domain no-lock where _sec-authentication-domain._tenant-name = _tenant._tenant-name:

      tt_tenant.secDomName = tt_tenant.secDomName + c + ( if _sec-authentication-domain._domain-name = '' then '""' else _sec-authentication-domain._domain-name ).
      if num-entries( tt_tenant.secDomName ) > 1 and length( tt_tenant.secDomName ) > 50 then tt_tenant.secDomName = substring( tt_tenant.secDomName, 1, 47 ) + "...".

      if lookup( _sec-authentication-domain._domain-type, tt_tenant.secDomType ) < 1 then tt_tenant.secDomType = tt_tenant.secDomType + c + _sec-authentication-domain._domain-type.
      if num-entries( tt_tenant.secDomType ) > 1 and length( tt_tenant.secDomType ) > 30 then tt_tenant.secDomType = substring( tt_tenant.secDomName, 1, 27 ) + "...".

      c = ",".

    end.

  end.

&ENDIF

  add2ds( temp-table tt_tenant:default-buffer-handle ).

  return.

end.

return.
