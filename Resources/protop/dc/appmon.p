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
 * appmon.p
 *
 *
 * application specific status
 *
 *   # etc/appmon.cfg
 *   #
 *   # id "app name" "script" "statusList"
 *
 *   1 "EB2 .NET Interface" "./bin/ebdotnet.sh" "Running,Down"
 *   2 "QXtend Inbound Receiver" "./bin/qxtend.sh" "Running,Down" 
 *
 * the script returns an integer status code:  0 = success or "all is well",
 * because codes start at 0, code + 1 is the index into the statusList for a
 * description of any status.
 *
 * optionally the script may also return "notes" - any text beyond the code is shown
 * as a note
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

define output parameter dcDescription as character no-undo initial "appActivity".

define temp-table tt_appcfg no-undo
  field xid        as integer   format ">>9"               label "Id"
  field appName    as character format "x(40)"             label "Description"
  field appScript  as character format "x(45)"             label "Script"
  field statusList as character format "x(22)"             label "Status List"
  index xid-idx       is unique xid
.

define temp-table tt_app no-undo
  field xid        as integer   format ">>9"               label "Id"
  field appName    as character format "x(40)"             label "Description"
  field appNote    as character format "x(88)"             label "Note"
  field appStatus  as character format "x(20)"             label "Status"
  field statCode   as integer   format ">>>9"              label "Code"
/*field numLines   as integer   format ">>>>9"             label "Lines" */
  index appName-idx   is primary appName statCode ascending
  index appStatus-idx appStatus statCode
  index statCode-idx  statCode
  index xid-idx       is unique xid
.

{lib/dumpTT.i tt_app}

procedure mon-init:


  define variable cfgFileName as character no-undo.
  define variable xline       as character no-undo extent 16.

  run updTick.

  run findCfgName( "appmon", input-output cfgFileName ).

  file-info:file-name = cfgFileName.
  if file-info:full-pathname = ? then return.

  input stream inStrm from value( file-info:full-pathname ).
  repeat:

    xline = ?.
    import stream inStrm xline.
    if xline[1] <> ? and xline[1] <> "" and xline[1] <> "#" then
      do:

        create tt_appcfg.
        assign
          tt_appcfg.xid        = integer( xline[1] )
          tt_appcfg.appName    = xline[2]
          tt_appcfg.appScript  = xline[3]
          tt_appcfg.statusList = xline[4]
          no-error
        .

      end.

  end.
  input stream inStrm close.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable inLine   as character no-undo.
  define variable numLines as integer   no-undo.

  empty temp-table tt_app.

  for each tt_appcfg:

    create tt_app.
    assign
      tt_app.xid       = tt_appcfg.xid
      tt_app.appName   = tt_appcfg.appName
      tt_app.statCode  = -1
      tt_app.appNote   = ""
      numLines         = 0
    .

    inLine = ?.
    input stream inStrm through value( tt_appcfg.appScript ).
    repeat:

      import stream inStrm unformatted inLine.
      numLines = numLines + 1.

      if numLines = 1 then
        do:

          tt_app.statCode = integer( entry( 1, inLine, " " )) no-error.
          if tt_app.statCode < 0 or tt_app.statCode = ? then
            tt_app.appStatus = "Status Check Failed".
           else
            do:
              if num-entries( tt_appcfg.statusList ) > tt_app.statCode then
                tt_app.appStatus = entry( tt_app.statCode + 1, tt_appcfg.statusList ).
               else
                tt_app.appStatus = "No Description".
            end.

          tt_app.appNote = trim( ( if num-entries( inLine, " " ) <= 1 then "" else substring( inLine, index( inLine, " " ) + 1 ))).

        end.
       else
        do:

          tt_app.appNote = tt_app.appNote + "~n" + inLine.

        end.

    end.
    input stream inStrm close.

  end.

  add2ds( temp-table tt_app:default-buffer-handle ).

  return.

end.

{ssg/appnote.i}

return.
