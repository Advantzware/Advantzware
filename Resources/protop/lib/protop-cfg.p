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
 * protop-cfg.p
 *
 *
 * Initialize global shared variables configured in etc/protop.cfg
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	June 15, 2006
 *
 * 	Moved responsibility for configuration of log files etc
 *	from lib/protop.i to lib/protop-cfg.p
 *	June 15, 2006
 * 
 */

define variable ptCfg_date as date    no-undo.
define variable ptCfg_time as integer no-undo.

define variable i as integer no-undo.
define variable j as integer no-undo.

{lib/protop.i}

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

/* only do this once -- at startup
 */

pt_uniqName  = ldbname( 1 ).
if pt_uniqName = ? then pt_uniqName = "pt".

/* if the user is not running with -rand 2 then random() will always repeat -- so we need to salt it
 * this is ugly but it doesn't need to be "truly" random for any other reason, we're just trying to
 * avoid obvious naming collisions
 */

do i = 1 to (( day( today ) * year( today )) / 50 ):
  j = random( 0, day( today ) * month( today )).
end.

pt_uniqName = pt_uniqName + string( random( 0, max( 100, integer( time ))), "99999" ).

message {&NOW} "pt_uniqName = " pt_uniqName.

assign
  pt_logname    = "&5.&2.&3"
  ptCfg_date    = ?
  ptCfg_time    = 0
  pt_votrx      = 1800
  pt_lktbllim   = 10000
  pt_bkupstale  = 26
  pt_bogomips   = 1000000
  pt_ioresp     = 100
  pt_ioFileName = ""
.

{ssg/sausage14.i}

run ptConfig.

if dbgMode >= 5 then
  message {&NOW}
    "pt_tmpdir  = " pt_tmpdir  " "
    "pt_logdir  = " pt_logdir  " "
    "pt_rptdir  = " pt_rptdir  " "
    "pt_mailcmd = " pt_mailcmd
  .

message {&NOW} "etc/protop.cfg complete.".

return.


/*** Make ptConfig available as a callable super-procedure
 ***
 ***/

procedure ptConfig:

  define variable dbListLine as character no-undo.

  define variable i as integer   no-undo.

  define variable k as character no-undo.
  define variable v as character no-undo.

  if connected( "dictdb" ) then run lib/setptname.p.

  /* set some reasonable defaults for key config variables
   */

  assign
    pt_mailcmd   = os-getenv( "MAILCMD" )	/*	'mailx -s "&1" ' 	*/
    pt_logdir    = os-getenv( "LOGDIR" )	/*	"/tmp"			*/
    pt_tmpdir    = os-getenv( "PTTMP" )		/*	TMPDIR gets whacked by the C library :(		*/
    pt_rptdir    = os-getenv( "RPTDIR" )	/*	"/tmp"			*/
  .

  if pt_mailcmd = ? or pt_mailcmd = "" then
    do:
      if opsys = "unix" then
        pt_mailcmd = 'mailx &3 -s "&2" &1'.
       else
        pt_mailcmd = 'mailx &1 "&2" '.
    end.

  if pt_logdir = ? or pt_logdir = "" then
    do:

      file-info:file-name = search( "logs" ).
      if file-info:full-pathname = ? then file-info:file-name = search( "logdir" ).
      if file-info:full-pathname = ? then file-info:file-name = "./logs".
      if file-info:full-pathname = ? then file-info:file-name = "./logdir".
      if file-info:full-pathname = ? then file-info:file-name = search( "vstmon" ).
      if file-info:full-pathname = ? then file-info:file-name = "./tmp".
      if file-info:full-pathname = ? then file-info:file-name = "./temp".

      if file-info:full-pathname <> ? then
        pt_logdir  = file-info:full-pathname.
       else
        pt_logdir  = session:temp-directory.

    end.

  if pt_tmpdir = ? or pt_tmpdir = "" then
    do:

      file-info:file-name = search( "tmp" ).
      if file-info:full-pathname = ? then file-info:file-name = "./tmp".

      if file-info:full-pathname <> ? then
        pt_tmpdir  = file-info:full-pathname.
       else
        pt_tmpdir  = session:temp-directory.

    end.

  /* figure out the server name
   *
   * the environment variable is useful in a cluster to set the cluster name rather than the node name
   *
   * etc/protop.cfg can also OVERRIDE this block of code
   *
   */

  pt_server = trim( os-getenv( "PTSERVER" )).

  if pt_server = ? or pt_server = "" then
    do:

      if opsys = "unix" then
        do:
          input stream inStrm through value( "uname -a" ).
          import stream inStrm ^ pt_server.
          input stream inStrm close.
        end.
       else
        do:
          input stream inStrm through value( "hostname" ).
          import stream inStrm pt_server.
          input stream inStrm close.
        end.

    end.  

  file-info:file-name = search( "etc/dblist.cfg" ).
  
  if file-info:full-pathname <> ? then
    do:

      input stream inStrm from value( file-info:full-pathname ).
      repeat:

        dbListLine = "".
        import stream inStrm unformatted dbListLine.

        if dbListLine = "" or dbListLine begins "#" then next.
        if entry( 1, dbListLine, "|" ) <> pt_shortName then next.
        if num-entries( dbListLine, "|" ) < 6 then next.

        pt_resrcType = entry( 6, dbListLine, "|" ).

      end.
      input stream inStrm close.

    end.

  /* if it exists use etc/protop.cfg
   *
   */

  file-info:file-name = search( "etc/protop.cfg" ).

  message {&NOW} "pt_config = " file-info:full-pathname.

  if file-info:full-pathname = ? then
    do:

      assign
        pt_logname    = "&5.&2.&3"
        ptCfg_date    = ?
        ptCfg_time    = 0
        pt_votrx      = 1800
        pt_lktbllim   = 10000
        pt_bkupstale  = 26
        pt_bogomips   = 1000000
        pt_ioresp     = 100
        pt_ioFileName = ""
        pt_AICheckInterval   = 60
        pt_PICACheckInterval = 60
        pt_appsrvStuck = 120
        pt_zoomTo      = 30
      .

      {ssg/sausage14.i}

    end.
   else
    do:

      if dbgMode >= 5 then
        message {&NOW}
          "check config:" file-info:file-mod-date ptCfg_date file-info:file-mod-time ptCfg_time
          ( file-info:file-mod-date <> ptCfg_date or file-info:file-mod-time <> ptCfg_time )
        .

      if file-info:file-mod-date <> ptCfg_date or file-info:file-mod-time <> ptCfg_time then
        do:

          assign
            ptCfg_date = file-info:file-mod-date
            ptCfg_time = file-info:file-mod-time
          .

          message {&NOW} "reading config file:" file-info:full-pathname.

          input stream inStrm from value( file-info:full-pathname ).

          repeat on endkey undo, leave:

            assign
              k = ""
              v = ""
            .

            import stream inStrm k v.

            if k = "" or k = ? or k begins "#" then next.

            if dbgMode >= 3 then message {&NOW} substitute( "&1 = [&2]", k, v ).

            case k:

              when "logdir"     then pt_logdir     = v.
              when "logname"    then pt_logname    = v.
              when "mailcmd"    then pt_mailcmd    = v.
              when "server"     then pt_server     = v.	/* over-ride uname -- useful for Windows but eliminates shared protop.cfg files	*/

              when "votrx"      then pt_votrx      = integer( v ) no-error.
              when "lktbllim"   then pt_lktbllim   = integer( v ) no-error.
              when "bkupstale"  then pt_bkupstale  = integer( v ) no-error.
              when "bogomips"   then pt_bogomips   = integer( v ) no-error.
              when "ioresp"     then pt_ioresp     = integer( v ) no-error.
              when "ioFileName" then pt_ioFileName = v.
              when "dfCmd"      then pt_dfCmd      = v.

              when "AICheckInterval"   then pt_AICheckInterval   = integer( v ) no-error.
              when "PICACheckInterval" then pt_PICACheckInterval = integer( v ) no-error.
              when "appsrvStuck"       then pt_appsrvStuck       = integer( v ) no-error.
              when "zoomTo"            then pt_zoomTo            = integer( v ) no-error.

              {ssg/sausage15.i}

            end.

          end.

          input stream inStrm close.

          /* bin/localenv variables
           */

          if os-getenv( "IOFILENAME" ) <> ? then pt_ioFileName = os-getenv( "IOFILENAME" ).
          if os-getenv( "DFCMD" )      <> ? then pt_dfCmd      = os-getenv( "DFCMD" ).
          if os-getenv( "VOTRX" )      <> ? then pt_votrx      = integer( os-getenv( "VOTRX"    )) no-error.
          if os-getenv( "LKTBLLIM" )   <> ? then pt_lktbllim   = integer( os-getenv( "LKTBLLIM" )) no-error.
          if os-getenv( "BOGOMIPS" )   <> ? then pt_bogomips   = integer( os-getenv( "BOGOMIPS" )) no-error.
          if os-getenv( "IORESP" )     <> ? then pt_ioresp     = integer( os-getenv( "IORESP"   )) no-error.

          if os-getenv( "AICHECKINTERVAL" )   <> ? then pt_AICheckInterval   = integer( os-getenv( "AICHECKINTERVAL"   )) no-error.
          if os-getenv( "PICACHECKINTERVAL" ) <> ? then pt_PICACheckInterval = integer( os-getenv( "PICACHECKINTERVAL" )) no-error.
          if os-getenv( "APPSRVSTUCK" )       <> ? then pt_appsrvStuck       = integer( os-getenv( "APPSRVSTUCK"       )) no-error.

          if os-getenv( "ZOOMTO" )            <> ? then pt_zoomTo            = integer( os-getenv( "ZOOMTO"            )) no-error.
          if pt_zoomTo = ? or pt_zoomTo = 0 then pt_zoomTo = 30.

          /* alert on long bi backup phase
           */

          pt_bibkUpAlert = integer( os-getenv( "BIBKUP_ALERT" )) no-error.
          if pt_bibkUpAlert = ? then pt_bibkUpAlert = 60.

          pt_bibkUpAlarm = integer( os-getenv( "BIBKUP_ALARM" )) no-error.
          if pt_bibkUpAlarm = ? then pt_bibkUpAlarm = 300.

          pt_bibkUpPage  = integer( os-getenv( "BIBKUP_PAGE" )) no-error.
          if pt_bibkUpPage = ? then pt_bibkUpPage = 0.

          /* alert on long backups
           */

          pt_bkUpAlert = integer( os-getenv( "BKUP_ALERT" )) no-error.
          if pt_bkUpAlert = ? then pt_bkUpAlert = 0.

          pt_bkUpAlarm = integer( os-getenv( "BKUP_ALARM" )) no-error.
          if pt_bkUpAlarm = ? then pt_bkUpAlarm = 0.

          pt_bkUpPage  = integer( os-getenv( "BKUP_PAGE" )) no-error.
          if pt_bkUpPage = ? then pt_bkUpPage = 0.

          /* other tweakable features
           */

          if os-getenv( "USEREXP" ) begins "n" then
            pt_doZippy = no.
           else
            pt_doZippy = yes.

          if os-getenv( "USERFUTIL" ) begins "n" then
            pt_useRFUtil = no.
           else
            pt_useRFUtil = yes.

          if os-getenv( "USERLOCK" ) begins "y" then
            pt_userLock = yes.
           else
            do:
              if connected( "dictdb"  ) then run lib/userlock.p ( input-output pt_userLock ).
            end.

          pt_updAreaData = integer( os-getenv( "UPDAREADATA" )) no-error.
          if pt_updAreaData = ? then pt_updAreaData = 3600 * 6.


          /* restricted features
           */

          file-info:file-name = "ssg/sausage26.r".
          if file-info:full-pathname = ? then
            file-info:file-name = "ssg/sausage26.p".

          if file-info:full-pathname <> ? then
            run ssg/sausage26.p.

        end.

    end.

end.
