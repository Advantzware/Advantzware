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
 * df.p
 *
 * Free disk space
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	November 2, 2003
 *
 */

{lib/protop.i}
{lib/protoplib.i}

define output parameter dcDescription as character no-undo initial "df".

define temp-table tt_diskFree no-undo
  field xid        as integer
  field xvalid     as logical
  field mnt        as character label "Mount"      format "x(20)"
  field fs-size    as decimal   label "Size (GB)"  format ">>>>>9.99"
  field fs-free    as decimal   label "Free (GB)"  format ">>>>>9.99"
  field fsPctUsed  as decimal   label "%Used"      format ">>9.99%"

  field inode-num  as decimal   label "#Inodes"    format ">>>>>>>>>9"
  field inode-free as decimal   label "InodesFree" format ">>>>>>>>>9"
  field inPctUsed  as decimal   label "%Inodes"    format ">>9.99%"

  field fs-type    as character label "FS Type"    format "x(8)" 
  field fs-opts    as character label "Mount Options" format "x(36)" 
  field dev        as character label "Device"     format "x(30)"
  field threshold  as integer extent 2 {&NOSERIALIZE}
  field fs-grow    as decimal extent 5 {&NOSERIALIZE}

  index xid-idx is unique xid
  index mnt-idx is unique mnt
  index fsPctUsed-idx is primary fsPctUsed descending
  index fs-type-idx fs-type
  index dev-idx     dev
  index fs-opts-idx fs-opts
  index fs-size-idx fs-size descending
  index fs-free-idx fs-free descending
.

{lib/dumpTT.i tt_diskFree}

define temp-table tt_df_cfg no-undo
  field order       as integer
  field xtype       as character
  field pattern     as character
  field threshold   as decimal
.

define variable zid    as integer no-undo.
define variable osName as character no-undo.


/* what does this do?
 *
 * if tt_df_cfg is empty then nothing
 *
 */

function checkPattern returns logical ( input xmnt as character, input-output thold as decimal ):

  define variable ok as logical no-undo.

    check_pattern: do:

      find tt_df_cfg no-lock where tt_df_cfg.xtype = "default" no-error.
      if available tt_df_cfg then thold = tt_df_cfg.threshold.

      ok = yes.

      for each tt_df_cfg no-lock where tt_df_cfg.xtype = "include" by tt_df_cfg.order:
        if xmnt matches tt_df_cfg.pattern then
          do:
            if tt_df_cfg.threshold > 0 then thold = tt_df_cfg.threshold.
            leave check_pattern.
          end.
      end.

      for each tt_df_cfg no-lock where tt_df_cfg.xtype = "exclude" by tt_df_cfg.order:
        if xmnt matches tt_df_cfg.pattern then
          do:
            ok = no.
            leave check_pattern.
          end.
      end.

    end.

  return ok.

end.

procedure getMountAIX:

  define variable xmnt    as character no-undo format "x(80)".
  define variable xdev    as character no-undo format "x(80)".
  define variable fstype  as character no-undo format "x(80)".
  define variable mntopts as character no-undo format "x(80)".

  define variable xxx     as character no-undo.
  define variable xtra    as character no-undo extent 32.
  define variable i as integer no-undo.

  input stream inStrm through value( "mount" ).
  import stream inStrm ^.
  import stream inStrm ^.

  repeat:

    xtra = "".

    import stream inStrm xdev xmnt fstype xtra.

    i = 4.
    if not xdev begins "/" then
      assign
        xxx  = xdev
        xdev = xmnt
        xmnt = fstype
        fstype = xtra[1]
        i = i + 1.
      .

    mntopts = "".
    do while i < 32 and xtra[i] <> "":
      mntopts = mntopts + trim( xtra[i] ) + ",".
      i = i + 1.
    end.
    mntopts = substring( mntopts, 1, length( mntopts ) - 1 ).

    find tt_diskFree where tt_diskFree.mnt = xmnt no-error.
    if available tt_diskFree then
      assign
        tt_diskFree.fs-type = fstype
        tt_diskFree.fs-opts = mntopts
      .

  end.
  input stream inStrm close.

end.

procedure getMountSunOS:

  define variable xmnt    as character no-undo format "x(80)".
  define variable xdev    as character no-undo format "x(80)".
  define variable fstype  as character no-undo format "x(80)".
  define variable mntopts as character no-undo format "x(80)".

  /*      device       device       mount      FS      fsck    mount      mount
   *      to mount     to fsck      point      type    pass    at boot    options
   */

  input stream inStrm through value( "mount -p" ).
  repeat:

    import stream inStrm xdev ^ xmnt fstype ^ ^ mntopts.

    find tt_diskFree where tt_diskFree.dev = xdev no-error.
    if available tt_diskFree then
      assign
        tt_diskFree.fs-type = fstype
        tt_diskFree.fs-opts = mntopts
      .

  end.
  input stream inStrm close.

end.

procedure getMountHP-UX:

  define variable xmnt    as character no-undo format "x(80)".
  define variable xdev    as character no-undo format "x(80)".
  define variable fstype  as character no-undo format "x(80)".
  define variable mntopts as character no-undo format "x(80)".

  input stream inStrm through value( "/etc/mount -p" ).
  repeat:

    import stream inStrm xdev xmnt fstype mntopts ^ ^.

    find tt_diskFree where tt_diskFree.dev = xdev no-error.
    if available tt_diskFree then
      assign
        tt_diskFree.fs-type = fstype
        tt_diskFree.fs-opts = mntopts
      .

  end.
  input stream inStrm close.

end.

procedure getMountWindows:

  /* nothing to do -- mount info is included with df info via the wmic command */

  return.

end.

procedure getMountLinux:

  define variable xmnt    as character no-undo format "x(80)".
  define variable xdev    as character no-undo format "x(80)".
  define variable fstype  as character no-undo format "x(80)".
  define variable mntopts as character no-undo format "x(80)".

  input stream inStrm through value( "mount" ).
  repeat:

    import stream inStrm xdev ^ xmnt ^ fstype mntopts.

    find tt_diskFree where tt_diskFree.dev = xdev no-error.
    if available tt_diskFree then
      assign
        tt_diskFree.fs-type = fstype
        tt_diskFree.fs-opts = mntopts
      .

  end.
  input stream inStrm close.

end.


/* proenv>wmic logicaldisk get compressed,deviceid,drivetype,filesystem,freespace,name,size,status,statusinfo,volumename,volumeserialnumber /format:csv
 *
 * Node,Compressed,DeviceID,DriveType,FileSystem,FreeSpace,Name,Size,Status,StatusInfo,VolumeName,VolumeSerialNumber
 * TOM-PC,FALSE,C:,3,NTFS,79466725376,C:,199750578176,,,,D2909E2F
 * TOM-PC,,D:,5,,,D:,,,,,
 * TOM-PC,FALSE,E:,3,NTFS,600009228288,E:,600124157952,,,New Volume,3C0CB4B6
 * TOM-PC,,F:,2,,,F:,,,,,
 * TOM-PC,FALSE,Z:,4,NTFS,687177990144,Z:,747827625984,,,share,2E0D04E7
 */

procedure getDFWindows:

  define variable inline   as character no-undo extent 20.		/* used by df commands that split lines		*/
  define variable xmnt     as character no-undo format "x(80)".
  define variable xdev     as character no-undo format "x(80)".
  define variable xfs-size as decimal   no-undo format ">>>>>>>>9".
  define variable xfs-free as decimal   no-undo format ">>>>>>>>9".

  define variable xinodes  as decimal   no-undo format ">>>>>>>>9".
  define variable xifree   as decimal   no-undo format ">>>>>>>>9".

  define variable ok as logical no-undo.
  define variable thold as decimal no-undo.

  define variable xline as character no-undo.

  define variable fsCompressed as character no-undo format "x(80)".
  define variable deviceid     as character no-undo format "x(80)".
  define variable drivetype    as character no-undo format "x(80)".
  define variable filesystem   as character no-undo format "x(80)".
  define variable freeSpace    as character no-undo format "x(80)".
  define variable fsName       as character no-undo format "x(80)".
  define variable fsSize       as character no-undo format "x(80)".
  define variable fsStatus     as character no-undo format "x(80)".
  define variable fsStatusinfo as character no-undo format "x(80)".
  define variable volumeName   as character no-undo format "x(80)".
  define variable volumeSerial as character no-undo format "x(80)".

  define variable dfcmd as character no-undo initial "wmic logicaldisk get compressed,deviceid,drivetype,filesystem,freespace,name,size,status,statusinfo,volumename,volumeserialnumber /format:csv".

  if pt_dfCmd <> "" then dfcmd = pt_dfCmd.

  input stream inStrm through value( dfcmd ).

  repeat:

    assign
      inline   = ""
      xmnt     = ""
      xdev     = ""
      xfs-size =  0
      xfs-free =  0
      thold    = 90

      fsCompressed = ""
      deviceid     = ""
      drivetype    = ""
      filesystem   = ""
      freeSpace    = ""
      fsName       = ""
      fsSize       = ""
      fsStatus     = ""
      fsStatusInfo = ""
      volumeName   = ""
      volumeSerial = ""
    .

    import stream inStrm delimiter "," ^ fsCompressed deviceid drivetype filesystem freespace fsName fsSize fsStatus fsStatusInfo volumeName volumeSerial.

    if filesystem = "" or fsSize = "" or ( filesystem = "filesystem" and fsSize = "size" ) then next.

    assign
      xdev     = deviceId
      xfs-size = ( decimal( fsSize ) / 1024 )
      xfs-free = ( decimal( freeSpace ) / 1024 )
    .

    if xmnt = "" and volumeName   <> "" then xmnt = volumeName.
    if xmnt = "" and volumeSerial <> "" then xmnt = volumeSerial.
    if xmnt = "" and fsName       <> "" then xmnt = fsName.

    xmnt = trim( xmnt ).

    if xmnt = "" then xmnt = xdev.

    if checkPattern( xmnt, thold ) then run update_diskFree( xmnt, xdev, xfs-size, xfs-free, thold, xinodes, xifree ).

    find tt_diskFree where tt_diskFree.dev = deviceid no-error.

    if available tt_diskFree then
      do:

        tt_diskFree.fs-type = filesystem.

        case drivetype:
          when "0" then tt_diskFree.fs-opts = "Unknown,".
          when "2" then tt_diskFree.fs-opts = "Removable,".
          when "3" then tt_diskFree.fs-opts = "Local,".
          when "4" then tt_diskFree.fs-opts = "Network,".
          when "5" then tt_diskFree.fs-opts = "CD,".
          when "6" then tt_diskFree.fs-opts = "RAM,".
          otherwise tt_diskFree.fs-opts = substitute( "Type &1,", trim( driveType )).
        end.

        if fsCompressed <> "false" then tt_diskFree.fs-opts = tt_diskFree.fs-opts + "Compressed,".
        if fsStatus     <> ""      then tt_diskFree.fs-opts = tt_diskFree.fs-opts + fsStatus + ",".
        if fsStatusInfo <> ""      then tt_diskFree.fs-opts = tt_diskFree.fs-opts + fsStatusInfo + ",".

        tt_diskFree.fs-opts = trim( tt_diskFree.fs-opts, "," ).

      end.

  end.

  input stream inStrm close.

  return.

end.


/*      $ bdf
 *      Filesystem          kbytes    used   avail %used Mounted on
 *      /dev/vg00/lvol3    1048576  602280  442904   58% /
 *      /dev/vg00/lvol1    1776056   82304 1516144    5% /stand
 *      /dev/vg00/lvol8    34078720 7331080 26541696   22% /var
 *      /dev/vg00/lvol7    4194304 2500712 1682064   60% /usr
 *      /dev/dvl/dvl       261095424 178366480 82082640   68% /train
 *      /dev/vg00/lvol6    4194304 1494216 2679016   36% /tmp
 *      /dev/rep_ai/rep_ai 20480000 2415310 16935658   12% /rep_ai
 *      /dev/rep/rep       261095424 204975432 55681576   79% /rep
 *      /dev/vg00/lvol5    8388608 3786096 4568040   45% /opt
 *      /dev/olbkup/olbkup 358350848 315188008 42825704   88% /onlinebkup
 *      /dev/vg00/lvol4    4194304   52968 4109008    1% /home2
 *      /dev/home/home     105906176 86823350 17890167   83% /home
 *      /dev/db08train/train8
 *                         153501696 135933835 16469873   89% /dvl
 *      /dev/db/prod       518324224 396028815 114652987   78% /db
 *      /dev/apps_p/apps_p 71671808  578222 66654040    1% /apps_p
 *      /dev/apps/apps     156237824 95440200 57001214   63% /apps
 *      /dev/prodai/prodai 56098816 5392408 50310288   10% /ai
 *      /dev/FY2012/FY2012 204151665 170145384 13591114   93% /FY2012
 *      /dev/FY2011/FY2011 209649664 189100095 19288130   91% /FY2011
 *      /dev/FY2010/FY2011 209649664 195166776 14375480   93% /FY2010
 *      DevFS                    3       3       0  100% /dev/deviceFileSystem
 */

procedure getDFHP-UX:

  define variable inline   as character no-undo extent 20.		/* used by df commands that split lines		*/
  define variable xmnt     as character no-undo format "x(80)".
  define variable xdev     as character no-undo format "x(80)".
  define variable xfs-size as decimal   no-undo format ">>>>>>>>9".
  define variable xfs-free as decimal   no-undo format ">>>>>>>>9".

  define variable xinodes  as decimal   no-undo format ">>>>>>>>9".
  define variable xifree   as decimal   no-undo format ">>>>>>>>9".

  define variable ok as logical no-undo.
  define variable thold as decimal no-undo.

  define variable dfcmd as character no-undo initial "bdf -l | cat".

  if pt_dfCmd <> "" then dfcmd = pt_dfCmd.

  input stream inStrm through value( dfcmd ).

  import stream inStrm ^.	/* eat the header line	*/

  repeat:

    assign
      inline   = ""
      xmnt     = ""
      xdev     = ""
      xfs-size =  0
      xfs-free =  0
      thold    = 90
    .

    import stream inStrm inline no-error.
    xdev = inline[1].
    if inline[2] > "" then
      assign
        xmnt     = inline[6]
        xfs-size =  decimal( inline[2] )
        xfs-free =  decimal( inline[4] )
      no-error.
     else
      do:
        import stream inStrm inline no-error.
        assign
          xmnt     = inline[5]
          xfs-size =  decimal( inline[1] )
          xfs-free =  decimal( inline[3] )
        no-error.
      end.

    if xmnt = "" or error-status:num-messages > 0 then next.

    if checkPattern( xmnt, thold ) then run update_diskFree( xmnt, xdev, xfs-size, xfs-free, thold, xinodes, xifree ).

  end.

  input stream inStrm close.

  return.

end.

/*	$ df -k
 *	Filesystem            kbytes    used   avail capacity  Mounted on
 *	/dev/vx/dsk/rootvol  4131384 1036087 3053984    26%    /
 *	/proc                      0       0       0     0%    /proc
 *	fd                         0       0       0     0%    /dev/fd
 *	mnttab                     0       0       0     0%    /etc/mnttab
 *	swap                 6795056      32 6795024     1%    /var/run
 *	swap                 6797904    2880 6795024     1%    /tmp
 *	/dev/vx/dsk/array/db5
 *	                     8388608 5290184 2904831    65%    /db5
 *	/dev/vx/dsk/array/db6
 *	                     8388608 5290232 2904786    65%    /db6
 *	/dev/vx/dsk/array/db7
 *	                     8388608 5290224 2904794    65%    /db7
 *	/dev/vx/dsk/array/db2
 *	                     8388608 5290240 2904779    65%    /db2
 *	/dev/vx/dsk/array/db1
 *	                     8388608 5290240 2904779    65%    /db1
 *	/dev/vx/dsk/array/hsto
 *	                     8388608 1498501 6489451    19%    /hsto
 *	/dev/vx/dsk/array/db3
 *	                     8388608 5290200 2904816    65%    /db3
 *	/dev/vx/dsk/array/admin
 *	                     8388608 7756342  593207    93%    /admin
 *	/dev/vx/dsk/array/db4
 *	                     8388608 5290232 2904786    65%    /db4
 *	/dev/vx/dsk/array/db8
 *	                     8388608 7704088  641796    93%    /db8
 *	/dev/vx/dsk/array/progress
 *	                     1048576  633763  388902    62%    /progress
 *	/dev/vx/dsk/datadg/ai1
 *	                     2097152  279119 1704411    15%    /ai1
 *	/dev/vx/dsk/datadg/bi2
 *	                     2097152  476808 1519075    24%    /bi2
 *	/dev/vx/dsk/datadg/bi1
 *	                     2097152  543144 1456885    28%    /bi1
 *	/dev/vx/dsk/datadg/ai2
 *	                     2097152  148175 1827171     8%    /ai2
 *	/dev/vx/dsk/datadg/dupcalls
 *	                     4194304 2436960 1647549    60%    /dup_calls
 *	/dev/vx/dsk/datadg/misc
 *	                     16777216  193343 15547963     2%    /misc
 *	/dev/vx/dsk/datadg/backup
 *	                     83886080 72892339 10306665    88%    /backup
 *	/dev/vx/dsk/crash    4608000    2233 4317914     1%    /var/crash
 *	/dev/vx/dsk/local    2097152  253963 1728043    13%    /usr/local
 *	/dev/vx/dsk/users    2097152 1732470  341943    84%    /usr/users
 */

procedure getDFSunOS:

  define variable inline   as character no-undo extent 20.
  define variable xmnt     as character no-undo format "x(80)".
  define variable xdev     as character no-undo format "x(80)".
  define variable xfs-size as decimal   no-undo format ">>>>>>>>9".
  define variable xfs-free as decimal   no-undo format ">>>>>>>>9".

  define variable xinodes  as decimal   no-undo format ">>>>>>>>9".
  define variable xifree   as decimal   no-undo format ">>>>>>>>9".

  define variable ok as logical no-undo.
  define variable thold as decimal no-undo.

  define variable dfcmd as character no-undo initial "df -k | cat".

  if pt_dfCmd <> "" then dfcmd = pt_dfCmd.

  input stream inStrm through value( dfcmd ).

  import stream inStrm ^.	/* eat the header line	*/

  repeat:

    assign
      inline   = ""
      xmnt     = ""
      xdev     = ""
      xfs-size =  0
      xfs-free =  0
      thold    = 90
    .

    import stream inStrm inline no-error.
    xdev = inline[1].
    if inline[2] > "" then
      assign
        xmnt     = inline[6]
        xfs-size =  decimal( inline[2] )
        xfs-free =  decimal( inline[4] )
      no-error.
     else
      do:
        import stream inStrm inline no-error.
        assign
          xmnt     = inline[5]
          xfs-size =  decimal( inline[1] )
          xfs-free =  decimal( inline[3] )
        no-error.
      end.

    if xmnt = "" or error-status:num-messages > 0 then next.

    if checkPattern( xmnt, thold ) then run update_diskFree( xmnt, xdev, xfs-size, xfs-free, thold, xinodes, xifree ).

  end.

  input stream inStrm close.

  return.

end.

/* AIX
 *
 *	> df -k
 * 	Filesystem    1024-blocks      Free %Used    Iused %Iused Mounted on
 *	/dev/hd4          2097152   1378156   35%    19749     6% /
 *	/dev/hd2          3670016   1497904   60%    44736    12% /usr
 *	/dev/hd9var       2097152   1360740   36%    13139     5% /var
 *	/dev/hd3          2883584   2869456    1%      216     1% /tmp
 *	/dev/hd1          7340032   4688428   37%    24000     3% /home
 */

procedure getDFAIX:

  define variable inline   as character no-undo extent 20.		/* used by df commands that split lines		*/
  define variable xmnt     as character no-undo format "x(80)".
  define variable xdev     as character no-undo format "x(80)".
  define variable xfs-size as decimal   no-undo format ">>>>>>>>9".
  define variable xfs-free as decimal   no-undo format ">>>>>>>>9".

  define variable xinodes  as decimal   no-undo format ">>>>>>>>9".
  define variable xiused   as decimal   no-undo format ">>>>>>>>9".
  define variable xifree   as decimal   no-undo format ">>>>>>>>9".
  define variable xipct    as decimal   no-undo format ">>>>>>>9%".

  define variable ok as logical no-undo.
  define variable thold as decimal no-undo.

  define variable dfcmd as character no-undo initial "df -k | cat".

  if pt_dfCmd <> "" then dfcmd = pt_dfCmd.

  input stream inStrm through value( dfcmd ).

  import stream inStrm ^.	/* eat the header line	*/

  repeat:

    assign
      inline   = ""
      xmnt     = ""
      xdev     = ""
      xfs-size =  0
      xfs-free =  0
      xinodes  =  0
      xiused   =  0
      xifree   =  0
      xipct    =  0
      thold    = 90
    .

    import stream inStrm inline no-error.
    xdev = inline[1].
    if inline[2] > "" then
      assign
        xmnt     = inline[7]
        xfs-size =  decimal( inline[2] )
        xfs-free =  decimal( inline[3] )
        xiused   =  decimal( inline[5] )
        xipct    =  decimal( replace( inline[6], "%", "" ))
      no-error.
     else
      do:
        import stream inStrm inline no-error.
        assign
          xmnt     = inline[6]
          xfs-size =  decimal( inline[1] )
          xfs-free =  decimal( inline[2] )
          xiused   =  decimal( inline[4] )
          xipct    =  decimal( replace( inline[5], "%", "" ))
        no-error.
      end.

    xipct   = xipct / 100.
    xinodes = (( 100 - xipct ) * xiused ) + xiused.
    xifree  = xinodes - xiused.

    if xmnt = "" or error-status:num-messages > 0 then next.

    if checkPattern( xmnt, thold ) then run update_diskFree( xmnt, xdev, xfs-size, xfs-free, thold, xinodes, xifree ).

  end.

  input stream inStrm close.

  return.

end.

/* Linux
 *
 *
 *	$ df -Pl
 *	Filesystem           1K-blocks      Used Available Use% Mounted on
 *	/dev/hda3             36369516  27146564   7375480  79% /
 *	tmpfs                   257304       144    257160   1% /dev/shm
 *	/dev/hdb1             38468908  33462384   3052392  92% /data
 *	/dev/hdd1              4194616     85084   4109532   3% /tmp1
 *	/dev/hdd2              3968884    569516   3197756  16% /tmp2
 *
 *   "l" means "local" and will thus suppress things like:
 *
 *      //192.168.1.23/share 730300416  17121280 713179136   3% /ts
 *
 *   which is good because NFS mounts will "hang" if the server cannot be reached
 *
 */

procedure getDFLinux:

  define variable inline   as character no-undo extent 20.		/* used by df commands that split lines		*/
  define variable xmnt     as character no-undo format "x(80)".
  define variable xdev     as character no-undo format "x(80)".
  define variable xfs-size as decimal   no-undo format ">>>>>>>>9".
  define variable xfs-free as decimal   no-undo format ">>>>>>>>9".

  define variable xinodes  as decimal   no-undo format ">>>>>>>>9".
  define variable xifree   as decimal   no-undo format ">>>>>>>>9".

  define variable ok as logical no-undo.
  define variable thold as decimal no-undo.

  for each tt_diskFree:
    tt_diskFree.xvalid = no.
  end.

  define variable dfcmd as character no-undo initial "df -Pl".

  if pt_dfCmd <> "" then dfcmd = pt_dfCmd.

  input stream inStrm through value( dfcmd ).

  import stream inStrm ^.	/* eat the header line	*/

  repeat:

    assign
      inline   = ""
      xmnt     = ""
      xdev     = ""
      xfs-size =  0
      xfs-free =  0
      thold    = 90
    .

    import stream inStrm inline no-error.
    xdev = inline[1].
    if inline[2] > "" then
      assign
        xmnt     = inline[6]
        xfs-size =  decimal( inline[2] )
        xfs-free =  decimal( inline[4] )
      no-error.
     else		/* the line is split */
      do:
        import stream inStrm inline no-error.
        assign
          xmnt     = inline[5]
          xfs-size =  decimal( inline[1] )
          xfs-free =  decimal( inline[3] )
        no-error.
      end.

    if xmnt = "" or error-status:num-messages > 0 then next.

    if checkPattern( xmnt, thold ) then run update_diskFree( xmnt, xdev, xfs-size, xfs-free, thold, xinodes, xifree ).

  end.

  input stream inStrm close.

  /* phase 2 - get the inode info...
   */

  dfcmd = "df -iPl".

  input stream inStrm through value( dfcmd ).

  import stream inStrm ^.	/* eat the header line	*/

  repeat:

    assign
      inline   = ""
      xmnt     = ""
      xdev     = ""
      xinodes  =  0
      xifree   =  0
    .

    import stream inStrm inline no-error.
    xdev = inline[1].
    if inline[2] > "" then
      assign
        xmnt     = inline[6]
        xinodes  =  decimal( inline[2] )
        xifree   =  decimal( inline[4] )
      no-error.
     else		/* the line is split */
      do:
        import stream inStrm inline no-error.
        assign
          xmnt     = inline[5]
          xinodes  =  decimal( inline[1] )
          xifree   =  decimal( inline[3] )
        no-error.
      end.

    if xmnt = "" or error-status:num-messages > 0 then next.

    run update_inodes( xmnt, xdev, xinodes, xifree ).

  end.

  input stream inStrm close.

  return.

end.

procedure update_diskFree:

  define input parameter p_mnt      as character no-undo.
  define input parameter p_dev      as character no-undo.
  define input parameter p_fs-size  as decimal   no-undo.
  define input parameter p_fs-free  as decimal   no-undo.
  define input parameter p_thold    as decimal   no-undo.
  define input parameter p_inodes   as decimal   no-undo.
  define input parameter p_ifree    as decimal   no-undo.

  find tt_diskFree where tt_diskFree.mnt = p_mnt no-error.

  /* message p_mnt available( tt_diskFree ) view-as alert-box.
   */

  if available tt_diskFree then
    do:

      assign
        tt_diskFree.xvalid       = yes				/* is this xid active?		*/
        tt_diskFree.mnt          = p_mnt
        tt_diskFree.dev          = p_dev
        tt_diskFree.threshold[1] = p_thold
        tt_diskFree.fs-size      = p_fs-size / ( 1024 * 1024 )
        tt_diskFree.fs-free      = p_fs-free / ( 1024 * 1024 )
        tt_diskFree.fs-grow[3]   = p_fs-free
        tt_diskFree.inode-num    = p_inodes
        tt_diskFree.inode-free   = p_ifree
        tt_diskFree.inPctUsed    = (( p_inodes - p_ifree ) / p_inodes ) * 100
      .

    end.
   else
    do:

      create tt_diskFree.
      assign
        zid = zid + 1
        tt_diskFree.xid          = zid
        tt_diskFree.xvalid       = yes				/* is this xid active?		*/
        tt_diskFree.mnt          = p_mnt
        tt_diskFree.dev          = p_dev
        tt_diskFree.threshold[1] = p_thold
        tt_diskFree.fs-size      = p_fs-size / ( 1024 * 1024 )
        tt_diskFree.fs-free      = p_fs-free / ( 1024 * 1024 )
        {lib/init-xrec.i tt_diskFree.fs-grow p_fs-free}
        tt_diskFree.inode-num    = p_inodes
        tt_diskFree.inode-free   = p_ifree
        tt_diskFree.inPctUsed    = (( p_inodes - p_ifree ) / p_inodes ) * 100
      .

    end.

  if tt_diskFree.inPctUsed = ? then tt_diskFree.inPctUsed = 0.

  return.

end.

procedure update_inodes:

  define input parameter p_mnt      as character no-undo.
  define input parameter p_dev      as character no-undo.
  define input parameter p_inodes   as decimal   no-undo.
  define input parameter p_ifree    as decimal   no-undo.

  find tt_diskFree where tt_diskFree.mnt = p_mnt no-error.

  /* message p_mnt available( tt_diskFree ) view-as alert-box.
   */

  if available tt_diskFree then
    do:

      assign
        tt_diskFree.xvalid       = yes				/* is this xid active?		*/
        tt_diskFree.mnt          = p_mnt
        tt_diskFree.dev          = p_dev
        tt_diskFree.inode-num    = p_inodes
        tt_diskFree.inode-free   = p_ifree
        tt_diskFree.inPctUsed    = (( p_inodes - p_ifree ) / p_inodes ) * 100
      .

    end.
   else
    do:

      create tt_diskFree.
      assign
        zid = zid + 1
        tt_diskFree.xid          = zid
        tt_diskFree.xvalid       = yes				/* is this xid active?		*/
        tt_diskFree.mnt          = p_mnt
        tt_diskFree.dev          = p_dev
        tt_diskFree.inode-num    = p_inodes
        tt_diskFree.inode-free   = p_ifree
        tt_diskFree.inPctUsed    = (( p_inodes - p_ifree ) / p_inodes ) * 100
      .

    end.

  if tt_diskFree.inPctUsed = ? then tt_diskFree.inPctUsed = 0.

  return.

end.

procedure age_diskFree:

  for each tt_diskFree:
    assign
      {lib/upd-xrec.i tt_diskFree.fs-grow tt_diskFree.fs-grow[3]}
      tt_diskFree.fsPctUsed  = 100 * (( tt_diskFree.fs-size - tt_diskFree.fs-free ) / tt_diskFree.fs-size )
      tt_diskFree.fsPctUsed  = ( if tt_diskFree.fsPctUsed = ? then 0 else tt_diskFree.fsPctUsed )
    .
  end.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  define variable t as decimal no-undo.
  define variable xorder as character no-undo.
  define variable xthold as character no-undo.

  define variable cfgFileName as character no-undo.

  empty temp-table tt_diskFree.
  empty temp-table tt_df_cfg.

  if opsys = "WIN32" then
    osName = "Windows".
   else
    do:
      input stream inStrm through value( "uname -a" ).
      import stream inStrm osName.
      input stream inStrm close.
    end.

  run findCfgName( "df", input-output cfgFileName ).

  file-info:file-name = cfgFileName.
  if file-info:full-pathname = ? then return.

  input stream inStrm from value( file-info:full-pathname ).

  repeat on endkey undo, leave:

    assign
      xorder = ""
      xthold = ""
    .

    create tt_df_cfg.
    import stream inStrm
      xorder
      tt_df_cfg.xtype
      tt_df_cfg.pattern
      xthold
    .

    if xorder begins "#" or xorder = "" then
      do:
        delete tt_df_cfg.
        next.
      end.

    assign
      tt_df_cfg.order = integer( xorder )
      tt_df_cfg.threshold = decimal( xthold )
    .

  end.

  delete tt_df_cfg.	/* delete the last line read -- it is always bogus.	*/
 
  input stream inStrm close.

  find tt_df_cfg no-lock where tt_df_cfg.xtype = "default" no-error.
  if available tt_df_cfg then
    do:
      t = tt_df_cfg.threshold.
      for each tt_df_cfg exclusive-lock:
        if tt_df_cfg.threshold = 0 or tt_df_cfg.threshold = ? then tt_df_cfg.threshold = t.
      end.
    end.

  /* do NOT shell out and get "df" info at startup!
   *
   * if a filesystem is "hung" in the kernel then doing this will also hang ProTop and
   * it will not be obvious where the problem is -- this is very difficult to track
   * down!
   *
   *
   *  run value( "getDF" + osName ).
   *  run value( "getMount" + osName ).
   *
   */

  return.

end.

/* update
 *
 */

procedure mon-update:

  define input parameter argList as character no-undo.

  run value( "getDF" + osName ).
  run value( "getMount" + osName ).

  run age_diskFree.

  publish "resizeBrowse" ( "df", zid ).

  add2ds( temp-table tt_diskFree:default-buffer-handle ).

  return.

end.

return.
