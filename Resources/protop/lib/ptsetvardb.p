/* ptsetvardb.p
 *
 * prompt the user for various settings that might require a db connection
 *
 */

{lib/protop.i}

define temp-table tt_CSC
  field uNum as integer
  index uNum-idx is unique primary uNum
.

session:add-super-procedure( this-procedure ).

subscribe to "protop_clearCSC"       anywhere run-procedure "clearCSC".
subscribe to "protop_getUserNum"     anywhere run-procedure "getUserNum".
subscribe to "protop_getUserPID"     anywhere run-procedure "getUserPID".
subscribe to "protop_getLockLimit"   anywhere run-procedure "getLockLimit".
subscribe to "protop_getUserTblName" anywhere run-procedure "getUserTblName".
subscribe to "protop_getUserIdxName" anywhere run-procedure "getUserIdxName".

return.


procedure clearCSC:

  define buffer updConnect for _Connect.

  define variable i  as integer no-undo.
  define variable ok as logical no-undo.
  define variable t  as character no-undo.

  for each tt_CSC:
    i = i + 1.
  end.

  if i = 0 or session:batch = yes then return.

  ok = yes.

  message color red
    skip(1)
    substitute( "    Clear the &1 session&2 that you enabled the Client Statement Cache for?    ", ( if i > 1 then trim( string( i, ">>>>>>" )) else "" ), ( if i > 1 then "s" else "" )) skip
    skip(1)
                "    This isn't strictly required but in most cases it is best to proactively   " skip
                "    clean these up rather than leave the sessions enabled.                     " skip
    skip(1)
    view-as alert-box question buttons yes-no
    title " Clear Client Statement Cache "
    update ok
  .
  if ok = no then return.

  for each tt_CSC:

      do for updConnect transaction:
        find updConnect exclusive-lock where _Connect-id = uNum + 1 no-error.
        if available( updConnect ) then
          do:
            buffer updConnect:handle:buffer-field( "_connect-cachingType" ):buffer-value = 0 no-error.
            delete tt_CSC.
          end.
      end.
    
  end.

  return.

end.


procedure getUserNum:

  define input-output parameter userNum as integer no-undo label "  usrNum" format ">>>>9".
  define input-output parameter userPID as integer no-undo.

  define buffer updConnect for _Connect.

  define variable cType as integer no-undo format "9".

  do on error undo, leave
    on endkey undo, leave:

    cType = ?.

    find _Connect no-lock where _Connect-id = userNum + 1 no-error.
    if available( _connect ) then
      cType = buffer _connect:handle:buffer-field( "_connect-cachingType" ):buffer-value no-error.

    update
      skip(1)
      '  Enter the DB Connection Number for the session of interest.  This is the "Usr#" column  ' skip
      '  found on many ProTop screens, in the DB .lg file, and in many Progress error messages.  ' skip
      skip(1)
      '  Usr#' userNum skip
      skip(1)
      '  Client statement cache type:' cType '     0 = Off'             skip
      '                                      1 = Single (Top of Stack)' skip
      '                                      2 = Full Call Stack'       skip
      skip(1)
      '      Most of the time you want "1".  You typically do not need the full stack            ' skip
      '      unless you are planning to look at the detailed User Info screen (the "U"           ' skip
      '      command).  The full stack option obviously takes more memory, uses more             ' skip
      '      space in the client/server networking communications and is more likely             ' skip
      '      to use the scratch disk (resulting in extraneous IO operations).                    ' skip
      skip(1)
     with
      frame getVar
      title " DB Connection Number "
      row 3
      centered
      width 92
      no-labels
      overlay
    .

    if userNum > 0 and userNum <> ? then
      do for updConnect transaction:
        find updConnect exclusive-lock where _Connect-id = userNum + 1 no-error.
        if available( updConnect ) then
          do:
            if cType = 1 or cType = 2 then
              do:
                find tt_CSC where uNum = _Connect-usr no-error.
                if not available tt_CSC then
                  do:
                    create tt_CSC.
                    uNum = _Connect-usr.
                  end.
                buffer updConnect:handle:buffer-field( "_connect-cachingType" ):buffer-value = cType no-error.
              end.
             else if cType = 0 then
              do:
                find tt_CSC where uNum = _Connect-usr no-error.
                if available tt_CSC then
                  delete tt_CSC.
                buffer updConnect:handle:buffer-field( "_connect-cachingType" ):buffer-value = 0 no-error.
              end.
          end.
      end.

  end.

  hide frame getVar.

  userPID = ?.

  return.

end.


procedure getUserPID:

  define input-output parameter userPID as integer no-undo label "  usrPID" format ">>>>>>>>>9".
  define input-output parameter userNum as integer no-undo.

  define buffer updConnect for _Connect.

  define variable cType as integer no-undo format "9".

  do on error undo, leave
    on endkey undo, leave:

    cType = ?.

    find _Connect no-lock where _Connect-PID = userPID no-error.
    if available( _connect ) then
      cType = buffer _connect:handle:buffer-field( "_connect-cachingType" ):buffer-value no-error.

    update
      skip(1)
      '  Enter the DB Connection PID for the session of interest. This is the "PID" column on many   ' skip
      '  ProTop screens, in the DB .lg file, some error messges or from OS commands such as "ps".    ' skip
      skip(1)
      '  usrPID:' userPID skip
      skip(1)
      '  Client statement cache type:' cType '    0 = Off, 1 = Single, 2 = Stack' skip
      '                                    (Most of the time you want "1")' skip
      skip(1)
     with
      frame getVar
      title " DB Connection Process ID "
      row 3
      centered
      width 96
      no-labels
      overlay
    .

    if userPID > 1 and userPID <> ? then
      do for updConnect transaction:
        find updConnect exclusive-lock where _Connect-PID = userPID no-error.
        if available( updConnect ) then
          do:
            if cType = 1 or cType = 2 then
              do:
                find tt_CSC where uNum = _Connect-usr no-error.
                if not available tt_CSC then
                  do:
                    create tt_CSC.
                    uNum = _Connect-usr.
                  end.
                buffer updConnect:handle:buffer-field( "_connect-cachingType" ):buffer-value = cType no-error.
              end.
             else if cType = 0 then
              do:
                find tt_CSC where uNum = _Connect-usr no-error.
                if available tt_CSC then
                  delete tt_CSC.
                buffer updConnect:handle:buffer-field( "_connect-cachingType" ):buffer-value = 0 no-error.
              end.
          end.
      end.

  end.

  hide frame getVar.

  userNum = ?.

  return.

end.


procedure getLockLimit:

  define input-output parameter lockLimit as integer no-undo format "->>>>9" label "  lockLimit".

  define variable ok as logical.

  do on error undo, leave
    on endkey undo, leave:

    update
      skip(1)
      '  Non-zero values are DANGEROUS!  This enables _Lock scanning for blocked sessions. '   skip
      skip(1)
      lockLimit skip
      skip(1)
      '  The lockLimit is the maximum number of _Lock entries to examine before giving up'     skip
      '  when attempting to determine who else might have a record lock.  This *might* be'     skip
      '  attempted for every user -- so the total number of attempts could be as high as'      skip
      '  -n * lockLimit.'                                                                      skip
      skip(1)
      '  This can be useful but it needs to be used with extreme caution.  Scanning the _Lock' skip
      '  table is very costly.  Especially on busy production systems with -L set to large'    skip
      '  values.  The use of this feature is strongly discouraged.'                            skip
      skip(1)
      '  If you are not *very* sure that you need this do NOT use it.'                         skip
      skip(1)
     with
      frame getVar
      title " Lock Limit "
      row 3
      centered
      width 100
      side-labels
      overlay
    .

  end.

  hide frame getVar.

  if lockLimit > 10 then
    do:
      message color red
        skip
        'You have been warned that this is a bad idea.' skip
        'Are you SURE that you want to ignore that advice?'
        skip(1)
        view-as alert-box question buttons yes-no
        title " This Is A Very Bad Idea "
        update ok
      .
      if ok = no then lockLimit = 0.
    end.

  return.

end.


procedure getUserTblName:

  define input-output parameter userTblName as character no-undo format "x(50)".
  define input-output parameter userTblNum  as integer   no-undo.

  do on error undo, leave
    on endkey undo, leave:

    update
      skip(1)
      '  Track top users of:' skip
      skip(1)
      userTblName
      skip(1)
      '  Enter a valid table name or "" or ? to clear user of table tracking.' skip
      skip(1)
     with
      frame getVar
      title " Table Name "
      row 3
      centered
      width 100
      side-labels
      overlay
    .

    if userTblName = "" or userTblName = ? then
      userTblNum = ?.
     else
      do:
        find tt_tbl where tt_tbl.tblName = userTblName no-error.
        if available tt_tbl then
          userTblNum = tt_tbl.xid.
         else
          do:
            message "Please enter a valid table name.".
            pause.
            undo, retry.
          end. 
      end.

  end.

  hide frame getVar.

  return.

end.


procedure getUserIdxName:

  define input-output parameter userIdxName as character no-undo format "x(50)".
  define input-output parameter userIdxNum  as integer   no-undo.

  do on error undo, leave
    on endkey undo, leave:

    update
      skip(1)
      '  Track top users of:' skip
      skip(1)
      userIdxName
      skip(1)
      '  Enter a valid index name or "" or ? to clear user of index tracking.' skip
      skip(1)
     with
      frame getVar
      title " Index Name "
      row 3
      centered
      width 100
      side-labels
      overlay
    .

    if userIdxName = "" or userIdxName = ? then
      userIdxNum = ?.
     else
      do:
        find tt_idx where tt_idx.tblName = entry( 1, userIdxName, "." ) and tt_idx.idxName = entry( 2, userIdxName, "." ) no-error.
        if available tt_idx then
          userIdxNum = tt_idx.xid.
         else
          do:
            message "Please enter a valid index name (table.index).".
            pause.
            undo, retry.
          end. 
      end.

  end.

  hide frame getVar.

  return.

end.

