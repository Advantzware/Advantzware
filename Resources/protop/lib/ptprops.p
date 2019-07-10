/* ptprops.p
 *
 * properties controlled by os-getenv variables
 *
 */

{lib/protop.i}

define new global shared variable custId     as character no-undo.
define new global shared variable pt_backend as character no-undo.		/* pt3 portal server				*/
define new global shared variable pt_proxy   as character no-undo.		/* proxy server					*/
define new global shared variable pt_dlcmd   as character no-undo.		/* auto download command			*/

session:add-super-procedure( this-procedure ).

subscribe to "protop_properties" anywhere run-procedure "ptproperties".

return.


/*** only relevant to pt3agent
      pt_lktbllim
      pt_bibkupAlert
      pt_bibkupAlarm
      pt_bibkupPage
      pt_bkupAlert
      pt_bkupAlarm
      pt_bkupPage
 ***/

procedure ptproperties:

    display
      skip(1)
      "                     custId:" custId               format "x(60)"     skip
      "              Friendly Name:" pt_shortname         format "x(60)"     skip
      "                Server Name:" pt_server            format "x(60)"     skip
      "             Resource Group:" pt_resrcType         format "x(60)"     skip
      "                     Portal:" pt_backend           format "x(60)"     skip
      "                      Proxy:" pt_proxy             format "x(60)"     skip
      "                Unique File:" pt_uniqName          format "x(60)"     skip
      "         Auto Update Method:" pt_dlcmd             format "x(60)"     skip
      "                     TMPDIR:" pt_tmpdir            format "x(60)"     skip
      "                     LOGDIR:" pt_logdir            format "x(60)"     skip
      "                     RPTDIR:" pt_rptdir            format "x(60)"     skip
      skip(1)
      "            Lock Scan Limit:" pt_lktbllim          format "->>,>>>,>>9" to 45 skip
      skip(1)
      "            BI Backup Alert:" pt_bibkupAlert       format "->>,>>>,>>9" to 45 "  (alerts only apply to paid portal customers)  " skip
      "            BI Backup Alarm:" pt_bibkupAlarm       format "->>,>>>,>>9" to 45 skip
      "             BI Backup Page:" pt_bibkupPage        format "->>,>>>,>>9" to 45 skip
      "               Backup Alert:" pt_bkupAlert         format "->>,>>>,>>9" to 45 skip
      "               Backup Alarm:" pt_bkupAlarm         format "->>,>>>,>>9" to 45 skip
      "                Backup Page:" pt_bkupPage          format "->>,>>>,>>9" to 45 skip
      skip(1)
      "                Debug Level:" dbgMode              format           "9" to 45 skip
      "           Very Old TRX Age:" pt_votrx             format "->>,>>>,>>9" to 45 skip
      "        BogoMIPS Iterations:" pt_bogomips          format "->>,>>>,>>9" to 45 skip
      "         IO Response Probes:" pt_ioresp            format "->>,>>>,>>9" to 45 skip
      "          AI Check Interval:" pt_AICheckInterval   format "->>,>>>,>>9" to 45 skip
      "        PICA Check Interval:" pt_PICACheckInterval format "->>,>>>,>>9" to 45 skip
      "           Appsrv Stuck Age:" pt_appsrvStuck       format "->>,>>>,>>9" to 45 skip
      "                 Top X Rows:" rowLimit             format "->>,>>>,>>9" to 45 skip
      "       Enable UserLock Data:" pt_userLock          format "yes/no"      to 45 "  (11.7+ only, very slow with large -L or user count)  " skip
      "     Enable User Experience:" pt_doZippy           format "yes/no"      to 45 '  (aka "zippy")  '                                       skip
      " Allow rfutil for AI status:" pt_useRFUtil         format "yes/no"      to 45 "  (needed prior to 11.7) "                               skip
      skip(1)
      '     Alternate "df" Command:' pt_dfCmd             format "x(60)"     skip
      '             "mail" Command:' pt_mailcmd           format "x(60)"     skip
      "      IO Response File List:" pt_ioFileName        format "x(60)"     skip
      skip(1)
     with
      frame getProps
      title " ProTop Properties "
      row 3
      centered
      width 104
      no-labels
      overlay
    .

  do on error undo, leave
    on endkey undo, leave:

    update
      dbgMode
      pt_votrx
      pt_bogomips
      pt_ioresp
      pt_AICheckInterval
      pt_PICACheckInterval
      pt_appsrvStuck
      rowLimit
      pt_userLock
      pt_doZippy
      pt_useRFUtil
      pt_ioFileName
     with
      frame getProps
    .

  end.

  hide frame getProps.

  return.

end.
