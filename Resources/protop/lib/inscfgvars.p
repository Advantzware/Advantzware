/* lib/inscfgvars.p
 *
 */

define input-output parameter envPT3 as character no-undo format "x(250)".
define input-output parameter envDLC as character no-undo format "x(250)".
define input-output parameter envLOG as character no-undo format "x(250)".
define input-output parameter envTMP as character no-undo format "x(250)".
define input-output parameter envRPT as character no-undo format "x(250)".

config_loop: do while session:batch = no:

  if envDLC = "" or envPT3 = "" or envLOG = "" or envTMP = "" or envRPT = "" or
     envDLC = ?  or envPT3 = ?  or envLOG = ?  or envTMP = ?  or envRPT = ? then
    do:

      display
        skip(1)
        envPT3 view-as fill-in size 40 by 1 label "    ProTop 3" skip
        envDLC view-as fill-in size 40 by 1 label "    OpenEdge" skip
        envLOG view-as fill-in size 40 by 1 label "   Log Files" skip
        envTMP view-as fill-in size 40 by 1 label "  Temp Files" skip
        envRPT view-as fill-in size 40 by 1 label "     Reports" skip
        skip(1)
       with
        frame updEnv
        title " ProTop 3 Configuration "
        centered
        row 3
        side-labels
      .

      update
        envDLC
        envLOG
        envTMP
        envRPT
       with
        frame updEnv
      .

      if envDLC = "" or envPT3 = "" or envLOG = "" or envTMP = "" or envRPT = "" or
         envDLC = ?  or envPT3 = ?  or envLOG = ?  or envTMP = ?  or envRPT = ? then
        next config_loop.

    end.

end.

return.
