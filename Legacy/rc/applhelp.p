/*  rc/applhelp.p - global help procedure.
07.17.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added help routing of *emul* to rc/emulpick.p.  Long overdue!.
03.18.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added help routing of *div* to rc/divpick.p.
02.03.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added routing to ic/applhelp.p for *bin* and *registrer*.
11.25.97 by CAH on \\ricky\robj8\ Log#0000:
1.  Removed debugging around CTRL-F alias calls; make conditional on top-debug.
02.21.97 by CAH on ricky@812<rprodev> Log#970221:
1.  Added test for "ED" application code and routing to ed/applhelp.p.
10.24.95 by CAH on NCR3550@RD-USA [/home/dpoch] Log#0000:
1.  Added qualification on bank, appears in multiple databases.
12.04.93 by cah on t14000:
1.  Added routing stub for pc pplication to module help proc.
10.21.93 by CAH on B28@111 Log#0000:
1.  Changed f11 and f12 keylabels for alternative help to be break-line
    and append-line keyfunctions which are more portable.
09.27.93 by CH on TI4000 Log#0000:
1.  Added AP application routing to module help procedure.
09.01.93 by CH on TI4000 Log#0000:
1.  Added save of ws_co into lo_co on development notes routing and restore
    upon return.
2.  Made f11 notes routing conditional upon connection of rdev database.
    This will be relaxed for general release of notes files.
06.22.93 by CH on TI4000:
1.  Added routing for MB application code.
2.  On EMP field when app = mb rerouted to mb/applhelp.p.
06.09.93 by CAH on B28@111 Log#0000:
1.  Changed po application routine to ic/applhelp.p.  As of this date
    there is no po/login.p or po/applhelp.p.
04.17.93 by CH on TI4000:
1.  oe is now a possible ws_app value; however help routing changed to
    ic/applhelp.p as oe/applhelp.p does not exists.  Ultimately these may
    be split.
10.16.92 by CH:
1.  Added trap for fast readkey to provide alternate routing.
10.15.92 by CH:
1.  Added DY as application for run dy/applhelp.p.
10.06.92 by CH:
1.  Add trap for custtype, was running custpick.
2.  Made *co* *co to eliminated conflict with contract.
3.  Add call for state pick.
4.  Add call for country pick.
5.  Add call for calendar pick.
6.  Add call for period pick.
7.  Add call for text pick.
*/
{rc/loginv.i}
&scoped-define app_system asi
/* message keylabel(lastkey). */
if can-do("F11,f21,SHIFT-F1",keylabel(lastkey)) then do:
        message "Notes!".
        run rc/fmnotes.p.
        return.
end.
if can-do("F12,f22,SHIFT-F2",keylabel(lastkey)) then do:
        {rc/listadd.i ws_context program-name(2)}
        {rc/listadd.i ws_context frame-name}
        {rc/listadd.i ws_context frame-field}
        message "Calling helper for context:" ws_context.
        run rc/helper.p.
        return.
end.
if can-do("F13,f23,SHIFT-F3",keylabel(lastkey)) then do:
        lo_co = ws_co.
        ws_co = 1.
        run pm/devlog.p.
        ws_co = lo_co.
        return.
end.
if can-do("F14,f24,SHIFT-F4",keylabel(lastkey)) then do:    /* 9611 CAH */
        run rc/showpstk.p.
        return.
end.
&IF "{&app_system}" = "RPRO" &THEN 
if can-do("CTRL-F*", keylabel(lastkey)) then do:
    if top-debug then do: MESSAGE KEYLABEL(LASTKEY). end.
    find rcalias
    where rcalias.alias_program = keylabel(lastkey) no-lock no-error.
    if avail rcalias
    and rcalias.program > ""
    then do:
        if top-debug then do:
            DISPLAY RCALIAS WITH FRAME F-ALIAS OVERLAY.
        end.    
        run rc/runifok.p (rcalias.program).
        return.
    end.
end.
&ELSEIF "{&APP_SYSTEM}" = "ASI" &THEN 
    run applhelp.p.
&ENDIF
if frame-field matches "*sic*" then do: /* 9611 cah */
    run rc/sicpick.p.
    if lib_recid_ret = ? then leave.
    frame-value = ws_sic.
end.
else
if frame-field matches "*vendor*" then do:
    run ap/vendpick.p.
    if lib_recid_ret = ? then leave.
    frame-value = ws_vendor.
end.
else if frame-field matches "*amt*" or frame-field matches "*amount*"
     OR FRAME-FIELD MATCHES "*cost*" or frame-field matches "*quan*" then do:
    run rc/calc.p.
end.
else if frame-field matches "*date*" then do:
    run rc/calend.p.
end.
else if frame-field matches "*div*" then do:
    run rc/divpick.p.
    if lib_recid_ret = ? then leave.
    else frame-value = ws_div.
end.    
else if frame-field matches "*dept*" then do:
    run rc/deptpick.p.
    if lib_recid_ret = ? then leave.
    else frame-value = ws_dept.
end.
else if frame-field matches "*unit*" then do:
    run rc/deptpick.p.
    if lib_recid_ret = ? then leave.
    else frame-value = ws_unit.
end.    
else if frame-field matches "*style*" then do:
    run ic/mastpick.p.
    if lib_recid_ret = ? then leave.
    else frame-value = ws_style.
end.
else if frame-field matches "*bin*"         then run ic/applhelp.p.
else if frame-field matches "*register*"    then run ic/applhelp.p.
else if frame-field matches "*custtype*"    then run ar/applhelp.p.
else if frame-field matches "*cust*" then do:
    run ar/custpick.p.
    if lib_recid_ret = ? then leave.
    frame-value = ws_cust.
end.
else if frame-field matches "*emp*" and ws_app <> 'MB' then do:
    run rc/emppick.p.
    if lib_recid_ret = ? then leave.
    frame-value = ws_emp.
end.
else if frame-field matches "*job*" then do:
    run tb/jobpick.p.
    if lib_recid_ret = ? then leave.
    frame-value = ws_job.
end.
else if frame-field matches "*acct*" then do:
    run gl/acctpick.p.
    if lib_recid_ret = ? then leave.
    frame-value = ws_acct.
end.
else if frame-field matches "*bank*" then do:
    run rc/bankpick.p.
    if lib_recid_ret = ? then leave.
    frame-value = ws_banknum.
end.
else if frame-field matches "*jsc*" then do:
    run value(ws_app + "/jscpick.p").
    if lib_recid_ret = ? then leave.
    if ws_jsc = ? then leave.
    else DO: frame-value = ws_jsc. APPLY KEYCODE("TAB"). END.
end.
else if frame-field matches "*term*" then do:
    run rc/termpick.p.
    if lib_recid_ret = ? then leave.
    if ws_terms = ? then leave.
    else frame-value = ws_terms.
end.
else if frame-field matches "*shipvia*" then do:
    run rc/shippick.p.
    if lib_recid_ret = ? then leave.
    if ws_shipvia = ? then leave.
    else frame-value = ws_shipvia.
end.
else if frame-field matches "*state*" then do:
    run rc/statpick.p.
    if lib_recid_ret = ? then leave.
    if ws_state = ? then leave.
    else frame-value = ws_state.
end.
else if frame-field matches "*local*" or frame-field matches "*taxjd*"
then do:
    run rc/taxpick.p.
    if lib_recid_ret = ? then leave.
    if ws_local = ? then leave.
    else frame-value = ws_local.
end.
else if frame-field matches "*country*" then do:
    run rc/ctrypick.p.
    if lib_recid_ret = ? then leave.
    if ws_shipvia = ? then leave.
    else frame-value = ws_shipvia.
end.
else if frame-field matches "*co" then do:
/*
10.06.92 by CH: Eliminated trailing match to fix problem noted below
12.04.91 by CH: Had to move *co* down as conflicts with *Contract*
*/
    run rc/copick.p.
    if lib_recid_ret = ? then leave.
    frame-value = ws_co.
end.
else if frame-field matches "*calendar" then do:
    run rc/clndpick.p.
    if lib_recid_ret = ? then leave.
    if ws_calendar = ? then leave.
    else frame-value = ws_calendar.
end.
else if frame-field matches "*per" then do:
    run rc/cperpick.p.
    if lib_recid_ret = ? then leave.
    if ws_per = ? then leave.
    else frame-value = ws_per.
end.
else if frame-field matches "*text*" then do:
    run ar/textpick.p.
    if lib_recid_ret = ? then leave.
    frame-value = ws_textid.
end.
else if frame-field matches "*emul*" then do:   /* 9807 CAH */
    run rc/emulpick.p.
    if lib_recid_ret = ? then leave.
    frame-value = save_emulation.
end.     
else if frame-field matches "*printer*" 
     or frame-field matches "*printfid*"
then do:    /* 9807 CAH */
    run rc/prtpick.p.
    if lib_recid_ret = ? then leave.
    frame-value = save_printfid.
end.    
else if frame-field matches "*printset*" 
     or frame-field matches "*setupstring*" 
     or frame-field matches "*setupdesc*"
then do:
    run rc/psetpick.p.
    if lib_recid_ret = ? then leave.
    frame-value = save_printstring.
end.     
else if (frame-file = "PrintEmul") or (Frame-File = "PrintSetup") then do:
    run rc/prthelp.p.
end.
else if frame-field matches "*city*" then do:
    run rc/citypick.p.
    if lib_recid_ret = ? then leave.
    if ws_city = ? then leave.
    else frame-value = ws_city.
end.
else if frame-field matches "*zip*" then do:
    run rc/zippick.p.
    if lib_recid_ret = ? then leave.
    if ws_zip = ? then leave.
    else frame-value = ws_zip.
end.
else if ws_app = "AR" then run ar/applhelp.p.   /* 04.09.92 by CH */
else if ws_app = "AP" then run ap/applhelp.p.   /* 09.27.93 by ch */
else if ws_app = "DY" then run dy/applhelp.p.   /* 10.15.92 by CH */
else if ws_app = "ED" then run ed/applhelp.p.   /* 02.21.97 by CH */
else if ws_app = "GL" then run GL/applhelp.p.   /* 04.18.92 by CH */
else if ws_app = "MA" then run ma/applhelp.p.
else if ws_app = "MB" then run mb/applhelp.p.
else if ws_app = "IC" then run ic/applhelp.p.
else if ws_app = "OE" then run ic/applhelp.p.
else if ws_app = "PO" then run ic/applhelp.p.
else if ws_app = "FS" then run fs/applhelp.p.   /* 04.09.92 by CH */
else if ws_app = "PC" then run pc/applhelp.p.   /* 12.04.93 by ch */
else if ws_app = "PM" then run pm/applhelp.p.   /* 11.20.92 by CH */
ELSE IF WS_APP = "TB" THEN RUN tb/applhelp.p.
ELSE IF WS_APP = "TX" THEN RUN tx/applhelp.p.   /* 01.16.97 by CAH */
/*  04.23.92 by CH: Had to move *svc* down as crosses applications */
else if frame-field matches "*svc*" then do:
    run value(ws_app + "/svcpick.p").
    if lib_recid_ret = ? then leave.
    if ws_svc = ? then leave.
    else frame-value = ws_svc.
end.
else
    message "No help is available".
