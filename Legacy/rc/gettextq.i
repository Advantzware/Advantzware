/*  RC/GETTEXTQ.I - GET TEXTFILE AND QUOTE IF NECESSARY
06.17.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added code to default quoter variables from and save into registry.

04.29.97 by CAH on \\ricky\rprodemo Log#0000:
1.  Added check that ws_char points to a valid filename, if not no default.
05.08.96 by CAH on Ricky@812<rprodemo> Log#0000:
1.  Added default of ws_char from filename if found.
11.13.95 by CAH on NCR3550@RD-USA [/home/dpoch] Log#0000:
1.  Added f5 (function get) to override options, e.g. delimiter / columns.

05.27.95 by CAH on Compaq972 @812:
1.  Changed osquoter.p call from prodict/misc to rc.

01.11.93 BY CH:
1.  Made msdos compatible.
2.  Changed to use osquoter.p instead of manually generated call.
*/
do with frame f-textfile color value(c_pop):

/*
do for rpro.registry on error undo, retry:
find rpro.registry
where rpro.registry.key = "quoter/dflt/" + program-name(1) no-lock no-error.
if avail rpro.registry then do:
    assign
    ws_char = entry(1, rpro.registry.v-char)
    answer = rpro.registry.v-logical
    ws_delim = entry(2, rpro.registry.v-char)
    ws_colum = entry(3, rpro.registry.v-char).
    if ws_delim = "" then ws_delim = ?.
    if ws_colum = "" then ws_colum = ?.
end.    
end.
*/
filename = if search(ws_char) <> ? then search(ws_char) else filename.
answer = if filename matches "*~.q" then true else answer.
update
    filename no-label skip
    answer label "Has this file been run through Quoter?"
with frame F-TEXTFILE center side-labels
    title " IMPORT FILENAME (" + kblabel("get") +  "=Options) "
editing:
    readkey.
    if {rc/termkey.i} then do:
        if frame-field = "filename" and input frame f-textfile filename
        matches "*~.q" then do:
                answer = true.
                display answer.
        end.
    end.
    if keyfunction(lastkey) = "get" then do:
        update
            ws_delim
            ws_colum
        with frame f-qopt center overlay
            title "Quoter Options".
        hide frame f-qopt no-pause.
        if ws_colum <= " " then ws_colum = ?.
        next.
    end.
    apply lastkey.
end.
check_filename = search(filename).
if check_filename = ? then do:
    bell.
    message "Cannot find " filename " - Aborted".
    undo, retry.
end.
if answer then quoted_filename = check_filename.

If not answer then do:
    IF can-do("MSDOS,WIN*", OPSYS)
    AND R-INDEX(CHECK_FILENAME,'.') > 0
        THEN QUOTED_FILENAME
            = SUBSTRING(CHECK_FILENAME,1,R-INDEX(CHECK_FILENAME,'.')) + 'q'.
    ELSE quoted_filename = check_filename + ".q".
    RUN rc/osquoter.p
        (INPUT CHECK_FILENAME, ws_delim, ws_colum, QUOTED_FILENAME).

END.  /* not answer */
end.

/*
do for rpro.registry on error undo, retry:
find rpro.registry
where rpro.registry.key = "quoter/dflt/" + program-name(1) exclusive-lock no-error.
if not avail rpro.registry then do:
    create rpro.registry.
    assign
        rpro.registry.key = "quoter/dflt/" + program-name(1)
        rpro.registry.co = ws_co
        rpro.registry.app = ws_app. 
end.
assign
    rpro.registry.v-char = check_filename + "," 
        + (if ws_delim = ? then "" else ws_delim) 
        + "," 
        + (if ws_colum = ? then "" else ws_colum)
    rpro.registry.v-logical = answer.
end.    /* do for rpro.registry */    
*/
ws_char = quoted_filename.
input {1} from VALUE(quoted_FILENAME) no-echo.
