/*  -------------------------------------------------------------------------
GETPRINT.i
Standard Printer Selection
-------------------------------------------------------------------------
08.31.98 by CAH on \\ricky\robj8\ Log#0000:
1.  In version 8 you may not define a stream inside of an internal procedure.
Changed references to s_faxcover to use stream s-export which is defined in rc/loginv.i and is therefore available in all procedures.  This might
preclude faxing output when exporting and export capable.

05.12.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added QUOTES around clipboard references per fred kinsler.

07.08.96 by CAH on Ricky@812<rprodemo> Log#0000:
1.  Added call to lookup registry value of page_length if no explicit value
was set by caller.
06.25.96 by CAH on CDS972@812<rprodemo> Log#0000:
1.  Added defaulting of export filename.
2.  Added unix opsys support, "thru lp -dxxxx -s" is typical dev spec.

06.09.96 by CAH on Ricky@812<rprodemo> Log#0000:
1.  Removed rc/printv.i inclusion from here, moved it to rc/hdg-xxxx.i.
2.  Added support for export option.

02.25.96 by CAH on CDS972@812<rprodemo> Log#0000:
1.  Minor correction on printstring check, was looking for <= ' '
however, leading escape code would be less than space, 27hex vs. 30h.
06.27.93 by CH on TI4000:
1.  Added parameter 2 to allow page length to be passed.  If missing
program assumes former default of 63.  This allows preprinted forms
to be printed without the need to close and reopen the print stream.

03.02.93 by ch:
1.  Minor change on check for nonblank printstring: was ' ', now <= ' '.
2.  Disabled MSDOS output to terminal restriction.  Appears to be ok in v6.2.

12.28.92 BY CH:
1.  Added support for output to CLIPBOARD.

10.25.92 BY CH:
1.  Split printer entry, picking, selection etc. into rc/getprint.p.
2.  Split printstring conversion into rc/prtcvt.p.
3.  The changes above should reduce code size and compile times significantly.

04.23.92 BY CH:
1.  CHANGED var EM TO WS_EMULATION, WHICH IS REQUIRED DUE TO FMX3.I
USING EXISTING varS.  THIS ALLOWS RC/PSETPICK.P TO SELECT FROM
THIS CODE OR FROM THAT SETUP PROGRAM.

02/27/92 B. HEINS
1. Added {1} to all OUTPUT statements as way of passing a NAMED STREAM.


11.07.91 by CH:
1.  Made 10.25.91 change conditional on OPSYS <> MSDOS - get error,
conflicting use if MSDOS.

10.25.91 by CH:
1.  Made output to screen PAGED - progress automatically pauses where
necessary.

02.22.91 by ch:

1.  Experimental version which will save the last setup string entered
and issue it if no setup is selected.

02.14.91 by ch:

1.  Hard wired for dos/novell device = lpt1:.  Queue name is required
in printdev record for use by capture connection command.  If printing
is done to this queue you end up with a file named lsqueue_

01.18.90 JZ
See EOF comments for associated info.
Added functionality:
01.  Select printer by name by pressing HELP - scroll pick.
02.  Select setup once printer has been established.  - F3.
03.  Output to unique file name if input is 'F'.

This version of GetPrint allows for enhanced printer support.  The RPro
system now supports three additional global files: PrintDev, associating user-
friendly names with system print devices; PrintEmul, defining escape sequences
for various types of printers/emulations; and PrintSetup, defining default
escape sequences that are to be sent to a given type of printer for a given
type of report.

12.07.90 JZ
restructured and set message for printing to 'printer' when 'P' selected.

*/

batch_job = FALSE.
IF export_capable THEN
_export:
DO WITH FRAME f-export
    CENTER SIDE-LABELS TITLE COLOR VALUE(c_err) ' EXPORT OPTION '
    COLOR VALUE(c_pop)
    ON ERROR UNDO, RETRY:
    
  UPDATE
    export_opt AUTO-RETURN
    HELP 'Enter Y TO EXPORT report data TO a file'.
  IF export_opt THEN
  DO:
    RUN rc/expfname.p (OUTPUT export_fid).
    UPDATE
      export_fid format 'x(44)'
      label 'to File'
      HELP 'Enter name OF EXPORT file'.
    ws_char = SEARCH(export_fid).
    IF ws_char <> ? THEN
    DO:
      BELL.
      HIDE MESSAGE NO-PAUSE.
      MESSAGE COLOR VALUE(c_wrn) 'File' ws_char 'already exists!'.
      export_action = 'O'.
      MESSAGE 'Select desired action:
      (O)verwrite, (A)ppend, (R)etry'
      UPDATE export_action AUTO-RETURN.
      IF export_action = 'R' THEN
      UNDO _export, RETRY _export.
    END.
    ELSE
    export_action = 'O'.
    IF export_action = 'O'
      THEN
    OUTPUT STREAM s-export TO VALUE(export_fid).
    ELSE
    IF export_action = 'A'
      THEN
    OUTPUT STREAM s-export TO VALUE(export_fid) APPEND.
    export_open = TRUE.
  END.    /* if export_opt */
  ELSE
  export_open = FALSE.
END.
ELSE
export_open = FALSE.

/* 9807 CAH: Added to allow rc/getprint.p to default the correct setup ... */
/* print-width = FRAME hdg-std:width-chars. */
IF print-width = 0 THEN
print-width = 80.   /* default */
IF print-width > 80 THEN
hdg_widerpt = TRUE.
ELSE
hdg_widerpt = FALSE.

RUN rc/getprint.p
  (OUTPUT printfid, OUTPUT printdest, OUTPUT printstring).
IF KEYFUNCTION(LASTKEY) = 'END-ERROR' THEN
UNDO, RETURN.

IF '{2}' > '' THEN
page_len = INTEGER({2} + 0).
ELSE
DO:
  RUN rc/regkey.p (INPUT 'page_length', OUTPUT ws_char).
  page_len = integer(ws_char).    /* 9607 CAH */
END.


IF NOT batch_job THEN
_map_printfid:
DO:
  IF PRINTDEST = "CLIPBOARD" AND OPSYS = 'MSDOS' OR OPSYS = 'WIN32' THEN
  DO:
    OUTPUT {1} TO "CLIPBOARD" PAGED PAGE-SIZE VALUE(page_len).
  END.
  ELSE
  IF printfid = 'S' OR printfid = '' THEN
  DO:
    /* IF OPSYS <> 'MSDOS' THEN */ OUTPUT {1} TO TERMINAL PAGED.
  END.
  ELSE
  IF PRINTfid <> 'S' THEN
  DO:
    MESSAGE 'Wait, printing report ' + hdg_desc + ' ==> ' +
      (IF printfid = 'P' THEN 'printer' ELSE
      IF printfid = 'V' THEN 'viewer'  ELSE printdest) + ' ...' /* wfk */ view-as alert-box .
    IF printfid = 'P' THEN
    DO:
      OUTPUT {1} TO printer PAGED PAGE-SIZE VALUE(page_len).
    END.
    ELSE
    IF OPSYS = "unix"
      THEN
    OUTPUT {1} thru VALUE(printdest) paged page-size VALUE(page_len).
    ELSE
    OUTPUT {1} TO VALUE(printdest) PAGED PAGE-SIZE VALUE(page_len).
    IF printfid matches "*FAX*" THEN
    DO:
      IF OPSYS = "CTOS" THEN
      PUT {1} CONTROL '@' ws_fax '@'.
      IF SEARCH("faxcover.q") <> ? THEN
      DO:
        INPUT STREAM s-export FROM SEARCH("faxcover.q") no-echo.
        DEF var cover_line AS char FORMAT "x(80)" NO-UNDO.
        REPEAT:
          SET STREAM s-export cover_line WITH FRAME f-faxcover
            no-box width 132.
          PUT {1} cover_line SKIP.
        END.
        INPUT STREAM s-export close.
      END.  /* found faxcover */
    END.
    IF printstring = '' THEN
    printstring = save_printstring.
    ELSE
    DO:
      RUN rc/prtcvt.p (INPUT-OUTPUT printstring, INPUT 0, INPUT 'no-unit').
    END.
    IF printstring <> "" THEN
    DO:
      PUT {1} CONTROL printstring.
      save_printstring = printstring.
      printstring = ''.
    END.
    save_printfid = printfid.
  END.
END.    /* _map_printfid */
ASSIGN hdg_text = "".
