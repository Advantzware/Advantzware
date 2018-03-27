/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\fmdoc.p
**       By: Chris Heins (c) 1997 Report Concepts, Inc.
** Descript: edi document controls editor.
10.27.98 by CAH on \\ricky\rv8 Log#0000:
1.  Added assignment of called and caller prior to running doc editor.
01.15.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added incrementation of eddoc.c-fatime on reset.  This field may then
be used by the outbound processing program to create unique identifiers on 
retransmitted documents, e.g. BSN01 on ASN's.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/stripvar.i NEW}
{rc/statline.i}
def var view_direction like ws_direction no-undo initial "*".
def var view_set like eddoc.setid no-undo    initial "*".
def var view_status-flag like eddoc.status-flag no-undo initial "*".
{rc/viewline.i &displayf="ws_partner view_direction view_set view_status-flag"}
IF ws_partner = "*" THEN
DO:
  FIND FIRST edmast NO-LOCK NO-ERROR.
  ws_partner = IF AVAIL edmast THEN
  edmast.partner ELSE ""
  .
END.
do while true:
{ed/getpart.i}
do with frame f-view:
    update
        view_direction
        view_set
        view_status-flag.
end.
{rc/scrvars.i}
FORM
  WITH FRAME f-det DOWN COLUMN 2 row 3 OVERLAY.
FORM
  WITH FRAME f-details COLUMN 1 side-labels
  3 COLUMNS
  .
FORM
  WITH FRAME f-inbound CENTER 3 COLUMNS OVERLAY COLOR VALUE(c_pop)
  TITLE "Inbound Transaction Fields".
FORM
  WITH FRAME f-outbound CENTER 2 COLUMNS OVERLAY COLOR VALUE(c_pop)
  TITLE "outbound Transaction Fields".
{rc/scrfm3.i
  &init =
  "f-det-title = 'EDI DOCUMENT INQUIRY FOR: ' + ws_partner."
  &FUNCTIONS  = "NYYY"
  &ROWS       = 5
  &TITLE      = "f-det-title"
  &FILE       = "eddoc"
  &INDEX      = " "
  &CONDITION  = "WHERE eddoc.partner = ws_partner
    and (if view_direction = '*' then true else eddoc.direction = view_direction)
    and (if view_set = '*' then true else eddoc.setid = view_set)
    and (if view_status-flag = '*' then true else eddoc.status-flag = view_status-flag) "
  &POSIT      = " "
  &DETFUNCT   = " "
  &CHOOSE     = "seq"
  &KEYEDIT    = " "
  &DISPLAYF   =
  "
  EDDoc.Seq
  EDDoc.Direction  column-label 'I/O'
  EDDoc.SetID
  EDDoc.DocID       format 'x(15)'
  EDDoc.Unique-Order-No format '>>>>>>'
  EDDoc.Error-count column-label 'Errs'
  EDDoc.Stat
  EDDoc.Status-Flag
  EDDoc.Posted
  EDDoc.OpenItem
"
  &DATAEDIT   = " "
  &TERMKEY    = " "
  &UPFLDS     =
  "
  EDDoc.EDI_Agency
  EDDoc.EDI_Standard
  skip
  EDDoc.UserRef
  EDDoc.Set-Test-Prod
  eddoc.st-code
  EDDoc.DocSeq format '>>>>>>'
  EDDoc.Unique-SDQ-No
  EDDoc.AddDate
  EDDoc.AddTime
  EDDoc.AddOp
  EDDoc.ChgDate
  EDDoc.ChgTime
  EDDoc.ChgOp
"
  &HELPKEY    = " "
  &DETEDIT    = " "
  &ADDCODE    = "assign eddoc.partner = ws_partner."
  &addpost    =
  "_menu: do while true:
 strip-list = 'Document editor,Inbound fields,Outbound fields,Reset,Exit'.
 run rc/strip.p.
 if strip-sel[1] begins 'Exit' or strip-f4 then leave _menu.
 else if strip-sel[1] begins 'Reset' then do:
   assign eddoc.openitem = false eddoc.stat = 0 eddoc.status-flag = 'RES'
        eddoc.posted = false
        eddoc.c-fatime = eddoc.c-fatime + 1 /* 9801 CAH resend counter */.
   display eddoc.openitem eddoc.stat eddoc.status-flag eddoc.posted
   with frame f-det.
 end.
 else if strip-sel[1] begins 'Document' then do:
   next_program = 'ed/fm' + eddoc.setid + '.p'.
   run rc/chkifok.p (next_program, output ws_logical, output ws_char).
   if ws_logical = false then do:
    bell.
    message color value(c_err) 'Document editor' next_program 'was not found'.
    next.
   end.
   hide frame f-det no-pause.
   hide frame f-details no-pause.
   called = true.
   caller = program-name(1).
   run value(next_program).
   view frame f-det.
   view frame f-details.
 end.
 else if strip-sel[1] begins 'Inbound' then do:
    update
  EDDoc.ISA
  EDDoc.GS
  EDDoc.ST
  skip
  EDDoc.FGAgency
  EDDoc.FGID
  EDDoc.Version
  skip
  EDDoc.FGSender format 'x(12)'
  EDDoc.FGDate
  EDDoc.FGTime
  skip
  EDDoc.FGRecvID format 'x(12)'
  EDDoc.InterpDate
  EDDoc.InterpTime
  EDDoc.Recv-Test-Prod
    with frame f-inbound.
    hide frame f-inbound no-pause.
 end.
 else if strip-sel[1] begins 'Outbound' then do:
    update
  EDDoc.C-FADate
  EDDoc.C-FATime
  EDDoc.P-FADate
  EDDoc.P-FATime
    with frame f-outbound.
    hide frame f-outbound no-pause.
 end.
 end.   /* _menu */
 assign {rc/stampcht.i eddoc}.
"
  &delcode    = "next_program = 'ed/fm' + eddoc.setID + 'del.p'.
    if search(next_program) <> ? then run value(next_program)
        (input recid(eddoc))."
  }
/*
&DATAGO     = " "
&DETGO      = " "
&HASHDISP   = " "
&HASHPLUS   = " "
&HASHMINUS  = " "
*/
hide frame f-details no-pause.
hide frame f-det no-pause.
end.
status default.
