/* mailx.p
 *
 * handle the sending of mail from a windows environment
 *
 * lib/ptsetvar.p getMailInfo() will run this
 *
 *
 */

{lib/protop.i}

define variable msgObject    as com-handle no-undo.
define variable cfgObject    as com-handle no-undo.

define variable msgBody      as character  no-undo.

define variable smtpUser     as character no-undo.
define variable smtpPassword as character no-undo.
define variable smtpServer   as character no-undo initial "smtp.gmail.com".
define variable smtpPort     as integer   no-undo initial 465.

/* define variable mailCmd                      as character no-undo initial "mailx &3 -s '&2' &1 > /dev/null 2>&&1". */
/* define new global shared variable pt_mailcmd as character no-undo initial 'mailx "-s &1" '. */

define variable sendTo       as character no-undo.
define variable xreplyTo     as character no-undo.

define variable xLine        as character extent 16 no-undo.
define variable inLine       as character no-undo.
define variable uname        as character no-undo.
define variable mailxBody    as {&LNGCR}  no-undo.

define variable dirSep       as character no-undo.
define variable tmpFileName  as character no-undo.

define variable cfgDate      as date      no-undo.
define variable cfgTime      as integer   no-undo.

define variable i as integer no-undo. 

define stream emailCfg.
define stream inFile.
define stream inStrm.

if opsys = "unix" then
  dirSep = "/".
 else
  dirSep = "~\".

create widget-pool.

if not ( opsys begins "WIN" ) then
  do:
    input stream inStrm through value( "uname -a" ).
    import stream inStrm unformatted uname.
    input stream inStrm close.
  end.

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

return. 


define stream mailTmp.

procedure ptSendMail:

  define input parameter mailTo       as character no-undo.
  define input parameter replyTo      as character no-undo.
  define input parameter mailSubject  as character no-undo.
  define input parameter mailBody     as {&LNGCR}  /* character */ no-undo.		/* character text					*/
  define input parameter mailFileName as character no-undo.		/* a text file to be appended to the body of the e-mail	*/

  mailxBody = mailBody.

  if mailTo = "" then mailTo = sendTo.					/* default from etc/mail.cfg				*/

  if mailTo = "" then return.

  if replyTo = "" then replyTo = xreplyTo.

  if mailxBody <> "" then mailxBody = mailxBody + chr(10) + chr(10).
  if replyTo   <> "" then mailxBody = mailxBody + "Please reply to: " + replyTo + chr(10) + chr(10).
  if uname     <> "" then mailxBody = mailxBody + uname + chr(10) + chr(10).

  if mailFileName <> "" then
    do:
      mailxBody = mailxBody + substitute( "=== &1 ===", mailFileName ) + chr(10) + chr(10).
      input stream inFile from value( search( mailFileName )).
      repeat:
        inLine = "".
        import stream inFile unformatted inLine.
        mailxBody = mailxBody + chr(10) + inLine.
      end.
      mailxBody = mailxBody + chr(10).
      input stream inFile close.
    end.

  tmpFileName = substitute( "&1&2pt3mail&3.tmp", pt_tmpdir, dirSep, string( random( 0, 99 ), "99" )).

  if dbgMode >= 4 then
    do:

      /* mailCmd "mailx &3 -s '&2' &1" */

      output to value( pt_logdir + "/mail.log" ) unbuffered append.
      message {&NOW} "mailCmd: " substitute( pt_mailCmd, mailTo, mailSubject ).
      output close.

    end.

&IF DEFINED( OE10 ) &THEN
  copy-lob from mailxBody to file tmpFileName.
&ELSE
  output stream mailTmp to value( tmpFileName ).
  put stream mailTmp unformatted mailxBody skip.
  output stream mailTmp close.
&ENDIF

  if opsys begins "WIN" then
    os-command silent value ( "type " + tmpFileName + " | " + substitute( pt_mailCmd, mailTo, mailSubject )).
   else
    os-command silent value ( "cat "  + tmpFileName + " | " + substitute( pt_mailCmd, mailTo, mailSubject )).

  os-delete value( tmpFileName ).

  return.

end.
