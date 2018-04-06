/* contact_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'contact_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="contact" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="contact" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="contact" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="contact" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/
def var i as int no-undo. 
def var v-start-compress as char init "" no-undo.
def var v-end-compress as char init "" no-undo.
def var v-delim as char format "x(1)" init '~~' no-undo.
def var print-head as log init yes no-undo.
def var v-last-date as date format "99/99/9999" label "Last Ord Date" no-undo.
def var v-cust-name like cust.name no-undo.
def var v-sname like sman.sname no-undo.
def var v-address as cha form "x(120)" no-undo.


assign v-start-compress = ""
       v-end-compress = "".
      
find first printer where printer.company eq gcompany
                     and printer.loc     eq gloc
                     and printer.pr-no   eq selected-printer no-lock no-error.
if avail printer then
do:
  if printer.pr-cmd ne "" then
  do i = 1 to num-entries(printer.pr-cmd):
    if entry(i,printer.pr-cmd) ne ? then
      v-start-compress = v-start-compress +
			  chr(int(entry(i,printer.pr-cmd))).
  end.
end.

put control v-start-compress.

if tbMailMrge then
  output stream s-mail to value(filename) page-size 0.

/*
for each cust where cust.company eq gcompany
                and cust.cust-no ge begin_cust-no
                and cust.cust-no le end_cust-no,
*/
for each contact where contact.company eq selected-company
                   and contact.cust-no ge begin_cust-no
                   and contact.cust-no le end_cust-no
                   and contact.sman    ge begin_contact_sman
                   and contact.sman    le end_contact_sman
                   and contact.zip     ge begin_contact_zip
                   and contact.zip     le end_contact_zip
    NO-LOCK 
    BREAK BY contact.sman
          BY contact.cust-no
    WITH FRAME {&FRAME-NAME} STREAM-IO NO-BOX WIDTH 170:

  find first cust where cust.company eq selected-company
                    and cust.cust-no eq contact.cust-no
                  no-lock no-error.
  find sman where sman.company eq selected-company
              and sman.sman eq contact.sman
              no-lock no-error.

  if tbMailMrge and contact.maillist then
  do:
    if print-head then
    do:
      put stream s-mail unformatted
          "sirname" v-delim
          "firstname" v-delim
          "lastname" v-delim
          "companyname" v-delim
          "address1" v-delim
          "address2" v-delim
          "city" v-delim
          "state" v-delim
          "zip" v-delim
          "country" v-delim skip.
      assign print-head = no.
    end.  
    v-cust-name = if avail cust then cust.name else "".
    
     put stream s-mail unformatted
          trim(contact.sirname) v-delim
          trim(contact.first-name) v-delim
          trim(contact.last-name) v-delim 
/*          trim(cust.name) v-delim */
          trim(v-cust-name) v-delim 
          trim(contact.addr1) v-delim
          trim(contact.addr2) v-delim
          trim(contact.city) v-delim
          trim(contact.state) v-delim
          trim(contact.zip) v-delim
          trim(contact.country) v-delim skip.
    create notes.
    assign notes.rec_key = contact.rec_key
           notes.note_date = TODAY
           notes.note_time = TIME
           notes.user_id = USERID("NOSWEAT")
           notes.note_title = FILL-IN-Title
           notes.note_text = "Automatic Note Generation from Mail Merge Report. " +
                                    string(notes.note_date,"99/99/9999") + " " +
                                    string(notes.note_time,"HH:MM:SS AM").
  end.

  v-sname = if avail sman then sman.sname else "".
  /* ========== address display    ==================== */
  v-address = "".
  IF contact.contact-loc eq "C" then
  DO:
    FIND cust WHERE cust.company = gcompany
                AND cust.cust-no = contact.cust-no
      NO-LOCK NO-ERROR.
    IF avail cust then
      ASSIGN v-address = cust.addr[1] + " "  +
                         (if cust.addr[2] <> "" then cust.addr[2] else "") + " " +
                         cust.city + " " + cust.state + ", "+ cust.zip + " " + cust.country
                        .
  end.          
  else IF contact.contact-loc eq "S" then
  DO:
    FIND shipto WHERE shipto.company = gcompany
                  AND shipto.ship-id = contact.ship-id
                  AND shipto.cust-no = contact.cust-no
         NO-LOCK NO-ERROR.
    IF avail shipto THEN 
          ASSIGN v-address = shipto.ship-addr[1] + " " +
                           (if shipto.ship-addr[2] <> "" then 
                            shipto.ship-addr[2] else "" ) + " " +
                         shipto.ship-city + " " +
                         shipto.ship-state + ", " +
                         shipto.ship-zip + " " +
                         shipto.country 
                         .
    
  end.
    
  if first-of(contact.cust-no) THEN  
         display "Sales Rep:" /*contact.sman  */ +
             v-sname form "x(26)"  
            "Customer:" + cust.name form "x(33)" when avail cust
            cust.cust-no when avail cust SKIP
            /*skip(1)*/ 
            "Customer Address:" v-address form "x(120)" 
            with no-attr-space STREAM-IO frame comp-head1 no-labels NO-BOX WIDTH 150.
                       
  if tbLastOrder then
  do:
    assign v-last-date = ?.
    for each oe-ord where oe-ord.company eq gcompany
                      and oe-ord.cust-no eq contact.cust-no
                    no-lock break by oe-ord.ord-date desc:
      assign v-last-date = oe-ord.ord-date.
      leave.
    end.
  end.

    display "( )" column-label "Mail"
            contact.first-name format "x(15)"
            contact.last-name format "x(15)" 
            contact.contact-title format "x(4)" 
            contact.phone format "XXX-XXX-XXXX" 
            contact.fax format "XXX-XXX-XXXX" 
            contact.email format "x(30)" 
            v-last-date
            skip(1).  

  if last-of(contact.cust-no) then do:
     display fill("=",113) form "x(113)" skip(1)
             with frame ln no-label no-box width 115.
  end.  
end.

if tbMailMrge then
  output stream s-mail close.

if avail printer then
do:
  if printer.pitch ne "" then
  do i = 1 to num-entries(printer.pitch):
    if entry(i,printer.pitch) ne ? then
      v-end-compress = v-end-compress + chr(int(entry(i,printer.pitch))).
  end.
end.

put control v-end-compress.

/*
{methods/lstlogic/shownote.i &db_table="contact" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="contact" &col="5" &frame-name="f-miscflds"}
*/
