/* ---------------------------------------------- */
/*  cecrep/jobhbp.p  factory ticket               */
/* ---------------------------------------------- */

&scoped-define PR-PORT FILE,TERMINAL,FAX_MODEM,VIPERJOBTICKET

def input parameter v-format as char.
DEF SHARED VAR s-prt-fgimage AS LOG NO-UNDO.

DEF VAR v-stock-no LIKE eb.stock NO-UNDO.
DEF VAR lv-sts-code AS cha INIT "O,R,C,T,N,X,Q" NO-UNDO.
DEF VAR lv-sts-desc AS cha INIT "O-Original,R-Repeat,C-Change,T-Transfer,N-New Customers,X-Complete Re-run,Q-Quality/Re-work" NO-UNDO.

DEF VAR r           AS   INT          NO-UNDO.
DEF VAR c           AS   INT          NO-UNDO.
DEF VAR v-text      AS   CHAR         NO-UNDO.
DEF VAR v-cs#-note  AS   CHAR         NO-UNDO.
DEF VAR v-eb-form   AS   CHAR         NO-UNDO.
DEF VAR v-die-no    LIKE eb.die-no    NO-UNDO.
DEF VAR v-plate-no  LIKE eb.plate-no  NO-UNDO.
DEF VAR v-tr-no     LIKE eb.tr-no     NO-UNDO.
DEF VAR v-cas-cnt   LIKE eb.cas-cnt   NO-UNDO.
DEF VAR v-layer-pad LIKE eb.layer-pad NO-UNDO.
DEF VAR v-cas-no    LIKE eb.cas-no    NO-UNDO.
DEF VAR vcount      AS INT NO-UNDO.
DEF VAR vlist       AS CHAR NO-UNDO.
DEF VAR vlast       AS INT NO-UNDO.


{jcrep/r-ticket.i "shared"}

DEF WORKFILE wrk-ink
    FIELD i-code   AS   CHAR FORMAT "x(10)"
    FIELD blank-no LIKE eb.blank-no
    FIELD i-dscr   AS   CHAR FORMAT "x(20)"
    FIELD i-qty    AS   DEC FORMAT  ">,>>9.9<"
    FIELD i-pass   AS   DEC.


DEF VAR vink AS INT NO-UNDO.
DEF VAR v-cus AS CHAR FORMAT "X(30)" EXTENT 4 NO-UNDO.
DEF VAR v-shp AS CHAR FORMAT "X(30)" EXTENT 4 NO-UNDO.
{sys/inc/VAR.i SHARED}
{cec/msfcalc.i}

def var v-po-no         like oe-ordl.po-no    no-undo.

DEF BUFFER xjob-hdr FOR job-hdr.
def var v-job-qty as int format "->>,>>>,>>9" NO-UNDO.
def var v-est-qty as int format "->>,>>>,>>9" NO-UNDO.
def var v-dsc like oe-ordl.part-dscr1 extent 2 NO-UNDO.
def var v-size as char format "x(26)" extent 2 NO-UNDO.
def var v-stypart like style.dscr NO-UNDO.
DEF VAR v-case-size AS CHAR NO-UNDO.
DEF VAR v-case-qty AS dec NO-UNDO.
def var v-up like eb.num-up NO-UNDO.
def var v-total-up like eb.num-up NO-UNDO.
DEF VAR v-case-count LIKE eb.cas-cnt NO-UNDO.
DEF VAR vpono     AS CHAR NO-UNDO.

{sys/inc/notes.i}
DO TRANSACTION:
   {sys/inc/tspostfg.i}
END.
{cecrep/jc-prem.i}
{custom/notesdef.i}

/*************************/
/* FUNCTIONS DEFINITIONS */
/*************************/

FUNCTION FNformat RETURNS char (INPUT v-text AS CHAR, v-len AS INT):

  DEF VAR vreturn AS CHAR.
  DEF VAR ventry  AS CHAR.
  ASSIGN v-text = REPLACE(v-text,CHR(10),"`")
         v-text = REPLACE(v-text,CHR(13)," ").
                   
  DO i = 1 TO NUM-ENTRIES(v-text,"`"):
     ASSIGN ventry = ENTRY(i,v-text,"`").

     DO WHILE TRUE:
     
        IF LENGTH(ventry) < v-len THEN DO:
           ASSIGN vreturn = vreturn + 
                            (IF vreturn <> "" THEN "`" ELSE "") + 
                            ventry.
           LEAVE.
        END.   
                        
        ASSIGN vreturn = vreturn + 
                         (IF vreturn <> "" THEN "`" ELSE "") + 
                         SUBSTRING(ventry,1,v-len)
               ventry = SUBSTRING(ventry,v-len + 1).

     END. /* DO WHILE TRUE: */
     
  END. /* DO i = 1 TO NUM-ENTRIES(v-text,"`"): */

  RETURN vreturn.
 
END FUNCTION.

FUNCTION FNnotes RETURNS char (INPUT v-reckey AS CHAR, v-codes AS CHAR, v-form-no AS INT):
   ASSIGN v-text = "".
   FOR EACH notes NO-LOCK
                  WHERE notes.rec_key = v-reckey 
                    AND CAN-DO(v-codes,notes.note_code)
                    AND notes.note_form_no = v-form-no, 
      FIRST dept NO-LOCK 
                 WHERE dept.code = notes.note_code
      BY notes.note_form_no 
      BY dept.fc 
      BY notes.note_date 
      BY notes.note_time:
 
      ASSIGN v-text = v-text +
                     /*(IF v-text <> "" THEN CHR(10) ELSE "") +*/
                      notes.note_text.
   END. /* FOR EACH notes NO-LOCK */
    
   RETURN v-text.
END FUNCTION.

/**************************/
/* PROCEDURES DEFINITIONS */
/**************************/
PROCEDURE pr-customer:
   assign
    i      = 0
    v-cus  = "".
   
   if cust.name ne "" then
     assign
      i        = i + 1
      v-cus[i] = cust.name.
      
   if cust.addr[1] ne "" then
     assign
      i        = i + 1
      v-cus[i] = cust.addr[1].
      
   if cust.addr[2] ne "" then
     assign
      i        = i + 1
      v-cus[i] = cust.addr[2].
      
   assign
    i        = i + 1
    v-cus[i] = trim(cust.city) + ", " + cust.state + "  " + cust.zip.

END. /* PROCEDURE pr-customer:*/


PROCEDURE PR-shipto:
        v-shp = "".
        find first oe-ordl
                where oe-ordl.company eq job-hdr.company
                  and oe-ordl.ord-no  eq job-hdr.ord-no
                  and oe-ordl.job-no  eq job-hdr.job-no
                  and oe-ordl.job-no2 eq job-hdr.job-no2
                  and oe-ordl.i-no    eq job-hdr.i-no
                no-lock no-error.
        IF AVAIL oe-ordl THEN 
            find first oe-rel
            where oe-rel.company eq cocode
              and oe-rel.ord-no  eq oe-ordl.ord-no
              and oe-rel.i-no    eq oe-ordl.i-no
              and oe-rel.line    eq oe-ordl.line
            no-lock no-error.
        IF AVAILABLE oe-rel THEN DO:
           find first shipto
              where shipto.company eq cocode
                and shipto.cust-no eq oe-rel.cust-no
                and shipto.ship-id eq oe-rel.ship-id
              no-lock no-error.  
          if avail shipto then DO:
             ASSIGN i = 0
                    v-shp = "".
             IF shipto.ship-name <> "" THEN
                ASSIGN i = i + 1
                       v-shp[i] = shipto.ship-name.
             IF shipto.ship-addr[1] <> "" THEN
                ASSIGN i = i + 1
                       v-shp[i] = shipto.ship-addr[1].
             IF shipto.ship-addr[2] <> "" THEN
                ASSIGN i = i + 1
                       v-shp[i] = shipto.ship-addr[2].                
             ASSIGN i = i + 1
                    v-shp[i] = TRIM(oe-rel.ship-city) + ", " +
                               oe-rel.ship-state + "  " + oe-rel.ship-zip. 
          END. /* if avail shipto then DO: */                                           
        end. /* IF AVAILABLE oe-rel THEN DO: */

       ASSIGN vpono = IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "".

END. /* PROCEDURE PR-shipto: */

PROCEDURE PR-getink:

   FOR EACH job-mat NO-LOCK
            WHERE job-mat.company = cocode
              AND job-mat.job     = job-hdr.job
              AND job-mat.frm     = eb.form-no,
      FIRST item NO-LOCK
            {sys/look/itemivW.i}
            AND item.i-no = job-mat.i-no:

      DO i = 1 TO 12:
         IF eb.i-code2[i] = job-mat.i-no THEN DO:

            FIND FIRST wrk-ink
                       WHERE wrk-ink.i-code   = eb.i-code2[i]
                         AND wrk-ink.blank-no = eb.blank-no
                         AND wrk-ink.i-pass   = eb.i-ps2[i]
                       NO-ERROR.
   
            IF NOT AVAILABLE wrk-ink THEN DO:
               CREATE wrk-ink.
               ASSIGN wrk-ink.i-code   = eb.i-code2[i]
                      wrk-ink.blank-no = eb.blank-no
                      wrk-ink.i-dscr   = eb.i-dscr2[i]
                      wrk-ink.i-pass   = eb.i-ps2[i].
            END. /* IF NOT AVAILABLE wrk-ink THEN DO: */
         END. /* IF eb.i-code2[i] = job-mat.i-no THEN DO: */
      END. /* DO i = 1 TO 12: */

      FIND FIRST wrk-ink
                 WHERE wrk-ink.i-code    = job-mat.i-no
                   AND (wrk-ink.blank-no = job-mat.blank-no OR
                       est.est-type = 4)
                 NO-ERROR.
                
      IF NOT AVAILABLE wrk-ink AND
               (job-mat.blank-no  = eb.blank-no OR
                (job-mat.blank-no = 0 AND eb.blank-no eq 1)) THEN DO:
        CREATE wrk-ink.
        ASSIGN wrk-ink.i-code   = job-mat.i-no
               wrk-ink.blank-no = eb.blank-no
               wrk-ink.i-dscr   = item.est-dscr
               wrk-ink.i-pass   = 1.
      END. /* IF NOT AVAILABLE wrk-ink AND */
      
      IF AVAILABLE wrk-ink AND
         ((est.est-type eq 4 AND eb.blank-no = job-mat.blank-no) OR
          est.est-type <> 4) THEN 
         ASSIGN wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.            
            
   END. /* FOR EACH job-mat NO-LOCK */
   
END. /* PROCEDURE PR-ink: */

/***********************************************/
/*                  MAIN LOOP                  */
/***********************************************/

FOR EACH job NO-LOCK USE-INDEX job
             WHERE job.company                   eq cocode
          and job.job-no                ge substr(fjob-no,1,6)
          and job.job-no                le substr(tjob-no,1,6)
          and job.stat                      ne "H",
          
   FIRST est NO-LOCK USE-INDEX est-no2
         where est.company = job.company
           AND est.est-no                    eq job.est-no 

   BREAK by job.job-no
         by job.job-no2:



   FOR EACH job-hdr NO-LOCK
                    where job-hdr.company               = job.company
                      and (job-hdr.ftick-prnt           eq reprint OR
                          PROGRAM-NAME(2) MATCHES "*r-tickt2*"  )
                      and job-hdr.job-no                ge substr(fjob-no,1,6)
                      and job-hdr.job-no                le substr(tjob-no,1,6)
            
                      and fill(" ",6 - length(trim(job-hdr.job-no))) +
                          trim(job-hdr.job-no) +
                          string(job-hdr.job-no2,"99")  ge fjob-no
            
                      and fill(" ",6 - length(trim(job-hdr.job-no))) +
                          trim(job-hdr.job-no) +
                          string(job-hdr.job-no2,"99")  le tjob-no
                    USE-INDEX job-no:
                   
      ACCUM 1 (COUNT).
      IF (ACCUM COUNT 1) = 1 THEN DO:
         FIND cust no-lock
                   where cust.company = job-hdr.company
                     and cust.cust-no = job-hdr.cust-no.
         RUN PR-customer.  
         RUN pr-shipto. 
              
      END.  
  
/* GET CS# NOTE */                
      ASSIGN v-cs#-note = "".        
      FOR FIRST itemfg FIELDS (itemfg.rec_key) NO-LOCK
                       WHERE itemfg.company = job-hdr.company
                         AND itemfg.i-no = job-hdr.i-no,
         FIRST notes FIELDS (note_title) NO-LOCK
                     WHERE notes.rec_key = itemfg.rec_key
                       AND NOTE_CODE = "CS#":
         ASSIGN v-cs#-note = notes.note_title.       
      END. /* FOR FIRST itemfg NO-LOCK */        
        
         
      FOR EACH ef NO-LOCK
                  WHERE ef.company = job-hdr.company
                    AND ef.est-no  = job-hdr.est-no
                    AND ef.form-no = job-hdr.frm:


         PUT "<P10></PROGRESS>" SKIP "<FCourier New><C2>" SKIP      
             "<#1><C1><FROM><C106><R+70><RECT><||3><C80><P10>"       
             "<=1><R-2><C33><B><P18>JOB #: " job-hdr.job-no "-" 
                       STRING(job.job-no2,"99") "    FORM #: " job-hdr.frm 
                       "</B><P10>" 
             "<=1><C2>DATE:" TODAY "<C20>CUSTOMER:" v-cus[1]
                       "<C70>SHIPPING DATE:" STRING(job-hdr.due-date,"99/99/99") SKIP
             "<=1><R+1><C2>PO: " vpono "<C20>ADDRESS :" v-cus[2] 
                       "<C70>SHIP TO:" v-shp[1] SKIP
             "<=1><R+2><C28>" v-cus[3] "<C77>" v-shp[2] SKIP 
             "<=1><R+3><C28>" v-cus[4] "<C77>" v-shp[3] SKIP
             "<=1><R+4><C77>" v-shp[4] SKIP                     
        
             "<#10><C1><FROM><C106><LINE><||3>" SKIP           
             "<=10><C2>F/B  FG ITEM#        ORDER QTY    JOB QTY  "
             "DESCRIPTION      STYLE  CARTON SIZE       #UP COLOUR STANDARD" 
             SKIP.
             

         FOR EACH wrk-ink: DELETE wrk-ink. END.
         ASSIGN r = 0
                v-die-no    = ""
                v-plate-no  = ""
                v-tr-no     = ""
                v-cas-cnt   = 0
                v-layer-pad = ""
                v-cas-no    = ""
                v-eb-form   = "".
                                               
         FOR EACH eb NO-LOCK
                     WHERE eb.company = ef.company
                       AND eb.est-no  = ef.est-no
                       AND eb.eqty    = ef.eqty
                       AND eb.form-no = ef.form-no:
                             
            ASSIGN v-stock-no = IF est.est-type = 2 
                                THEN job-hdr.i-no 
                                ELSE eb.stock
                   v-total-up = v-total-up + eb.num-up
                   v-eb-form  = v-eb-form +
                                (IF v-eb-form <> "" 
                                    THEN ","
                                    ELSE "") +
                                STRING(eb.blank-no).
                                
            ACCUM 1 (COUNT).
            IF (ACCUM COUNT 1) = 1 THEN                                  
               ASSIGN v-die-no    = eb.die-no
                      v-plate-no  = eb.plate-no
                      v-tr-no     = eb.tr-no
                      v-cas-cnt   = eb.cas-cnt
                      v-layer-pad = eb.layer-pad
                      v-cas-no    = eb.cas-no.

            RUN PR-getink.

            /** ITEM **/
            FIND FIRST oe-ordl
                       WHERE oe-ordl.company = job-hdr.company
                         AND oe-ordl.ord-no  = job-hdr.ord-no
                         AND oe-ordl.job-no  = job-hdr.job-no
                         AND oe-ordl.job-no2 = job-hdr.job-no2
                         AND oe-ordl.i-no    = v-stock-no
                       NO-LOCK NO-ERROR.
            IF AVAILABLE oe-ordl THEN
               ASSIGN v-est-qty = oe-ordl.qty
                      v-po-no   = oe-ordl.po-no.
            ELSE
               ASSIGN v-est-qty = v-job-qty
                      v-po-no   = "".                 

            ASSIGN v-job-qty = 0.
            FOR EACH xjob-hdr FIELDS (qty) NO-LOCK
                     WHERE xjob-hdr.company = job-hdr.company
                       AND xjob-hdr.job     = job-hdr.job
                       AND xjob-hdr.job-no  = job-hdr.job-no
                       AND xjob-hdr.job-no2 = job-hdr.job-no2
                       AND xjob-hdr.i-no    = v-stock-no
                       AND xjob-hdr.frm     = job-hdr.frm:  
               ASSIGN v-job-qty = v-job-qty + xjob-hdr.qty.
            END.  
          
            FOR FIRST style FIELDS (dscr) NO-LOCK
                      WHERE style.company = eb.company
                        AND style.style   = eb.style:
               ASSIGN v-stypart = style.dscr.
            END.
            
            ASSIGN v-dsc[1] = eb.part-dscr1
                   v-dsc[2] = eb.part-dscr2
                   v-size[1] = string(eb.len) + "x" + string(eb.wid) + "x" +
                               string(eb.dep)
                   v-size[2] = eb.i-coldscr
                   v-case-size = string(eb.cas-len) + "x" + 
                                 string(eb.cas-wid) + "x" +
                                 string(eb.cas-dep)
                   v-up = eb.num-up
                   v-po-no = (IF AVAIL oe-ordl THEN oe-ordl.po-no ELSE "")
           	       v-case-count = (IF AVAIL oe-ordl AND oe-ordl.cas-cnt <> 0 
           	                       THEN oe-ordl.cas-cnt
                                   ELSE eb.cas-cnt)
                   v-case-qty = v-job-qty / v-case-count.         
            

            ASSIGN r = r + 1.
            PUT "<=10><R+" STRING(r) "><C2>"
                trim(string(eb.form-no,">>9")) + "-" +
                trim(string(eb.blank-no,">>9")) FORM "x(5)"   
                v-stock-no      
                (IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0) " "
                v-job-qty format "->,>>>,>>9" "  "             
                v-dsc[1] FORM "x(16)" " "
                eb.style " "
                v-size[1] FORM "x(16)" " "
                v-up " "   
                v-cs#-note FORMAT "x(35)"
                skip.                   

         END. /* FOR EACH eb NO-LOCK */
       
         PUT "<#20><C1><FROM><C106><LINE><||3>"  
             "<=20><C2><B>BOARD</B>" 
             "<=20><C50><FROM><R+8><LINE><||3>"          
             "<=20><R+1><C5>P.O.: _______________________"
             "<=20><R+1><C52>GUILLOTINE: _______________________"         
             "<=20><R+2><C5>DESCRIPTION: _______________________" 
             "<=20><R+2><C52>SHEETS: _______________________"          
             "<=20><R+3><C5>SUPPLIER: _______________________"  
             "<=20><R+3><C52>PREVIOUS JOB#: _______________________"        
             "<=20><R+4><C5>SHEETS SUPPLIED TO PRESS: _______________________"  
             "<=20><R+5><C5>SHEETS RETURNED TO STOCK: _______________________"  
             "<=20><R+6><C5>WASTE: _______________________" SKIP(1). 
       
       
/************/             
/* PRINTING */             
/************/         
         PUT "<#30><C1><FROM><C106><LINE><||3>" SKIP
             "<=30><C49><B><U>PRINTING</U></B>" 
             "<=30><R+1><C60><FROM><R+8><LINE><||3>"         
             "<=30><R+1><C2>PLATE LOCATION : " v-plate-no   
             "<=30><R+1><C62>BLANKET LOCATION:" 
             "<=30><R+2><C2><U>ITEMS</U>    <U>NAME</U>"                          
             "<=30><R+2><C62>BOARD:" ef.brd-dscr SKIP
             "<=30><R+3><C62>TOTAL #UP:" v-total-up 
             "<=30><R+4><C62>INSTRUCTIONS:" .
          
         ASSIGN vink = 0.    
         FOR EACH wrk-ink:
            ASSIGN vink = vink + 1.
         END.
                      
         IF vink > 1 THEN    
            PUT "<=30><R+2><C32><U>ITEMS</U>    <U>NAME</U>".      
            
         ASSIGN r = 2.   
         FOR EACH wrk-ink BREAK BY wrk-ink.i-code
         											  BY wrk-ink.blank-no.
         		IF FIRST-OF(wrk-ink.i-code) THEN 
         		   ASSIGN vcount = 1
         		          vlist = STRING(wrk-ink.blank-no)
         		          vlast = wrk-ink.blank-no.
         		
         		IF vcount > 1 THEN DO:
         		   IF vlast + 1 = wrk-ink.blank-no THEN
         		      ASSIGN vlast = wrk-ink.blank-no.
         		   ELSE
         		      ASSIGN vlist = vlist + "-" + STRING(vlast) + "," +
         		                     STRING(wrk-ink.blank-no)
         		             vlast = wrk-ink.blank-no.          
         		END. /* IF vcount > 1 THEN DO: */

         		IF NOT LAST-OF(wrk-ink.i-code) THEN         		     
         		   ASSIGN vcount = vcount + 1.     
         		          
         		IF LAST-OF(wrk-ink.i-code) THEN DO:
         		   IF vcount > 1 THEN 
         		      IF vlast = wrk-ink.blank-no THEN
         		         ASSIGN vlist = vlist + "-" + STRING(wrk-ink.blank-no).
         		      ELSE
         		         ASSIGN vlist = vlist + "," + STRING(wrk-ink.blank-no).         		
         		
         		
               ACCUM 1 (COUNT).
               IF (ACCUM COUNT 1) MOD 2 = 1 THEN
                  ASSIGN c = 2
                         r = r + 1.
               ELSE
                  ASSIGN c = 32. 

                PUT "<=30><R+" STRING(r) "><C" STRING(c) ">"
                    vlist  FORMAT "x(8)" " " 
                    wrk-ink.i-dscr FORMAT "X(20)" SKIP.
         		   
         		END. /* IF LAST-OF(wrk-ink.i-code) THEN DO: */         
         		               
         END. /* FOR EACH wrk-ink BREAK BY wrk-ink.i-code */              
        
         ASSIGN v-text = FNformat(FNnotes(job.rec_key,"pr",ef.form-no),40)
                r = 5.
         DO i = 1 TO NUM-ENTRIES(v-text,"`"):
            PUT "<=30><R+" STRING(r) " ><C62>" 
                ENTRY(i,v-text,"`") FORMAT "X(40)" SKIP. 
            ASSIGN r = r + 1.
         END.                

         PUT "<=30><R+9><C1><FROM><C106><LINE><||3>".

/****************/             
/* OUTSIDE WORK */             
/****************/ 
         PUT "<#40><C47><B><U>OUTSIDE WORK</U></B>" SKIP.
         
         r = 1.
         
         DO i = 1 TO 5: /*no room for all 6*/
           IF ef.mis-cost[i] NE "" THEN DO:
             PUT "<=40><R+" STRING(r) " ><C2>" 
                 ef.mis-cost[i] FORMAT "X(20)" SKIP. 
            ASSIGN r = r + 1.
           END.
         END.


         ASSIGN v-text = FNformat(FNnotes(job.rec_key,"OW",ef.form-no),85)
                r = 1.
         DO i = 1 TO NUM-ENTRIES(v-text,"`"):
            PUT "<=40><R+" STRING(r) " ><C22>" 
                ENTRY(i,v-text,"`") FORMAT "X(85)" SKIP. 
            ASSIGN r = r + 1.
         END.  
         PUT "<=40><R+6><C1><FROM><C106><LINE><||3>".
        
/**************/             
/* DIE CUTTING*/             
/**************/        
         PUT "<#50><C48><B><U>DIE CUTTING</U></B>" 
             "<=50><R+1><C50><FROM><R+5><LINE><||3>"        
             "<=50><R+1><C2>DIE #:" v-die-no 
             "<=50><R+1><C52>INSTRUCTIONS:"                             
             "<=50><R+2><C2>STRIPPING DIE#:" 
             "<=50><R+3><C2>MAKE READY:"  
             "<=50><R+4><C2>FRAME/BLANKER #:" SKIP. 
             
         ASSIGN v-text = FNformat(FNnotes(job.rec_key,"DC",ef.form-no),40)
                r = 2.
         DO i = 1 TO NUM-ENTRIES(v-text,"`"):
            PUT "<=50><R+" STRING(r) " ><C52>" 
                ENTRY(i,v-text,"`") FORMAT "X(40)" SKIP. 
            ASSIGN r = r + 1.
         END.               
          
         PUT "<=50><R+6><C1><FROM><C106><LINE><||3>".   

/**********************/             
/* SPECIAL PROCESSING */             
/**********************/                      
         PUT "<#55><C44><B><U>SPECIAL PROCESSING</U></B>" SKIP.  
             
         ASSIGN v-text = FNformat(FNnotes(job.rec_key,"WN,ME,TT",ef.form-no),100)
                r = 1.
         DO i = 1 TO NUM-ENTRIES(v-text,"`"):
            PUT "<=55><R+" STRING(r) " ><C2>" 
                ENTRY(i,v-text,"`") FORMAT "X(100)" SKIP. 
            ASSIGN r = r + 1.
         END.  
         PUT "<=55><R+10><C1><FROM><C106><LINE><||3>".                               
         
/*************/                   
/* FINISHING */
/*************/                     
         PUT "<#60><C48><B><U>FINISHING</U></B>" SKIP
             "<=60><R+1><C2>INSTRUCTIONS:" SKIP.
              
         ASSIGN v-text = FNformat(FNnotes(job.rec_key,"GL",ef.form-no),75)
                r = 2.
 
         DO i = 1 TO NUM-ENTRIES(v-text,"`"):
            PUT "<=60><R+" STRING(r) " ><C2>" 
                ENTRY(i,v-text,"`") FORMAT "X(75)" SKIP. 
            ASSIGN r = r + 1.
         END.                               
         PUT "<=60><R+10><C1><FROM><C106><LINE><||3>". 
                          
         PUT "<#70><C50><FROM><R+7><LINE><||3>"       
             "<=70><R+0><C2>PACKING:" SKIP 
             "<=70><R+1><C2>CORRUGATED CASE: " v-cas-no  
             "<=70><R+1><C52><b>NOTES</b>:"      
             "<=70><R+2><C2>PADS: " v-layer-pad
             "<=70><R+3><C2>PACK/CASE: " v-cas-cnt                      
             "<=70><R+4><C2>SKID TYPE: " v-tr-no
             "<=70><R+5><C2>PALLET PATTERN:" SKIP.
            
     			PAGE.
       
      END. /* FOR EACH ef NO-LOCK  */     
     
   END. /* FOR EACH job-hdr NO-LOCK */    

END. /* FOR EACH job NO-LOCK USE-INDEX job */

hide all no-pause.

/* end ---------------------------------- copr. 1997  advanced software, inc. */
