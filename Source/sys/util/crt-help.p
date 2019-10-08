/* crt-help.p   load help database text from text file */
/*
File-Name-------- Field-Name------- Format-----------

help-msg          MSG-LINE          >>>9
help-msg          MSG-NUMBER        >>>>9
help-msg          MSG-TXT           x(76)
hlp-head          DB-NAME           x(20)
hlp-head          FIELD-ORD         >>9
hlp-head          FIL-NAME          x(15)
hlp-head          FLD-NAME          x(25)
hlp-head          FRM-NAME          x(15)
hlp-head          FRM-TITLE         x(20)
hlp-head          LANGUAGE          x(2)
hlp-head          MSG-NUM           >>>9
hlp-head          USR-TXT           yes/no
*/


def var ls-line as cha form "x(1000)" no-undo.
def var li-nex-msg-num as int no-undo.
def var li-line-no as int no-undo.
def var ll-new-line as log no-undo.
def var ls-field as cha no-undo.
def var ls-table as cha no-undo.
def var ls-sub1 as cha no-undo.
def var ls-sub2 as cha no-undo.
def var ls-sub3 as cha no-undo.
def var ll-field-next as log no-undo.
def var li-next-msg-num as int no-undo.
def var ls-frm-name as cha no-undo.
def var ls-language as cha no-undo.
def var ll-usr-txt as log no-undo.

input from r:\asi_gui\docs\ap.txt no-echo.

assign ll-field-next = no.
find last hlp-head use-index mess-num no-lock no-error.
li-next-msg-num = if avail hlp-head then hlp-head.msg-num + 1 else 1.
    
ll-new-line = yes.
repeat:
    assign ls-field = ""
           ls-table = ""
           ls-frm-name = ""
           ls-sub1 = ""
           ls-sub2 = ""
           ls-sub3 = ""
           ls-line = ""
           .
    import unformatted ls-line .
    if ls-line begins "%%" then do:
    /*
       FIND FIRST asihlp.hlp-head WHERE asihlp.hlp-head.fld-name = v-fld-name AND
	  asihlp.hlp-head.fil-name = v-fil-name AND
	  asihlp.hlp-head.db-name = v-db-name AND
	  asihlp.hlp-head.frm-name = v-frm-name AND
	  asihlp.hlp-head.language = v-lang AND
	  asihlp.hlp-head.usr-txt = v-usr-txt NO-ERROR.
	IF AVAILABLE asihlp.hlp-head THEN

       create hlp-head.
       assign hlp-head.msg-num = li-next-msg-num
              hlp-head.usr-txt = yes
              hlp-head.language = "" 
              .
       li-next-msg-num = li-next-msg-num + 1.       

      */
       li-line-no = 1.       
       ll-field-next = yes.    /* next line is field info */
       repeat :
          import unformatted ls-line . 
          if ls-line begins "@@" or ls-line begins "  @@" then do:
             leave.            
          end.
          else do:  /* text line */
             if ll-field-next then do:
                   if index(ls-line,"^") > 0 then 
                   do:                       
                       assign ls-field = substring(ls-line,1,index(ls-line,"^") - 1)
                              ls-sub1 = substring(ls-line,index(ls-line,"^") + 1).
                       if index(ls-sub1,"^") > 0 then do:                        
                          assign ls-table = substring(ls-sub1,1,index(ls-sub1,"^") - 1)
                                 ls-sub2 =  substring(ls-sub1,index(ls-sub1,"^") + 1). 
                          if index(ls-sub2,"^") > 0 then  do:
                             assign ls-frm-name = substring(ls-sub2,1,index(ls-sub2,"^") - 1)
                                    ls-sub3 =  substring(ls-sub2,index(ls-sub2,"^") + 1).                              
                          end.
                          else ls-frm-name = ls-sub2.       
                       end.          
                       else ls-table = ls-sub1.
                   end.
                   else ls-field = ls-line.
/*                   
message substring(ls-line,1,50) skip
        ls-field "," ls-sub1 "," ls-sub2 "," ls-sub3 skip
        ls-field "," ls-table "," ls-frm-name 
        view-as alert-box. 
 */ 
                FIND FIRST hlp-head WHERE hlp-head.fld-name = ls-field AND
         	             hlp-head.fil-name = ls-table AND
                           hlp-head.db-name = ls-sub3 /*db-name*/ AND
                           hlp-head.frm-name = ls-frm-name AND
                           hlp-head.language = ls-language AND
                           hlp-head.usr-txt = ll-usr-txt NO-ERROR.
                IF not AVAILABLE hlp-head THEN  do:
                   create hlp-head.
                   assign hlp-head.msg-num = li-next-msg-num
                          hlp-head.usr-txt = yes
                          hlp-head.language = "" 
                          hlp-head.fld-name = ls-field
                          hlp-head.fil-name = ls-table
                          hlp-head.db-name  = ls-sub3
                          hlp-head.frm-name = ls-frm-name
                          hlp-head.frm-title = ""
                          hlp-head.field-ord = 0
                          ll-field-next = no
                          .
                   li-next-msg-num = li-next-msg-num + 1.       
                end.          
             end.                          
             else do :
                create help-msg.
                assign help-msg.msg-number = hlp-head.msg-num
                       help-msg.msg-line = li-line-no
                       help-msg.msg-txt = ls-line
                       .
                li-line-no = li-line-no + 1.       
             end.         
          end.  /* else - text line */ 
       end.  /* nested repeat */
       
    end.  /* %% */
    
    else if ls-line begins "@@" then do:
         ll-new-line = yes.
    end.
    else if ls-line begins "  @@" then do:
    
    end.   
    else do:  /* general menu contents help */
       if ll-new-line then do:
          create hlp-head.
          assign hlp-head.msg-num = li-next-msg-num
              hlp-head.usr-txt = yes
              hlp-head.language = "" 
              hlp-head.fld-name = string(li-next-msg-num)
              .
          li-next-msg-num = li-next-msg-num + 1.
          ll-new-line = no.
       end.
       create help-msg.
       assign help-msg.msg-number = hlp-head.msg-num
              help-msg.msg-line = li-line-no.
              help-msg.msg-txt = ls-line
              .
       li-line-no = li-line-no + 1.       
    
    end.
   
end.
