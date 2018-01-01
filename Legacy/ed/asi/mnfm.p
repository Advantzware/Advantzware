{sys/inc/var.i shared}
def shared var parent_cmdlist as char no-undo.
def shared var child_chosen   as int  no-undo.
/* number of menu option */
define var chosen     as int                              initial 1.
/* number of choices on the menu */
def    var cmdcount   as int                              initial 9.
/* menu strings */
def    var cmnd       as char    format "x(30)" extent 9.
def    var cmd        as char    format "x(30)" extent 16.
def    var cmdlist    as char                   initial "123456789".
def    var cmdlist2   as char                   initial "ABCDEFGHIJKLMRSX".
define var lastchoice as int                              initial 1.
define var proglist   as char                   extent 9.
define var hyde       as ch                     initial "yyyyyyyyy".
define var qgo        as logical                          init false.
define shared frame f-cmd.
define shared frame f-cmd2.
{sys/form/s-top.f}   /* 1st Screen line - date,title, time */
{menu.f}
{ed/asi/menu.f}
form
	skip(1)
	cmnd[1] skip
	cmnd[2] skip
	cmnd[3] skip
	cmnd[4] skip
	cmnd[5] skip
	cmnd[6] skip
	cmnd[7] skip
	cmnd[8] skip
	cmnd[9] skip(1)
	with title color value(col-warn)
	     "   FILE MAINTENANCE   "
	     frame f-cmd3 row 4 no-labels  column 48 overlay
	     color value(col-norm) prompt value(col-input).
       ASSIGN
       proglist[1]  =  "ed/fmmast.r"
       proglist[2]  =  "ed/fmcode.r"
       proglist[3]  =  "ed/fmxship.r"
       proglist[4]  =  "ed/fmxitem.r"
       proglist[5]  =  "ed/fmxvia.r"
       proglist[6]  =  "ed/fmco.r"
       cmnd[1]      =  " 1. Trading Partners"
       cmnd[2]      =  " 2. Transaction Sets"
       cmnd[3]      =  " 3. Ship-to Locations"
       cmnd[4]      =  " 4. Customer Items"
       cmnd[5]      =  " 5. Via/SCAC Codes"
       cmnd[6]      =  " 6. Controls"
       .
if parent_cmdlist > "" then cmdlist2 = parent_cmdlist.
pause 0.
display cmnd with frame f-cmd3.
color display value(col-mess) cmnd[chosen] with frame f-cmd3.
/* main menu loop */
GETCHOICE:
repeat:
    {sys/sho/s-top.v}  /* display screen line 1 */
    /* if cursor moved to new choice , rest hilites */
    if lastchoice ne chosen
    then do with frame f-cmd3:
	color display value(col-norm)  cmnd[lastchoice].
	color display value(col-mess)  cmnd[chosen].
	lastchoice = chosen.
    end.
    readkey pause 60.
    hide message no-pause.
    /* on escape, exit is selected */
    if keyfunction(lastkey) = "end-error" then do:
       hide frame f-cmd3 no-pause.
       leave.
    end.
    else
    if lastkey = -1
    then do:
	     undo GETCHOICE.
	     next GETCHOICE.
    end.
    else
    if keyfunction(lastkey) = "help"
    then do:
	apply(lastkey).
	next GETCHOICE.
    end.
    else   /* pick previous option */
    if lastkey = keycode("up") or lastkey = keycode("page-up")
    then do:
	chosen = chosen - 1.
	if chosen = 0 then chosen = cmdcount.
	next GETCHOICE.
    end.
    else  /* pick next option */
    if ( lastkey = keycode("down") or lastkey = keycode("page-down") or
	lastkey = keycode("tab") )
    then do:
	chosen = chosen + 1.
	if chosen gt cmdcount then chosen = 1.
	next GETCHOICE.
    end.
    else
    if lastkey = keycode("home")
    then do:
	chosen = 1.
	next GETCHOICE.
    end.
    else
    if lastkey = keycode("end")
    then do:
	chosen = 6.
	next GETCHOICE.
    end.
    else /* user pressed first letter of a menu choice */
    if index(cmdlist,keylabel(lastkey)) gt 0
    then do:
	chosen = index(cmdlist,keylabel(lastkey)).
	qgo = true.
    end.
    else /* Pressed key of previous menu... */
    if index(cmdlist2,keylabel(lastkey)) gt 0
    then do:
       hide frame f-cmd3 no-pause.
       child_chosen = index(cmdlist2,keylabel(lastkey)).
       leave.
    end.
    /* go ahead */
    if lastkey = keycode("return") or keyfunction(lastkey) = "go" or qgo
    then do:
	/* reset hilites */
	if lastchoice ne chosen
	then do with frame f-cmd3:
	   color display value(col-norm) cmnd[lastchoice].
	   color display value(col-mess) cmnd[chosen].
	   lastchoice = chosen.
	end.
	do with frame f-cmd3:
	   color display value(col-inac) cmnd[chosen].
	   display cmnd[chosen].
	   if substring(hyde,chosen,1) = "y" then do:
	      hide frame f-cmd3 no-pause.
	      hide frame f-cmd2 no-pause.
	      hide frame f-cmd  no-pause.
	   end.
	   pause 0.
	   {sys/inc/runprog.i proglist[chosen]}
	   if frame-col(f-cmd) = 0 then view frame f-cmd.  pause 0.
	   if frame-col(f-cmd2) = 0 then view frame f-cmd2.  pause 0.
	   if frame-col(f-cmd3) = 0 then view frame f-cmd3.
	   color display value(col-mess) cmnd[chosen].
	   display cmnd[chosen].
	   view frame f-cmd3.
	   if index(cmdlist2,keylabel(lastkey)) gt 0
	   then do:
	      hide frame f-cmd3 no-pause.
	      child_chosen = index(cmdlist2,keylabel(lastkey)).
	      leave GETCHOICE.
	   end.
	end.
    end.
    /* Wrong key  */
    else  do with frame err row 21 width 81 centered no-box no-labels overlay
			    color value(col-error):
       display
"                              Invalid  Choice                                 "
.
       pause 2 no-message.
    end.
    hide frame err no-pause.
end.    /* end getchoice */
/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
