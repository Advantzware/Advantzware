/* pt/shared - LIST OF SHARED VARIABLES FROM MENU SYSTEM

lisakay matchen 7/10/00 call # 11248 find login record for all reports.
lisakay matchen 7/17/00 bar coding add variable 
lisakay matchen 7/5/02 call # 16960 up array size
lisakay matchen call# 19697 2/8/05 fix format of systemname
lisakay matchen 3/27/06 add wfile-name and wkey-list to new global shared variable.s

*/
/**************** Copyright Statement Follows ***************************
* (C) Copyright, 1996 - 1997 Foresight Software, Inc.  All Rights       *
* Reserved.  This is unpublished material and contains trade secrets    *
* and other confidential information and is subject to licensing and    *
* a confidentiality agreement.  The unauthorized possession, use,       *
* reproduction, reverse engineering, distribution, display, or          *
* disclosure of this material or of any information contained herein    *
* or any information derived from this material is strictly prohibited. *
************************************************************************/

&IF  "{&WINDOW-SYSTEM}":U  ne  "TTY":U &THEN
  &GLOBAL-DEFINE UGUI-SCR  v6frame three-d
  &GLOBAL-DEFINE UGUI-RPT  stream-io
  &GLOBAL-DEFINE UGUI-85  85
  &GLOBAL-DEFINE UGUI-90  90
  &GLOBAL-DEFINE UGUI-92  92
  &GLOBAL-DEFINE UGUI-95  95
&ELSE
  &GLOBAL-DEFINE UGUI-SCR
  &GLOBAL-DEFINE UGUI-RPT
  &GLOBAL-DEFINE UGUI-85  80
  &GLOBAL-DEFINE UGUI-90  80
  &GLOBAL-DEFINE UGUI-92  80
  &GLOBAL-DEFINE UGUI-95  80
&ENDIF



/* Extend init-val from extent 20 to extent 30      Luc Chalifour   March 94 */
/* Extend init-val from extent 30 to extent 50      Serge Gariepy   March 94 */


define {1} shared variable systemname as char format "x(60)" no-undo.
   /* System name to be used on report headers. Application dependent */

define {1} shared variable menuname as char format "x(14)" no-undo.
   /* current menu selected in the menu system */

define {1} shared variable functionname as char format "x(14)" no-undo.
   /* current function selected in the menu system */

define {1} shared variable functdesc as char format "x(34)" no-undo.
   /* The description of the function selected. Used on report headers */

define {1} {2} shared variable terminalid as character format "X(8)" no-undo.
   /* terminal identification as available by the UNIX `who am i` command */
   /* or set to 'DOSxx':Uif under MS-DOS. Where xx is returned by pt/getlogin */

define {1} {2} shared variable xfocus# as widget-handle no-undo.
   /* Set by applhelp.p to frame functions and reset upon exit */

define {1} shared variable fr-file as char no-undo.
define {1} shared variable use-file as char no-undo.
define {1} shared variable fr-field as char no-undo.
define {1} shared variable use-field as char no-undo.
define {1} shared variable fr-value as char no-undo.
define {1} shared variable fr-col as int no-undo.
define {1} shared variable fr-db as char no-undo.
define {1} shared variable fr-line as int no-undo.
define {1} shared variable fr-name as char no-undo.
define {1} shared variable fr-row as int no-undo.
define {1} shared variable fr-index as int no-undo.

define {1} shared variable fr-pgm2 as char no-undo.

  /* used to pass calling program @ help time */

define {1} shared variable helpmode as logical no-undo.

  /* set to yes if in helpmode */

define {1} shared variable condition as char no-undo.

  /* Utility variable to define list of data fields, used by browse routines */

define {1} shared variable init-val as char extent 250 no-undo.

  /* values of fields pointed by the "condition":U variable.
     Used to condition searches in browse inquiries */

define {1} shared variable alpha-mode as log init ? no-undo.

  /* tells the browse inquiry which index to use for browsing.
     ? to figure it by itself. Set by zoom driver */

define {1} shared variable restrict as log no-undo.

  /* tells browse to restrict view to legit children only.
     e.g. order -> order-line. Set by zoom driver */

define {1} shared variable backwards as log no-undo.

  /* tells browse to read records in reverse
     e.g. on invoices: read last ones first. Set by zoom driver */

define {1} shared variable xyz as int extent 3 format "z9" no-undo.

  /* Browse routine uses this to position help window:
     xyz[1] = column
     xyz[2] = row
     xyz[3] = down (if -1 or 0, then use all that is available)
     Reset by pt/menu */

define {1} shared variable usize as int format ">9" no-undo.

  /* xyz[3] is set to usize */

define {1} shared variable can-select as log no-undo.

  /* Data entry program should set this to 'yes':U if it can handle a result
     returned by the browse routine. Reset by pt/menu */

define {1} shared variable result as char no-undo.

  /* Browse routines can pass back rowids of records selected in 'result':U */

define {1} shared variable mail as log no-undo.

  /* Browse routine sets it to 'yes':U if there is a result to return.
     Reset by pt/menu */

define {1} shared variable file-to-do as char format "x(12)" no-undo.

  /* Utility variable to define the "file to do":U. Reset by pt/menu */

define {1} shared variable exec-keys as char extent 2 no-undo.

  /* 'pt/keys 1':U displays the content. They show the user important execution
     keys that he can use use during data-entry */

define {1} shared variable next-prev as char no-undo.

  /* 'pt/keys 2':U displays the content. They show the user that
     the next / previous logic is enabled during data-entry */

define {1} shared variable del-key   as char no-undo.

  /* 'pt/keys 3':U displays the content. They show the user that
     the delete record logic is enabled during data-entry */

define {1} shared var logincontrol as char format "x(8)" no-undo.

  /* pt/menu sets this variable with the current control entity from the
    login file. Each function ( z_funct) can indicate which controlling
    entity is required. This eliminates the xx/std logic */

define {1} shared variable login-entity as char format "x(8)" no-undo.

  /* Same as above, but this entity is the default for divisional posting */

define {1} shared variable login-yr as int format ">>>9" no-undo.

  /* pt/menu sets this variable with the current  login year from login */

define {1} shared variable login-prd as int format ">9" no-undo.

  /* pt/menu sets this variable with the current login period from login */

define {1} shared variable login-date as date format "99/99/99" no-undo.

  /* pt/menu sets this variable with the current login date from login. */

define variable z_copyright as character format "x" no-undo.

  /* Allows copyright notice in object modules */

define {1} shared variable demon as logical no-undo.

  /* used in pt/newpage to activate put, pt/demon sets to 'yes':U for b/g print */

define {1} shared variable wnice as logical format "yes/no" no-undo.

  /* used in pt/newpage to lower priority of report or batch */

define {1} shared variable ulevel as int format "9" no-undo.

  /* To store user security level */

  /* color definitions */
def {1} shared var sv-color-header as char format "x(20)" no-undo.
def {1} shared var sv-color-m-title as char format "x(20)" no-undo.
def {1} shared var sv-color-m-display as char format "x(20)" no-undo.
def {1} shared var sv-color-m-prompt as char format "x(20)" no-undo.
def {1} shared var sv-color-m-tag as char format "x(20)" no-undo.
def {1} shared var sv-color-o-title as char format "x(20)" no-undo.
def {1} shared var sv-color-o-display as char format "x(20)" no-undo.
def {1} shared var sv-color-o-prompt as char format "x(20)" no-undo.
def {1} shared var sv-color-o-tag  as char format "x(20)" no-undo.
def {1} shared var sv-color-warning as char format "x(20)" no-undo.
def {1} shared var sv-color-error as char format "x(20)" no-undo.
def {1} shared var sv-color-message as char format "x(20)" no-undo.

def {1} shared var sv-site-code as char format "x(8)" no-undo.
/*
def {1} shared var sv-central-aud as char no-undo.
*/
def {1} shared var sv-central-site as char  no-undo.
def {1} shared var theversion as char format "x(5)" no-undo.
def {1} shared var sv-prefix as char format "xx" no-undo.
def {1} shared var sv-userlang  as char format "x(4)".
def {1} shared var no-update    as char no-undo.
def {1} shared var no-display   as char no-undo.
def {1} shared var w-inqmode as l init "no" no-undo.
/* pop menu */
define {1} shared var pop-al as widget-handle no-undo.
define {1} shared var dumy-menu as widget-handle no-undo.  
define {1} shared var pop-al2 as widget-handle no-undo.
define {1} shared var pop-fn1 as widget-handle no-undo.
define {1} shared var pop-fn2 as widget-handle no-undo.  
define {1} shared var pop-fn3 as widget-handle no-undo.
define {1} shared var pop-fn4 as widget-handle no-undo.
define {1} shared var pop-fn5 as widget-handle no-undo.
define {1} shared var pop-fn6 as widget-handle no-undo.
define {1} shared var pop-fn9 as widget-handle no-undo.
define {1} shared var pop-fn11 as widget-handle no-undo.
define {1} shared var pop-fn12 as widget-handle no-undo. 
define {1} shared var pop-al21 as widget-handle extent 20 no-undo.     
define {1} shared var dumy-rule as widget-handle no-undo.
define {1} shared var v-fun-list as char no-undo.
define {1} shared var v-runfun as char no-undo.
DEFINE {1} {2} SHARED VAR hMessage AS HANDLE NO-UNDO.
DEFINE {1} {2} SHARED VAR hEvent AS HANDLE NO-UNDO.
DEFINE {1} {2} SHARED VAR hLookup AS HANDLE NO-UNDO.
DEFINE {1} {2} SHARED VAR hCombox AS HANDLE NO-UNDO.
DEFINE {1} {2} SHARED VAR hhelp9 as handle no-undo.
DEFINE {1} {2} SHARED VAR hmenuvalue as handle no-undo.
DEFINE {1} {2} SHARED VAR hwinhlp as char no-undo.
DEFINE {1} {2} SHARED VAR v-barcoding as logical init no no-undo.
define {1} {2} shared var do-gui as logical init yes no-undo.
def {1} shared var cModuleList as char no-undo.

DEF {1} {2} SHARED VARIABLE cSDO-name AS CHAR NO-UNDO.
DEF {1} {2} SHARED VARIABLE cBrowser-name AS CHAR NO-UNDO.
DEF {1} {2} SHARED VARIABLE cViewer-name AS CHAR NO-UNDO.
def {1}     shared variable uomerr as LOG no-undo.
def {1} {2} shared variable wFileName as char.
def {1} {2} shared variable wKeylist as char.

DEF {1} {2} SHARED VAR hAPRule AS HANDLE.
DEF {1} {2} SHARED VAR hARRule AS HANDLE.
DEF {1} {2} SHARED VAR hBMRule AS HANDLE.
DEF {1} {2} SHARED VAR hCARule AS HANDLE.
DEF {1} {2} SHARED VAR hCFRule AS HANDLE.
DEF {1} {2} SHARED VAR hFFRule AS HANDLE.
DEF {1} {2} SHARED VAR hHDRule AS HANDLE NO-UNDO.
DEF {1} {2} SHARED VAR hINRule AS HANDLE.
DEF {1} {2} SHARED VAR hOPRule AS HANDLE.
DEF {1} {2} SHARED VAR hOPRule2 AS HANDLE.
DEF {1} {2} SHARED VAR hOPBillRule AS HANDLE.
DEF {1} {2} SHARED VAR hOPInvRule AS HANDLE.
DEF {1} {2} SHARED VAR hPORule AS HANDLE.
DEF {1} {2} SHARED VAR hPRRule AS HANDLE.
DEF {1} {2} SHARED VAR hSWRule AS HANDLE NO-UNDO.

/* 11248 
if connected("ptdb") then do:
find  login where  login.terminal-no = terminalid
     and  login.user-id = entry(1,userid("ptdb":U), "@")
     and  login.system = substring(program-name(2),1,
          index(program-name(2),"/") - 1) no-lock no-error.
if avail  login then
   find  entity of  login no-lock no-error.
end.
*/