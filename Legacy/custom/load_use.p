DEF VAR lv-ini-file AS cha NO-UNDO.
DEF VAR v1 AS cha .

/* not reading progress.ini 
lv-ini-file = "progress.ini".
FILE-INFO:FILE-NAME = lv-ini-file.
lv-ini-file = FILE-INFO:FULL-PATHNAME.

lv-ini-file = ".\progress".
LOAD lv-ini-file.
USE lv-ini-file.
*/

LOAD "progress".
USE "progress".

GET-KEY-VALUE SECTION "fonts" KEY "font12" VALUE v1. 
  
DISP v1 FORM "x(50)".
