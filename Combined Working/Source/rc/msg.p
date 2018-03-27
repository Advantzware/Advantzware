/* msg.p: Display message in standard format
** TLD 09/28/88 for DSM
** TLD 02/16/89 Modified for colors on DOS.
** DNM 07/12/92 If batch mode, save msg1, msg2 in x-batch-msg1, x-batch-msg2.
** DE  06/10/94 WR12957 Intercept message text if calling program was run
**       by non-interactive EDI processing
** JLM 08/11/94 WR25529 Added test to determine if running in batch mode.
**              V7 Progress does not like the color phrase on the message
**              statement when running in batch mode.
**
**==================================================================
**
**          NOTICE -- PROPRIETARY PROGRAM INFORMATION
**
**  This program contains trade secrets which are proprietary to
**  DATA SYSTEMS & MANAGEMENT, INC., a Minnesota corporation.
**  This program, whether in source code or object (compiled) code
**  format, is not to be reproduced, used or disclosed except in
**  accordance with an authorized program license, or upon express
**  written permission of DATA SYSTEMS & MANAGEMENT, INC.
**
**  COPYRIGHT (C) 1989 - 1994
**  BY DATA SYSTEMS & MANAGEMENT, INC.
**  ALL RIGHTS RESERVED, INCLUDING THE RIGHT OF
**  REPRODUCTION IN WHOLE OR IN PART IN ANY FORM.
**
**==================================================================
**
*/
{rc/loginv.i}
def var msg-color as char.
/**WR12957**/
if screen-name begins "EDI BACKGROUND" then
  do:
    run ed/edi-msg.p.
    return.
  end.
/**WR12957**/
if terminal = "CO80" then
   do:
      if msg-is-warn then
         assign msg-color = "light-red/gray".
      else
      if msg-is-err  then
         assign msg-color = "blink-light-red/gray".
      else
         assign msg-color = "yellow/gray".
   end.
else
   assign msg-color = "messages".
hide message no-pause.
/* BE SURE MESSAGE LINES START AND END WITH A SPACE, FOR READABILITY */
if msg1 <> "" then
   do:
      if length(msg1) < 78 and substring(msg1,1,1) <> " " then
         assign msg1 = " " + msg1.
      if length(msg1) < 78 and substring(msg1,length(msg1),1) <> " " then
         assign msg1 = msg1 + " ".
   end.
if msg2 <> "" then
   do:
      if length(msg2) < 78 and substring(msg2,1,1) <> " " then
         assign msg2 = " " + msg2.
      if length(msg2) < 78 and substring(msg2,length(msg2),1) <> " " then
         assign msg2 = msg2 + " ".
      /* IF THERE ARE 2 MSG LINES, MAKE THEM THE SAME LENGTH, FOR ESTHETICS */
      if length(msg1) < length(msg2) then
         assign msg1 = msg1 + fill(" ", (length(msg2) - length(msg1) ) ).
      else
      if length(msg1) > length(msg2) then
         assign msg2 = msg2 + fill(" ", (length(msg1) - length(msg2) ) ).
   end.
/* WR25529 - IF BATCH MODE THEN DO NOT USE THE COLOR PHRASE */
if x-batch-mode then
   do:
      if msg1 <> "" then
         message msg1.
      else
         message " ".
      if msg2 <> "" then
         message msg2.
   end.
else
   do:
      if msg1 <> "" then
         message color value(msg-color) msg1.
      else
         message color normal " ".
      if msg2 <> "" then
         message color value(msg-color) msg2.
   end.
if not x-batch-mode then
   do:
      if msg-is-err then
         pause message
         " **ERROR: Please Review Carefully (Press space bar to continue)".
      else
      if msg-is-warn then
         pause message
         " WARNING: Please Review Carefully (Press space bar to continue)".
      else
      if msg-sec > 0 then
         pause msg-sec message " Press space bar to continue. ".
   end.
else
   do:
      if msg-is-err then
         assign x-batch-err  = yes
                x-batch-msg1 = msg1
                x-batch-msg2 = msg2.
   end.
if msg-sec > 0
or msg-is-err
or msg-is-warn then
   hide message no-pause.
assign msg1        = ""
       msg2        = ""
       msg-sec     = msg-sec-dflt
       msg-is-err  = no
       msg-is-warn = no.
