/* viewrate.i */

/* Logic to check and see if rates should be set */
/* 
   IF (user can see rate eq true) THEN
   
   We need to determine how we are going to decide
   what user can see the rates and who cannot
*/

/* Now we are simply returning false always*/

/*
IF false then
*/
FIND prgrms WHERE prgrms.prgmname = "rates" NO-LOCK NO-ERROR.
IF CAN-DO(prgrms.can_run,USERID("NOSWEAT")) or
   CAN-DO(prgrms.can_run,"*") THEN

