/* nosweat.p */

&SCOPED-DEFINE loginProcedure nosweat/login.w
&SCOPED-DEFINE checkUserRecord YES
&SCOPED-DEFINE connectDatabases YES
&SCOPED-DEFINE runAsiLoad YES
&SCOPED-DEFINE createSingleUserPFs YES
&SCOPED-DEFINE execProgram mainMenu.    
&SCOPED-DEFINE checkExpiredLicense YES
&GLOBAL-DEFINE checkUserCount YES

/* uncomment to check for open transaction scope */
/*RUN system/monitor.w PERSISTENT.*/

FOR EACH userlog:
    DELETE userlog.
END.

{nosweat.i}
