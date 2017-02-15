/* winReSizeMax.i */

   /* check if resize blocked by user */
IF SEARCH('users/' + USERID('NOSWEAT') + '/' + v-prgmname + 'winReSize') EQ ? AND
   /* check if universally blocked for all users */
   SEARCH('users/' + v-prgmname + 'winReSize') EQ ? THEN DO:
  APPLY 'WINDOW-MAXIMIZED':U TO {&WINDOW-NAME}.
END.

