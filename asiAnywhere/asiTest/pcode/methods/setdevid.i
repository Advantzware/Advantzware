/* setdevid.i */

FOR EACH users WHERE users.developer = YES NO-LOCK:
  g_developer = IF g_developer = "" THEN users.user_id
                ELSE g_developer + "," + users.user_id.
END.
