/* users_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'users_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="users" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="users" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="users" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="users" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DISPLAY
  users.user_id
  users.user_name
  users.phone
  users.userType 
  users.securityLevel FORMAT ">999  "
  /*users.track_usage
  users.use_colors
  users.use_fonts
  users.use_ctrl_keys
  users.developer*/.
IF users.image_filename NE "" THEN
PUT "Email:" AT 11 users.image_filename.

{methods/lstlogic/shownote.i &db_table="users" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="users" &col="5" &frame-name="f-miscflds"}
