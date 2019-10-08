/* prgrms_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to lower section of
   'lstlogic/persist.p' alphabetically and use the following syntax:
   RUN <ip-name> IN Persistent-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'prgrms_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="prgrms" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="prgrms" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="prgrms" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="prgrms" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DISPLAY
  prgrms.prgmname
  prgrms.prgtitle
  prgrms.prgm_ver LABEL "Version"
  prgrms.dir_group
  prgrms.menu_item COLUMN-LABEL "MnuItm"
  prgrms.track_usage LABEL "Use"
  prgrms.popup LABEL "Pop"
  prgrms.run_persistent LABEL "Per"
  prgrms.use_colors LABEL "Clr"
  prgrms.use_fonts LABEL "Fnt".

PUT UNFORMATTED
  "    Can Run: " AT 5 prgrms.can_run
  " Can Create: " AT 5 prgrms.can_create
  " Can Update: " AT 5 prgrms.can_update
  " Can Delete: " AT 5 prgrms.can_delete.

{methods/lstlogic/shownote.i &db_table="prgrms" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="prgrms" &col="5" &frame-name="f-miscflds"}
