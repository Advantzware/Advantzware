/* hideDowntime.i */

/*------------------------------------------------------------------------------
  Purpose:     instead of deleting widgets, simply hide unused objects
  Parameters:  last used object count
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipIdx AS INTEGER NO-UNDO.

  {{&includes}/ttblWidgetHide.i "downtimeWidget" ipIdx}
