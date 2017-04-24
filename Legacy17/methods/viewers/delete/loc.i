/* loc.i */
if loc.loc = "MAIN" THEN DO:
   message 'Cannot delete the "MAIN" warehouse' view-as alert-box error.
   return error.
end.
