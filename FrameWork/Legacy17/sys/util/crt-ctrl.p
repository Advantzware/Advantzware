FOR EACH company NO-LOCK:
find first gl-ctrl where gl-ctrl.company = company.company no-lock no-error.
 if not available gl-ctrl then do:
		create gl-ctrl. gl-ctrl.company = company.company.
 end.
 find first rm-ctrl where rm-ctrl.company = company.company no-lock no-error.
 if not available rm-ctrl then do:
			create rm-ctrl. rm-ctrl.company = company.company.
 end.
 find first fg-ctrl where fg-ctrl.company = company.company no-lock no-error.
 if not available fg-ctrl then do:
			create fg-ctrl. fg-ctrl.company = company.company.
 end.
 find first ap-ctrl where ap-ctrl.company = company.company no-lock no-error.
 if not available ap-ctrl then do:
			create ap-ctrl. ap-ctrl.company = company.company.
 end.
 find first ar-ctrl where ar-ctrl.company = company.company no-lock no-error.
 if not available ar-ctrl then do:
		create ar-ctrl. ar-ctrl.company = company.company.
 end.
 find first oe-ctrl where oe-ctrl.company = company.company no-lock no-error.
 if not available oe-ctrl then do:
	       create oe-ctrl. oe-ctrl.company = company.company.
 end.
 FIND FIRST loc WHERE loc.company = company.company AND loc.loc = "main" NO-LOCK NO-ERROR.
 IF NOT AVAIL loc THEN do:
     create loc.
     assign loc.company = company.company
        loc.loc     = "Main"
        loc.dscr    = "Main".
 END.
 for each loc where loc.company = company.company:
   	 find first ce-ctrl where ce-ctrl.company = company.company and
	                          ce-ctrl.loc = loc.loc
					          no-lock no-error.
     if not available ce-ctrl then do:
		    create ce-ctrl.
		    assign
		    ce-ctrl.company = company.company
		    ce-ctrl.loc     = loc.loc.
	 end.
 end.
END.
 
