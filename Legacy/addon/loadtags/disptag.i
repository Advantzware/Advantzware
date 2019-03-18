/* loadtag/disptag.i  get tag# and display tag information for Loadtag */


   FIND FIRST loadtag WHERE loadtag.company = g_company
                        AND loadtag.ITEM-type = NO
                        AND loadtag.tag-no = {1} NO-LOCK NO-ERROR.
   IF NOT AVAIL loadtag THEN DO:
      MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   ASSIGN  /*loadtag.po-no:SCREEN-VALUE = IF loadtag.po-no = 0 THEN "" ELSE string(loadtag.po-no) */
           tt-tag.job-no:SCREEN-VALUE = loadtag.job-no 
           tt-tag.job-no2:SCREEN-VALUE = string(loadtag.job-no2)
        /*   loadtag.ord-no:SCREEN-VALUE = STRING(loadtag.ord-no) */
           tt-tag.i-no:SCREEN-VALUE = loadtag.i-no 
       /*    loadtag.i-name:SCREEN-VALUE =  loadtag.i-name  */
           tt-tag.pallet-count:SCREEN-VALUE = STRING(loadtag.pallet-count) /*qty*/
           tt-tag.qty-case:SCREEN-VALUE = string(loadtag.qty-case)
           tt-tag.case-bundle:SCREEN-VALUE = STRING(loadtag.case-bundle)
           tt-tag.tot-CASEs:SCREEN-VALUE = string(loadtag.tot-cases)
           tt-tag.loc:SCREEN-VALUE = loadtag.loc
           tt-tag.loc-bin:SCREEN-VALUE = loadtag.loc-bin
           tt-tag.tag-date:SCREEN-VALUE = string(loadtag.tag-date).

