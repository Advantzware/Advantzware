/* loadtag/disp-tag.i  get tag# and display tag information */

IF "{1}" = "FGItem" THEN DO:
   FIND FIRST loadtag WHERE loadtag.company = g_company
                        AND loadtag.ITEM-type = NO
                        AND loadtag.tag-no = {2} NO-LOCK NO-ERROR.
   IF NOT AVAIL loadtag THEN DO:
      MESSAGE "Invalid Loadtag#. " VIEW-AS ALERT-BOX ERROR.
      RETURN NO-APPLY.
   END.

   ASSIGN  fg-rctd.po-no:SCREEN-VALUE = IF loadtag.po-no = 0 THEN "" ELSE string(loadtag.po-no)
           fg-rctd.job-no:SCREEN-VALUE = loadtag.job-no 
           fg-rctd.job-no2:SCREEN-VALUE = string(loadtag.job-no2)
        /*   fg-rctd.ord-no:SCREEN-VALUE = STRING(loadtag.ord-no) */
           fg-rctd.i-no:SCREEN-VALUE = loadtag.i-no 
           fg-rctd.i-name:SCREEN-VALUE =  loadtag.i-name 
           fg-rctd.t-qty:SCREEN-VALUE = STRING(loadtag.qty)
           fg-rctd.qty-case:SCREEN-VALUE = string(loadtag.qty-case)
           fg-rctd.cases:SCREEN-VALUE = STRING(loadtag.case-bundle)
           fg-rctd.cases-unit:SCREEN-VALUE = string(loadtag.pallet-count)
           fg-rctd.loc:SCREEN-VALUE = loadtag.loc
           fg-rctd.loc-bin:SCREEN-VALUE = loadtag.loc-bin
           fg-rctd.rct-date:SCREEN-VALUE = IF fg-rctd.rct-date:SCREEN-VALUE = "" THEN STRING(TODAY) ELSE fg-rctd.rct-date:SCREEN-VALUE  .
    APPLY "entry" TO fg-rctd.loc.
    RETURN NO-APPLY.

END.
ELSE IF "{1}}" = "RMItem" THEN DO:

END.
