/* addon/touch/tspostfg.i */
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ company_code
                                     AND sys-ctrl.name    EQ "TSPOSTFG" /*"AUTOPOST"*/
                                     NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
                    CREATE sys-ctrl.
                    ASSIGN sys-ctrl.company = company_code
                         sys-ctrl.name    = "TSPOSTFG"
                         sys-ctrl.descrip = "Autopost to Finished Goods Receipts".
                     MESSAGE "System control record NOT found.  Would you LIKE to have Touchscreen Data collection autopost to FG?"
                           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO  UPDATE sys-ctrl.log-fld.
END.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN DO:
                  /*  {1} = "RUN proc-form-cmplt"  */
                     {1}
                  /* END. */
END.
