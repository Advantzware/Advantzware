/* custom/getcomp.p */

{custom/globdefs.i}

FIND FIRST usercomp WHERE usercomp.user_id = USERID('NOSWEAT') AND
                       usercomp.loc = '' AND
                       usercomp.company_default = YES NO-LOCK NO-ERROR.
g_company = IF AVAIL usercomp THEN usercomp.company ELSE "001".


FIND FIRST usercomp WHERE usercomp.user_id = USERID("NOSWEAT") AND
                                usercomp.company = g_company AND
                                usercomp.loc NE "" AND
                                usercomp.loc_default = yes
                                NO-LOCK NO-ERROR.
g_loc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".

