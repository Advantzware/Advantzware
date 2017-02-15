{custom/globdefs.i}
FIND FIRST usercomp WHERE usercomp.user_id = USERID('NOSWEAT') AND
                            usercomp.company_default = YES NO-LOCK NO-ERROR.
g_company = IF AVAIL usercomp THEN usercomp.company ELSE "".
FIND FIRST usercomp WHERE usercomp.user_id = USERID('NOSWEAT') AND
                            usercomp.company = g_company AND
                            usercomp.loc <> "" NO-LOCK NO-ERROR.
g_loc = IF AVAIL usercomp THEN usercomp.loc ELSE "".
