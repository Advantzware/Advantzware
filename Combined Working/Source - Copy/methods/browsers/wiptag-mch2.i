/* wiptag-mch2.i */

/* CASE browse-order:                                                                          */
/*   WHEN 1 THEN                                                                               */
/*   OPEN QUERY {&BROWSE-NAME} FOR EACH ASI.wiptag-mch OF ASI.wiptag NO-LOCK,                  */
/*       EACH asi.item WHERE TRUE /* Join to asi.wiptag incomplete */                          */
/*          AND asi.item.company = cocode                                                      */
/*          AND asi.item.i-no = wiptag.rm-i-no NO-LOCK,                                        */
/*       EACH asi.mach WHERE TRUE /* Join to ASI.wiptag-mch incomplete */                      */
/*           AND asi.mach.company = cocode                                                     */
/*           AND asi.mach.m-code = asi.wiptag-mch.m-code NO-LOCK,                              */
/*       EACH asi.dept WHERE TRUE /* Join to asi.mach incomplete */                            */
/*           AND asi.dept.code = asi.mach.dept[1] NO-LOCK                                      */
/*       BY asi.dept.seq .                                                                     */
/* /*   WHEN 2 THEN                                                                         */ */
/* /*   OPEN QUERY {&BROWSE-NAME} FOR EACH emaildtl OF emailcod  NO-LOCK,                   */ */
/* /*        EACH phone NO-LOCK WHERE phone.rec_key EQ emaildtl.table_rec_key,              */ */
/* /*        FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ phone.table_rec_key */ */
/* /*        AND {&emailTable}.name BEGINS auto_find                                        */ */
/* /*        BY {&emailTable}.name.                                                         */ */
/* /*   WHEN 3 THEN                                                                         */ */
/* /*   OPEN QUERY {&BROWSE-NAME} FOR EACH emaildtl OF emailcod NO-LOCK,                    */ */
/* /*        EACH phone NO-LOCK WHERE phone.rec_key EQ emaildtl.table_rec_key               */ */
/* /*        AND phone.attention BEGINS auto_find,                                          */ */
/* /*        FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ phone.table_rec_key */ */
/* /*        BY phone.attention.                                                            */ */
/* /*   WHEN 4 THEN                                                                         */ */
/* /*   OPEN QUERY {&BROWSE-NAME} FOR EACH emaildtl OF emailcod  NO-LOCK,                   */ */
/* /*        EACH phone NO-LOCK WHERE phone.rec_key EQ emaildtl.table_rec_key               */ */
/* /*        AND phone.titlcode BEGINS auto_find,                                           */ */
/* /*        FIRST {&emailTable} NO-LOCK WHERE {&emailTable}.rec_key EQ phone.table_rec_key */ */
/* /*        BY phone.titlcode.                                                             */ */
/* END CASE.                                                                                   */

/* FOR EACH ASI.wiptag-mch OF ASI.wiptag NO-LOCK,                          */
/*       EACH asi.item WHERE TRUE /* Join to asi.wiptag incomplete */      */
/*       AND asi.item.company = cocode                                     */
/*  AND asi.item.i-no = wiptag.rm-i-no NO-LOCK,                            */
/*       EACH asi.mach WHERE TRUE /* Join to ASI.wiptag-mch incomplete */  */
/*       AND asi.mach.company = cocode and                                 */
/* asi.mach.m-code = asi.wiptag-mch.m-code NO-LOCK,                        */
/*       EACH asi.dept WHERE TRUE /* Join to asi.mach incomplete */        */
/*       AND asi.dept.code = asi.mach.dept[1] NO-LOCK                      */
/*     ~{&SORTBY-PHRASE}:                                                  */

