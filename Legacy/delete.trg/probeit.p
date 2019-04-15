&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME probeit

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}


/*FOR EACH reftable                                                    */
/*    WHERE reftable.reftable EQ "ce/com/probemk.p"                    */
/*      AND reftable.company  EQ {&TABLENAME}.company                  */
/*      AND reftable.loc      EQ {&TABLENAME}.est-no                   */
/*      AND reftable.code     EQ STRING({&TABLENAME}.line,"9999999999")*/
/*      AND reftable.code2    EQ {&TABLENAME}.part-no:                 */
/*  DELETE reftable.                                                   */
/*END.                                                                 */
