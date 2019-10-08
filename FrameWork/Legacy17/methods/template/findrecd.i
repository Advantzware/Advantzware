/* findrecd.i 

01.01.2011 YSK Changed for multiple table browser
01.30.2012 RES Changed to allow for MATCHES functionality

*/


&IF "{&WHERE-STATEMENT}" EQ "" &THEN
 &IF DEFINED(defaultWhere) NE 0 &THEN
           &SCOPED-DEFINE WHERE-STATEMENT {&defaultWhere}
        &ELSE
           &SCOPED-DEFINE WHERE-STATEMENT TRUE 
        &ENDIF
&ENDIF

&IF "{&IAMWHAT}" EQ "SEARCH" &THEN
{methods/template/srchrecd.i {1}}
&ELSE
  &IF "{&FLDNAME{1}}" NE "" &THEN
  WHEN {1} THEN DO:
    &IF "{&DATATYP{1}}" NE "" AND "{&browse2}" EQ "" &THEN
    IF NOT find-auto THEN DO:
      IF auto_find NE "" THEN
      MESSAGE
        "The Sort-By field has a Data Type of ~"{&DATATYP{1}}~" and" SKIP
        "trying to find a specfic record with only a partial value" SKIP
        "will not work.  Enter the full value to find in Auto Find" SKIP
        "field and Hit RETURN." VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
    END. /* if not find-auto */
    auto_find = STRING({&DATATYP{1}}(auto_find),"{&FORMAT-{1}}") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE
        "Unable to evaluate Auto Find value for Data Type ~"{&DATATYP{1}}~""
            VIEW-AS ALERT-BOX ERROR.
      RETURN.
    END. /* if error-status:error */
    &ENDIF
    &SCOPED-DEFINE SORTBY-PHRASE {&IDXNAME{1}} {&SORTBY-{1}}
    &IF "{&DATATYP{1}}" EQ "" &THEN
        &IF DEFINED(useMatches) NE 0  &THEN
           &SCOPED-DEFINE KEY-PHRASE {&WHERE-STATEMENT} AND ({&FLDNAME{1}} BEGINS auto_find OR ({&FLDNAME{1}} MATCHES '*' + auto_find + '*' AND auto_find BEGINS '*' ))   /*task 10171311   */
        &ELSE
           &SCOPED-DEFINE KEY-PHRASE {&WHERE-STATEMENT} AND TRIM({&FLDNAME{1}}) BEGINS auto_find
        &ENDIF   
    &ELSE
    &SCOPED-DEFINE KEY-PHRASE {&WHERE-STATEMENT} AND {&FLDNAME{1}} EQ {&DATATYP{1}}(auto_find)
    &ENDIF
    &IF "{&item-key-phrase}" EQ "est" AND INT("{1}") GE 2 &THEN    /* YSK  11/01/01 for 2 table browser */
       /*&undefine item-key-phrase    */
       &SCOPED-DEFINE key-phrase true          
       OPEN QUERY Browser-Table FOR EACH ASI.est WHERE {&KEY-PHRASE}
                                     AND ASI.est.est-type GE 5 NO-LOCK,
                        FIRST ASI.est-qty WHERE ASI.est-qty.company EQ ASI.est.company
                                  AND ASI.est-qty.est-no EQ ASI.est.est-no OUTER-JOIN NO-LOCK,
                        FIRST ASI.ef WHERE ASI.ef.company EQ ASI.est.company
                                   AND ASI.ef.est-no EQ ASI.est.est-no NO-LOCK,
                        FIRST ASI.eb WHERE ASI.eb.company EQ ASI.est.company
                                   AND ASI.eb.est-no EQ ASI.est.est-no and
                                   {&WHERE-STATEMENT} and {&FLDNAME{1}} BEGINS auto_find
                                   NO-LOCK {&SORTBY-PHRASE}.                          
      /* ================= end of mods ===========*/  
    &ELSEIF DEFINED(browse2) <> 0 &THEN
      /* ========== */
       {{&browse2}}     
      /* ==========*/
    &ELSE
      {&OPEN-QUERY-{&BROWSE-NAME}}
    &ENDIF
    RUN dispatch ('row-changed').  /* ysk added */   
    &SCOPED-DEFINE KEY-PHRASE TRUE
    &Undefine SORTBY-PHRASE
  END. /* do block of when statement */
  &ENDIF
&ENDIF
