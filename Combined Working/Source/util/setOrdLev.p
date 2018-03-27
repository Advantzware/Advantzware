
DEF VAR i AS INT.
DEF VAR j AS INT.


/* {custom/gcompany.i}                                                        */
/* {custom/gloc.i}                                                            */
/*                                                                            */
/* /* {sys/inc/var.i NEW SHARED} */                                           */
/* {custom/globdefs.i}                                                        */
/* /* Local Variable Definitions ---                                       */ */
/* DEF VAR cocode AS CHAR.                                                    */
/* DEF VAR locode AS CHAR.                                                    */
/* /*def var gcompany as char.                                                */
/* def var gloc as char. */                                                   */
/* /*def var g_company as char.                                               */
/* def var g_loc as char. */                                                  */
/*                                                                            */
/* DEF VAR c1 AS INT NO-UNDO.                                                 */
/* DEF VAR c2 AS INT NO-UNDO.                                                 */

DEF VAR h_wmessage AS HANDLE.
/* DEF TEMP-TABLE tt-itemfg-loc LIKE itemfg-loc. */


MESSAGE "Press YES to copy order levels from 'All' to the main warehouse."
VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO-CANCEL
UPDATE ll AS LOG.

IF ll EQ YES THEN DO:
  
/*                                                                      */
/*                                                                      */
/*   FIND FIRST usercomp WHERE usercomp.user_id = USERID('NOSWEAT') AND */
/*   usercomp.loc = '' AND                                              */
/*   usercomp.company_default = YES NO-LOCK NO-ERROR.                   */
/*   g_company = IF AVAIL usercomp THEN usercomp.company ELSE "001".    */
/*                                                                      */
/*                                                                      */
/*   FIND FIRST usercomp WHERE usercomp.user_id = USERID("NOSWEAT") AND */
/*   usercomp.company = g_company AND                                   */
/*   usercomp.loc NE "" AND                                             */
/*   usercomp.loc_default = YES                                         */
/*   NO-LOCK NO-ERROR.                                                  */
/*   g_loc = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN".           */
/*   cocode = g_company.                                                */
/*   locode = g_loc.                                                    */
/*   gcompany = g_company.                                              */
/*   gloc = g_loc.                                                      */
/*   c1 = 100.                                                          */
  PAUSE BEFORE-HIDE.
  
  /* ******************************************************************** */
  /* Populate location for each release record                            */
  /* ******************************************************************** */
  
  /****************************/
  FOR EACH usercomp WHERE usercomp.user_id = USERID('NOSWEAT') AND
    usercomp.loc_default = YES NO-LOCK
      /*EACH company,*/,
   EACH itemfg WHERE itemfg.company = usercomp.company 
      AND (itemfg.ord-max GT 0
            OR itemfg.ord-min GT 0
            OR itemfg.ord-lev GT 0) 
   NO-LOCK.


   FIND FIRST itemfg-loc WHERE itemfg-loc.company = itemfg.company
        AND itemfg-loc.i-no = itemfg.i-no
        AND itemfg-loc.loc  = usercomp.loc
       NO-ERROR.
   

   IF AVAIL itemfg-loc AND
    (itemfg-loc.ord-max EQ 0
            OR itemfg-loc.ord-min EQ 0
            OR itemfg-loc.ord-lev EQ 0) THEN DO:

     ASSIGN
       itemfg-loc.ord-max = itemfg.ord-max
       itemfg-loc.ord-min = itemfg.ord-min
       itemfg-loc.ord-lev = itemfg.ord-lev.
   END.
   
    i = i + 1.
    IF i GT 99 THEN DO:
      i = 1.
      j = j + 100.
      STATUS DEFAULT STRING(j).
    END.
  END. /* each oe-ord */
END.
MESSAGE "Processing Complete."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
