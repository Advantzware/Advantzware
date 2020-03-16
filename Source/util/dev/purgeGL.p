
/*------------------------------------------------------------------------
    File        : purgeGL.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Dec 27 14:31:06 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE NEW SHARED VARIABLE g_lookup-var  AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE g_track_usage AS LOGICAL       NO-UNDO.
DEFINE NEW SHARED VARIABLE g_header_line AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE g_groups      AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE init_menu     AS LOGICAL       NO-UNDO.
DEFINE NEW SHARED VARIABLE g_developer   AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE g_version     AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE g_rec_key     AS CHARACTER     NO-UNDO.
DEFINE NEW SHARED VARIABLE g_pageno      AS INTEGER       NO-UNDO.
DEFINE NEW SHARED VARIABLE g_mainmenu    AS WIDGET-HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */
{methods/defines/hndldefs.i &new=NEW}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i NEW SHARED}
{custom/globdefs.i &NEW=NEW}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{util/dev/deltable.i gltrans}
{util/dev/deltable.i glhist}
{util/dev/deltable.i gl-jrnl}
{util/dev/deltable.i gl-rpt}
/* {util/dev/deltable.i ap-chk} */ /* code in trigger, set bank.last-chk, del ap-sel */
/*FOR EACH ap-chk EXCLUSIVE-LOCK.                                               */
/*    DELETE ap-chk.                                                            */
/*END.                                                                          */
/*{util/dev/deltable.i ap-dis}  /* code in trigger, set bank.last-chk*/             */
/*{util/dev/deltable.i ap-disl}                                                     */
/*{util/dev/deltable.i ap-inv}  /* code in trigger, reset fg-rcpth.b-no */          */
/*{util/dev/deltable.i ap-ledger}                                                   */
/*FOR EACH ap-invl EXCLUSIVE-LOCK:                                              */
/*    FIND FIRST po-ordl                                                        */
/*        WHERE po-ordl.company   EQ ap-invl.company                            */
/*        AND po-ordl.po-no     EQ ap-invl.po-no                                */
/*        AND po-ordl.line      EQ {ap/invlline.i -1}                           */
/*        AND po-ordl.item-type EQ NO                                           */
/*        NO-LOCK NO-ERROR.                                                     */
/*                                                                              */
/*    IF AVAILABLE po-ordl THEN                                                 */
/*        FOR EACH fg-rcpth                                                     */
/*            WHERE fg-rcpth.company   EQ ap-invl.company                       */
/*            AND fg-rcpth.i-no      EQ po-ordl.i-no                            */
/*            AND fg-rcpth.po-no     EQ trim(STRING(po-ordl.po-no,">>>>>>>>>>"))*/
/*            AND fg-rcpth.rita-code EQ "R"                                     */
/*            AND fg-rcpth.b-no      EQ ap-invl.i-no                            */
/*            USE-INDEX item-po:                                                */
/*                                                                              */
/*            fg-rcpth.b-no = 0.                                                */
/*        END.                                                                  */
/*                                                                              */
/*END.                                                                          */
/*{util/dev/deltable.i ap-invl}                                                        */
/*{util/dev/deltable.i ap-invlr}                                                       */
/*{util/dev/deltable.i ap-ledger}                                                      */
/*{util/dev/deltable.i ap-pay} /* code in trigger, deletes child record */             */
/*{util/dev/deltable.i ap-payl}                                                        */
/*{util/dev/deltable.i ap-sel}                                                         */
/*{util/dev/deltable.i aphist}                                                         */
/*                                                                                 */
/*                                                                                 */
/*FOR EACH ar-cash EXCLUSIVE-LOCK:                                                 */
/*    FIND FIRST reftable WHERE                                                    */
/*         reftable.reftable = "ARCASHHOLD" AND                                    */
/*         reftable.rec_key = ar-cash.rec_key                                      */
/*         USE-INDEX rec_key                                                       */
/*         EXCLUSIVE-LOCK NO-ERROR.                                                */
/*                                                                                 */
/*    IF AVAIL reftable THEN                                                       */
/*       DELETE reftable.                                                          */
/*                                                                                 */
/*END.                                                                             */
/*{util/dev/deltable.i ar-cash} /* code in trigger, del child, del reftable */         */
/*                                                                                 */
/*{util/dev/deltable.i ar-cashl}                                                       */
/*                                                                                 */
/*FOR EACH ar-inv EXCLUSIVE-LOCK:                                                  */
/*    FIND FIRST reftable WHERE                                                    */
/*     reftable.reftable EQ "brokerbol" AND                                        */
/*     reftable.CODE EQ STRING(ar-inv.inv-no)                                      */
/*     NO-ERROR.                                                                   */
/*                                                                                 */
/*    IF AVAIL reftable THEN                                                       */
/*       DELETE reftable.                                                          */
/*                                                                                 */
/*END.                                                                             */
/*{util/dev/deltable.i ar-inv}  /* code in trigger, child, reftable, ar-ctrl.last-inv*/*/
/*{util/dev/deltable.i ar-invl}                                                        */
/*{util/dev/deltable.i ar-invm}                                                        */
/*{util/dev/deltable.i ar-ledger}                                                      */
/*/* {util/dev/deltable.i ar-mcash } */ /* code in trigger (2 reftables) */            */
/*FOR EACH ar-mcash EXCLUSIVE-LOCK:                                                */
/*    DELETE ar-mcash.                                                             */
/*END.                                                                             */
{util/dev/deltable.i account } /* code in trigger  - just validation */
/*
          IF ar-ctrl.last-inv = ar-inv.inv-no THEN
             ar-ctrl.last-inv = ar-ctrl.last-inv - 1.
*/
