
DEFINE TEMP-TABLE ttbolInv NO-UNDO
BEFORE-TABLE beforebolInv
     FIELD vBolNo          LIKE oe-boll.bol-no  FORMAT ">>>>>>>9"
     FIELD vBolIno         LIKE oe-boll.i-no  FORMAT "x(15)"
     FIELD vBolJobNo       LIKE oe-boll.job-no FORMAT "x(6)"
     FIELD vBolinNo2       LIKE oe-boll.job-no2 FORMAT "99"
     FIELD vBolLoc         LIKE oe-boll.loc FORMAT "x(5)"
     FIELD vBolLocbin      LIKE oe-boll.loc-bin FORMAT "x(8)"
     FIELD vBolTag         LIKE oe-boll.tag FORMAT "x(23)"
     FIELD vBolCases       LIKE oe-boll.cases FORMAT "->>>,>>Z"
     FIELD vBolQtycase     LIKE oe-boll.qty-case FORMAT "->>>,>>Z"
     FIELD vBolPartial     LIKE oe-boll.partial FORMAT "->>>,>>9"
     FIELD vBolweight      LIKE oe-boll.weight FORMAT "->>>,>>9"
     FIELD vBolScode       LIKE oe-boll.s-code FORMAT "x"
    FIELD vTotal AS DECIMAL 
     FIELD vRowid AS RECID
    . 
DEFINE DATASET dsbolInv FOR ttbolInv .
DEFINE QUERY q-bolInvQuery FOR ttbolInv.
DEFINE DATA-SOURCE src-bolInv  FOR QUERY q-bolInvQuery.
BUFFER ttbolInv :ATTACH-DATA-SOURCE(DATA-SOURCE src-bolInv  :HANDLE).






