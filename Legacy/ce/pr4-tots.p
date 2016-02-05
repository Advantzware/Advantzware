/* --------------------------------------------------- ce/pr4-tots.p 1/94 cd  */

{sys/inc/var.i SHARED}

DEF SHARED BUFFER xest FOR est.
DEF SHARED BUFFER xef FOR ef.
DEF SHARED BUFFER xeb FOR eb.

DEF SHARED VAR qty AS INT NO-UNDO.

{ce/print4.i SHARED SHARED}

DEF VAR op-tot-t LIKE op-tot NO-UNDO.
DEF VAR qm AS de NO-UNDO.
DEF VAR ld-dm-mrkp AS DEC NO-UNDO.
DEF VAR ld-pmrkp AS DEC EXTENT 3 NO-UNDO.


  {sys/inc/cerun.i F}
  {sys/inc/cematl.i}

   qm = qty / 1000.

   find first ce-ctrl where ce-ctrl.company = cocode and
                            ce-ctrl.loc     = locode no-lock no-error.
   find first style where style.company = cocode and style.style = xeb.style
   no-lock no-error.
   if avail style then ctrl2[18] = style.royalty.

   {est/calcpcts.i xest}
   IF calcpcts.val[1] EQ 0 THEN calcpcts.val[2] = 0.

   IF cematl-log THEN ld-dm-mrkp = dm-tot[5] * cematl-dec / 100.

   ASSIGN
     xxx = dm-tot[5] - calcpcts.val[2] + tprep-mat + mis-tot[1]
     ctrl2[9] = xxx * ctrl[9]
     xxx = op-tot[5] + tprep-lab + mis-tot[3]
     ctrl2[10] = xxx * ctrl[10].

   IF cerunf EQ "Dee" THEN DO:
     FOR EACH est-prep NO-LOCK
         WHERE est-prep.company EQ xest.company
           AND est-prep.est-no  EQ xest.est-no
           AND est-prep.code    NE "":

       IF est-prep.ml THEN
         ld-pmrkp[1] = ld-pmrkp[1] +
                       ((est-prep.cost * est-prep.qty) *
                        (IF est-prep.amtz NE 0 THEN est-prep.amtz / 100 ELSE 1)).
       ELSE
         ld-pmrkp[2] = ld-pmrkp[2] +
                       ((est-prep.cost * est-prep.qty) *
                        (IF est-prep.amtz NE 0 THEN est-prep.amtz / 100 ELSE 1)).
     END.

     ASSIGN
      ld-pmrkp[1] = tprep-mat - ld-pmrkp[1]
      ld-pmrkp[2] = tprep-lab - ld-pmrkp[2]
      ld-pmrkp[3] = ld-pmrkp[1] + ld-pmrkp[2]
      tprep-mat   = tprep-mat - ld-pmrkp[1]
      tprep-lab   = tprep-lab - ld-pmrkp[2].
   END.

   ASSIGN
     fac-tot = dm-tot[5] + op-tot[5] +
               tprep-mat + tprep-lab + mis-tot[1] + mis-tot[3]
     calcpcts.val[2] = calcpcts.val[2] * calcpcts.val[1] / 100.

   FIND CURRENT calcpcts NO-LOCK NO-ERROR.

   ASSIGN
    ctrl2[1] = (fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10] + ld-dm-mrkp) *
               ctrl[1]
    ctrl2[13] = (fac-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10] + ld-dm-mrkp) *
                ctrl[19].

   find first xeb where xeb.company = xest.company
                    AND xeb.est-no eq xest.est-no
                    and xeb.form-no ne 0 no-error.

   if xeb.chg-method eq "P" and ctrl[6] ne 0 then fac-tot = fac-tot + fr-tot.

   assign
    ctrl2[4] = 0
    ctrl2[5] = 0
    ctrl2[11] = 0
    ctrl2[12] = 0.

   if ctrl[4]  gt 0 and ctrl[4] le 1
      then ctrl2[4]  = fac-tot * ctrl[4].
      else if ctrl[4] gt 1 then ctrl2[4] = ctrl[4].
   if ctrl[11] gt 0 then do:             /* set spec#2 */
      if ctrl[11] le 1
      then ctrl2[11] = fac-tot * ctrl[11].
      else ctrl2[11] = ctrl[11].
   end.
   if ctrl[12] gt 0 then do:             /* set spec#3 */
      if ctrl[12] le 1
      then ctrl2[12] = fac-tot * ctrl[12].
      else ctrl2[12] = ctrl[12].
   end.
   if (ctrl[5] gt 0 and ctrl[17] eq 1)
      then ctrl2[5] = (fac-tot * (xeb.comm / 100)).

   /*if cerunf NE "Dee" AND avail style then do:
      if (style.royalty ne 0 and ctrl[18] eq 1) then do:
         if style.royalty le 1
         then ctrl2[18] = fac-tot * ctrl[18].
         else ctrl2[18] = ctrl[18] * qm.
      end.
   end.*/

   if (not vmclean) or xest.est-type eq 3 then do:

   /* view frame hdr. */
      put
   "*** T O T A L S                           Cost/M     MR $      Run $  Total Cost" skip
      "Direct Material" dm-tot[4] to 48 dm-tot[3] format ">>>>9.99" to 57
                        dm-tot[5] format "->>>>>>9.99" to 80 skip.

      if not vmclean AND cerunf NE "Dee" THEN
      put
      "Direct Labor" op-tot[5] / qm       to 48
                     op-tot[3] format ">>>>9.99"    to 57
                     op-tot[4] format ">>>>>>9.99"  to 68
                     op-tot[5] format "->>>>>>9.99" to 80 skip.
      if tprep-mat ne 0 then put
         "Prep.  Material" tprep-mat / qm to 48 tprep-mat to 80  skip.
      if tprep-lab ne 0 then put
         "Prep.  Labor   " tprep-lab / qm to 48 tprep-lab to 80  skip.
      if mis-tot[1] ne 0 then put
      "Misc.  Material" mis-tot[2] to 48 mis-tot[1] to 80 skip.
      if mis-tot[3] ne 0 then put
      "Misc.  Labor   " mis-tot[4] to 48 mis-tot[3] to 80 skip.

      if vmclean OR cerunf EQ "Dee" then do:
        assign
         op-tot-t[1] = op-tot[1]
         op-tot-t[2] = op-tot[2]
         op-tot-t[3] = op-tot[3]
         op-tot-t[4] = op-tot[4]
         op-tot-t[5] = op-tot[5]

         op-tot[5]   = op-tot[5] - op-tot[6] - op-tot[7]
         op-tot[3]   = round(op-tot[5] * (op-tot-t[3] / op-tot-t[5]),2)
         op-tot[4]   = round(op-tot[5] * (op-tot-t[4] / op-tot-t[5]),2)
         fac-tot     = fac-tot   - op-tot[7].

        IF op-tot[3] EQ ? THEN op-tot[3] = 0.
        IF op-tot[4] EQ ? THEN op-tot[4] = 0.

        put "Direct Labor"
            op-tot[5] / qm                 to 48
            op-tot[3] format ">>>>9.99"    to 57
            op-tot[4] format ">>>>>>9.99"  to 68
            op-tot[5] format ">>>>,>>9.99" to 80 skip.

        assign
         op-tot[3]   = round(op-tot[6] * (op-tot-t[3] / op-tot-t[5]),2)
         op-tot[4]   = round(op-tot[6] * (op-tot-t[4] / op-tot-t[5]),2).

        IF op-tot[3] EQ ? THEN op-tot[3] = 0.
        IF op-tot[4] EQ ? THEN op-tot[4] = 0.

        put "Variable Overhead"
            op-tot[6] / qm                 to 48
            op-tot[3] format ">>>>9.99"    to 57
            op-tot[4] format ">>>>>>9.99"  to 68
            op-tot[6] format ">>>>,>>9.99" to 80 skip.
      end.

      if xeb.chg-method eq "P" and ctrl[6] ne 0 and fr-tot ne 0 then
        put "Freight"  fr-tot / qm to 48 fr-tot to 80 skip.

      put "DIRECT FACTORY COST" fac-tot / qm to 48
                          fac-tot format ">>>>,>>9.99" to 80 skip.

      fac-tot2 = fac-tot.

      if vmclean OR cerunf EQ "Dee" then do:
        assign
         op-tot[3]   = round(op-tot[7] * (op-tot-t[3] / op-tot-t[5]),2)
         op-tot[4]   = round(op-tot[7] * (op-tot-t[4] / op-tot-t[5]),2).

        IF op-tot[3] EQ ? THEN op-tot[3] = 0.
        IF op-tot[4] EQ ? THEN op-tot[4] = 0.

        put "Fixed Overhead"
            op-tot[7] / qm                 to 48
            op-tot[3] format ">>>>9.99"    to 57
            op-tot[4] format ">>>>>>9.99"  to 68
            op-tot[7] format ">>>>,>>9.99" to 80 skip.

        fac-tot2 = fac-tot2 + op-tot[7].

        IF cerunf EQ "Dee" THEN DO:
          PUT "Prep Markup"
              ld-pmrkp[3] / qm                 TO 48
              ld-pmrkp[3] FORMAT ">>>>,>>9.99" TO 80 SKIP.

          fac-tot2 = fac-tot2 + ld-pmrkp[3].
        END.
      end.

      if (ctrl[13] eq 1 OR cerunf EQ "Dee") and ctrl[4] ne 0 then do:
         fac-tot2 = fac-tot2 + ctrl2[4].
         if ctrl[4] gt 0 then put ce-ctrl.spec-l[1].
         if ctrl[4] le 1 then
         put string(ce-ctrl.spec-%[1] * 100,">>9.99") + "%" to 30.
         put ctrl2[4] / qm to 48 ctrl2[4] to 80 skip.
      end.
      if (ctrl[14] eq 1 OR cerunf EQ "Dee") and ctrl[11] ne 0 then do:
         fac-tot2 = fac-tot2 + ctrl2[11].
         if ctrl[11] gt 0 then put
         ce-ctrl.spec-l[2] space(1).
         if ctrl[11] le 1 then
         put string(ce-ctrl.spec-%[2] * 100,">>9.99") + "%" to 30.
         put ctrl2[11] / qm to 48 ctrl2[11] to 80 skip.
      end.
      if (ctrl[15] eq 1 OR cerunf EQ "Dee") and ctrl[12] ne 0 then do:
         fac-tot2 = fac-tot2 + ctrl2[12].
         if ctrl[12] gt 0  then put ce-ctrl.spec-l[3] space(1).
         if ctrl[12] le 1 then
         put string(ce-ctrl.spec-%[3] * 100,">>9.99") + "%" to 30.
         put ctrl2[12] / qm to 48 ctrl2[12] to 80 skip.
      end.

      IF ctrl[16] NE 0 OR cerunf EQ "Dee" THEN DO:
        PUT "GS&A Board"
            STRING(calcpcts.val[1],">>9.99") + "%"                  TO 30
            calcpcts.val[2] / qm                                    TO 48
            calcpcts.val[2]                                         TO 80 SKIP

            "GS&A Material" STRING(ctrl[9] * 100,">>9.99")  + "%"   TO 30
            ctrl2[9]  / qm                                          TO 48
            ctrl2[9]                                                TO 80 SKIP

            "GS&A Labor" STRING(ctrl[10] * 100,">>9.99") + "%"      TO 30
            ctrl2[10] / qm                                          TO 48
            ctrl2[10]                                               TO 80 SKIP.

        IF ld-dm-mrkp NE 0 THEN
          PUT "Direct Material Markup"
              STRING(cematl-dec,">>9.99") + "%"                     TO 30
              ld-dm-mrkp / qm                                       TO 48
              ld-dm-mrkp                                            TO 80 SKIP.

        fac-tot2 = fac-tot2 + calcpcts.val[2] + ctrl2[9] + ctrl2[10] +
                              ld-dm-mrkp.
      END.

      if cerunf NE "Dee" AND ctrl[18] gt 0 and ctrl2[18] ne 0 then do:   /* Royalty */
         if ctrl2[18] le 1 then do:
            put "Royalty" string(ctrl2[18] * 100,">>9.99") + "%" to 30
                                     (ctrl2[18] * fac-tot) to 80 skip.
            fac-tot2 = fac-tot2 + (fac-tot * ctrl2[18]).
         end.
         else do:
            put "Royalty" ctrl2[18] / qm to 48 ctrl2[18] to 80 skip.
            fac-tot2 = fac-tot2 + ctrl2[18].
         end.
      end.
          
      IF cerunf EQ "Dee" THEN
        PUT "TOTAL CONTRIBUTION"        FORMAT "x(19)"
            (fac-tot2 - fac-tot) / qm   TO 48
            (fac-tot2 - fac-tot)        TO 80 SKIP.
      ELSE
        PUT "TOTAL FACTORY COST"        FORMAT "x(19)"
            fac-tot2 / qm               TO 48
            fac-tot2                    TO 80 SKIP.

      assign
       ord-cost = fac-tot2
       tt-tot   = fac-tot2.

      IF cerunf NE "Dee" THEN DO:
        if ctrl2[1] ne 0 then do:

           put "Warehousing" string(ctrl[1] * 100,">>9.99") + "%" to 30
                ctrl2[1] to 80 skip.
           tt-tot = tt-tot + ctrl2[1].
        end.

        IF ctrl2[13] NE 0 THEN DO:
           PUT "Folding" STRING(ctrl[19] * 100,">>9.99") + "%" to 30
               ctrl2[13] TO 80 SKIP.
           tt-tot = tt-tot + ctrl2[13].
        END.

        if xeb.chg-method eq "P" and ctrl[6] eq 0 and fr-tot ne 0 then do:
           put "Freight" fr-tot / qm to 48 fr-tot to 80 skip.
           tt-tot = tt-tot + fr-tot.
        end.

        if ctrl[13] eq 0 and ctrl[4] ne 0 then do:
           if ctrl[4] le 1 then ctrl2[4]  = fac-tot2 * ctrl[4].
           else if ctrl[4] gt 1 then ctrl2[4] = ctrl[4].
           put ce-ctrl.spec-l[1].
           if ctrl[4] le 1 then
              put string(ce-ctrl.spec-%[1] * 100,">>9.99") + "%" to 30.
           put ctrl2[4] / qm to 48 ctrl2[4] to 80 skip.
           tt-tot = tt-tot + ctrl2[4].
        end.
        if ctrl[14] eq 0 and ctrl[11] ne 0 then do:             /* set spec#2 */
           if ctrl[11] le 1 then ctrl2[11] = fac-tot2 * ctrl[11].
           else ctrl2[11] = ctrl[11].
           put ce-ctrl.spec-l[2].
           if ctrl[11] le 1
              then put string(ce-ctrl.spec-%[2] * 100,">>9.99") + "%" to 30.
           put ctrl2[11] / qm to 48 ctrl2[11] to 80 skip.
           tt-tot = tt-tot + ctrl2[11].
        end.
        if ctrl[15] eq 0 and ctrl[12] ne 0 then do:             /* set spec#3 */
           if ctrl[12] le 1 then ctrl2[12] = fac-tot2 * ctrl[12].
           else ctrl2[12] = ctrl[12].
           put ce-ctrl.spec-l[3].
           if ctrl[12] le 1
              then put string(ce-ctrl.spec-%[3] * 100,">>9.99") + "%" to 30.
           put ctrl2[12] / qm to 48 ctrl2[12] to 80 skip.
           tt-tot = tt-tot + ctrl2[12].
        end.

        IF ctrl[16] EQ 0 THEN DO:
          PUT "GS&A Board"
              STRING(calcpcts.val[1],">>9.99") + "%"                  TO 30
              calcpcts.val[2] / qm                                    TO 48
              calcpcts.val[2]                                         TO 80 SKIP

              "GS&A Material" STRING(ctrl[9] * 100,">>9.99")  + "%"   TO 30
              ctrl2[9]  / qm                                          TO 48
              ctrl2[9]                                                TO 80 SKIP

              "GS&A Labor" STRING(ctrl[10] * 100,">>9.99") + "%"      TO 30
              ctrl2[10] / qm                                          TO 48
              ctrl2[10]                                               TO 80 SKIP.

          IF ld-dm-mrkp NE 0 THEN
            PUT "Direct Material Markup"
                STRING(cematl-dec,">>9.99") + "%"                     TO 30
                ld-dm-mrkp / qm                                       TO 48
                ld-dm-mrkp                                            TO 80 SKIP.

          tt-tot = tt-tot + calcpcts.val[2] + ctrl2[9] + ctrl2[10] + ld-dm-mrkp.
        END.

        if ctrl[18] eq 0 and ctrl2[18] ne 0 then do:   /* Royalty */
           if ctrl2[18] le 1 then do:
              put "Royalty" string(ctrl2[18] * 100,">>9.99") + "%" to 30
                  (ctrl2[18] * fac-tot2) / qm to 48
                  (ctrl2[18] * fac-tot2)   to 80 skip.
              tt-tot = tt-tot + (fac-tot2 * ctrl2[18]).
           end.
           else do:
              put "Royalty" ctrl2[18] / qm to 48 ctrl2[18] to 80 skip.
              tt-tot = tt-tot + ctrl2[18].
           end.
        end.
      END.  /* cerun NE "Dee" */
   end.

   else do:
     assign
      vmcl-desc = "Prep.  Material"
      vmcl-cost = tprep-mat / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      vmcl-desc = "Prep.  Labor"
      vmcl-cost = tprep-lab / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      vmcl-desc = "Misc.  Material"
      vmcl-cost = mis-tot[2].
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      vmcl-desc = "Misc.  Labor"
      vmcl-cost = mis-tot[4].
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     op-tot[5] = op-tot[5] - op-tot[6] - op-tot[7].
     fac-tot   = fac-tot   - op-tot[7].

     assign
      vmcl-desc = "Direct Material"
      vmcl-cost = dm-tot[4].
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      vmcl-desc = "Direct Labor"
      vmcl-cost = op-tot[5] / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      vmcl-desc = "Variable Overhead"
      vmcl-cost = op-tot[6] / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     if xeb.chg-method eq "P" and ctrl[6] ne 0 and fr-tot ne 0 then do:
       assign
        vmcl-desc = "Freight"
        vmcl-cost = fr-tot / qm.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     assign
      vmcl-desc = "DIRECT FACTORY COST"
      vmcl-cost = fac-tot / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     fac-tot2 = fac-tot.

     assign
      vmcl-desc = "Fixed Overhead"
      vmcl-cost = op-tot[7] / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     fac-tot2 = fac-tot2 + op-tot[7].
     if ctrl[13] ne 0 then fac-tot2 = fac-tot2 + ctrl2[4].
     if ctrl[14] ne 0 then fac-tot2 = fac-tot2 + ctrl2[11].
     if ctrl[15] ne 0 then fac-tot2 = fac-tot2 + ctrl2[12].
     if ctrl[18] ne 0 then fac-tot2 = fac-tot2 + ctrl2[18].

     if ctrl[13] eq 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[1]
        vmcl-cost = ctrl2[4] / qm.

       if ctrl[4] le 1 then
         vmcl-desc = vmcl-desc + " - " +
                     trim(string(ce-ctrl.spec-%[1] * 100,">>9.99%")).

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     if ctrl[14] eq 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[2]
        vmcl-cost = ctrl2[11] / qm.

       if ctrl[11] le 1 then
         vmcl-desc = vmcl-desc + " - " +
                     trim(string(ce-ctrl.spec-%[2] * 100,">>9.99%")).

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     if ctrl[15] eq 1 then do:
       assign
        vmcl-desc = ce-ctrl.spec-l[3]
        vmcl-cost = ctrl2[12] / qm.

       if ctrl[12] le 1 then
         vmcl-desc = vmcl-desc + " - " +
                     trim(string(ce-ctrl.spec-%[3] * 100,">>9.99%")).

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     IF ctrl[16] NE 0 THEN DO:
       ASSIGN
        vmcl-desc = "GS&A Board"
        vmcl-cost = calcpcts.val[2] / qm
        fac-tot2  = fac-tot2 + calcpcts.val[2].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsabrd".

       ASSIGN
        vmcl-desc = "    GS&A Board %"
        vmcl-cost = calcpcts.val[1].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsabrd".

       ASSIGN
        vmcl-desc = "GS&A Material"
        vmcl-cost = ctrl2[9] / qm
        fac-tot2  = fac-tot2 + ctrl2[9].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsamat".

       ASSIGN
        vmcl-desc = "    GS&A Material %"
        vmcl-cost = ctrl[9] * 100.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsamat".

       ASSIGN
        vmcl-desc = "GS&A Labor"
        vmcl-cost = ctrl2[10] / qm
        fac-tot2  = fac-tot2 + ctrl2[10].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsalab".

       ASSIGN
        vmcl-desc = "    GS&A Labor %"
        vmcl-cost = ctrl[10] * 100.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsalab".

       IF ld-dm-mrkp NE 0 THEN DO:
         ASSIGN
          vmcl-desc = "Direct Material Markup"
          vmcl-cost = ld-dm-mrkp / qm
          fac-tot   = fac-tot + ld-dm-mrkp.

         {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
         mclean.rec-type = "dm-mrkp".

         ASSIGN
          vmcl-desc = "    Direct Material Markup %"
          vmcl-cost = cematl-dec.

         {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
         mclean.rec-type = "dm-mrkp".
       END.
     END.

     if ctrl[18] gt 0 then do:   /* Royalty */
       vmcl-desc = "Royalty".

       if ctrl2[18] le 1 then
         assign
          vmcl-desc = vmcl-desc + " - " +
                      trim(string(ctrl2[18] * 100,">>9.99%"))
          vmcl-cost = (ctrl2[18] * fac-tot).

       else vmcl-cost = ctrl2[18].

       assign
        fac-tot2  = fac-tot2 + vmcl-cost
        vmcl-cost = vmcl-cost / qm.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     assign
      vmcl-desc = "TOTAL FACTORY COST"
      vmcl-cost = fac-tot2 / qm.
     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}

     assign
      ord-cost = fac-tot2
      tt-tot   = fac-tot2.

     assign
      vmcl-desc = "Warehousing"
      vmcl-cost = ctrl2[1] / qm
      tt-tot    = tt-tot + ctrl2[1].

     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     mclean.rec-type = "warehousing".
       
     assign
      vmcl-desc = "    Warehousing %"
      vmcl-cost = ctrl[1] * 100.

     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     mclean.rec-type = "warehousing".

     ASSIGN
      vmcl-desc = "Folding"
      vmcl-cost = ctrl2[13] / qm
      tt-tot    = tt-tot + ctrl2[13].

     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     mclean.rec-type = "folding".
       
     assign
      vmcl-desc = "    Folding %"
      vmcl-cost = ctrl[19] * 100.

     {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     mclean.rec-type = "folding".

     if xeb.chg-method eq "P" and ctrl[6] eq 0 and fr-tot ne 0 then do:
       assign
        vmcl-desc = "Freight"
        vmcl-cost = fr-tot / qm
        tt-tot    = tt-tot + fr-tot.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     if ctrl[13] eq 0 then do:
       vmcl-desc = ce-ctrl.spec-l[1].

       if ctrl[4] le 1 then
         assign
          vmcl-desc = vmcl-desc + " - " +
                      trim(string(ce-ctrl.spec-%[1] * 100,">>9.99%"))
          ctrl2[4]  = fac-tot2 * ctrl[4].
       else ctrl2[4] = ctrl[4].

       assign
        vmcl-cost = ctrl2[4] / qm
        tt-tot    = tt-tot + ctrl2[4].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     if ctrl[14] = 0 then do:             /* set spec#2 */
       vmcl-desc = ce-ctrl.spec-l[2].

       if ctrl[11] le 1 then
         assign
          vmcl-desc = vmcl-desc + " - " +
                      trim(string(ce-ctrl.spec-%[2] * 100,">>9.99%"))
          ctrl2[11]  = fac-tot2 * ctrl[11].
       else ctrl2[11] = ctrl[11].

       assign
        vmcl-cost = ctrl2[11] / qm
        tt-tot    = tt-tot + ctrl2[11].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     if ctrl[15] eq 0 then do:             /* set spec#3 */
       vmcl-desc = ce-ctrl.spec-l[3].

       if ctrl[12] le 1 then
         assign
          vmcl-desc = vmcl-desc + " - " +
                      trim(string(ce-ctrl.spec-%[3] * 100,">>9.99%"))
          ctrl2[12]  = fac-tot2 * ctrl[12].
       else ctrl2[12] = ctrl[12].

       assign
        vmcl-cost = ctrl2[12] / qm
        tt-tot    = tt-tot + ctrl2[12].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.

     IF ctrl[16] EQ 0 THEN DO:
       ASSIGN
        vmcl-desc = "GS&A Board"
        vmcl-cost = calcpcts.val[2] / qm
        tt-tot    = tt-tot + calcpcts.val[2].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsabrd".

       ASSIGN
        vmcl-desc = "    GS&A Board %"
        vmcl-cost = calcpcts.val[1].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsabrd".

       ASSIGN
        vmcl-desc = "GS&A Material"
        vmcl-cost = ctrl2[9] / qm
        tt-tot    = tt-tot + ctrl2[9].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsamat".

       ASSIGN
        vmcl-desc = "    GS&A Material %"
        vmcl-cost = ctrl[9] * 100.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsamat".

       ASSIGN
        vmcl-desc = "GS&A Labor"
        vmcl-cost = ctrl2[10] / qm
        tt-tot    = tt-tot + ctrl2[10].

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsalab".

       ASSIGN
        vmcl-desc = "    GS&A Labor %"
        vmcl-cost = ctrl[10] * 100.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
       mclean.rec-type = "gsalab".

       IF ld-dm-mrkp NE 0 THEN DO:
         ASSIGN
          vmcl-desc = "Direct Material Markup"
          vmcl-cost = ld-dm-mrkp / qm
          tt-tot    = tt-tot + ld-dm-mrkp.

         {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
         mclean.rec-type = "dm-mrkp".

         ASSIGN
          vmcl-desc = "    Direct Material Markup %"
          vmcl-cost = cematl-dec.

         {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
         mclean.rec-type = "dm-mrkp".
       END.
     END.

     if ctrl[18] eq 0 then do:   /* Royalty */
       vmcl-desc = "Royalty".

       if ctrl2[18] le 1 then
         assign
          vmcl-desc = vmcl-desc + " - " +
                      trim(string(ctrl2[18] * 100,">>9.99%"))
          vmcl-cost = (ctrl2[18] * fac-tot).

       else vmcl-cost = ctrl2[18].

       assign
        tt-tot    = tt-tot + vmcl-cost
        vmcl-cost = vmcl-cost / qm.

       {ce/pr4-mcln.i vmcl-desc vmcl vmcl-cost}
     end.
   end.

   IF cerunf EQ "Dee" THEN
     ASSIGN
      tprep-mat = tprep-mat + ld-pmrkp[1]
      tprep-lab = tprep-lab + ld-pmrkp[2].

/* end ---------------------------------- copr. 1993  advanced software, inc. */
