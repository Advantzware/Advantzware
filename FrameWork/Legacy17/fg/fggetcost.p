DEF INPUT PARAMETER ip-op-ordl-rowid AS ROWID.
DEF INPUT PARAMETER ip-cost-uom AS CHAR.
def INPUT PARAMETER ip-bwt like po-ordl.s-len no-undo.
def INPUT PARAMETER v-len like po-ordl.s-len no-undo.
def INPUT PARAMETER v-wid like po-ordl.s-len no-undo.
def INPUT PARAMETER v-dep like po-ordl.s-len no-undo. 
DEF INPUT PARAMETER ip-out-ea AS DEC NO-UNDO.
DEF INPUT PARAMETER ip-fg-uom-list AS CHAR NO-UNDO.

DEF OUTPUT PARAMETER lv-out-cost AS DEC NO-UNDO.

DEF VAR v-cost-per-ea AS DEC NO-UNDO.
DEF VAR v-cost-setup AS DEC NO-UNDO.
DEF VAR v-corr AS LOG NO-UNDO.
DEF VAR v-basis-w LIKE ITEM.basis-w.
DEF VAR v-tot-msf AS DEC NO-UNDO.
DEF VAR v-qty-per-msf AS DEC NO-UNDO.
DEF VAR v-tot-cost AS DEC NO-UNDO.
DEF VAR v-cost-with-setup AS DEC NO-UNDO.

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = g_company
       locode = g_loc.

 FIND po-ordl WHERE ROWID(po-ordl) = ip-op-ordl-rowid NO-LOCK NO-ERROR.
 IF NOT AVAIL po-ordl THEN
     RETURN.

 
   /* ADD IN SET UP TO COST */
   RUN rm/convcuom.p(ip-cost-uom, 'EA',                   
                      ip-bwt, v-len, v-wid, v-dep,
                    lv-out-cost, OUTPUT v-cost-per-ea).

   v-cost-setup = po-ordl.setup / ip-out-ea.
   FIND FIRST job
        WHERE job.company EQ po-ordl.company
          AND job.job-no  EQ po-ordl.job-no
          AND job.job-no2 EQ INT(po-ordl.job-no2)
        NO-LOCK NO-ERROR.
   IF AVAIL job THEN DO:
        FIND FIRST job-hdr WHERE
                job-hdr.company EQ job.company AND
                job-hdr.job-no EQ job.job-no AND
                job-hdr.job-no2 EQ job.job-no2
              NO-LOCK NO-ERROR.
       
     IF AVAIL job-hdr THEN
     DO:
         FIND FIRST est WHERE
             est.company EQ job-hdr.company AND
             est.est-no EQ job-hdr.est-no
            NO-LOCK NO-ERROR.
       
         IF AVAIL est AND est.est-type GT 4 THEN
             v-corr = YES.
     END.
   END.



   FIND item WHERE item.company = g_company AND
                     item.i-no = po-ordl.i-no NO-LOCK NO-ERROR.

   IF AVAIL ITEM THEN
     ASSIGN v-basis-w = item.basis-w.

   IF po-ordl.pr-qty-uom{2} EQ "EA"       OR
     (NOT po-ordl.item-type AND
      LOOKUP(po-ordl.pr-qty-uom,ip-fg-uom-list) GT 0) THEN
     v-tot-msf = IF v-corr THEN ((v-len * v-wid * .007 * dec(ip-out-ea)) / 1000)
                           ELSE ((((v-len * v-wid) / 144) * dec(ip-out-ea)) / 1000).
   else do:
     /*convert whatever the UOM is into "EACH" first*/
     v-tot-msf = 0.
     if po-ordl.pr-qty-uom{2} NE "EA" then do:
        v-tot-msf = 0.

        /*now convert from "EACH" into MSF*/   
        v-tot-msf = if v-corr THEN
                       ((v-len * v-wid * .007 * ip-out-ea) / 1000)
                    else
                       ((((v-len * v-wid) / 144) * ip-out-ea) / 1000).
          IF po-ordl.pr-qty-uom{2} EQ "ROLL" THEN
             v-tot-msf = v-tot-msf * (12 / v-len).
     end. 
   end.

   v-qty-per-msf = v-tot-msf.
   v-tot-cost = po-ordl.cost * v-qty-per-msf.
   v-cost-per-ea = v-tot-cost / ip-out-ea.

   /* if have already added in a setup charge in prior call, take it out */
   v-cost-with-setup = v-cost-per-ea + v-cost-setup.

  /* convert back from 'ea' to original uom */
  RUN rm/convcuom.p("ea", ip-cost-uom,                   
                    ip-bwt, v-len, v-wid, v-dep,
                    v-cost-with-setup, OUTPUT lv-out-cost).
 
