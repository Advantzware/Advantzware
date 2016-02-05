/* ---------------------------------------------------- ce/u1kinc1.p 6/92 cd  */
/*                                                                            */
/* calculate each of the 12 formulas in style file!                           */
/*                                                                            */
/* -------------------------------------------------------------------------- */
/*
{sys/inc/var.i shared}
{sys/form/s-top.f}
def shared var save_id as recid.
def shared var chosen as logical format "y/n".
def shared var head as ch format "x(78)" extent 20.

	Last change:  YSK  20 Feb 2001    7:20 pm
*/

def input parameter ip-rfq-id as recid .
def input parameter ip-item-id as recid.
def shared temp-table formule field formule as dec extent 12 .
def buffer xrfq for rfq.
def buffer xitem for rfqitem.

def var mess as ch format "x(80)" extent 2.
def var op as ch extent 100.
def var nextop as int.
def var num as de extent 100.
def var curnum as ch.
def var kar as ch format "x".  /* style formula kalk variables */
def var tmpstore as cha no-undo.
def var i as int no-undo.
/*
help-id = program-name(1).
*/

blok:
do on error undo:
      find xrfq where recid(xrfq) = ip-rfq-id no-lock.
      find xitem where recid(xitem) = ip-item-id no-lock.
      find first style WHERE style.company = xrfq.company and style.style = xitem.style
      no-lock no-error.
      find first formule no-error.
      if not avail formule then create formule.
      tmpstore = style.formula[1].
         {sys/inc/kstyle.i &for=1 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-wid &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock }

      tmpstore = style.formula[2].
         {sys/inc/kstyle.i &for=2 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-len &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock }

      tmpstore = style.formula[3].
         {sys/inc/kstyle.i &for=3 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-wid &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock }

      tmpstore = style.formula[4].
         {sys/inc/kstyle.i &for=4 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-len &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock }

      tmpstore = style.formula[5].
         {sys/inc/kstyle.i &for=5 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-wid &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock }

      tmpstore = style.formula[6].
         {sys/inc/kstyle.i &for=6 &l=xitem.len &w=xitem.wid &d=xitem.dep
            &k=xitem.k-len &t=xitem.tuck &g=xitem.gluelap &b=xitem.fpanel
            &f=xitem.dust  &o=xitem.lock }

end.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
