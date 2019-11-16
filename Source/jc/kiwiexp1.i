
  def var v-exp as log.
  DEF VAR v-n-out AS INT NO-UNDO.

  
  v-exp = no.
  
  output to value(v-out + v-slash + "HOSTKIWI") append.
  
  if job.stat eq "X" then do:
    v-kiwi = "".
     
    /* Add Kiwi TRAN_REQ */
    v-kiwi = "DEL  ".

    /* Add Kiwi TRAN_ANS */
    v-kiwi = v-kiwi + "     ".
  
    /* Add Kiwi co_pokey and ASI Job# */
    v-kiwi = v-kiwi + string(trim(job.job-no) + "-" +
                             string(job.job-no2,"99"),"x(10)").
                               
    put v-kiwi format "x(318)" skip.
  end.

  else
  for first est
      where est.company   eq job.company
        and est.est-no    eq job.est-no
        and (est.est-type eq 5 or
             est.est-type eq 6)
      no-lock,
      
      first job-hdr
      where job-hdr.company eq cocode
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
      no-lock,
      
      first cust
      where cust.company eq cocode
        and cust.cust-no eq job-hdr.cust-no
      no-lock
      
      transaction:
    
    find first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq job-hdr.ord-no
        no-lock no-error.
        
    if avail oe-ord then
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq job-hdr.ord-no
          and oe-ordl.i-no    eq job-hdr.i-no
          and oe-ordl.job-no  eq job-hdr.job-no
          and oe-ordl.job-no2 eq job-hdr.job-no2
        no-lock no-error.
        
    for each job-mch
        where job-mch.company eq cocode
          and job-mch.job     eq job.job
          and job-mch.job-no  eq job.job-no
          and job-mch.job-no2 eq job.job-no2
          and job-mch.dept    eq "CR"
        no-lock,
        
        first mach
        where mach.company eq cocode
          and mach.m-code  eq job-mch.m-code
        no-lock,
        
        first ef
        where ef.e-num   eq est.e-num
          and ef.form-no eq job-mch.frm
        no-lock,

        first eb
        where eb.e-num   eq ef.e-num
          and eb.form-no eq ef.form-no
        no-lock
        
        break by job-mch.frm:
        
      if last-of(job-mch.frm) then do:
        v-kiwi = "".
      
        /* Add Kiwi TRAN_REQ */
        v-kiwi = if job.exported then "CHANG"
                 else "ADD  ".

        /* Add Kiwi TRAN_ANS */
        v-kiwi = v-kiwi + "     ".
  
        /* Add Kiwi co_pokey and ASI Job# */
        v-kiwi = v-kiwi + string(trim(job.job-no) + "-" +
                                 string(job.job-no2,"99"),"x(10)").
  
        /* Add Kiwi co_cust_alpha and ASI Cust Name */
        v-kiwi = v-kiwi + string(cust.name,"x(10)").

        /* Add Kiwi co_unit_desc */
        v-kiwi = v-kiwi + "   ".

        /* Add Kiwi co_due_date and ASI Order Due Date */
        if avail oe-ord then   
          v-kiwi = v-kiwi + substr(string(oe-ord.due-date,"99/99/99"),7,2) +
                            substr(string(oe-ord.due-date,"99/99/99"),1,2) +
                            substr(string(oe-ord.due-date,"99/99/99"),4,2).
                        
        else
          v-kiwi = v-kiwi + substr(string(job.start-date,"99/99/99"),7,2) +
                            substr(string(job.start-date,"99/99/99"),1,2) +
                            substr(string(job.start-date,"99/99/99"),4,2).
                        
        /* Add Kiwi LEADTIME and ASI Warehouse Days */
        v-kiwi = v-kiwi +
                 if cust.ship-days gt 0 then string(cust.ship-days,"9999")
                 else "****".

        /* Add Kiwi co_order_qty and ASI Order Line Qty * qty/set */
        assign
         v-dec  = (if avail oe-ordl then oe-ordl.qty else job-hdr.qty) *
                  eb.yld-qty
         v-kiwi = v-kiwi + string(v-dec,"99999999").
    
        /* Add Kiwi co_target_qty and ASI Job QTY */
        assign
         v-dec  = job-hdr.qty * eb.yld-qty
         v-kiwi = v-kiwi + string(v-dec,"99999999").
    
        /* Add Kiwi co_pct_over and ASI Overrun */
        assign
         v-dec  = if avail oe-ordl then oe-ordl.over-pct else
                  if avail oe-ord  then oe-ord.over-pct  else 5
         v-kiwi = v-kiwi + string(v-dec,"999").
    
        /* Add Kiwi co_pct_under and ASI Underrun */
        assign
         v-dec  = if avail oe-ordl then oe-ordl.under-pct else
                  if avail oe-ord  then oe-ord.under-pct  else 5
         v-kiwi = v-kiwi + string(v-dec,"999").
   
        /* Add Kiwi co_stack_height and ASI Order Line Count */
        assign
         v-dec  = if avail oe-ordl then oe-ordl.cas-cnt else 0
         v-kiwi = v-kiwi + string(v-dec,"999999").

        /* Add Kiwi spare */
        v-kiwi = v-kiwi + "  ".

        /* Add Kiwi co_stack_count */
        v-kiwi = v-kiwi + "01".

        /* Add Kiwi co_pospec_no and ASI Estimate Number */
        v-kiwi = v-kiwi + string(trim(job.est-no),"x(10)").
     
        /* Add Kiwi co_length and ASI Blank Length in total 16ths */
        assign
         v-dec  = ef.gsh-len * 16
         v-kiwi = v-kiwi + string(v-dec,"99999").
     
        /* Add Kiwi co_width and ASI Blank Width */
        assign
         v-dec  = ef.gsh-wid * 16
         v-kiwi = v-kiwi + string(v-dec,"99999").
     
        /* Add Kiwi co_creases and ASI Scores */
        do i = 1 to 16:
          assign
           v-dec  = if ef.xgrain eq "N" then eb.k-wid-array2[i]
                                        else eb.k-len-array2[i]
           v-kiwi = v-kiwi + string(v-dec * 16,"9999").
        end.

        /* Add Kiwi co_reduction */
        v-kiwi = v-kiwi + "**".

        /* Add Kiwi co_max_splits */
        v-kiwi = v-kiwi + "00".

        /* Add Kiwi co_run_rate */
        v-kiwi = v-kiwi + "1".

        /* Add Kiwi co_destination */
        v-kiwi = v-kiwi + " ".

        /* Add Kiwi CMBFLAG */
        v-kiwi = v-kiwi + " ".

        /* Add Kiwi co_combi_trim */
        v-kiwi = v-kiwi + "   ".

        /* Add Kiwi co_number_up */
        v-kiwi = v-kiwi + "   ".

        /* Add Kiwi Reserved */
        v-kiwi = v-kiwi + fill(" ",60).

        /* Add Kiwi max_out_knife 1 */
        v-kiwi = v-kiwi + "  ".

        /* Add Kiwi max_out_knife 2 */
        v-kiwi = v-kiwi + "  ".

        /* Add Kiwi max_out_knife 3 */
        v-kiwi = v-kiwi + "  ".

        /* Add Kiwi co_allow_rotate */
        v-kiwi = v-kiwi + "N".

        /* Add Kiwi co_rotate_partial */
        v-kiwi = v-kiwi + "N".

        /* Add Kiwi co_status and ASI always 1 */
        v-kiwi = v-kiwi + (if v-kiwi begins "ADD" then "1" else " ").

        /* Add Kiwi co_unprog_qty and ASI Job Qty */
        assign
         v-dec  = job-hdr.qty * eb.yld-qty
         v-kiwi = v-kiwi + string(v-dec,"99999999").

        /* Add Kiwi co_denominator and ASI Job Material Up */
        run sys/inc/numup.p (est.e-num, ef.form-no, output v-int).
        RUN est/ef-#out.p (ROWID(ef), OUTPUT v-n-out).
        assign
         v-dec  = v-int * v-n-out
         v-kiwi = v-kiwi + string(v-dec,"999").

        /* Add Kiwi co_numerator */
        v-kiwi = v-kiwi + "001".

        /* Add Kiwi co_corrugator and ASI CR Machine Sequence */
        assign
         v-dec  = if mach.d-seq le 9 then mach.d-seq else 9
         v-kiwi = v-kiwi + string(v-dec,"9").

        /* Add Kiwi DATETIME and ASI Current Date and Time */
        v-kiwi = v-kiwi + substr(string(today,"99/99/99"),7,2) +
                          substr(string(today,"99/99/99"),1,2) +
                          substr(string(today,"99/99/99"),4,2) +
                          substr(string(time, "HH:MM:SS"),1,2) +
                          substr(string(time, "HH:MM:SS"),4,2) +
                          substr(string(time, "HH:MM:SS"),7,2).

        /* Add Kiwi co_board_type and ASI Board Code */
        v-brdkey = trim(string(ef.board)). 
        do i = 1 to 8:
          find first item-bom
              where item-bom.company  eq cocode 
                and item-bom.parent-i eq ef.board 
                and item-bom.line#    eq i
              no-lock no-error.
          if avail item-bom then
            v-brdkey = v-brdkey + trim(string(item-bom.i-no)).
        end.
        v-kiwi = v-kiwi + string(v-brdkey,"x(30)").

        /* Add Kiwi co_crease_type */
        v-kiwi = v-kiwi + fill(" ",15).

        /* Add Kiwi co_plant_code */
        v-kiwi = v-kiwi + "1".

        /* Add Kiwi co_upg_level */
        v-kiwi = v-kiwi + "0".

        /* Add Kiwi co_dwn_level */
        v-kiwi = v-kiwi + "0".

        /* Add Kiwi co_max_grades */
        v-kiwi = v-kiwi + "0".

        /* Add Kiwi co_discharge */
        v-kiwi = v-kiwi + " ".

        put v-kiwi format "x(318)" skip.
        
        v-exp = yes.
      end.
    end.
    
    for each job-mch
        where job-mch.company eq cocode
          and job-mch.job     eq job.job
          and job-mch.job-no  eq job.job-no
          and job-mch.job-no2 eq job.job-no2
          and job-mch.dept    ne "CR"
        no-lock,
        
        first mach
        where mach.company eq cocode
          and mach.m-code  eq job-mch.m-code
        no-lock,
        
        first dept where dept.code eq job-mch.dept no-lock,

        first ef
        where ef.e-num   eq est.e-num
          and ef.form-no eq job-mch.frm
        no-lock,

        first eb
        where eb.e-num   eq ef.e-num
          and eb.form-no eq ef.form-no
        no-lock
        
        break by job-mch.frm
              by job-mch.line:
        
      if first-of(job-mch.frm) then do:
        assign
         v-seq  = 0
         v-kiwi = "".
      
        /* Add Kiwi TRANS REQ */
        v-kiwi = string(job.exported,"CHOD1/ADOD1").
     
        /* Add Kiwi TRANS ANS */
        v-kiwi = v-kiwi + "     ".
      
        /* Add Kiwi job_job_no and ASI Job# */
        v-kiwi = v-kiwi + string(trim(job.job-no) + "-" +
                                 string(job.job-no2,"99"),"x(10)").
                               
        /* Add Kiwi job_spec_no and ASI Est# */
        v-kiwi = v-kiwi + string(trim(est.est-no),"x(10)").
      
        /* Add Kiwi job_cust_ord and ASI Ord# */
        v-kiwi = v-kiwi +
                 string(trim(string(job-hdr.ord-no,"99999999")),"x(25)").
      
        /* Add Kiwi job_cust_con and ASI Cust# */
        v-kiwi = v-kiwi + string(cust.name,"x(10)").
      
        /* Add Kiwi job_cust_name and ASI Cust Name */
        v-kiwi = v-kiwi + string(cust.cust-no,"x(10)").
      
        /* Add Kiwi job_ord_quan and ASI Order Line Qty * qty/set */
        assign
         v-dec  = if avail oe-ordl then oe-ordl.qty else job-hdr.qty
         v-kiwi = v-kiwi + string(v-dec,"99999999").
         
        /* Add Kiwi job_brd_len and ASI Gross Sheet Length */
        v-kiwi = v-kiwi + string(ef.gsh-len * 16,"9999").

        /* Add Kiwi job_brd_wid and ASI Gross Sheet Width */
        v-kiwi = v-kiwi + string(ef.gsh-wid * 16,"9999").
        
        /* Add Kiwi job_fin_len, job_fin_wid, job_scores and
               ASI Blank Length/Width & Width Scores */
        if ef.xgrain eq "N" then do:
          assign
           v-kiwi = v-kiwi + string(eb.k-len * 16,"9999")
           v-kiwi = v-kiwi + string(eb.k-wid * 16,"9999").
           
          do i = 1 to 20:
            v-kiwi = v-kiwi + string(eb.k-wid-array2[i] * 16,"9999").
          end.
        end.
        
        else do:
          assign
           v-kiwi = v-kiwi + string(eb.k-wid * 16,"9999")
           v-kiwi = v-kiwi + string(eb.k-len * 16,"9999").
           
          do i = 1 to 20:
            v-kiwi = v-kiwi + string(eb.k-len-array2[i] * 16,"9999").
          end.
        end.
      
        v-kiwi = v-kiwi + fill("0",20).
        
        /* Add Kiwi job_scor_typ */
        v-kiwi = v-kiwi + fill("N",24).
        
        /* Add Kiwi job_glu_flap */
        v-kiwi = v-kiwi + " ".
        
        /* Add Kiwi job_num_steps */
        v-dec = 0.
        for each b-job-mch
            where b-job-mch.company eq cocode
              and b-job-mch.job     eq job-mch.job
              and b-job-mch.job-no  eq job-mch.job-no
              and b-job-mch.job-no2 eq job-mch.job-no2
              and b-job-mch.frm     eq job-mch.frm
              and b-job-mch.dept    ne "CR"
            no-lock:
            
          v-dec = v-dec + 1.
        end.
        
        v-kiwi = v-kiwi + string(v-dec,"99999999").
        
        /* Add Kiwi job_long_short */
        v-kiwi = v-kiwi + " ".
        
        /* Add Kiwi job_sl_dp1_cor and ASI First Panel Size along  Corrugation
                    job_sl_dp2_cor and ASI Last  Panel Size along  Corrugation
                    job_sl_dp1_acr and ASI First Panel Size across Corrugation
                    job_sl_dp2_acr and ASI Last  Panel Size across Corrugation*/
        v-dim = 0.
        do i = 1 to 20:
          if eb.k-wid-array[i] ne 0 then do:
            if v-dim[1] eq 0 then v-dim[1] = eb.k-wid-array2[i].
            v-dim[2] = eb.k-wid-array2[i].
          end.
          
          if eb.k-len-array[i] ne 0 then do:
            if v-dim[3] eq 0 then v-dim[3] = eb.k-len-array2[i].
            v-dim[4] = eb.k-len-array2[i].
          end.
        end.
        
        do i = 1 to 4:
          v-dim[i] = v-dim[i] * 16.
        end.
        
        if ef.xgrain eq "N" then 
          v-kiwi = v-kiwi + string(v-dim[1],"9999") + string(v-dim[2],"9999")
                          + string(v-dim[3],"9999") + string(v-dim[4],"9999").
        else
          v-kiwi = v-kiwi + string(v-dim[3],"9999") + string(v-dim[3],"9999")
                          + string(v-dim[1],"9999") + string(v-dim[2],"9999").         /* Add Kiwi job_manf_diff */
        v-kiwi = v-kiwi + "0".
        
        /* Add Kiwi job_sauer_eq */
        v-kiwi = v-kiwi + " ".
        
        /* Add Kiwi job_clos_det and ASI Joint Mat'l */
        find first item
            where item.company eq cocode
              and item.i-no    eq eb.adhesive
              and eb.adhesive  ne ""
            no-lock no-error.
        v-kiwi = v-kiwi +
                 (if avail item then (item.mat-type + string(eb.tab-in,"I/O"))
                                else "  ").
        
        /* Add Kiwi job_fefco */
        v-kiwi = v-kiwi + "        ".
        
        /* Add Kiwi job_route_stat and Job On Hold */
        v-kiwi = v-kiwi + (if job.stat eq "H" then "A" else "1").
        
        /* Add Kiwi job_rep_init */
        v-kiwi = v-kiwi + "    ".
        
        /* Add Kiwi job_unit_quan and ASI per thousand */
        v-kiwi = v-kiwi + "1000".

        /* Add Kiwi job_sell_pr and ASI Order Selling Price */
        assign
         v-dec  = if avail oe-ordl then
                    (oe-ordl.t-price / oe-ordl.qty * 100000) else 0
         v-kiwi = v-kiwi + string(v-dec,"9999999999").
                 
        /* Add Kiwi job_valuation and ASI Job Cost */
        v-kiwi = v-kiwi + string(if avail oe-ordl then oe-ordl.cost
                                 else job-hdr.std-tot-cost,"9999999999").
        
        /* Add Kiwi job_priority */
        v-kiwi = v-kiwi + (if v-kiwi begins "ADOD1" then "05" else "  ").
        
        /* Add Kiwi job_export_cust */
        v-kiwi = v-kiwi + "N".
        
        /* Add Kiwi std_wast_pct */
        v-kiwi = v-kiwi + "    ".
        
        /* Add Kiwi job_die_num and ASI Die Number */
        v-kiwi = v-kiwi + string(eb.die-no,"x(10)").
        
        /* Add Kiwi job_ster_num and ASI Plate Number */
        v-kiwi = v-kiwi + string(eb.plate-no,"x(10)").
        
        /* Add Kiwi job_prt_qualty */
        v-kiwi = v-kiwi + " ".
        
        /* Add Kiwi job_die_stat or std_status */
        v-kiwi = v-kiwi + "  ".
        
        /* Add Kiwi job_ster_stat or std_status */
        v-kiwi = v-kiwi + "  ".
        
        /* Add Kiwi Stock Order Flag */
        v-kiwi = v-kiwi + "0".
        
        put v-kiwi format "x(318)" skip.
        
        v-kiwi = "".
      
        /* Add Kiwi TRANS REQ */
        v-kiwi = string(job.exported,"CHOD2/ADOD2").
     
        /* Add Kiwi TRANS ANS */
        v-kiwi = v-kiwi + "     ".
      
        /* Add Kiwi job_job_no and ASI Job# */
        v-kiwi = v-kiwi + string(trim(job.job-no) + "-" +
                                 string(job.job-no2,"99"),"x(10)").
                               
        /* Add Kiwi job_spec_no and ASI Est# */
        v-kiwi = v-kiwi + string(trim(est.est-no),"x(10)").
        
        /* Add Kiwi job_ink_codes and ASI Inks */
        do i = 1 to 8:
          v-kiwi = v-kiwi + string(trim(eb.i-code[i]),"x(6)").
        end.
        
        /* Add Kiwi job_address_no and ASI Ship no */
        v-kiwi = v-kiwi + string(eb.ship-no,"999").
        
        /* Add Kiwi job_ul_type */
        v-kiwi = v-kiwi + "  ".
        
        /* Add Kiwi job_dep_asm_tm */
        v-kiwi = v-kiwi + "    ".
        
        /* Add Kiwi job_prf_del_tm */
        v-kiwi = v-kiwi + "    ".
        
        /* Add Kiwi job_del_tm_al */
        v-kiwi = v-kiwi + "    ".
        
        /* Add Kiwi job_del_dist */
        v-kiwi = v-kiwi + "    ".
        
        /* Add Kiwi job_num_p_set and ASI Qty/Set */
        v-kiwi = v-kiwi + string(eb.yld-qty,"9999").
        
        /* Add Kiwi job_part_no and ASI Form No */
        v-kiwi = v-kiwi + string(ef.form-no,"99").
        
        /* Add Kiwi job_num_parts and ASI Form Qty */
        v-kiwi = v-kiwi + string(est.form-qty,"99").
        
        /* Add Kiwi job_ac_scores and ASI Length Scores */
        do i = 1 to 10:
          assign
           v-dec = if ef.xgrain eq "N" then eb.k-len-array2[i]
                                       else eb.k-wid-array2[i]
           v-kiwi = v-kiwi + string(v-dec * 16,"9999").
        end.
        
        /* Add Kiwi job_ac_scr_typ */
        v-kiwi = v-kiwi + fill("N",9).
        
        /* Add Kiwi job_under_pc and ASI Underrun */
        assign
         v-dec  = if avail oe-ordl then oe-ordl.under-pct else
                  if avail oe-ord  then oe-ord.under-pct  else 5
         v-kiwi = v-kiwi + string(v-dec,"999").
         
        /* Add Kiwi job_over_pc and ASI Overrun */
        assign
         v-dec  = if avail oe-ordl then oe-ordl.over-pct else
                  if avail oe-ord  then oe-ord.over-pct  else 5
         v-kiwi = v-kiwi + string(v-dec,"999").
    
        /* Add Kiwi job_del_dest */
        v-kiwi = v-kiwi + "    ".
        
        /* Add Kiwi job_int_len, wid, dep */
        v-kiwi = v-kiwi + fill(" ",12).
        
        /* Add Kiwi job_extra_joint_code */
        v-kiwi = v-kiwi + "  ".
        
        /* Add Kiwi (spare) */
        v-kiwi = v-kiwi + "  ".
        
        /* Add Kiwi job_due_date */
        if avail oe-ord then   
          v-kiwi = v-kiwi + substr(string(oe-ord.due-date,"99/99/99"),7,2) +
                            substr(string(oe-ord.due-date,"99/99/99"),1,2) +
                            substr(string(oe-ord.due-date,"99/99/99"),4,2).
                        
        else
          v-kiwi = v-kiwi + substr(string(job.start-date,"99/99/99"),7,2) +
                            substr(string(job.start-date,"99/99/99"),1,2) +
                            substr(string(job.start-date,"99/99/99"),4,2).
                            
        /* Add Kiwi job_due_tm */
        v-kiwi = v-kiwi + "    ".
        
        /* Add Kiwi job_brd_rec and ASI Board Code */
        v-brdkey = trim(string(ef.board)). 
        do i = 1 to 8:
          find first item-bom
              where item-bom.company  eq cocode 
                and item-bom.parent-i eq ef.board 
                and item-bom.line#    eq i
              no-lock no-error.
          if avail item-bom then
            v-brdkey = v-brdkey + trim(string(item-bom.i-no)).
        end.
        v-kiwi = v-kiwi + string(v-brdkey,"x(30)").
        
        /* Add Kiwi job_ac_glu_flp */
        v-kiwi = v-kiwi + " ".
        
        /* Add Kiwi job_job_desc and ASI Job & Part No's */
        v-kiwi = v-kiwi + string("Job " + trim(job.job-no) + "-" +
                                          string(job.job-no2,"99") +
                                 " For " + trim(eb.part-no),"x(30)").
        
        /* Add Kiwi job_prod_desc and ASI Part Descriptions */
        v-kiwi = v-kiwi + string(trim(eb.part-dscr1) + " " +
                                 trim(eb.part-dscr2),"x(30)").
                                 
        /* Add Kiwi job_early_dt */
        v-kiwi = v-kiwi + "      ".
        
        /* Add Kiwi job_early_tm */
        v-kiwi = v-kiwi + "    ".
        
        /* Add Kiwi job_slot_type */
        v-kiwi = v-kiwi + " ".
        
        /* Add Kiwi job_sales_clerk */
        v-kiwi = v-kiwi + "    ".
        
        /* Add Kiwi job_tgt_over */
        v-kiwi = v-kiwi + "  ".
        
        /* Add Kiwi job_truck_unit_load */
        v-kiwi = v-kiwi + "        ".
        
        /* Add Kiwi (spare) */
        v-kiwi = v-kiwi + fill(" ",17).
        
        /* Add Kiwi Stock Order Flag */
        v-kiwi = v-kiwi + "0".
                                 
        put v-kiwi format "x(318)" skip.
        
        v-exp = yes.
      end.
      
      assign
       v-seq  = v-seq + 1
       v-kiwi = "".
      
      /* Add Kiwi TRANS REQ */
      v-kiwi = string(job.exported,"CHPCS/ADPCS").
     
      /* Add Kiwi TRANS ANS */
      v-kiwi = v-kiwi + "     ".
      
      /* Add Kiwi wip_job_no and ASI Job# */
      v-kiwi = v-kiwi + string(trim(job.job-no) + "-" +
                               string(job.job-no2,"99"),"x(10)").
                               
      /* Add Kiwi wip_repeat_num */
      v-kiwi = v-kiwi + "1".
      
      /* Add Kiwi (spare) */
      v-kiwi = v-kiwi + "       ".
      
      /* Add Kiwi wip_postn and ASI Machine Seq */
      v-kiwi = v-kiwi + string(v-seq,"99").
      
      /* Add Kiwi wip_mc_num and ASI Machine Code */
      v-kiwi = v-kiwi + string(job-mch.m-code,"x(4)").
      
      /* Add Kiwi wip_optn_inc */
      v-kiwi = v-kiwi + " ".
      
      /* Add Kiwi wip_entry_l & w and ASI Gross, Net, or Blank Length & Width
                  wip_entry_num(1) & (2) and ASI Number On/Out/Up
                  wip_exit_l & w  and ASI Gross, Net, or Blank Length & Width
                  wip_exit_num(1) & (2)  and ASI Number On/Out/Up */
      run sys/inc/numup.p (est.e-num, ef.form-no, output v-int).
                  
      v-dim = 0.
      if dept.fc gt v-dc-seq then do:
        if ef.xgrain eq "N" then
          assign
           v-dim[1] = eb.k-len
           v-dim[2] = eb.k-wid.
        else
          assign
           v-dim[1] = eb.k-wid
           v-dim[2] = eb.k-len.
           
        assign
         v-dim[3] = v-dim[1]
         v-dim[4] = v-dim[2]
         v-up[1]  = 1
         v-up[2]  = v-up[1].
      end.
        
      else
      if dept.fc eq v-dc-seq then do:
        assign
         v-dim[1] = ef.nsh-len
         v-dim[2] = ef.nsh-wid
         v-up[1]  = v-int
         v-up[2]  = 1.
         
        if ef.xgrain eq "N" then
          assign
           v-dim[3] = eb.k-len
           v-dim[4] = eb.k-wid.
        else
          assign
           v-dim[3] = eb.k-wid
           v-dim[4] = eb.k-len.
      end.
      
      else
      if dept.fc gt v-rc-seq then
        assign
         v-dim[1] = ef.nsh-len
         v-dim[2] = ef.nsh-wid
         v-dim[3] = v-dim[1]
         v-dim[4] = v-dim[2]
         v-up[1]  = v-int
         v-up[2]  = v-up[1].
         
      else
      if dept.fc eq v-rc-seq then
        assign
         v-dim[1] = ef.gsh-len
         v-dim[2] = ef.gsh-wid
         v-dim[3] = ef.nsh-len
         v-dim[4] = ef.nsh-wid
         v-up[1]  = v-int * v-n-out
         v-up[2]  = v-int.
         
      else
        assign
         v-dim[1] = ef.gsh-len
         v-dim[2] = ef.gsh-wid
         v-dim[3] = v-dim[1]
         v-dim[4] = v-dim[2]
         v-up[1]  = v-int * v-n-out
         v-up[2]  = v-up[1].
                        
      assign
       v-kiwi = v-kiwi + string(v-dim[1],"9999")
       v-kiwi = v-kiwi + string(v-dim[2],"9999")
       v-kiwi = v-kiwi + "001"
       v-kiwi = v-kiwi + string(v-up[1],"999")
       
       v-kiwi = v-kiwi + string(v-dim[3],"9999")
       v-kiwi = v-kiwi + string(v-dim[4],"9999")
       v-kiwi = v-kiwi + "001"
       v-kiwi = v-kiwi + string(v-up[2],"999").
       
      /* Add Kiwi wip_op_wst */
      v-kiwi = v-kiwi + "        ".
      
      /* Add Kiwi wip_pre_wait */
      v-kiwi = v-kiwi + "    ".
      
      /* Add Kiwi wip_std_setup and ASI Make Ready Time */
      v-kiwi = v-kiwi + string(job-mch.mr-hr * 60,"9999").
      
      /* Add Kiwi wip_sup_wst and ASI Make Ready Sheets/Blanks */
      v-kiwi = v-kiwi + string(job-mch.mr-waste,"9999").
      
      /* Add Kiwi (spare) */
      v-kiwi = v-kiwi + "    ".
      
      /* Add Kiwi wip_run_waste and ASI Run Waste Pct */
      assign
       v-dec  = job-mch.run-qty - job-mch.mr-waste
       v-int  = round(v-dec * job-mch.wst-prct / 100,0)
       v-dec  = v-int / (v-dec / 1000)
       v-kiwi = v-kiwi + string(v-dec,"9999").
      
      /* Add Kiwi wip_num_crew */
      v-kiwi = v-kiwi + "**".
      
      /* Add Kiwi wip_post_wait */
      v-kiwi = v-kiwi + "****".
      
      /* Add Kiwi wip_join_job */
      v-kiwi = v-kiwi + "          ".
      
      /* Add Kiwi wip_sep_job */
      v-kiwi = v-kiwi + "          ".
      
      /* Add Kiwi wip_run_spd and ASI Run Speed */
      v-kiwi = v-kiwi + string(if job-mch.run-hr ne 0 then
                               (job-mch.run-qty / job-mch.run-hr) else 0,
                               "999999").
      
      /* Add Kiwi Closure Codes */
      v-kiwi = v-kiwi + "    ".
      
      /* Add Kiwi Pre/Post Wait Time Units */
      v-kiwi = v-kiwi + "M".
      
      /* Add Kiwi wip_no_inks */
      v-kiwi = v-kiwi + " ".
      
      /* Add Kiwi wip_pct_ink_cov */
      v-kiwi = v-kiwi + "   ".
      
      /* Add Kiwi wip_rot_entry */
      v-kiwi = v-kiwi + " ".
      
      /* Add Kiwi wip_rot_exit */
      v-kiwi = v-kiwi + " ".
      
      /* Add Kiwi wip_hole_type */
      v-kiwi = v-kiwi + " ".
      
      /* Add Kiwi wip_alt_mch */
      v-kiwi = v-kiwi + fill(" ",16).
      
      /* Add Kiwi (reserved) */
      v-kiwi = v-kiwi + fill(" ",16).
      
      /* Add Kiwi (spare) */
      v-kiwi = v-kiwi + fill(" ",6).
      
      /* Add Kiwi Step2 */
      v-kiwi = v-kiwi + fill(" ",145).
      
      put v-kiwi format "x(318)" skip.
      
      v-exp = yes.
    end.
    
    if v-exp then job.exported = yes.
  end.
  
  output close.
    
