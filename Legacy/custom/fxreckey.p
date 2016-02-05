run account_rec_key.
procedure account_rec_key:
  for each account exclusive:
    create rec_key.
    assign
      account.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = account.rec_key
      rec_key.table_name = "account".
  end.
end procedure.
run ap-buy_rec_key.
procedure ap-buy_rec_key:
  for each ap-buy exclusive:
    create rec_key.
    assign
      ap-buy.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-buy.rec_key
      rec_key.table_name = "ap-buy".
  end.
end procedure.
run ap-chk_rec_key.
procedure ap-chk_rec_key:
  for each ap-chk exclusive:
    create rec_key.
    assign
      ap-chk.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-chk.rec_key
      rec_key.table_name = "ap-chk".
  end.
end procedure.
run ap-ctrl_rec_key.
procedure ap-ctrl_rec_key:
  for each ap-ctrl exclusive:
    create rec_key.
    assign
      ap-ctrl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-ctrl.rec_key
      rec_key.table_name = "ap-ctrl".
  end.
end procedure.
run ap-dis_rec_key.
procedure ap-dis_rec_key:
  for each ap-dis exclusive:
    create rec_key.
    assign
      ap-dis.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-dis.rec_key
      rec_key.table_name = "ap-dis".
  end.
end procedure.
run ap-disl_rec_key.
procedure ap-disl_rec_key:
  for each ap-disl exclusive:
    create rec_key.
    assign
      ap-disl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-disl.rec_key
      rec_key.table_name = "ap-disl".
  end.
end procedure.
run ap-inv_rec_key.
procedure ap-inv_rec_key:
  for each ap-inv exclusive:
    create rec_key.
    assign
      ap-inv.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-inv.rec_key
      rec_key.table_name = "ap-inv".
  end.
end procedure.
run ap-invl_rec_key.
procedure ap-invl_rec_key:
  for each ap-invl exclusive:
    create rec_key.
    assign
      ap-invl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-invl.rec_key
      rec_key.table_name = "ap-invl".
  end.
end procedure.
run ap-ledger_rec_key.
procedure ap-ledger_rec_key:
  for each ap-ledger exclusive:
    create rec_key.
    assign
      ap-ledger.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-ledger.rec_key
      rec_key.table_name = "ap-ledger".
  end.
end procedure.
run ap-pay_rec_key.
procedure ap-pay_rec_key:
  for each ap-pay exclusive:
    create rec_key.
    assign
      ap-pay.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-pay.rec_key
      rec_key.table_name = "ap-pay".
  end.
end procedure.
run ap-payl_rec_key.
procedure ap-payl_rec_key:
  for each ap-payl exclusive:
    create rec_key.
    assign
      ap-payl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-payl.rec_key
      rec_key.table_name = "ap-payl".
  end.
end procedure.
run ap-sel_rec_key.
procedure ap-sel_rec_key:
  for each ap-sel exclusive:
    create rec_key.
    assign
      ap-sel.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ap-sel.rec_key
      rec_key.table_name = "ap-sel".
  end.
end procedure.
run aphist_rec_key.
procedure aphist_rec_key:
  for each aphist exclusive:
    create rec_key.
    assign
      aphist.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = aphist.rec_key
      rec_key.table_name = "aphist".
  end.
end procedure.
run ar-cash_rec_key.
procedure ar-cash_rec_key:
  for each ar-cash exclusive:
    create rec_key.
    assign
      ar-cash.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ar-cash.rec_key
      rec_key.table_name = "ar-cash".
  end.
end procedure.
run ar-cashl_rec_key.
procedure ar-cashl_rec_key:
  for each ar-cashl exclusive:
    create rec_key.
    assign
      ar-cashl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ar-cashl.rec_key
      rec_key.table_name = "ar-cashl".
  end.
end procedure.
run ar-ctrl_rec_key.
procedure ar-ctrl_rec_key:
  for each ar-ctrl exclusive:
    create rec_key.
    assign
      ar-ctrl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ar-ctrl.rec_key
      rec_key.table_name = "ar-ctrl".
  end.
end procedure.
run ar-inv_rec_key.
procedure ar-inv_rec_key:
  for each ar-inv exclusive:
    create rec_key.
    assign
      ar-inv.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ar-inv.rec_key
      rec_key.table_name = "ar-inv".
  end.
end procedure.
run ar-invl_rec_key.
procedure ar-invl_rec_key:
  for each ar-invl exclusive:
    create rec_key.
    assign
      ar-invl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ar-invl.rec_key
      rec_key.table_name = "ar-invl".
  end.
end procedure.
run ar-ledger_rec_key.
procedure ar-ledger_rec_key:
  for each ar-ledger exclusive:
    create rec_key.
    assign
      ar-ledger.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ar-ledger.rec_key
      rec_key.table_name = "ar-ledger".
  end.
end procedure.
run ar-mcash_rec_key.
procedure ar-mcash_rec_key:
  for each ar-mcash exclusive:
    create rec_key.
    assign
      ar-mcash.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ar-mcash.rec_key
      rec_key.table_name = "ar-mcash".
  end.
end procedure.
run asinotes_rec_key.
procedure asinotes_rec_key:
  for each asinotes exclusive:
    create rec_key.
    assign
      asinotes.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = asinotes.rec_key
      rec_key.table_name = "asinotes".
  end.
end procedure.
run bank_rec_key.
procedure bank_rec_key:
  for each bank exclusive:
    create rec_key.
    assign
      bank.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = bank.rec_key
      rec_key.table_name = "bank".
  end.
end procedure.
run box-design-hdr_rec_key.
procedure box-design-hdr_rec_key:
  for each box-design-hdr exclusive:
    create rec_key.
    assign
      box-design-hdr.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = box-design-hdr.rec_key
      rec_key.table_name = "box-design-hdr".
  end.
end procedure.
run box-design-line_rec_key.
procedure box-design-line_rec_key:
  for each box-design-line exclusive:
    create rec_key.
    assign
      box-design-line.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = box-design-line.rec_key
      rec_key.table_name = "box-design-line".
  end.
end procedure.
run buyer_rec_key.
procedure buyer_rec_key:
  for each buyer exclusive:
    create rec_key.
    assign
      buyer.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = buyer.rec_key
      rec_key.table_name = "buyer".
  end.
end procedure.
run carr-mtx_rec_key.
procedure carr-mtx_rec_key:
  for each carr-mtx exclusive:
    create rec_key.
    assign
      carr-mtx.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = carr-mtx.rec_key
      rec_key.table_name = "carr-mtx".
  end.
end procedure.
run carrier_rec_key.
procedure carrier_rec_key:
  for each carrier exclusive:
    create rec_key.
    assign
      carrier.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = carrier.rec_key
      rec_key.table_name = "carrier".
  end.
end procedure.
run ce-ctrl_rec_key.
procedure ce-ctrl_rec_key:
  for each ce-ctrl exclusive:
    create rec_key.
    assign
      ce-ctrl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ce-ctrl.rec_key
      rec_key.table_name = "ce-ctrl".
  end.
end procedure.
run company_rec_key.
procedure company_rec_key:
  for each company exclusive:
    create rec_key.
    assign
      company.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = company.rec_key
      rec_key.table_name = "company".
  end.
end procedure.
run costtype_rec_key.
procedure costtype_rec_key:
  for each costtype exclusive:
    create rec_key.
    assign
      costtype.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = costtype.rec_key
      rec_key.table_name = "costtype".
  end.
end procedure.
run crew_rec_key.
procedure crew_rec_key:
  for each crew exclusive:
    create rec_key.
    assign
      crew.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = crew.rec_key
      rec_key.table_name = "crew".
  end.
end procedure.
run cust_rec_key.
procedure cust_rec_key:
  for each cust exclusive:
    create rec_key.
    assign
      cust.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = cust.rec_key
      rec_key.table_name = "cust".
  end.
end procedure.
run custype_rec_key.
procedure custype_rec_key:
  for each custype exclusive:
    create rec_key.
    assign
      custype.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = custype.rec_key
      rec_key.table_name = "custype".
  end.
end procedure.
run db-ctrl_rec_key.
procedure db-ctrl_rec_key:
  for each db-ctrl exclusive:
    create rec_key.
    assign
      db-ctrl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = db-ctrl.rec_key
      rec_key.table_name = "db-ctrl".
  end.
end procedure.
run dept_rec_key.
procedure dept_rec_key:
  for each dept exclusive:
    create rec_key.
    assign
      dept.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = dept.rec_key
      rec_key.table_name = "dept".
  end.
end procedure.
run e-item_rec_key.
procedure e-item_rec_key:
  for each e-item exclusive:
    create rec_key.
    assign
      e-item.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = e-item.rec_key
      rec_key.table_name = "e-item".
  end.
end procedure.
run e-item-vend_rec_key.
procedure e-item-vend_rec_key:
  for each e-item-vend exclusive:
    create rec_key.
    assign
      e-item-vend.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = e-item-vend.rec_key
      rec_key.table_name = "e-item-vend".
  end.
end procedure.
run e-itemfg_rec_key.
procedure e-itemfg_rec_key:
  for each e-itemfg exclusive:
    create rec_key.
    assign
      e-itemfg.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = e-itemfg.rec_key
      rec_key.table_name = "e-itemfg".
  end.
end procedure.
run eb_rec_key.
procedure eb_rec_key:
  for each eb exclusive:
    create rec_key.
    assign
      eb.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = eb.rec_key
      rec_key.table_name = "eb".
  end.
end procedure.
run EDAPCheck_rec_key.
procedure EDAPCheck_rec_key:
  for each EDAPCheck exclusive:
    create rec_key.
    assign
      EDAPCheck.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDAPCheck.rec_key
      rec_key.table_name = "EDAPCheck".
  end.
end procedure.
run EDCat_rec_key.
procedure EDCat_rec_key:
  for each EDCat exclusive:
    create rec_key.
    assign
      EDCat.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDCat.rec_key
      rec_key.table_name = "EDCat".
  end.
end procedure.
run EDCatline_rec_key.
procedure EDCatline_rec_key:
  for each EDCatline exclusive:
    create rec_key.
    assign
      EDCatline.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDCatline.rec_key
      rec_key.table_name = "EDCatline".
  end.
end procedure.
run EDCatPrice_rec_key.
procedure EDCatPrice_rec_key:
  for each EDCatPrice exclusive:
    create rec_key.
    assign
      EDCatPrice.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDCatPrice.rec_key
      rec_key.table_name = "EDCatPrice".
  end.
end procedure.
run EDCo_rec_key.
procedure EDCo_rec_key:
  for each EDCo exclusive:
    create rec_key.
    assign
      EDCo.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDCo.rec_key
      rec_key.table_name = "EDCo".
  end.
end procedure.
run EDCode_rec_key.
procedure EDCode_rec_key:
  for each EDCode exclusive:
    create rec_key.
    assign
      EDCode.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDCode.rec_key
      rec_key.table_name = "EDCode".
  end.
end procedure.
run EDDoc_rec_key.
procedure EDDoc_rec_key:
  for each EDDoc exclusive:
    create rec_key.
    assign
      EDDoc.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDDoc.rec_key
      rec_key.table_name = "EDDoc".
  end.
end procedure.
run EDICXref_rec_key.
procedure EDICXref_rec_key:
  for each EDICXref exclusive:
    create rec_key.
    assign
      EDICXref.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDICXref.rec_key
      rec_key.table_name = "EDICXref".
  end.
end procedure.
run EDIVAddon_rec_key.
procedure EDIVAddon_rec_key:
  for each EDIVAddon exclusive:
    create rec_key.
    assign
      EDIVAddon.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDIVAddon.rec_key
      rec_key.table_name = "EDIVAddon".
  end.
end procedure.
run EDIVLine_rec_key.
procedure EDIVLine_rec_key:
  for each EDIVLine exclusive:
    create rec_key.
    assign
      EDIVLine.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDIVLine.rec_key
      rec_key.table_name = "EDIVLine".
  end.
end procedure.
run EDIVTran_rec_key.
procedure EDIVTran_rec_key:
  for each EDIVTran exclusive:
    create rec_key.
    assign
      EDIVTran.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDIVTran.rec_key
      rec_key.table_name = "EDIVTran".
  end.
end procedure.
run EDMast_rec_key.
procedure EDMast_rec_key:
  for each EDMast exclusive:
    create rec_key.
    assign
      EDMast.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDMast.rec_key
      rec_key.table_name = "EDMast".
  end.
end procedure.
run EDPD_rec_key.
procedure EDPD_rec_key:
  for each EDPD exclusive:
    create rec_key.
    assign
      EDPD.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDPD.rec_key
      rec_key.table_name = "EDPD".
  end.
end procedure.
run EDPOAddon_rec_key.
procedure EDPOAddon_rec_key:
  for each EDPOAddon exclusive:
    create rec_key.
    assign
      EDPOAddon.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDPOAddon.rec_key
      rec_key.table_name = "EDPOAddon".
  end.
end procedure.
run EDPOLine_rec_key.
procedure EDPOLine_rec_key:
  for each EDPOLine exclusive:
    create rec_key.
    assign
      EDPOLine.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDPOLine.rec_key
      rec_key.table_name = "EDPOLine".
  end.
end procedure.
run EDPOTran_rec_key.
procedure EDPOTran_rec_key:
  for each EDPOTran exclusive:
    create rec_key.
    assign
      EDPOTran.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDPOTran.rec_key
      rec_key.table_name = "EDPOTran".
  end.
end procedure.
run EDShipto_rec_key.
procedure EDShipto_rec_key:
  for each EDShipto exclusive:
    create rec_key.
    assign
      EDShipto.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDShipto.rec_key
      rec_key.table_name = "EDShipto".
  end.
end procedure.
run EDShipVia_rec_key.
procedure EDShipVia_rec_key:
  for each EDShipVia exclusive:
    create rec_key.
    assign
      EDShipVia.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDShipVia.rec_key
      rec_key.table_name = "EDShipVia".
  end.
end procedure.
run EDSHLine_rec_key.
procedure EDSHLine_rec_key:
  for each EDSHLine exclusive:
    create rec_key.
    assign
      EDSHLine.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDSHLine.rec_key
      rec_key.table_name = "EDSHLine".
  end.
end procedure.
run EDSHOrd_rec_key.
procedure EDSHOrd_rec_key:
  for each EDSHOrd exclusive:
    create rec_key.
    assign
      EDSHOrd.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDSHOrd.rec_key
      rec_key.table_name = "EDSHOrd".
  end.
end procedure.
run EDSHPack_rec_key.
procedure EDSHPack_rec_key:
  for each EDSHPack exclusive:
    create rec_key.
    assign
      EDSHPack.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDSHPack.rec_key
      rec_key.table_name = "EDSHPack".
  end.
end procedure.
run EDSHTare_rec_key.
procedure EDSHTare_rec_key:
  for each EDSHTare exclusive:
    create rec_key.
    assign
      EDSHTare.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDSHTare.rec_key
      rec_key.table_name = "EDSHTare".
  end.
end procedure.
run EDSHTran_rec_key.
procedure EDSHTran_rec_key:
  for each EDSHTran exclusive:
    create rec_key.
    assign
      EDSHTran.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = EDSHTran.rec_key
      rec_key.table_name = "EDSHTran".
  end.
end procedure.
run ef_rec_key.
procedure ef_rec_key:
  for each ef exclusive:
    create rec_key.
    assign
      ef.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ef.rec_key
      rec_key.table_name = "ef".
  end.
end procedure.
run emp_rec_key.
procedure emp_rec_key:
  for each emp exclusive:
    create rec_key.
    assign
      emp.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = emp.rec_key
      rec_key.table_name = "emp".
  end.
end procedure.
run est_rec_key.
procedure est_rec_key:
  for each est exclusive:
    create rec_key.
    assign
      est.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = est.rec_key
      rec_key.table_name = "est".
  end.
end procedure.
run est-flm_rec_key.
procedure est-flm_rec_key:
  for each est-flm exclusive:
    create rec_key.
    assign
      est-flm.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = est-flm.rec_key
      rec_key.table_name = "est-flm".
  end.
end procedure.
run est-inst_rec_key.
procedure est-inst_rec_key:
  for each est-inst exclusive:
    create rec_key.
    assign
      est-inst.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = est-inst.rec_key
      rec_key.table_name = "est-inst".
  end.
end procedure.
run est-op_rec_key.
procedure est-op_rec_key:
  for each est-op exclusive:
    create rec_key.
    assign
      est-op.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = est-op.rec_key
      rec_key.table_name = "est-op".
  end.
end procedure.
run est-prep_rec_key.
procedure est-prep_rec_key:
  for each est-prep exclusive:
    create rec_key.
    assign
      est-prep.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = est-prep.rec_key
      rec_key.table_name = "est-prep".
  end.
end procedure.
run est-summ_rec_key.
procedure est-summ_rec_key:
  for each est-summ exclusive:
    create rec_key.
    assign
      est-summ.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = est-summ.rec_key
      rec_key.table_name = "est-summ".
  end.
end procedure.
run expiration_rec_key.
procedure expiration_rec_key:
  for each expiration exclusive:
    create rec_key.
    assign
      expiration.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = expiration.rec_key
      rec_key.table_name = "expiration".
  end.
end procedure.
run fg-act_rec_key.
procedure fg-act_rec_key:
  for each fg-act exclusive:
    create rec_key.
    assign
      fg-act.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fg-act.rec_key
      rec_key.table_name = "fg-act".
  end.
end procedure.
run fg-bin_rec_key.
procedure fg-bin_rec_key:
  for each fg-bin exclusive:
    create rec_key.
    assign
      fg-bin.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fg-bin.rec_key
      rec_key.table_name = "fg-bin".
  end.
end procedure.
run fg-ctrl_rec_key.
procedure fg-ctrl_rec_key:
  for each fg-ctrl exclusive:
    create rec_key.
    assign
      fg-ctrl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fg-ctrl.rec_key
      rec_key.table_name = "fg-ctrl".
  end.
end procedure.
run fg-hist_rec_key.
procedure fg-hist_rec_key:
  for each fg-hist exclusive:
    create rec_key.
    assign
      fg-hist.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fg-hist.rec_key
      rec_key.table_name = "fg-hist".
  end.
end procedure.
run fg-rcpth_rec_key.
procedure fg-rcpth_rec_key:
  for each fg-rcpth exclusive:
    create rec_key.
    assign
      fg-rcpth.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fg-rcpth.rec_key
      rec_key.table_name = "fg-rcpth".
  end.
end procedure.
run fg-rcpts_rec_key.
procedure fg-rcpts_rec_key:
  for each fg-rcpts exclusive:
    create rec_key.
    assign
      fg-rcpts.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fg-rcpts.rec_key
      rec_key.table_name = "fg-rcpts".
  end.
end procedure.
run fg-rdtl_rec_key.
procedure fg-rdtl_rec_key:
  for each fg-rdtl exclusive:
    create rec_key.
    assign
      fg-rdtl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fg-rdtl.rec_key
      rec_key.table_name = "fg-rdtl".
  end.
end procedure.
run fg-rdtlh_rec_key.
procedure fg-rdtlh_rec_key:
  for each fg-rdtlh exclusive:
    create rec_key.
    assign
      fg-rdtlh.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fg-rdtlh.rec_key
      rec_key.table_name = "fg-rdtlh".
  end.
end procedure.
run fg-set_rec_key.
procedure fg-set_rec_key:
  for each fg-set exclusive:
    create rec_key.
    assign
      fg-set.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fg-set.rec_key
      rec_key.table_name = "fg-set".
  end.
end procedure.
run fgcat_rec_key.
procedure fgcat_rec_key:
  for each fgcat exclusive:
    create rec_key.
    assign
      fgcat.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fgcat.rec_key
      rec_key.table_name = "fgcat".
  end.
end procedure.
run file_rec_key.
procedure file_rec_key:
  for each file exclusive:
    create rec_key.
    assign
      file.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = file.rec_key
      rec_key.table_name = "file".
  end.
end procedure.
run fraction_rec_key.
procedure fraction_rec_key:
  for each fraction exclusive:
    create rec_key.
    assign
      fraction.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = fraction.rec_key
      rec_key.table_name = "fraction".
  end.
end procedure.
run gl-ctrl_rec_key.
procedure gl-ctrl_rec_key:
  for each gl-ctrl exclusive:
    create rec_key.
    assign
      gl-ctrl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = gl-ctrl.rec_key
      rec_key.table_name = "gl-ctrl".
  end.
end procedure.
run gl-freq_rec_key.
procedure gl-freq_rec_key:
  for each gl-freq exclusive:
    create rec_key.
    assign
      gl-freq.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = gl-freq.rec_key
      rec_key.table_name = "gl-freq".
  end.
end procedure.
run gl-jrn_rec_key.
procedure gl-jrn_rec_key:
  for each gl-jrn exclusive:
    create rec_key.
    assign
      gl-jrn.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = gl-jrn.rec_key
      rec_key.table_name = "gl-jrn".
  end.
end procedure.
run gl-jrnl_rec_key.
procedure gl-jrnl_rec_key:
  for each gl-jrnl exclusive:
    create rec_key.
    assign
      gl-jrnl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = gl-jrnl.rec_key
      rec_key.table_name = "gl-jrnl".
  end.
end procedure.
run gl-rpt_rec_key.
procedure gl-rpt_rec_key:
  for each gl-rpt exclusive:
    create rec_key.
    assign
      gl-rpt.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = gl-rpt.rec_key
      rec_key.table_name = "gl-rpt".
  end.
end procedure.
run glhist_rec_key.
procedure glhist_rec_key:
  for each glhist exclusive:
    create rec_key.
    assign
      glhist.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = glhist.rec_key
      rec_key.table_name = "glhist".
  end.
end procedure.
run gltrans_rec_key.
procedure gltrans_rec_key:
  for each gltrans exclusive:
    create rec_key.
    assign
      gltrans.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = gltrans.rec_key
      rec_key.table_name = "gltrans".
  end.
end procedure.
run inv-head_rec_key.
procedure inv-head_rec_key:
  for each inv-head exclusive:
    create rec_key.
    assign
      inv-head.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = inv-head.rec_key
      rec_key.table_name = "inv-head".
  end.
end procedure.
run inv-line_rec_key.
procedure inv-line_rec_key:
  for each inv-line exclusive:
    create rec_key.
    assign
      inv-line.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = inv-line.rec_key
      rec_key.table_name = "inv-line".
  end.
end procedure.
run inv-misc_rec_key.
procedure inv-misc_rec_key:
  for each inv-misc exclusive:
    create rec_key.
    assign
      inv-misc.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = inv-misc.rec_key
      rec_key.table_name = "inv-misc".
  end.
end procedure.
run item_rec_key.
procedure item_rec_key:
  for each item exclusive:
    create rec_key.
    assign
      item.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = item.rec_key
      rec_key.table_name = "item".
  end.
end procedure.
run item-bom_rec_key.
procedure item-bom_rec_key:
  for each item-bom exclusive:
    create rec_key.
    assign
      item-bom.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = item-bom.rec_key
      rec_key.table_name = "item-bom".
  end.
end procedure.
run item-spec_rec_key.
procedure item-spec_rec_key:
  for each item-spec exclusive:
    create rec_key.
    assign
      item-spec.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = item-spec.rec_key
      rec_key.table_name = "item-spec".
  end.
end procedure.
run itemfg_rec_key.
procedure itemfg_rec_key:
  for each itemfg exclusive:
    create rec_key.
    assign
      itemfg.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = itemfg.rec_key
      rec_key.table_name = "itemfg".
  end.
end procedure.
run itemfgdtl_rec_key.
procedure itemfgdtl_rec_key:
  for each itemfgdtl exclusive:
    create rec_key.
    assign
      itemfgdtl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = itemfgdtl.rec_key
      rec_key.table_name = "itemfgdtl".
  end.
end procedure.
run jc-ctrl_rec_key.
procedure jc-ctrl_rec_key:
  for each jc-ctrl exclusive:
    create rec_key.
    assign
      jc-ctrl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = jc-ctrl.rec_key
      rec_key.table_name = "jc-ctrl".
  end.
end procedure.
run job_rec_key.
procedure job_rec_key:
  for each job exclusive:
    create rec_key.
    assign
      job.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = job.rec_key
      rec_key.table_name = "job".
  end.
end procedure.
run job-all_rec_key.
procedure job-all_rec_key:
  for each job-all exclusive:
    create rec_key.
    assign
      job-all.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = job-all.rec_key
      rec_key.table_name = "job-all".
  end.
end procedure.
run job-brd_rec_key.
procedure job-brd_rec_key:
  for each job-brd exclusive:
    create rec_key.
    assign
      job-brd.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = job-brd.rec_key
      rec_key.table_name = "job-brd".
  end.
end procedure.
run job-cat_rec_key.
procedure job-cat_rec_key:
  for each job-cat exclusive:
    create rec_key.
    assign
      job-cat.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = job-cat.rec_key
      rec_key.table_name = "job-cat".
  end.
end procedure.
run job-code_rec_key.
procedure job-code_rec_key:
  for each job-code exclusive:
    create rec_key.
    assign
      job-code.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = job-code.rec_key
      rec_key.table_name = "job-code".
  end.
end procedure.
run job-hdr_rec_key.
procedure job-hdr_rec_key:
  for each job-hdr exclusive:
    create rec_key.
    assign
      job-hdr.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = job-hdr.rec_key
      rec_key.table_name = "job-hdr".
  end.
end procedure.
run job-mat_rec_key.
procedure job-mat_rec_key:
  for each job-mat exclusive:
    create rec_key.
    assign
      job-mat.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = job-mat.rec_key
      rec_key.table_name = "job-mat".
  end.
end procedure.
run job-mch_rec_key.
procedure job-mch_rec_key:
  for each job-mch exclusive:
    create rec_key.
    assign
      job-mch.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = job-mch.rec_key
      rec_key.table_name = "job-mch".
  end.
end procedure.
run job-prep_rec_key.
procedure job-prep_rec_key:
  for each job-prep exclusive:
    create rec_key.
    assign
      job-prep.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = job-prep.rec_key
      rec_key.table_name = "job-prep".
  end.
end procedure.
run loc_rec_key.
procedure loc_rec_key:
  for each loc exclusive:
    create rec_key.
    assign
      loc.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = loc.rec_key
      rec_key.table_name = "loc".
  end.
end procedure.
run mach_rec_key.
procedure mach_rec_key:
  for each mach exclusive:
    create rec_key.
    assign
      mach.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mach.rec_key
      rec_key.table_name = "mach".
  end.
end procedure.
run mach-adder_rec_key.
procedure mach-adder_rec_key:
  for each mach-adder exclusive:
    create rec_key.
    assign
      mach-adder.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mach-adder.rec_key
      rec_key.table_name = "mach-adder".
  end.
end procedure.
run mach-calendar_rec_key.
procedure mach-calendar_rec_key:
  for each mach-calendar exclusive:
    create rec_key.
    assign
      mach-calendar.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mach-calendar.rec_key
      rec_key.table_name = "mach-calendar".
  end.
end procedure.
run mat_rec_key.
procedure mat_rec_key:
  for each mat exclusive:
    create rec_key.
    assign
      mat.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mat.rec_key
      rec_key.table_name = "mat".
  end.
end procedure.
run mat-act_rec_key.
procedure mat-act_rec_key:
  for each mat-act exclusive:
    create rec_key.
    assign
      mat-act.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mat-act.rec_key
      rec_key.table_name = "mat-act".
  end.
end procedure.
run matprep_rec_key.
procedure matprep_rec_key:
  for each matprep exclusive:
    create rec_key.
    assign
      matprep.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = matprep.rec_key
      rec_key.table_name = "matprep".
  end.
end procedure.
run mch-act_rec_key.
procedure mch-act_rec_key:
  for each mch-act exclusive:
    create rec_key.
    assign
      mch-act.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mch-act.rec_key
      rec_key.table_name = "mch-act".
  end.
end procedure.
run mch-srt_rec_key.
procedure mch-srt_rec_key:
  for each mch-srt exclusive:
    create rec_key.
    assign
      mch-srt.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mch-srt.rec_key
      rec_key.table_name = "mch-srt".
  end.
end procedure.
run misc-act_rec_key.
procedure misc-act_rec_key:
  for each misc-act exclusive:
    create rec_key.
    assign
      misc-act.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = misc-act.rec_key
      rec_key.table_name = "misc-act".
  end.
end procedure.
run mmtx_rec_key.
procedure mmtx_rec_key:
  for each mmtx exclusive:
    create rec_key.
    assign
      mmtx.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mmtx.rec_key
      rec_key.table_name = "mmtx".
  end.
end procedure.
run mmtx2_rec_key.
procedure mmtx2_rec_key:
  for each mmtx2 exclusive:
    create rec_key.
    assign
      mmtx2.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mmtx2.rec_key
      rec_key.table_name = "mmtx2".
  end.
end procedure.
run mmty_rec_key.
procedure mmty_rec_key:
  for each mmty exclusive:
    create rec_key.
    assign
      mmty.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mmty.rec_key
      rec_key.table_name = "mmty".
  end.
end procedure.
run mnu-item_rec_key.
procedure mnu-item_rec_key:
  for each mnu-item exclusive:
    create rec_key.
    assign
      mnu-item.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mnu-item.rec_key
      rec_key.table_name = "mnu-item".
  end.
end procedure.
run monthly_rec_key.
procedure monthly_rec_key:
  for each monthly exclusive:
    create rec_key.
    assign
      monthly.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = monthly.rec_key
      rec_key.table_name = "monthly".
  end.
end procedure.
run mstd_rec_key.
procedure mstd_rec_key:
  for each mstd exclusive:
    create rec_key.
    assign
      mstd.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = mstd.rec_key
      rec_key.table_name = "mstd".
  end.
end procedure.
run nrf-color_rec_key.
procedure nrf-color_rec_key:
  for each nrf-color exclusive:
    create rec_key.
    assign
      nrf-color.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = nrf-color.rec_key
      rec_key.table_name = "nrf-color".
  end.
end procedure.
run nrf-size_rec_key.
procedure nrf-size_rec_key:
  for each nrf-size exclusive:
    create rec_key.
    assign
      nrf-size.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = nrf-size.rec_key
      rec_key.table_name = "nrf-size".
  end.
end procedure.
run oe-bolh_rec_key.
procedure oe-bolh_rec_key:
  for each oe-bolh exclusive:
    create rec_key.
    assign
      oe-bolh.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-bolh.rec_key
      rec_key.table_name = "oe-bolh".
  end.
end procedure.
run oe-boll_rec_key.
procedure oe-boll_rec_key:
  for each oe-boll exclusive:
    create rec_key.
    assign
      oe-boll.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-boll.rec_key
      rec_key.table_name = "oe-boll".
  end.
end procedure.
run oe-boll-qty_rec_key.
procedure oe-boll-qty_rec_key:
  for each oe-boll-qty exclusive:
    create rec_key.
    assign
      oe-boll-qty.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-boll-qty.rec_key
      rec_key.table_name = "oe-boll-qty".
  end.
end procedure.
run oe-ctrl_rec_key.
procedure oe-ctrl_rec_key:
  for each oe-ctrl exclusive:
    create rec_key.
    assign
      oe-ctrl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-ctrl.rec_key
      rec_key.table_name = "oe-ctrl".
  end.
end procedure.
run oe-ord_rec_key.
procedure oe-ord_rec_key:
  for each oe-ord exclusive:
    create rec_key.
    assign
      oe-ord.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-ord.rec_key
      rec_key.table_name = "oe-ord".
  end.
end procedure.
run oe-ordl_rec_key.
procedure oe-ordl_rec_key:
  for each oe-ordl exclusive:
    create rec_key.
    assign
      oe-ordl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-ordl.rec_key
      rec_key.table_name = "oe-ordl".
  end.
end procedure.
run oe-ordm_rec_key.
procedure oe-ordm_rec_key:
  for each oe-ordm exclusive:
    create rec_key.
    assign
      oe-ordm.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-ordm.rec_key
      rec_key.table_name = "oe-ordm".
  end.
end procedure.
run oe-prmtx_rec_key.
procedure oe-prmtx_rec_key:
  for each oe-prmtx exclusive:
    create rec_key.
    assign
      oe-prmtx.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-prmtx.rec_key
      rec_key.table_name = "oe-prmtx".
  end.
end procedure.
run oe-rel_rec_key.
procedure oe-rel_rec_key:
  for each oe-rel exclusive:
    create rec_key.
    assign
      oe-rel.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-rel.rec_key
      rec_key.table_name = "oe-rel".
  end.
end procedure.
run oe-relh_rec_key.
procedure oe-relh_rec_key:
  for each oe-relh exclusive:
    create rec_key.
    assign
      oe-relh.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-relh.rec_key
      rec_key.table_name = "oe-relh".
  end.
end procedure.
run oe-rell_rec_key.
procedure oe-rell_rec_key:
  for each oe-rell exclusive:
    create rec_key.
    assign
      oe-rell.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-rell.rec_key
      rec_key.table_name = "oe-rell".
  end.
end procedure.
run oe-reth_rec_key.
procedure oe-reth_rec_key:
  for each oe-reth exclusive:
    create rec_key.
    assign
      oe-reth.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-reth.rec_key
      rec_key.table_name = "oe-reth".
  end.
end procedure.
run oe-retl_rec_key.
procedure oe-retl_rec_key:
  for each oe-retl exclusive:
    create rec_key.
    assign
      oe-retl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-retl.rec_key
      rec_key.table_name = "oe-retl".
  end.
end procedure.
run oe-ship_rec_key.
procedure oe-ship_rec_key:
  for each oe-ship exclusive:
    create rec_key.
    assign
      oe-ship.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = oe-ship.rec_key
      rec_key.table_name = "oe-ship".
  end.
end procedure.
run pc-misc_rec_key.
procedure pc-misc_rec_key:
  for each pc-misc exclusive:
    create rec_key.
    assign
      pc-misc.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = pc-misc.rec_key
      rec_key.table_name = "pc-misc".
  end.
end procedure.
run pc-prdd_rec_key.
procedure pc-prdd_rec_key:
  for each pc-prdd exclusive:
    create rec_key.
    assign
      pc-prdd.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = pc-prdd.rec_key
      rec_key.table_name = "pc-prdd".
  end.
end procedure.
run pc-prdh_rec_key.
procedure pc-prdh_rec_key:
  for each pc-prdh exclusive:
    create rec_key.
    assign
      pc-prdh.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = pc-prdh.rec_key
      rec_key.table_name = "pc-prdh".
  end.
end procedure.
run pdd_rec_key.
procedure pdd_rec_key:
  for each pdd exclusive:
    create rec_key.
    assign
      pdd.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = pdd.rec_key
      rec_key.table_name = "pdd".
  end.
end procedure.
run pdh_rec_key.
procedure pdh_rec_key:
  for each pdh exclusive:
    create rec_key.
    assign
      pdh.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = pdh.rec_key
      rec_key.table_name = "pdh".
  end.
end procedure.
run period_rec_key.
procedure period_rec_key:
  for each period exclusive:
    create rec_key.
    assign
      period.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = period.rec_key
      rec_key.table_name = "period".
  end.
end procedure.
run perm_rec_key.
procedure perm_rec_key:
  for each perm exclusive:
    create rec_key.
    assign
      perm.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = perm.rec_key
      rec_key.table_name = "perm".
  end.
end procedure.
run permg_rec_key.
procedure permg_rec_key:
  for each permg exclusive:
    create rec_key.
    assign
      permg.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = permg.rec_key
      rec_key.table_name = "permg".
  end.
end procedure.
run permx_rec_key.
procedure permx_rec_key:
  for each permx exclusive:
    create rec_key.
    assign
      permx.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = permx.rec_key
      rec_key.table_name = "permx".
  end.
end procedure.
run po-all_rec_key.
procedure po-all_rec_key:
  for each po-all exclusive:
    create rec_key.
    assign
      po-all.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = po-all.rec_key
      rec_key.table_name = "po-all".
  end.
end procedure.
run po-ctrl_rec_key.
procedure po-ctrl_rec_key:
  for each po-ctrl exclusive:
    create rec_key.
    assign
      po-ctrl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = po-ctrl.rec_key
      rec_key.table_name = "po-ctrl".
  end.
end procedure.
run po-ord_rec_key.
procedure po-ord_rec_key:
  for each po-ord exclusive:
    create rec_key.
    assign
      po-ord.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = po-ord.rec_key
      rec_key.table_name = "po-ord".
  end.
end procedure.
run po-ordl_rec_key.
procedure po-ordl_rec_key:
  for each po-ordl exclusive:
    create rec_key.
    assign
      po-ordl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = po-ordl.rec_key
      rec_key.table_name = "po-ordl".
  end.
end procedure.
run po-rcpts_rec_key.
procedure po-rcpts_rec_key:
  for each po-rcpts exclusive:
    create rec_key.
    assign
      po-rcpts.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = po-rcpts.rec_key
      rec_key.table_name = "po-rcpts".
  end.
end procedure.
run prep_rec_key.
procedure prep_rec_key:
  for each prep exclusive:
    create rec_key.
    assign
      prep.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = prep.rec_key
      rec_key.table_name = "prep".
  end.
end procedure.
run printer_rec_key.
procedure printer_rec_key:
  for each printer exclusive:
    create rec_key.
    assign
      printer.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = printer.rec_key
      rec_key.table_name = "printer".
  end.
end procedure.
run probe_rec_key.
procedure probe_rec_key:
  for each probe exclusive:
    create rec_key.
    assign
      probe.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = probe.rec_key
      rec_key.table_name = "probe".
  end.
end procedure.
run probeit_rec_key.
procedure probeit_rec_key:
  for each probeit exclusive:
    create rec_key.
    assign
      probeit.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = probeit.rec_key
      rec_key.table_name = "probeit".
  end.
end procedure.
run procat_rec_key.
procedure procat_rec_key:
  for each procat exclusive:
    create rec_key.
    assign
      procat.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = procat.rec_key
      rec_key.table_name = "procat".
  end.
end procedure.
run prod_rec_key.
procedure prod_rec_key:
  for each prod exclusive:
    create rec_key.
    assign
      prod.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = prod.rec_key
      rec_key.table_name = "prod".
  end.
end procedure.
run prodl_rec_key.
procedure prodl_rec_key:
  for each prodl exclusive:
    create rec_key.
    assign
      prodl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = prodl.rec_key
      rec_key.table_name = "prodl".
  end.
end procedure.
run quote_rec_key.
procedure quote_rec_key:
  for each quote exclusive:
    create rec_key.
    assign
      quote.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = quote.rec_key
      rec_key.table_name = "quote".
  end.
end procedure.
run quoteit_rec_key.
procedure quoteit_rec_key:
  for each quoteit exclusive:
    create rec_key.
    assign
      quoteit.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = quoteit.rec_key
      rec_key.table_name = "quoteit".
  end.
end procedure.
run RCErr_rec_key.
procedure RCErr_rec_key:
  for each RCErr exclusive:
    create rec_key.
    assign
      RCErr.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = RCErr.rec_key
      rec_key.table_name = "RCErr".
  end.
end procedure.
run reftable_rec_key.
procedure reftable_rec_key:
  for each reftable exclusive:
    create rec_key.
    assign
      reftable.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = reftable.rec_key
      rec_key.table_name = "reftable".
  end.
end procedure.
run rejct-cd_rec_key.
procedure rejct-cd_rec_key:
  for each rejct-cd exclusive:
    create rec_key.
    assign
      rejct-cd.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = rejct-cd.rec_key
      rec_key.table_name = "rejct-cd".
  end.
end procedure.
run report_rec_key.
procedure report_rec_key:
  for each report exclusive:
    create rec_key.
    assign
      report.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = report.rec_key
      rec_key.table_name = "report".
  end.
end procedure.
run rm-bin_rec_key.
procedure rm-bin_rec_key:
  for each rm-bin exclusive:
    create rec_key.
    assign
      rm-bin.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = rm-bin.rec_key
      rec_key.table_name = "rm-bin".
  end.
end procedure.
run rm-ctrl_rec_key.
procedure rm-ctrl_rec_key:
  for each rm-ctrl exclusive:
    create rec_key.
    assign
      rm-ctrl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = rm-ctrl.rec_key
      rec_key.table_name = "rm-ctrl".
  end.
end procedure.
run rm-rcpt_rec_key.
procedure rm-rcpt_rec_key:
  for each rm-rcpt exclusive:
    create rec_key.
    assign
      rm-rcpt.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = rm-rcpt.rec_key
      rec_key.table_name = "rm-rcpt".
  end.
end procedure.
run rm-rcpth_rec_key.
procedure rm-rcpth_rec_key:
  for each rm-rcpth exclusive:
    create rec_key.
    assign
      rm-rcpth.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = rm-rcpth.rec_key
      rec_key.table_name = "rm-rcpth".
  end.
end procedure.
run rm-rdtl_rec_key.
procedure rm-rdtl_rec_key:
  for each rm-rdtl exclusive:
    create rec_key.
    assign
      rm-rdtl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = rm-rdtl.rec_key
      rec_key.table_name = "rm-rdtl".
  end.
end procedure.
run rm-rdtlh_rec_key.
procedure rm-rdtlh_rec_key:
  for each rm-rdtlh exclusive:
    create rec_key.
    assign
      rm-rdtlh.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = rm-rdtlh.rec_key
      rec_key.table_name = "rm-rdtlh".
  end.
end procedure.
run rm-receipts_rec_key.
procedure rm-receipts_rec_key:
  for each rm-receipts exclusive:
    create rec_key.
    assign
      rm-receipts.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = rm-receipts.rec_key
      rec_key.table_name = "rm-receipts".
  end.
end procedure.
run routing_rec_key.
procedure routing_rec_key:
  for each routing exclusive:
    create rec_key.
    assign
      routing.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = routing.rec_key
      rec_key.table_name = "routing".
  end.
end procedure.
run routing-mtx_rec_key.
procedure routing-mtx_rec_key:
  for each routing-mtx exclusive:
    create rec_key.
    assign
      routing-mtx.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = routing-mtx.rec_key
      rec_key.table_name = "routing-mtx".
  end.
end procedure.
run shift_rec_key.
procedure shift_rec_key:
  for each shift exclusive:
    create rec_key.
    assign
      shift.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = shift.rec_key
      rec_key.table_name = "shift".
  end.
end procedure.
run shipto_rec_key.
procedure shipto_rec_key:
  for each shipto exclusive:
    create rec_key.
    assign
      shipto.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = shipto.rec_key
      rec_key.table_name = "shipto".
  end.
end procedure.
run sman_rec_key.
procedure sman_rec_key:
  for each sman exclusive:
    create rec_key.
    assign
      sman.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = sman.rec_key
      rec_key.table_name = "sman".
  end.
end procedure.
run sman-mtx_rec_key.
procedure sman-mtx_rec_key:
  for each sman-mtx exclusive:
    create rec_key.
    assign
      sman-mtx.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = sman-mtx.rec_key
      rec_key.table_name = "sman-mtx".
  end.
end procedure.
run soldto_rec_key.
procedure soldto_rec_key:
  for each soldto exclusive:
    create rec_key.
    assign
      soldto.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = soldto.rec_key
      rec_key.table_name = "soldto".
  end.
end procedure.
run stack-flute_rec_key.
procedure stack-flute_rec_key:
  for each stack-flute exclusive:
    create rec_key.
    assign
      stack-flute.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = stack-flute.rec_key
      rec_key.table_name = "stack-flute".
  end.
end procedure.
run stack-size_rec_key.
procedure stack-size_rec_key:
  for each stack-size exclusive:
    create rec_key.
    assign
      stack-size.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = stack-size.rec_key
      rec_key.table_name = "stack-size".
  end.
end procedure.
run state_rec_key.
procedure state_rec_key:
  for each state exclusive:
    create rec_key.
    assign
      state.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = state.rec_key
      rec_key.table_name = "state".
  end.
end procedure.
run stax_rec_key.
procedure stax_rec_key:
  for each stax exclusive:
    create rec_key.
    assign
      stax.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = stax.rec_key
      rec_key.table_name = "stax".
  end.
end procedure.
run stax-group_rec_key.
procedure stax-group_rec_key:
  for each stax-group exclusive:
    create rec_key.
    assign
      stax-group.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = stax-group.rec_key
      rec_key.table_name = "stax-group".
  end.
end procedure.
run std-code_rec_key.
procedure std-code_rec_key:
  for each std-code exclusive:
    create rec_key.
    assign
      std-code.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = std-code.rec_key
      rec_key.table_name = "std-code".
  end.
end procedure.
run style_rec_key.
procedure style_rec_key:
  for each style exclusive:
    create rec_key.
    assign
      style.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = style.rec_key
      rec_key.table_name = "style".
  end.
end procedure.
run terms_rec_key.
procedure terms_rec_key:
  for each terms exclusive:
    create rec_key.
    assign
      terms.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = terms.rec_key
      rec_key.table_name = "terms".
  end.
end procedure.
run terr_rec_key.
procedure terr_rec_key:
  for each terr exclusive:
    create rec_key.
    assign
      terr.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = terr.rec_key
      rec_key.table_name = "terr".
  end.
end procedure.
run test-red_rec_key.
procedure test-red_rec_key:
  for each test-red exclusive:
    create rec_key.
    assign
      test-red.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = test-red.rec_key
      rec_key.table_name = "test-red".
  end.
end procedure.
run uom_rec_key.
procedure uom_rec_key:
  for each uom exclusive:
    create rec_key.
    assign
      uom.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = uom.rec_key
      rec_key.table_name = "uom".
  end.
end procedure.
run usr_rec_key.
procedure usr_rec_key:
  for each usr exclusive:
    create rec_key.
    assign
      usr.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = usr.rec_key
      rec_key.table_name = "usr".
  end.
end procedure.
run usr-grp_rec_key.
procedure usr-grp_rec_key:
  for each usr-grp exclusive:
    create rec_key.
    assign
      usr-grp.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = usr-grp.rec_key
      rec_key.table_name = "usr-grp".
  end.
end procedure.
run usrx_rec_key.
procedure usrx_rec_key:
  for each usrx exclusive:
    create rec_key.
    assign
      usrx.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = usrx.rec_key
      rec_key.table_name = "usrx".
  end.
end procedure.
run vend_rec_key.
procedure vend_rec_key:
  for each vend exclusive:
    create rec_key.
    assign
      vend.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = vend.rec_key
      rec_key.table_name = "vend".
  end.
end procedure.
run ventype_rec_key.
procedure ventype_rec_key:
  for each ventype exclusive:
    create rec_key.
    assign
      ventype.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = ventype.rec_key
      rec_key.table_name = "ventype".
  end.
end procedure.
run view-dtl_rec_key.
procedure view-dtl_rec_key:
  for each view-dtl exclusive:
    create rec_key.
    assign
      view-dtl.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = view-dtl.rec_key
      rec_key.table_name = "view-dtl".
  end.
end procedure.
run view-hdr_rec_key.
procedure view-hdr_rec_key:
  for each view-hdr exclusive:
    create rec_key.
    assign
      view-hdr.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = view-hdr.rec_key
      rec_key.table_name = "view-hdr".
  end.
end procedure.
