using System;
using System.Data;
using System.Configuration;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;
using System.Web.Caching;
using ASINET;
using ASIDataNS;
using Progress.Open4GL.Proxy;

/// <summary>
/// Summary description for itemhistory
/// </summary>
[System.ComponentModel.DataObject]
public class browspo:AppServerConnect.AppServer
{
    public browspo()
	{
		//
		// TODO: Add constructor logic here
		//
	}

   
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBrwsPoDataSet SelectBrowsPO(string prmUser, string prmAction, string prmOrderNum, string prmLine, string prmPo, string prmVend, string prmItem, string prmVendItem, string prmDate, string prmJob, string prmJob2, string prmOpen, string prmClosed)
    {

        dsBrwsPoDataSet dsBrwsPo = new dsBrwsPoDataSet();
        dsBrwsPo = null;
        AppServerConnect();
        aoObject.BrwsPo(prmUser, prmAction, prmOrderNum, prmLine, prmPo, prmVend, prmItem, prmVendItem, prmDate, prmJob, prmJob2, prmOpen, prmClosed, ref dsBrwsPo);
        AppServerDisconnect();

        return dsBrwsPo;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewPoDataSet SelectPO(string prmUser, string prmAction, string prmOrderNum, string prmItemNum, string prmPo)
    {

        dsViewPoDataSet dsViewPo = new dsViewPoDataSet();
        dsViewPo = null;
        AppServerConnect();
        aoObject.ViewPo(prmUser, prmAction, prmOrderNum, prmItemNum, prmPo, ref dsViewPo);
        AppServerDisconnect();

        return dsViewPo;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsInvoicePoDataSet SelectInvoice(string prmUser, string prmAction, string prmOrderNum, string prmItemNum, string prmPo)
    {

        dsInvoicePoDataSet dsInvoice = new dsInvoicePoDataSet();
        dsInvoice = null;
        AppServerConnect();
        aoObject.InvoicePo(prmUser, prmAction, prmOrderNum, prmItemNum, prmPo, ref dsInvoice);
        AppServerDisconnect();

        return dsInvoice;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsb_po_inqlistDataSet SelectListPO(string prmUser, string prmAction, string prmPono, string prmVendor, string prmFgItem, string prmVenItem, string prmDueDate, string prmJob, string prmJob2, string prmOpen, string prmClose)
    {

        dsb_po_inqlistDataSet dsb_po_inqlist = new dsb_po_inqlistDataSet();
        dsb_po_inqlist = null;
        AppServerConnect();
        aoObject.poinq(prmUser, prmAction, prmPono, prmVendor, prmFgItem, prmVenItem, prmDueDate, prmJob, prmJob2, prmOpen, prmClose, ref dsb_po_inqlist);
        AppServerDisconnect();

        return dsb_po_inqlist;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewPurOrdDataSet SelectViewPo(string prmUser, string prmAction, Int32 prmPoNo, string prmPoDate, string prmType, string prmStat, string prmVendNo, string prmVendName, string prmVendAdd1, string prmVendAdd2, string prmVendCity, string prmVendState, string prmVendZip, string prmVendAreaCode, string prmVendPhone, string prmShipId, string prmShipName, string prmShipAddr, string prmShipCity, string prmShipState, string prmShipZip, string prmShipAreaCode, string prmShipPhone, string prmBuyer, string prmContact, string prmDueDate, string prmLastShipDate, Int32 prmUnderPct, Int32 prmOverPct, string prmCarrier, string prmTaxGr, string prmTerms, string prmFrtPay, string prmFobCode, Int32 prmTFreight, Int32 prmTax, Int32 prmTCost, string prmRecKey)
    {
        string cError = "";
        
        dsViewPurOrdDataSet dsViewPurOrd = new dsViewPurOrdDataSet();
        dsViewPurOrd = null;
        AppServerConnect();
        aoObject.viewpord(prmUser, prmAction, prmPoNo, prmPoDate, prmType, prmStat, prmVendNo, prmVendName, prmVendAdd1, prmVendAdd2, prmVendCity, prmVendState, prmVendZip, prmVendAreaCode, prmVendPhone, prmShipId, prmShipName, prmShipAddr, prmShipCity, prmShipState, prmShipZip, prmShipAreaCode, prmShipPhone, prmBuyer, prmContact, prmDueDate, prmLastShipDate, prmUnderPct, prmOverPct, prmCarrier, prmTaxGr, prmTerms, prmFrtPay, prmFobCode, prmTFreight, prmTax, prmTCost, prmRecKey, out cError, ref dsViewPurOrd);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsViewPurOrd;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsb_po_item_inqlistDataSet SelectListItemPO(string prmUser, string prmAction, Int32 prmpoNo, string prmpoItemNo, string prmpoRecKey)
    {

        dsb_po_item_inqlistDataSet dsb_po_item_inqlist = new dsb_po_item_inqlistDataSet();
        dsb_po_item_inqlist = null;
        AppServerConnect();
        aoObject.listitempo(prmUser, prmAction, prmpoNo, prmpoItemNo, prmpoRecKey, ref dsb_po_item_inqlist);
        AppServerDisconnect();

        return dsb_po_item_inqlist;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsb_po_item_inq_viewDataSet SelectViewItemPo(string prmUser, string prmAction, Int32 prmpoLine,Int32 prmpoNo,string prmpoDate,string prmpoLoc,string prmpoType,decimal prmpoTMsf,string prmpoStat,string prmpoItemNo,string prmpoItemName,string prmpoJobNo,Int32 prmpoJobNo2,Int32 prmpoSNum,decimal prmpoOrdQty,decimal prmpoCost,string prmpoCustNo,string prmpoDueDate,string prmpoItemType,Int32 prmpoBNum,string prmpoPrQtyUom,decimal prmpoConsQty,string prmpoDscr,string prmpoDscr2,string prmpoPrUom,decimal prmpoConsCost,string prmpoConsUom,decimal prmpoSetup, decimal prmpoSwid,decimal prmpoSlen,decimal prmpoDisc,string prmpoActNum,string prmpoVendINo,string prmpoTax,decimal prmpoOverPct,decimal prmpoUnderPct,Int32 prmpoOrdNo,decimal prmpoTCost,string prmpoFiUom,string prmpoScrConsUom,decimal prmpoSDep,string prmpoWidFrac,string prmpoLenFrac, string prmpoDepFrac,string prmpoGlDesc,decimal prmpoPbQty,decimal prmpoPbCst,decimal prmpoQOnh,decimal prmpoQOno,decimal prmpoQComm,decimal prmpoQBack,decimal prmpoQAvail,decimal prmpoMOnh,decimal prmpoMOno,decimal prmpoMComm,decimal prmpoMBack,decimal prmpoMAvail,string prmpoMsf,decimal prmpoTonnage,string prmpoaddress,string prmpoRecKey)
    {
        string cError = "";
        dsb_po_item_inq_viewDataSet dsb_po_item_inq_view = new dsb_po_item_inq_viewDataSet();
        dsb_po_item_inq_view = null;
        AppServerConnect();
        aoObject.viewitempo(prmUser, prmAction, prmpoLine, prmpoNo, prmpoDate, prmpoLoc, prmpoType, prmpoTMsf, prmpoStat, prmpoItemNo, prmpoItemName, prmpoJobNo, prmpoJobNo2, prmpoSNum, prmpoOrdQty, prmpoCost, prmpoCustNo, prmpoDueDate, prmpoItemType, prmpoBNum, prmpoPrQtyUom, prmpoConsQty, prmpoDscr, prmpoDscr2, prmpoPrUom, prmpoConsCost, prmpoConsUom, prmpoSetup, prmpoSwid, prmpoSlen, prmpoDisc, prmpoActNum, prmpoVendINo, prmpoTax, prmpoOverPct, prmpoUnderPct, prmpoOrdNo, prmpoTCost, prmpoFiUom, prmpoScrConsUom, prmpoSDep, prmpoWidFrac, prmpoLenFrac, prmpoDepFrac, prmpoGlDesc, prmpoPbQty, prmpoPbCst, prmpoQOnh, prmpoQOno, prmpoQComm, prmpoQBack, prmpoQAvail, prmpoMOnh, prmpoMOno, prmpoMComm, prmpoMBack, prmpoMAvail, prmpoMsf, prmpoTonnage, prmpoaddress, prmpoRecKey, out cError, ref dsb_po_item_inq_view);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }

        return dsb_po_item_inq_view;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsQpoBoxDesignDataSet SelectPoDesign(string prmUser, string prmAction, string prmComp, Int32 prmPoNo, string prmReckey, Int32 prmLineno)
    {

        dsQpoBoxDesignDataSet dsQpoBoxDesign = new dsQpoBoxDesignDataSet();
        dsQpoBoxDesign = null;
        AppServerConnect();
        aoObject.qpobox(prmUser, prmAction, prmComp, prmPoNo, prmReckey, prmLineno, ref dsQpoBoxDesign);
        AppServerDisconnect();

        return dsQpoBoxDesign;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsItemLookalloneDataSet SelectFgAllItemLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText,string prmIndu,string prmMatType,string prmPoNo,string prmJob,string prmJob2,string prmComp,string prmType,Int32 prmpoline,string prmino,Int32 prmsno,Int32 prmbno,Int32 prmqty,string prmpoitemtype,string prmCust,decimal prmcost, string prmconsuom,string prmprqtyuom,string prmpruom, decimal prmdiscount)
    {
        dsItemLookalloneDataSet dsItemLookallone = new dsItemLookalloneDataSet();
        dsItemLookallone = null;
        AppServerConnect();
        aoObject.ItemLookall(prmAction, prmUser, prmField, prmCondition, prmText,prmIndu,prmMatType,prmPoNo,prmJob,prmJob2,prmComp,prmType,prmpoline,prmino,prmsno,prmbno,prmqty,prmpoitemtype,prmCust,prmcost,prmconsuom,prmprqtyuom,prmpruom,prmdiscount, ref dsItemLookallone);
        AppServerDisconnect();
        return dsItemLookallone;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsJobhdrLookupDataSet SelectjobhdrLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmItem, string prmChar, string prmJobno, Int32 prmJob2, Int32 prmSnum, Int32 prmBnum, string prmPruom, string prmPrqtyuom, string prmOrdqty, decimal prmSwid, decimal prmSlen, decimal prmSdep, decimal prmDis,decimal prmSetup,string prmConsuom,string prmCust,string prmItemType,Int32 prmpoNo,Int32 prmLine, ref string error)
    {
        string cError = "";
        dsJobhdrLookupDataSet dsJobhdrLookup = new dsJobhdrLookupDataSet();
        dsJobhdrLookup = null;
        AppServerConnect();
        aoObject.JobhdrLook(prmAction, prmUser, prmField, prmCondition, prmText, prmItem, prmChar, prmJobno, prmJob2, prmSnum, prmBnum, prmPruom, prmPrqtyuom, prmOrdqty, prmSwid, prmSlen, prmSdep,prmDis,prmSetup,prmConsuom,prmCust,prmItemType,prmpoNo,prmLine, out cError, ref dsJobhdrLookup);
        AppServerDisconnect();
        
        if (cError != "")
        {
            error = cError;
            //HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
        }
        return dsJobhdrLookup;        
    }

      


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsJobMatLookupDataSet SelectjobmatLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmJob, string prmjob2, string prmItem)
    {
        dsJobMatLookupDataSet dsJobMatLookup = new dsJobMatLookupDataSet();
        dsJobMatLookup = null;
        AppServerConnect();
        aoObject.JobMatLook(prmAction, prmUser, prmField, prmCondition, prmText, prmJob, prmjob2, prmItem, ref dsJobMatLookup);
        AppServerDisconnect();
        return dsJobMatLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsbuyerLookupDataSet Selectbuyerlook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsbuyerLookupDataSet dsbuyerLookup = new dsbuyerLookupDataSet();
        dsbuyerLookup = null;
        AppServerConnect();
        aoObject.buyerlook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsbuyerLookup);
        AppServerDisconnect();
        return dsbuyerLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateViewItemPo(string prmUser, string prmAction, Int32 prmpoLine, Int32 prmpoNo, string prmpoDate, string prmpoLoc, string prmpoType, decimal prmpoTMsf, string prmpoStat, string prmpoItemNo, string prmpoItemName, string prmpoJobNo, Int32 prmpoJobNo2, Int32 prmpoSNum, decimal prmpoOrdQty, decimal prmpoCost, string prmpoCustNo, string prmpoDueDate, string prmpoItemType, Int32 prmpoBNum, string prmpoPrQtyUom, decimal prmpoConsQty, string prmpoDscr, string prmpoDscr2, string prmpoPrUom, decimal prmpoConsCost, string prmpoConsUom, decimal prmpoSetup, decimal prmpoSwid, decimal prmpoSlen, decimal prmpoDisc, string prmpoActNum, string prmpoVendINo, string prmpoTax, decimal prmpoOverPct, decimal prmpoUnderPct, Int32 prmpoOrdNo, decimal prmpoTCost, string prmpoFiUom, string prmpoScrConsUom, decimal prmpoSDep, string prmpoWidFrac, string prmpoLenFrac, string prmpoDepFrac, string prmpoGlDesc, decimal prmpoPbQty, decimal prmpoPbCst, decimal prmpoQOnh, decimal prmpoQOno, decimal prmpoQComm, decimal prmpoQBack, decimal prmpoQAvail, decimal prmpoMOnh, decimal prmpoMOno, decimal prmpoMComm, decimal prmpoMBack, decimal prmpoMAvail, string prmpoMsf, decimal prmpoTonnage, string prmpoaddress, string prmpoRecKey)
    {
        string cError = "";
        dsb_po_item_inq_viewDataSet dsb_po_item_inq_view = new dsb_po_item_inq_viewDataSet();
        AppServerConnect();
        aoObject.viewitempo(prmUser, prmAction, prmpoLine, prmpoNo, prmpoDate, prmpoLoc, prmpoType, prmpoTMsf, prmpoStat, prmpoItemNo, prmpoItemName, prmpoJobNo, prmpoJobNo2, prmpoSNum, prmpoOrdQty, prmpoCost, prmpoCustNo, prmpoDueDate, prmpoItemType, prmpoBNum, prmpoPrQtyUom, prmpoConsQty, prmpoDscr, prmpoDscr2, prmpoPrUom, prmpoConsCost, prmpoConsUom, prmpoSetup, prmpoSwid, prmpoSlen, prmpoDisc, prmpoActNum, prmpoVendINo, prmpoTax, prmpoOverPct, prmpoUnderPct, prmpoOrdNo, prmpoTCost, prmpoFiUom, prmpoScrConsUom, prmpoSDep, prmpoWidFrac, prmpoLenFrac, prmpoDepFrac, prmpoGlDesc, prmpoPbQty, prmpoPbCst, prmpoQOnh, prmpoQOno, prmpoQComm, prmpoQBack, prmpoQAvail, prmpoMOnh, prmpoMOno, prmpoMComm, prmpoMBack, prmpoMAvail, prmpoMsf, prmpoTonnage, prmpoaddress, prmpoRecKey, out cError, ref dsb_po_item_inq_view);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }

    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateListPo(string prmUser, string prmAction, Int32 prmPoNo, string prmPoDate, string prmType, string prmStat, string prmVendNo, string prmVendName, string prmVendAdd1, string prmVendAdd2, string prmVendCity, string prmVendState, string prmVendZip, string prmVendAreaCode, string prmVendPhone, string prmShipId, string prmShipName, string prmShipAddr, string prmShipCity, string prmShipState, string prmShipZip, string prmShipAreaCode, string prmShipPhone, string prmBuyer, string prmContact, string prmDueDate, string prmLastShipDate, Int32 prmUnderPct, Int32 prmOverPct, string prmCarrier, string prmTaxGr, string prmTerms, string prmFrtPay, string prmFobCode, Int32 prmTFreight, Int32 prmTax, Int32 prmTCost, string prmRecKey)
    {
        string cError = "";
        dsViewPurOrdDataSet dsViewPurOrd = new dsViewPurOrdDataSet();
        AppServerConnect();
        aoObject.viewpord(prmUser, prmAction, prmPoNo, prmPoDate, prmType, prmStat, prmVendNo, prmVendName, prmVendAdd1, prmVendAdd2, prmVendCity, prmVendState, prmVendZip, prmVendAreaCode, prmVendPhone, prmShipId, prmShipName, prmShipAddr, prmShipCity, prmShipState, prmShipZip, prmShipAreaCode, prmShipPhone, prmBuyer, prmContact, prmDueDate, prmLastShipDate, prmUnderPct, prmOverPct, prmCarrier, prmTaxGr, prmTerms, prmFrtPay, prmFobCode, prmTFreight, prmTax, prmTCost, prmRecKey, out cError, ref dsViewPurOrd);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;
        }
        else
        {
            return true;
        }

    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsb_cost_qty_poDataSet CostQty(string prmUser, string prmAction, Int32 prmpoLine, Int32 prmpoNo, string prmpoitemtype, decimal prmqty, decimal prmcost, decimal prmsetup, decimal prmdiscount, string prmprqtyuom, string prmpruom, decimal prmslen, decimal prmswid, string prmino, string prmconsuom, string prmRecKey,string prmCust,string prmJob,Int32 prmJob2,Int32 prmSno, Int32 prmBno)
    {
        string cError = "";
        dsb_cost_qty_poDataSet dsb_cost_qty_po = new dsb_cost_qty_poDataSet();
        dsb_cost_qty_po = null;
        AppServerConnect();
        aoObject.Costqtypo(prmUser, prmAction, prmpoLine, prmpoNo, prmpoitemtype, prmqty, prmcost, prmsetup, prmdiscount, prmprqtyuom, prmpruom, prmslen, prmswid, prmino, prmconsuom, prmRecKey,prmCust,prmJob, prmJob2, prmSno,  prmBno, out cError, ref dsb_cost_qty_po);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsb_cost_qty_po;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public ds_g_iss_LookupDataSet Selectreplacejobmat(string prmAction, string prmUser, Int32 prmsno, Int32 prmbno, string prmjob, Int32 prmjob2, string prmitem, decimal prmqty, string prmqtyuom, string prmpruom, decimal prmcost, string prmpo, Int32 prmvout, Int32 prmline, string prmReckey)
    {
        ds_g_iss_LookupDataSet ds_g_iss_Lookup = new ds_g_iss_LookupDataSet();
        ds_g_iss_Lookup = null;
        AppServerConnect();
        aoObject.g_iss(prmAction, prmUser, prmsno, prmbno, prmjob, prmjob2, prmitem, prmqty, prmqtyuom, prmpruom, prmcost, prmpo, prmvout, prmline, prmReckey, ref ds_g_iss_Lookup);
        AppServerDisconnect();
        return ds_g_iss_Lookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsPoItemLookpoDataSet PoItemLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        dsPoItemLookpoDataSet dsPoItemLookpo = new dsPoItemLookpoDataSet();
        dsPoItemLookpo = null;
        AppServerConnect();
        aoObject.PoItemLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsPoItemLookpo);
        AppServerDisconnect();
        return dsPoItemLookpo;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsfgtagLookDataSet SelectfgtakLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmitmtype, string prmcurval)
    {
        dsfgtagLookDataSet dsfgtagLook = new dsfgtagLookDataSet();
        dsfgtagLook = null;
        AppServerConnect();
        aoObject.fgtaglook(prmAction, prmUser, prmField, prmCondition, prmText, prmitmtype, prmcurval, ref dsfgtagLook);
        AppServerDisconnect();
        return dsfgtagLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dstrnstagLookDataSet SelecttrnstagLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmTag, string prmcurval)
    {
        dstrnstagLookDataSet dstrnstagLook = new dstrnstagLookDataSet();
        dstrnstagLook = null;
        AppServerConnect();
        aoObject.trnstaglook(prmAction, prmUser, prmField, prmCondition, prmText, prmTag, prmcurval, ref dstrnstagLook);
        AppServerDisconnect();
        return dstrnstagLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTrnsFgBinLookDataSet trnsfgbinLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmTag)
    {
        dsTrnsFgBinLookDataSet dsTrnsFgBinLook = new dsTrnsFgBinLookDataSet();
        dsTrnsFgBinLook = null;
        AppServerConnect();
        aoObject.trns_fgbin_look(prmAction, prmUser, prmField, prmCondition, prmText, prmTag, ref dsTrnsFgBinLook);
        AppServerDisconnect();
        return dsTrnsFgBinLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsTrnsFgBintgLookDataSet trnsfgbintgLook2(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmitem)
    {
        dsTrnsFgBintgLookDataSet dsTrnsFgBintgLook = new dsTrnsFgBintgLookDataSet();
        dsTrnsFgBintgLook = null;
        AppServerConnect();
        aoObject.trns_fgbintg_look2(prmAction, prmUser, prmField, prmCondition, prmText, prmitem, ref dsTrnsFgBintgLook);
        AppServerDisconnect();
        return dsTrnsFgBintgLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsreturntaglookDataSet SelectretrntakLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmitmtype, string prmcurval)
    {
        dsreturntaglookDataSet dsreturntaglook = new dsreturntaglookDataSet();
        dsreturntaglook = null;
        AppServerConnect();
        aoObject.retrntaglook(prmAction, prmUser, prmField, prmCondition, prmText, prmitmtype, prmcurval, ref dsreturntaglook);
        AppServerDisconnect();
        return dsreturntaglook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPrintPoReportDataSet SelectPrintPoReport(string prmUser, string prmprtpo, Int32 prmbegpo, Int32 prmendpo, string prmbegvend, string prmendvend, string prmreprt, string prmreprtcl, string prmdelete, string prmprttrm, string prmspec, string prmcorr, string prmgrpnts, string prmsmmritm, string prmitmdsr, string prmscrtyp, string prmmetric, string prmprtprice, string prmOut, string prmysno)
    {
        string cError = "";
        dsPrintPoReportDataSet dsPrintPoReport = new dsPrintPoReportDataSet();
        dsPrintPoReport = null;
        AppServerConnect();
        aoObject.printpo(prmUser, prmprtpo, prmbegpo, prmendpo, prmbegvend, prmendvend, prmreprt, prmreprtcl, prmdelete, prmprttrm, prmspec, prmcorr, prmgrpnts, prmsmmritm, prmitmdsr, prmscrtyp, prmmetric, prmprtprice, prmOut, prmysno, out cError, ref dsPrintPoReport);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
        }
        return dsPrintPoReport;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTopAttachpoitemDataSet SelectAttachpo(string prmAction, string prmUser, string prmRecKey, string prmAttFile, string prmPoNo, string prmFgitem, string prmDate, string prmOpenWith, string prmSerchEst)
    {
        string cError = "";
        dsTopAttachpoitemDataSet dsTopAttachpoitem = new dsTopAttachpoitemDataSet();
        dsTopAttachpoitem = null;
        AppServerConnect();
        aoObject.TopAttachpo(prmAction, prmUser, prmRecKey, prmAttFile, prmPoNo, prmFgitem, prmDate, prmOpenWith, prmSerchEst, ref dsTopAttachpoitem, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsTopAttachpoitem;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUserManTableDataSet SelectUserManTable(string prmAction, string prmComp, string prmUser, string prmCondition, string prmText, string prmsman, string prmsname, string prmsdefault, string prmReckey)
    {
        string cError = "";
        dsUserManTableDataSet dsUserManTable = new dsUserManTableDataSet();
        dsUserManTable = null;
        AppServerConnect();
        aoObject.userman(prmAction, prmComp, prmUser, prmCondition, prmText, prmsman, prmsname, prmsdefault, prmReckey, ref dsUserManTable, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsUserManTable;
    }
                
}

