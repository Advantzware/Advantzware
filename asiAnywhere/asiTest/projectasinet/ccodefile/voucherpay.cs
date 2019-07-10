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
public class voucherpay:AppServerConnect.AppServer
{
    public voucherpay()
	{
		//
		// TODO: Add constructor logic here
		//
	}

   
    

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public bool ValidateListPo(string prmUser, string prmAction, Int32 prmPoNo, string prmPoDate, string prmType, string prmStat, string prmVendNo, string prmVendName, string prmVendAdd1, string prmVendAdd2, string prmVendCity, string prmVendState, string prmVendZip, string prmVendAreaCode, string prmVendPhone, string prmShipId, string prmShipName, string prmShipAddr, string prmShipCity, string prmShipState, string prmShipZip, string prmShipAreaCode, string prmShipPhone, string prmBuyer, string prmContact, string prmDueDate, string prmLastShipDate, Int32 prmUnderPct, Int32 prmOverPct, string prmCarrier, string prmTaxGr, string prmTerms, string prmFrtPay, string prmFobCode, Int32 prmTFreight, Int32 prmTax, Int32 prmTCost, string prmRecKey)
    //{
    //    string cError = "";
    //    dsViewPurOrdDataSet dsViewPurOrd = new dsViewPurOrdDataSet();
    //    AppServerConnect();
    //    aoObject.viewpord(prmUser, prmAction, prmPoNo, prmPoDate, prmType, prmStat, prmVendNo, prmVendName, prmVendAdd1, prmVendAdd2, prmVendCity, prmVendState, prmVendZip, prmVendAreaCode, prmVendPhone, prmShipId, prmShipName, prmShipAddr, prmShipCity, prmShipState, prmShipZip, prmShipAreaCode, prmShipPhone, prmBuyer, prmContact, prmDueDate, prmLastShipDate, prmUnderPct, prmOverPct, prmCarrier, prmTaxGr, prmTerms, prmFrtPay, prmFobCode, prmTFreight, prmTax, prmTCost, prmRecKey, out cError, ref dsViewPurOrd);
    //    AppServerDisconnect();
    //    if (cError != "")
    //    {
    //        HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
    //        return false;
    //    }
    //    else
    //    {
    //        return true;
    //    }

    //}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVendorinvoiceslistDataSet SelectVendor(string prmAction, string prmComp, string prmUser, string prmvend, string prmInv, string prmPosted, string prmunPosted, string prmvendname, string prmInvdate, decimal prmnet, decimal prmPaid, decimal prmBaldue, decimal prmTaxamt, decimal prmDue, string prmDuedate, string prmTaxcode, decimal prmDiscount, Int32 prmDiscdays, string prmCurrcode, decimal prmExrate, string prmMnlchac, string prmTxovrwrt, decimal prmFreight, string prmReckey)
    {
        string cError = "";
        dsVendorinvoiceslistDataSet dsVendorinvoiceslist = new dsVendorinvoiceslistDataSet();
        dsVendorinvoiceslist = null;
        AppServerConnect();
        aoObject.vendinv_list(prmAction,  prmComp,  prmUser,  prmvend,  prmInv,  prmPosted,  prmunPosted,  prmvendname,  prmInvdate,  prmnet,  prmPaid,  prmBaldue,  prmTaxamt,  prmDue,  prmDuedate,  prmTaxcode,  prmDiscount,  prmDiscdays,  prmCurrcode,  prmExrate,  prmMnlchac,  prmTxovrwrt,  prmFreight,  prmReckey, ref dsVendorinvoiceslist,out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVendorinvoiceslist;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateUpdateVendor(string prmAction, string prmComp, string prmUser, string prmvend, string prmInv, string prmPosted, string prmunPosted, string prmvendname, string prmInvdate, decimal prmnet, decimal prmPaid, decimal prmBaldue, decimal prmTaxamt, decimal prmDue, string prmDuedate, string prmTaxcode, decimal prmDiscount, Int32 prmDiscdays, string prmCurrcode, decimal prmExrate, string prmMnlchac, string prmTxovrwrt, decimal prmFreight, string prmReckey)
    {
        string cError = "";
        dsVendorinvoiceslistDataSet dsVendorinvoiceslist = new dsVendorinvoiceslistDataSet();
        AppServerConnect();
        aoObject.vendinv_list(prmAction, prmComp, prmUser, prmvend, prmInv, prmPosted, prmunPosted, prmvendname, prmInvdate, prmnet, prmPaid, prmBaldue, prmTaxamt, prmDue, prmDuedate, prmTaxcode, prmDiscount, prmDiscdays, prmCurrcode, prmExrate, prmMnlchac, prmTxovrwrt, prmFreight, prmReckey, ref dsVendorinvoiceslist, out cError);
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
    public dsVendApViewInvlLineDataSet SelectViewVendor(string prmAction, string prmComp, string prmUser, string prmInv, Int32 prmPono, Int32 prmLine, string prmActnum, string prmActdscr, string prmIno, string prmIdscr, decimal prmInvqty, string prmConsuom, decimal prmUnitPrice, string prmQtyUomPri, string prmTax, decimal prmSqft, decimal prmAmt, decimal prmTotlmsf, string prmJob, Int32 prmSnum, Int32 prmBnum, string prmReckey)
    {
        string cError = "";
        dsVendApViewInvlLineDataSet dsVendApViewInvlLine = new dsVendApViewInvlLineDataSet();
        dsVendApViewInvlLine = null;
        AppServerConnect();
        aoObject.vendview_Inv(prmAction, prmComp, prmUser, prmInv, prmPono, prmLine, prmActnum, prmActdscr, prmIno, prmIdscr, prmInvqty, prmConsuom, prmUnitPrice, prmQtyUomPri, prmTax, prmSqft, prmAmt, prmTotlmsf, prmJob, prmSnum, prmBnum, prmReckey, ref dsVendApViewInvlLine, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVendApViewInvlLine;
    }
   
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateViewVendor(string prmAction, string prmComp, string prmUser, string prmInv, Int32 prmPono, Int32 prmLine, string prmActnum, string prmActdscr, string prmIno, string prmIdscr, decimal prmInvqty, string prmConsuom, decimal prmUnitPrice, string prmQtyUomPri, string prmTax, decimal prmSqft, decimal prmAmt, decimal prmTotlmsf, string prmJob, Int32 prmSnum, Int32 prmBnum, string prmReckey)
    {
        string cError = "";
        dsVendApViewInvlLineDataSet dsVendApViewInvlLine = new dsVendApViewInvlLineDataSet();
        dsVendApViewInvlLine = null;
        AppServerConnect();
        aoObject.vendview_Inv(prmAction, prmComp, prmUser, prmInv, prmPono, prmLine, prmActnum, prmActdscr, prmIno, prmIdscr, prmInvqty, prmConsuom, prmUnitPrice, prmQtyUomPri, prmTax, prmSqft, prmAmt, prmTotlmsf, prmJob, prmSnum, prmBnum, prmReckey, ref dsVendApViewInvlLine, out cError);
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
    public dsPoPriceHistoryLookDataSet SelectPriceHistoryVendor(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, Int32 prmPono, String prmext)
    {
        string cError = "";
        dsPoPriceHistoryLookDataSet dsPoPriceHistoryLook = new dsPoPriceHistoryLookDataSet();
        dsPoPriceHistoryLook = null;
        AppServerConnect();
        aoObject.PoprhstryLook(prmAction, prmUser, prmField, prmCondition, prmText, prmPono, prmext, ref dsPoPriceHistoryLook);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsPoPriceHistoryLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPoPriceItemLook2DataSet SelectPriceItemVendor2(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, Int32 prmPono, string prmslect, Int32 prmrdqty)
    {
        string cError = "";
        dsPoPriceItemLook2DataSet dsPoPriceItemLook2 = new dsPoPriceItemLook2DataSet();
        dsPoPriceItemLook2 = null;
        AppServerConnect();
        aoObject.PopriceLook2(prmAction, prmUser, prmField, prmCondition, prmText, prmPono, prmslect, prmrdqty, out cError, ref dsPoPriceItemLook2);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsPoPriceItemLook2;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRecurVendorinvlistDataSet SelectRecrApVen(string prmAction, string prmComp, string prmUser, string prmvend, string prmInv, string prmPosted, string prmunPosted, string prmvendname, decimal prmnet, string prmFreqcode, string prmReckey)
    {
        string cError = "";
        dsRecurVendorinvlistDataSet dsRecurVendorinvlist = new dsRecurVendorinvlistDataSet();
        dsRecurVendorinvlist = null;
        AppServerConnect();
        aoObject.RecrApInv_list(prmAction, prmComp, prmUser, prmvend, prmInv, prmPosted, prmunPosted, prmvendname, prmnet, prmFreqcode, prmReckey, ref dsRecurVendorinvlist, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsRecurVendorinvlist;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVendInvPostDataSet SelectVendInvPost(string prmUser, string prmvendpost, string prmBeginvend, string prmEndvend, string prmBeginUsr, string prmEndUsr, string prmBegindate, string prmEnddate, string prmPstDate, string prmGlActNm, Int32 prmperiod, string prmOut, string prmGl2)
    {
        string cError = "";
        dsVendInvPostDataSet dsVendInvPost = new dsVendInvPostDataSet();
        dsVendInvPost = null;
        AppServerConnect();
        aoObject.vendinvspost(prmUser, prmvendpost, prmBeginvend, prmEndvend, prmBeginUsr, prmEndUsr, prmBegindate, prmEnddate, prmPstDate, prmGlActNm, prmperiod, prmOut, prmGl2, out cError, ref dsVendInvPost);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
           // HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVendInvPost;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVendorTotalDataSet SelectVendTotal(string prmAction, string prmUser, string prmReckey, string prmvend, string prmvendname, Decimal prmpurch, Decimal prmlst_yr, Decimal prmytd_msf, Decimal prmlyytd_msf, Decimal prmhibal, string prmhibal_date, Int32 prmnum_inv, Decimal prmlpay, string prmlpay_date, Int32 prmAVG_pay, Decimal prmacc_bal, Decimal prmpurchase, Decimal prmtot_msf, Decimal prmordbal, string prmext)
    {
        string cError = "";
        dsVendorTotalDataSet dsVendorTotal = new dsVendorTotalDataSet();
        dsVendorTotal = null;
        AppServerConnect();
        aoObject.vendtotl(prmAction, prmUser, prmReckey, prmvend, prmvendname, prmpurch, prmlst_yr, prmytd_msf, prmlyytd_msf, prmhibal, prmhibal_date, prmnum_inv, prmlpay, prmlpay_date, prmAVG_pay, prmacc_bal, prmpurchase, prmtot_msf, prmordbal, prmext, ref dsVendorTotal, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVendorTotal;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVendorUpdateDataSet SelectVendorList(string prmAction, string prmUser, string prmReckey, string prmActive, string prmVendor, string prmName, string prmAdd1, string prmAdd2, string prmCity, string prmState, string prmZip, string prmCountry, string prmPostal, string prmTaxid, string prmRemit, string prmRadd1, string prmRadd2, string prmRcity, string prmRstate, string prmRzip, string prmRcountry, string prmRpostal, string prmCheckmemo, string prmType, string prmContact, string prmBuyer, string prmAreacode, string prmPhone, string prmFaxarea, string prmFax, string prmFaxprefix, string prmFaxcountry, decimal prmOverpct, decimal prmUnderpct, string prmActnum, string prmCurrcode, string prmTaxgr, string prmCode1099, string prmAnedivend, string prmTerms, decimal prmDisc, decimal prmRebate, string prmFrtpay, Int32 prmDiscdays, string prmCarrier, string prmFobcode, string prmTtypedscr, string prmBuyerdscr, string prmTermsdscr, string prmCarrierdscr, string prmCurrdscr, string prmActdscr, string prmPoexport)
    {
        string cError = "";
        dsVendorUpdateDataSet dsVendorUpdate = new dsVendorUpdateDataSet();
        dsVendorUpdate = null;
        AppServerConnect();
        aoObject.vendupdate(prmAction, prmUser, prmReckey, prmActive, prmVendor, prmName, prmAdd1, prmAdd2, prmCity, prmState, prmZip, prmCountry, prmPostal, prmTaxid, prmRemit, prmRadd1, prmRadd2, prmRcity, prmRstate, prmRzip, prmRcountry, prmRpostal, prmCheckmemo, prmType, prmContact, prmBuyer, prmAreacode, prmPhone, prmFaxarea, prmFax, prmFaxprefix, prmFaxcountry, prmOverpct, prmUnderpct, prmActnum, prmCurrcode, prmTaxgr, prmCode1099, prmAnedivend, prmTerms, prmDisc, prmRebate, prmFrtpay, prmDiscdays, prmCarrier, prmFobcode, prmTtypedscr, prmBuyerdscr, prmTermsdscr, prmCarrierdscr, prmCurrdscr, prmActdscr, prmPoexport, ref dsVendorUpdate, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVendorUpdate;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSysParamCharValLookDataSet SelectSysPramChar(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        string cError = "";
        dsSysParamCharValLookDataSet dsSysParamCharValLook = new dsSysParamCharValLookDataSet();
        dsSysParamCharValLook = null;
        AppServerConnect();
        aoObject.syspramchrlook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsSysParamCharValLook);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsSysParamCharValLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVendorTypeLookupDataSet SelectVendorTypeLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        string cError = "";
        dsVendorTypeLookupDataSet dsVendorTypeLookup = new dsVendorTypeLookupDataSet();
        dsVendorTypeLookup = null;
        AppServerConnect();
        aoObject.vendtypelook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsVendorTypeLookup);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVendorTypeLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPaymentTermsDataSet SelectPaymentTerms(string prmAction, string prmUser, string prmReckey, string prmtcode, string prmdscr, decimal prmdisc_rate, Int32 prmdisc_dys, Int32 prmnet_dys, Int32 prmCUT_date, string prmvtype, string prmcod)
    {
        string cError = "";
        dsPaymentTermsDataSet dsPaymentTerms = new dsPaymentTermsDataSet();
        dsPaymentTerms = null;
        AppServerConnect();
        aoObject.payterms(prmAction, prmUser, prmReckey, prmtcode, prmdscr, prmdisc_rate, prmdisc_dys, prmnet_dys, prmCUT_date, prmvtype, prmcod, ref dsPaymentTerms, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsPaymentTerms;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsSalesTaxCodeListDataSet SelectSalesTaxCode(string prmAction, string prmComp, string prmUser, string prmtaxgrp, string prmcode1, string prmdscr1, Decimal prmrate1, string prmaccl, string prmfrt1, string prmcode2, string prmdscr2, Decimal prmrate2, string prmacc2, string prmfrt2, string prmcode3, string prmdscr3, Decimal prmrate3, string prmacc3, string prmfrt3, string prmcode4, string prmdscr4, Decimal prmrate4, string prmacc4, string prmfrt4, string prmcode5, string prmdscr5, Decimal prmrate5, string prmacc5, string prmfrt5, string prmtax, string prmReckey)
    {
        string cError = "";
        dsSalesTaxCodeListDataSet dsSalesTaxCodeList = new dsSalesTaxCodeListDataSet();
        dsSalesTaxCodeList = null;
        AppServerConnect();
        aoObject.salestax_list(prmAction, prmComp, prmUser, prmtaxgrp, prmcode1, prmdscr1, prmrate1, prmaccl, prmfrt1, prmcode2, prmdscr2, prmrate2, prmacc2, prmfrt2, prmcode3, prmdscr3, prmrate3, prmacc3, prmfrt3, prmcode4, prmdscr4, prmrate4, prmacc4, prmfrt4, prmcode5, prmdscr5, prmrate5, prmacc5, prmfrt5, prmtax, prmReckey, ref dsSalesTaxCodeList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsSalesTaxCodeList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVendTypeListDataSet SelectVendTypeList(string prmAction, string prmComp, string prmUser, string prmvtype, string prmvdscr, string prmReckey)
    {
        string cError = "";
        dsVendTypeListDataSet dsVendTypeList = new dsVendTypeListDataSet();
        dsVendTypeList = null;
        AppServerConnect();
        aoObject.vendtype_list(prmAction, prmComp, prmUser, prmvtype, prmvdscr, prmReckey, ref dsVendTypeList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVendTypeList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsNewBuyersListDataSet SelectNewBuyersList(string prmAction, string prmComp, string prmUser, string prmvbuyer, string prmvnum, string prmReckey)
    {
        string cError = "";
        dsNewBuyersListDataSet dsNewBuyersList = new dsNewBuyersListDataSet();
        dsNewBuyersList = null;
        AppServerConnect();
        aoObject.buyers_list(prmAction, prmComp, prmUser, prmvbuyer, prmvnum, prmReckey, ref dsNewBuyersList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsNewBuyersList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsAPControlViewDataSet SelectAPControl(string prmAction, string prmComp, string prmUser, string prmpayable, string prmpurch, string prmcashact, string prmdisc, string prmstax, string prmfreight, string prmReckey)
    {
        string cError = "";
        dsAPControlViewDataSet dsAPControlView = new dsAPControlViewDataSet();
        dsAPControlView = null;
        AppServerConnect();
        aoObject.apctrl_view(prmAction, prmComp, prmUser, prmpayable, prmpurch, prmcashact, prmdisc, prmstax, prmfreight, prmReckey, ref dsAPControlView, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsAPControlView;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, false)]
    public dsttTaxGroupLookDataSet SelectTaxGroupLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {

        dsttTaxGroupLookDataSet dsttTaxGroupLook = new dsttTaxGroupLookDataSet();
        dsttTaxGroupLook = null;
        AppServerConnect();
        aoObject.TaxGroupLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsttTaxGroupLook);
        AppServerDisconnect();
        return dsttTaxGroupLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsDbCrMemoListDataSet SelectDbCrMemo(string prmAction, string prmComp, string prmUser, string prmvend, string prmvname, Int32 prmcheckno, string prmcheckdate, decimal prmamt, decimal prmcheckamt, string prmReckey)
    {
        string cError = "";
        dsDbCrMemoListDataSet dsDbCrMemoList = new dsDbCrMemoListDataSet();
        dsDbCrMemoList = null;
        AppServerConnect();
        aoObject.dbcrmemo(prmAction, prmComp, prmUser, prmvend, prmvname, prmcheckno, prmcheckdate, prmamt, prmcheckamt, prmReckey, ref dsDbCrMemoList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsDbCrMemoList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsDebitCreditItemDataSet SelectViewDbCrMemo(string prmAction, string prmComp, string prmUser, string prminvno, string prmduedate, Decimal prmamtdue, Decimal prmamtpaid, decimal prmamtdisc, string prmactnum, string prmactdscr, string prmReckey, string prmmemo, string prmapmain)
    {
        string cError = "";
        dsDebitCreditItemDataSet dsDebitCreditItem = new dsDebitCreditItemDataSet();
        dsDebitCreditItem = null;
        AppServerConnect();
        aoObject.dbmemo(prmAction, prmComp, prmUser, prminvno, prmduedate, prmamtdue, prmamtpaid, prmamtdisc, prmactnum, prmactdscr, prmReckey, prmmemo, prmapmain, ref dsDebitCreditItem, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsDebitCreditItem;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool  ValidateViewDbCrMemo(string prmAction, string prmComp, string prmUser, string prminvno, string prmduedate, Decimal prmamtdue, Decimal prmamtpaid, decimal prmamtdisc, string prmactnum, string prmactdscr, string prmReckey, string prmmemo, string prmapmain)
    {
        string cError = "";
        dsDebitCreditItemDataSet dsDebitCreditItem = new dsDebitCreditItemDataSet();
        dsDebitCreditItem = null;
        AppServerConnect();
        aoObject.dbmemo(prmAction, prmComp, prmUser, prminvno, prmduedate, prmamtdue, prmamtpaid, prmamtdisc, prmactnum, prmactdscr, prmReckey, prmmemo, prmapmain, ref dsDebitCreditItem, out cError);
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
    public bool ValidateDbCrMemo(string prmAction, string prmComp, string prmUser, string prmvend, string prmvname, Int32 prmcheckno, string prmcheckdate, decimal prmamt, decimal prmcheckamt, string prmReckey)
    {
        string cError = "";
        dsDbCrMemoListDataSet dsDbCrMemoList = new dsDbCrMemoListDataSet();
        dsDbCrMemoList = null;
        AppServerConnect();
        aoObject.dbcrmemo(prmAction, prmComp, prmUser, prmvend, prmvname, prmcheckno, prmcheckdate, prmamt, prmcheckamt, prmReckey, ref dsDbCrMemoList, out cError);
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
    public dsInvoiceInfoLookupDataSet SelectInvoiceInfolook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmvend)
    {
        string cError = "";
        dsInvoiceInfoLookupDataSet dsInvoiceInfoLookup = new dsInvoiceInfoLookupDataSet();
        dsInvoiceInfoLookup = null;
        AppServerConnect();
        aoObject.invinfolook(prmAction, prmUser, prmField, prmCondition, prmText, prmvend, ref dsInvoiceInfoLookup);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsInvoiceInfoLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsDebitCreditRegisterDataSet SelectDbCrRegisterPost( string prmUser, string prmvendpost, string prmBegindate, string prmEnddate, string prmPstDate, Int32 prmperiod, string prmOut)
    {
        string cError = "";
        dsDebitCreditRegisterDataSet dsDebitCreditRegister = new dsDebitCreditRegisterDataSet();
        dsDebitCreditRegister = null;
        AppServerConnect();
        aoObject.dbcrregtr(prmUser, prmvendpost, prmBegindate, prmEnddate, prmPstDate, prmperiod, prmOut, out cError, ref dsDebitCreditRegister);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");           
        }
        return dsDebitCreditRegister;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBManChkListDataSet SelectManCheck(string prmAction, string prmComp, string prmUser, string prmvendno, string prmbankcode, Int32 prmcheckno, string prmcheckdate, decimal prmcheckamt, string prmreckey, string prmmanchk)
    {
        string cError = "";
        dsBManChkListDataSet dsBManChkList = new dsBManChkListDataSet();
        dsBManChkList = null;
        AppServerConnect();
        aoObject.manchk(prmAction, prmComp, prmUser, prmvendno, prmbankcode, prmcheckno, prmcheckdate, prmcheckamt, prmreckey, prmmanchk, ref dsBManChkList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsBManChkList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateManCheck(string prmAction, string prmComp, string prmUser, string prmvendno, string prmbankcode, Int32 prmcheckno, string prmcheckdate, decimal prmcheckamt, string prmreckey, string prmmanchk)
    {
        string cError = "";
        dsBManChkListDataSet dsBManChkList = new dsBManChkListDataSet();
        dsBManChkList = null;
        AppServerConnect();
        aoObject.manchk(prmAction, prmComp, prmUser, prmvendno, prmbankcode, prmcheckno, prmcheckdate, prmcheckamt, prmreckey, prmmanchk, ref dsBManChkList, out cError);
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
    public dsManSalApChkItemDataSet SelectManSel(string prmAction, string prmComp, string prmUser, string prminvno, string prmduedate, decimal prmamtdue, decimal prmamtpaid, decimal prmamtdisc, string prmreckey, string prmchkno, string prmapsal)
    {
        string cError = "";
        dsManSalApChkItemDataSet dsManSalApChkItem = new dsManSalApChkItemDataSet();
        dsManSalApChkItem = null;
        AppServerConnect();
        aoObject.mansel(prmAction, prmComp, prmUser, prminvno, prmduedate, prmamtdue, prmamtpaid, prmamtdisc, prmreckey, prmchkno, prmapsal, ref dsManSalApChkItem, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsManSalApChkItem;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateManSel(string prmAction, string prmComp, string prmUser, string prminvno, string prmduedate, decimal prmamtdue, decimal prmamtpaid, decimal prmamtdisc, string prmreckey, string prmchkno, string prmapsal)
    {
        string cError = "";
        dsManSalApChkItemDataSet dsManSalApChkItem = new dsManSalApChkItemDataSet();
        dsManSalApChkItem = null;
        AppServerConnect();
        aoObject.mansel(prmAction, prmComp, prmUser, prminvno, prmduedate, prmamtdue, prmamtpaid, prmamtdisc, prmreckey, prmchkno, prmapsal, ref dsManSalApChkItem, out cError);
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
    public dsPrintAPCheckDataSet SelectPrintAPChecks(string prmUser, string prmapchek, string prmChekdate, string prmbnkcode, string prmbnkname, Int32 prmstrtchek, string prmBegVend, string prmEndVend, string prmOut)
    {
        string cError = "";
        dsPrintAPCheckDataSet dsPrintAPCheck = new dsPrintAPCheckDataSet();
        dsPrintAPCheck = null;
        AppServerConnect();
        aoObject.prntapchk(prmUser, prmapchek, prmChekdate, prmbnkcode, prmbnkname, prmstrtchek, prmBegVend, prmEndVend, prmOut, out cError, ref dsPrintAPCheck);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsPrintAPCheck;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVoidedCheckRegisterDataSet SelectAPVoidChkRegister(string prmUser, string prmvoidchk, string prmPstDate, Int32 prmperiod, string prmOut)
    {
        string cError = "";
        dsVoidedCheckRegisterDataSet dsVoidedCheckRegister = new dsVoidedCheckRegisterDataSet();
        dsVoidedCheckRegister = null;
        AppServerConnect();
        aoObject.voidchkreg(prmUser, prmvoidchk, prmPstDate, prmperiod, prmOut, out cError, ref dsVoidedCheckRegister);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
        }
        return dsVoidedCheckRegister;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateAPVoidChkRegister(string prmUser, string prmvoidchk, string prmPstDate, Int32 prmperiod, string prmOut)
    {
        string cError = "";
        dsVoidedCheckRegisterDataSet dsVoidedCheckRegister = new dsVoidedCheckRegisterDataSet();
        dsVoidedCheckRegister = null;
        AppServerConnect();
        aoObject.voidchkreg(prmUser, prmvoidchk, prmPstDate, prmperiod, prmOut, out cError, ref dsVoidedCheckRegister);
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
    public dsVoidApChecksDataSet SelectVoidAPCheck(string prmUser, string prmAction, string prmvend, Int32 prmchkno, string prmbnkcod, string prmchkdate, decimal prmchkamt, string prmmanchk, string prmvendname, string prmbnkname, string prmvoided, string prmRecKey)
    {
        string cError = "";
        dsVoidApChecksDataSet dsVoidApChecks = new dsVoidApChecksDataSet();
        dsVoidApChecks = null;
        AppServerConnect();
        aoObject.voidapchk(prmUser, prmAction, prmvend, prmchkno, prmbnkcod, prmchkdate, prmchkamt, prmmanchk, prmvendname, prmbnkname, prmvoided, prmRecKey, out cError, ref dsVoidApChecks);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsVoidApChecks;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBankLookupDataSet SelectBankLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        string cError = "";
        dsBankLookupDataSet dsBankLookup = new dsBankLookupDataSet();
        dsBankLookup = null;
        AppServerConnect();
        aoObject.banklook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsBankLookup);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsBankLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCheckRegisterDataSet SelectAPChkRegister(string prmUser, string prmchkreg, string prmPstDate, string prmvoidskip, Int32 prmperiod, string prmprtacc, string prmchkfile, string prmapchkfile, string prmpstmnl, string prmOut, string prmpost)
    {
        string cError = "";
        dsCheckRegisterDataSet dsCheckRegister = new dsCheckRegisterDataSet();
        dsCheckRegister = null;
        AppServerConnect();
        aoObject.chkregstr(prmUser, prmchkreg, prmPstDate, prmvoidskip, prmperiod, prmprtacc, prmchkfile, prmapchkfile, prmpstmnl, prmOut, prmpost, out cError, ref dsCheckRegister);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
        }
        return dsCheckRegister;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validateAPChkRegister(string prmUser, string prmchkreg, string prmPstDate, string prmvoidskip, Int32 prmperiod, string prmprtacc, string prmchkfile, string prmapchkfile, string prmpstmnl, string prmOut, string prmpost)
    {
        string cError = "";
        dsCheckRegisterDataSet dsCheckRegister = new dsCheckRegisterDataSet();
        dsCheckRegister = null;
        AppServerConnect();
        aoObject.chkregstr(prmUser, prmchkreg, prmPstDate, prmvoidskip, prmperiod, prmprtacc, prmchkfile, prmapchkfile, prmpstmnl, prmOut, prmpost, out cError, ref dsCheckRegister);
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
    public dsBankReconciliationDataSet SelectBankReconciliation(string prmAction, string prmComp, string prmUser, string prmnumber, string prmvdate, decimal prmamt, Int32 prmbank, string prmvend, string prmvendname, string prmcleared, string prmreckey)
    {
        string cError = "";
        dsBankReconciliationDataSet dsBankReconciliation = new dsBankReconciliationDataSet();
        dsBankReconciliation = null;
        AppServerConnect();
        aoObject.bankrecon(prmAction, prmComp, prmUser, prmnumber, prmvdate, prmamt, prmbank, prmvend, prmvendname, prmcleared, prmreckey, ref dsBankReconciliation, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
        return dsBankReconciliation;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsReconciliationRegisterDataSet SelectReconciliationRegister(string prmUser, string prmrecreg, string prmendDate, string prmbnkcod, string prmOut)
    {
        string cError = "";
        dsReconciliationRegisterDataSet dsReconciliationRegister = new dsReconciliationRegisterDataSet();
        dsReconciliationRegister = null;
        AppServerConnect();
        aoObject.recreg(prmUser, prmrecreg, prmendDate, prmbnkcod, prmOut, out cError, ref dsReconciliationRegister);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
        }
        return dsReconciliationRegister;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateReconciliationRegister(string prmUser, string prmrecreg, string prmendDate, string prmbnkcod, string prmOut)
    {
        string cError = "";
        dsReconciliationRegisterDataSet dsReconciliationRegister = new dsReconciliationRegisterDataSet();
        dsReconciliationRegister = null;
        AppServerConnect();
        aoObject.recreg(prmUser, prmrecreg, prmendDate, prmbnkcod, prmOut, out cError, ref dsReconciliationRegister);
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
    public dsReconciliationReportDataSet SelectReconciliationReport(string prmUser, string prmrecrpt, string prmbnkcod, string prmbegvend, string prmendvend, string prmbegchkdt, string prmendchkdt, Int32 prmbegchk, Int32 prmendchk, Int32 prmbegjno, Int32 prmendjno, string prmdep, string prmjournl, string prmprint, string prmclrunrec, string prmunclrunrec, string prmsrtvend, string prmextra, string prmOut)
    {
        string cError = "";
        dsReconciliationReportDataSet dsReconciliationReport = new dsReconciliationReportDataSet();
        dsReconciliationReport = null;
        AppServerConnect();
        aoObject.recreport(prmUser, prmrecrpt, prmbnkcod, prmbegvend, prmendvend, prmbegchkdt, prmendchkdt, prmbegchk, prmendchk, prmbegjno, prmendjno, prmdep, prmjournl, prmprint, prmclrunrec, prmunclrunrec, prmsrtvend, prmextra, prmOut, out cError, ref dsReconciliationReport);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
        }
        return dsReconciliationReport;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsBankReconciliationDataSet SelectBankRecon(string prmAction, string prmComp, string prmUser, string prmnumber, string prmvdate, decimal prmamt, Int32 prmbank, string prmvend, string prmvendname, string prmcleared, string prmreckey)
    {
        string cError = "";
        dsBankReconciliationDataSet dsBankReconciliation = new dsBankReconciliationDataSet();
        dsBankReconciliation = null;
        AppServerConnect();
        aoObject.bankrecon(prmAction, prmComp, prmUser, prmnumber, prmvdate, prmamt, prmbank, prmvend, prmvendname, prmcleared, prmreckey, ref dsBankReconciliation, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
        }
        return dsBankReconciliation;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsDisbursementsDataSet SelectDisbursements(string prmAction, string prmComp, string prmUser, string prmvend, string prmvname, Int32 prmcheckno, string prmcheckdate, string prmpayee, decimal prmcheckamt, string prmbnkcod, string prmbnkname, string prmcurrcod, decimal prmexrat, string prmreckey)
    {
        string cError = "";
        dsDisbursementsDataSet dsDisbursements = new dsDisbursementsDataSet();
        dsDisbursements = null;
        AppServerConnect();
        aoObject.listdisbr(prmAction, prmComp, prmUser, prmvend, prmvname, prmcheckno, prmcheckdate, prmpayee, prmcheckamt, prmbnkcod, prmbnkname, prmcurrcod, prmexrat, prmreckey, ref dsDisbursements, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsDisbursements;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateDisbursements(string prmAction, string prmComp, string prmUser, string prmvend, string prmvname, Int32 prmcheckno, string prmcheckdate, string prmpayee, decimal prmcheckamt, string prmbnkcod, string prmbnkname, string prmcurrcod, decimal prmexrat, string prmreckey)
    {
        string cError = "";
        dsDisbursementsDataSet dsDisbursements = new dsDisbursementsDataSet();
        dsDisbursements = null;
        AppServerConnect();
        aoObject.listdisbr(prmAction, prmComp, prmUser, prmvend, prmvname, prmcheckno, prmcheckdate, prmpayee, prmcheckamt, prmbnkcod, prmbnkname, prmcurrcod, prmexrat, prmreckey, ref dsDisbursements, out cError);
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
    public dsViewDisbursementsDataSet SelectViewDisbursements(string prmAction, string prmComp, string prmUser, Int32 prmvline, string prmdscr, string prmactnum, decimal prmqty, decimal prmuntprice, decimal prmamt, string prmreckey, string prmchkno)
    {
        string cError = "";
        dsViewDisbursementsDataSet dsViewDisbursements = new dsViewDisbursementsDataSet();
        dsViewDisbursements = null;
        AppServerConnect();
        aoObject.viewdisbr(prmAction, prmComp, prmUser, prmvline, prmdscr, prmactnum, prmqty, prmuntprice, prmamt, prmreckey, prmchkno, ref dsViewDisbursements, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsViewDisbursements;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateViewDisbursements(string prmAction, string prmComp, string prmUser, Int32 prmvline, string prmdscr, string prmactnum, decimal prmqty, decimal prmuntprice, decimal prmamt, string prmreckey, string prmchkno)
    {
        string cError = "";
        dsViewDisbursementsDataSet dsViewDisbursements = new dsViewDisbursementsDataSet();
        dsViewDisbursements = null;
        AppServerConnect();
        aoObject.viewdisbr(prmAction, prmComp, prmUser, prmvline, prmdscr, prmactnum, prmqty, prmuntprice, prmamt, prmreckey, prmchkno, ref dsViewDisbursements, out cError);
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
    public dsDisbursementRegisterDataSet SelectDisbursementRegister(string prmUser, string prmapdis, string prmpstdate, string prmbegdate, string prmenddate, string prmvndseq, Int32 prmperiod, string prmOut)
    {
        string cError = "";
        dsDisbursementRegisterDataSet dsDisbursementRegister = new dsDisbursementRegisterDataSet();
        dsDisbursementRegister = null;
        AppServerConnect();
        aoObject.prntapdis(prmUser, prmapdis, prmpstdate, prmbegdate, prmenddate, prmvndseq, prmperiod, prmOut, out cError, ref dsDisbursementRegister);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsDisbursementRegister;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateDisbursementRegister(string prmUser, string prmapdis, string prmpstdate, string prmbegdate, string prmenddate, string prmvndseq, Int32 prmperiod, string prmOut)
    {
        string cError = "";
        dsDisbursementRegisterDataSet dsDisbursementRegister = new dsDisbursementRegisterDataSet();
        dsDisbursementRegister = null;
        AppServerConnect();
        aoObject.prntapdis(prmUser, prmapdis, prmpstdate, prmbegdate, prmenddate, prmvndseq, prmperiod, prmOut, out cError, ref dsDisbursementRegister);
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

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsCheckRegisterImportDataSet SelectCheckRegisterImport(string prmUser, string prmapimp, string prmpayrol, string prmimpfile, string prmOut)
    //{
    //    string cError = "";
    //    dsCheckRegisterImportDataSet dsCheckRegisterImport = new dsCheckRegisterImportDataSet();
    //    dsCheckRegisterImport = null;
    //    AppServerConnect();
    //    aoObject.apimp(prmUser, prmapimp, prmpayrol, prmimpfile, prmOut, out cError, ref dsCheckRegisterImport);
    //    AppServerDisconnect();
    //    if (cError != "")
    //    {
    //        HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

    //    }
    //    return dsCheckRegisterImport;
    //}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsInvoiceBalanceDataSet BalanceInvoice(string prmAction, string prmComp, string prmUser, string prmvend, string prmInv, string prmchkno, string prmtrnsdate, string prmdscr, decimal prmcredit, decimal prmdebit, decimal prmbalance, string prmpo, string prminvbal, string prmvendname, string prmbeginv, string prmendinv, Int32 prmupto, string prmvopen, string prmshrtby, string prmout, string prmcon, string prmReckey)
    {
        string cError = "";
        dsInvoiceBalanceDataSet dsInvoiceBalance = new dsInvoiceBalanceDataSet();
        dsInvoiceBalance = null;
        AppServerConnect();
        aoObject.invbal(prmAction, prmComp, prmUser, prmvend, prmInv, prmchkno, prmtrnsdate, prmdscr, prmcredit, prmdebit, prmbalance, prmpo, prminvbal, prmvendname, prmbeginv, prmendinv, prmupto, prmvopen, prmshrtby, prmout, prmcon, prmReckey, ref dsInvoiceBalance, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsInvoiceBalance;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsChecksInvoiceDataSet ChecksInvoice(string prmAction, string prmComp, string prmUser, string prminvno, Int32 prmchkno, string prmchkdate, string prmduedate, decimal prmgrosamt, decimal prmamtdisc, decimal prmamtpaid, string prmchkbal, string prmvend, string prmvendname, string prmbeginv, string prmendinv, Int32 prmbegcchk, Int32 prmendchk, string prmbegdate, string prmenddate, string prmshrtby, string prmout, string prmcon, string prmReckey)
    {
        string cError = "";
        dsChecksInvoiceDataSet dsChecksInvoice = new dsChecksInvoiceDataSet();
        dsChecksInvoice = null;
        AppServerConnect();
        aoObject.checkinv(prmAction, prmComp, prmUser, prminvno, prmchkno, prmchkdate, prmduedate, prmgrosamt, prmamtdisc, prmamtpaid, prmchkbal, prmvend, prmvendname, prmbeginv, prmendinv, prmbegcchk, prmendchk, prmbegdate, prmenddate, prmshrtby, prmout, prmcon, prmReckey, ref dsChecksInvoice, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsChecksInvoice;
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dscustprintreportDataSet TopPrintReport(string prmAction, string prmComp, string prmUser, string prmBegCust, string prmBegName, string prmEndCust, string prmEndName, string prmShiptoAdd, string prmSoldtoAdd, string prmTotal, string prmListOrder, string prmShowNote, string prmShowSelPar, string prmMisc, string prmShowAdd, string prmShowPhone, string prmReckey)
    {
        string cError = "";
        dscustprintreportDataSet dscustprintreport = new dscustprintreportDataSet();
        dscustprintreport = null;
        AppServerConnect();
        aoObject.custprintrep(prmAction, prmComp, prmUser, prmBegCust, prmBegName, prmEndCust, prmEndName, prmShiptoAdd, prmSoldtoAdd, prmTotal, prmListOrder, prmShowNote, prmShowSelPar, prmMisc, prmShowAdd, prmShowPhone, prmReckey, ref dscustprintreport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dscustprintreport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTopAttachpoinvoiceDataSet TopAttachpoinvoice(string prmAction, string prmUser, string prmRecKey, string prmAttFile, string prmPoNo, string prmFgitem, string prmDate, string prmOpenWith, string prmSerchEst)
    {
        string cError = "";
        dsTopAttachpoinvoiceDataSet dsTopAttachpoinvoice = new dsTopAttachpoinvoiceDataSet();
        dsTopAttachpoinvoice = null;
        AppServerConnect();
        aoObject.TopAttachinv(prmAction, prmUser, prmRecKey, prmAttFile, prmPoNo, prmFgitem, prmDate, prmOpenWith, prmSerchEst, ref dsTopAttachpoinvoice, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsTopAttachpoinvoice;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTopViewNoteinvoiceDataSet TopViewNoteinvoice(string prmAction, string prmUser, string prmRecKey, string prmHeader, DateTime prmNoteDate, string prmNoteTime, string prmUserId, string prmViewed, string prmDeptCode, string prmDeptName, Int32 prmForm, string prmNoteTitle, string prmNewNoteTitle, string prmNoteText, string prmEstimate, string prmType, string prmGroup)
    {
        string cError = "";
        dsTopViewNoteinvoiceDataSet dsTopViewNoteinvoice = new dsTopViewNoteinvoiceDataSet();
        dsTopViewNoteinvoice = null;
        AppServerConnect();
        aoObject.TopNotesInv(prmAction, prmUser, prmRecKey, prmHeader, prmNoteDate, prmNoteTime, prmUserId, prmViewed, prmDeptCode, prmDeptName, prmForm, prmNoteTitle, prmNewNoteTitle, prmNoteText, prmEstimate, prmType, prmGroup, ref dsTopViewNoteinvoice, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsTopViewNoteinvoice;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVendorAgingReportDataSet RepVendorAging(string prmUser, string prmAction, string prmOut, string prmBeginCompany, string prmEndCompany, string prmBeginVendor, string prmEndVendor, string prmBeginCurr, string prmEndCurr, string prmBeginType, string prmEndType, string prmAsof, string prmInvPost, string prmDay1, string prmDay2, string prmDay3, string prmDay4, string prmSort, string prmDetail)
    {
        string cError = "";
        dsVendorAgingReportDataSet dsVendorAgingReport = new dsVendorAgingReportDataSet();
        dsVendorAgingReport = null;
        AppServerConnect();
        aoObject.VendorAging(prmUser, prmAction, prmOut, prmBeginCompany, prmEndCompany, prmBeginVendor, prmEndVendor, prmBeginCurr, prmEndCurr, prmBeginType, prmEndType, prmAsof, prmInvPost, prmDay1, prmDay2, prmDay3, prmDay4, prmSort, prmDetail, ref dsVendorAgingReport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVendorAgingReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCashRequirementsDataSet CashRequirements(string prmUser, string prmcash, string prmbegcom, string prmendcom, string prmbegdt1, string prmbegdt2, string prmbegdt3, string prmshrtby, string prmdiscdt, string prmcompny, string prmOut)
    {
        string cError = "";
        dsCashRequirementsDataSet dsCashRequirements = new dsCashRequirementsDataSet();
        dsCashRequirements = null;
        AppServerConnect();
        aoObject.cashreq(prmUser, prmcash, prmbegcom, prmendcom, prmbegdt1, prmbegdt2, prmbegdt3, prmshrtby, prmdiscdt, prmcompny, prmOut, out cError, ref dsCashRequirements);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCashRequirements;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsAPVendorAnalysisDataSet APVendorAnalysis(string prmUser, string prmvenana, string prmbegcom, string prmendcom, string prmenddt, string prmOut)
    {
        string cError = "";
        dsAPVendorAnalysisDataSet dsAPVendorAnalysis = new dsAPVendorAnalysisDataSet();
        dsAPVendorAnalysis = null;
        AppServerConnect();
        aoObject.vendana(prmUser, prmvenana, prmbegcom, prmendcom, prmenddt, prmOut, out cError, ref dsAPVendorAnalysis);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsAPVendorAnalysis;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVendorMasterListDataSet VendorMasterList(string prmUser, string prmvenmstr, string prmbegvend, string prmbegtyp, string prmbegbuy, string prmendvend, string prmendtyp, string prmendbuy, string prmdetail, string prmOut)
    {
        string cError = "";
        dsVendorMasterListDataSet dsVendorMasterList = new dsVendorMasterListDataSet();
        dsVendorMasterList = null;
        AppServerConnect();
        aoObject.vendmstr(prmUser, prmvenmstr, prmbegvend, prmbegtyp, prmbegbuy, prmendvend, prmendtyp, prmendbuy, prmdetail, prmOut, out cError, ref dsVendorMasterList);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVendorMasterList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsAPMailingListDataSet APMailingList(string prmUser, string prmmail, string prmbegstat, string prmbegtyp, string prmbegbuy, string prmendstat, string prmendtyp, string prmendbuy, string prmactiv, string prmoutput, string prmOut)
    {
        string cError = "";
        dsAPMailingListDataSet dsAPMailingList = new dsAPMailingListDataSet();
        dsAPMailingList = null;
        AppServerConnect();
        aoObject.maillist(prmUser, prmmail, prmbegstat, prmbegtyp, prmbegbuy, prmendstat, prmendtyp, prmendbuy, prmactiv, prmoutput, prmOut, out cError, ref dsAPMailingList);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsAPMailingList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsMonthlyTaxDataSet MonthlyTax(string prmUser, string prmtaxmon, Int32 prmbegyr, Int32 prmperiod, string prmbegdt, string prmenddt, string prmOut)
    {
        string cError = "";
        dsMonthlyTaxDataSet dsMonthlyTax = new dsMonthlyTaxDataSet();
        dsMonthlyTax = null;
        AppServerConnect();
        aoObject.taxmonth(prmUser, prmtaxmon, prmbegyr, prmperiod, prmbegdt, prmenddt, prmOut, out cError, ref dsMonthlyTax);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsMonthlyTax;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTaxDistributionScheduleDataSet TaxDistributionSchedule(string prmUser, string prmtaxdis, Int32 prmbegyr, Int32 prmperiod, string prmbegdt, string prmenddt, string prmOut)
    {
        string cError = "";
        dsTaxDistributionScheduleDataSet dsTaxDistributionSchedule = new dsTaxDistributionScheduleDataSet();
        dsTaxDistributionSchedule = null;
        AppServerConnect();
        aoObject.taxdist(prmUser, prmtaxdis, prmbegyr, prmperiod, prmbegdt, prmenddt, prmOut, out cError, ref dsTaxDistributionSchedule);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsTaxDistributionSchedule;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsMtdDestinationSubtotalsDataSet MtdDestinationSubtotals(string prmUser, string prmmtdsub, Int32 prmbegyr, Int32 prmperiod, string prmbegdt, string prmenddt, string prmOut)
    {
        string cError = "";
        dsMtdDestinationSubtotalsDataSet dsMtdDestinationSubtotals = new dsMtdDestinationSubtotalsDataSet();
        dsMtdDestinationSubtotals = null;
        AppServerConnect();
        aoObject.mtdsub(prmUser, prmmtdsub, prmbegyr, prmperiod, prmbegdt, prmenddt, prmOut, out cError, ref dsMtdDestinationSubtotals);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsMtdDestinationSubtotals;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTaxScheduleByCustomerDataSet TaxScheduleByCustomer(string prmUser, string prmtaxcust, Int32 prmbegyr, Int32 prmperiod, string prmbegdt, string prmenddt, string prmOut)
    {
        string cError = "";
        dsTaxScheduleByCustomerDataSet dsTaxScheduleByCustomer = new dsTaxScheduleByCustomerDataSet();
        dsTaxScheduleByCustomer = null;
        AppServerConnect();
        aoObject.taxcust(prmUser, prmtaxcust, prmbegyr, prmperiod, prmbegdt, prmenddt, prmOut, out cError, ref dsTaxScheduleByCustomer);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsTaxScheduleByCustomer;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVendor1099ReportDataSet Vendor1099Report(string prmUser, string prmAction, string prmOut, string prmBeginVendor, string prmEndVendor, string prmBegdt, string prmEnddt, string prmrddate, string prmincld)
    {
        string cError = "";
        dsVendor1099ReportDataSet dsVendor1099Report = new dsVendor1099ReportDataSet();
        dsVendor1099Report = null;
        AppServerConnect();
        aoObject.ven1099(prmUser, prmAction, prmOut, prmBeginVendor, prmEndVendor, prmBegdt, prmEnddt, prmrddate, prmincld, ref dsVendor1099Report, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVendor1099Report;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsAPInvoicesByVendorDataSet APInvoicesByVendor(string prmUser, string prmvendinv, string prmbegvend, string prmbegdt, string prmendvend, string prmenddt, string prmrun, string prmOut)
    {
        string cError = "";
        dsAPInvoicesByVendorDataSet dsAPInvoicesByVendor = new dsAPInvoicesByVendorDataSet();
        dsAPInvoicesByVendor = null;
        AppServerConnect();
        aoObject.invbyvend(prmUser, prmvendinv, prmbegvend, prmbegdt, prmendvend, prmenddt, prmrun, prmOut, out cError, ref dsAPInvoicesByVendor);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsAPInvoicesByVendor;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsAPCheckRegisterDataSet APCheckRegister(string prmUser, string prmapchk, string prmbegvend, string prmbegdt, Int32 prmbegchk, string prmbegbnk, string prmendvend, string prmenddt, Int32 prmendchk, string prmendbnk, string prmglact, string prmrunpst, string prmOut)
    {
        string cError = "";
        dsAPCheckRegisterDataSet dsAPCheckRegister = new dsAPCheckRegisterDataSet();
        dsAPCheckRegister = null;
        AppServerConnect();
        aoObject.apchkrg(prmUser, prmapchk, prmbegvend, prmbegdt, prmbegchk, prmbegbnk, prmendvend, prmenddt, prmendchk, prmendbnk, prmglact, prmrunpst, prmOut, out cError, ref dsAPCheckRegister);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsAPCheckRegister;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsMISC1099ReportDataSet MISC1099Report(string prmUser, string prmAction, string prmOut, string prmBeginVendor, string prmEndVendor, string prmBegdt, string prmEnddt, string prmrddate, string prmincld, Int32 prmcopy)
    {
        string cError = "";
        dsMISC1099ReportDataSet dsMISC1099Report = new dsMISC1099ReportDataSet();
        dsMISC1099Report = null;
        AppServerConnect();
        aoObject.misc1099(prmUser, prmAction, prmOut, prmBeginVendor, prmEndVendor, prmBegdt, prmEnddt, prmrddate, prmincld, prmcopy, ref dsMISC1099Report, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsMISC1099Report;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsAPAccountsByVendorDataSet APAccountsByVendor(string prmUser, string prmvendacc, string prmbegvend, string prmbegdt, string prmbegact, string prmendvend, string prmenddt, string prmendact, string prmOut)
    {
        string cError = "";
        dsAPAccountsByVendorDataSet dsAPAccountsByVendor = new dsAPAccountsByVendorDataSet();
        dsAPAccountsByVendor = null;
        AppServerConnect();
        aoObject.vendacc(prmUser, prmvendacc, prmbegvend, prmbegdt, prmbegact, prmendvend, prmenddt, prmendact, prmOut, out cError, ref dsAPAccountsByVendor);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsAPAccountsByVendor;
    }
                
}

