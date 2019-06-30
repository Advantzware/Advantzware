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
public class account:AppServerConnect.AppServer
{
    public account()
	{
		//
		// TODO: Add constructor logic here
		//
	}

   
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsActWriteCreditDebitlistDataSet ActWriteCrDbist(string prmAction, string prmComp, string prmUser, string prmcustno, string prmcustname, Int32 prmchkno, string prmchkdt, decimal prmamt, string prmstat, string prmcur_cod, decimal prmex_rate, string prmout, string prmReckey)
    {
        string cError = "";
        dsActWriteCreditDebitlistDataSet dsActWriteCreditDebitlist = new dsActWriteCreditDebitlistDataSet();
        dsActWriteCreditDebitlist = null;
        AppServerConnect();
        aoObject.actdbcr_list(prmAction, prmComp, prmUser, prmcustno, prmcustname, prmchkno, prmchkdt, prmamt, prmstat, prmcur_cod, prmex_rate, prmout, prmReckey, ref dsActWriteCreditDebitlist, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsActWriteCreditDebitlist;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validActWriteCrDbist(string prmAction, string prmComp, string prmUser, string prmcustno, string prmcustname, Int32 prmchkno, string prmchkdt, decimal prmamt, string prmstat, string prmcur_cod, decimal prmex_rate, string prmout, string prmReckey)
    {
        string cError = "";
        dsActWriteCreditDebitlistDataSet dsActWriteCreditDebitlist = new dsActWriteCreditDebitlistDataSet();
        dsActWriteCreditDebitlist = null;
        AppServerConnect();
        aoObject.actdbcr_list(prmAction, prmComp, prmUser, prmcustno, prmcustname, prmchkno, prmchkdt, prmamt, prmstat, prmcur_cod, prmex_rate, prmout, prmReckey, ref dsActWriteCreditDebitlist, out cError);
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
    public dsActWriteDebitCreditViewDataSet ActWriteDbCrView(string prmAction, string prmComp, string prmUser, string prmcustno, Int32 prminvno, string prminvdt, decimal prmbaldu, decimal prmdisc, decimal prmtotl_app, string prmactno, string prmactdscr, string prmmemodscr, string prmout, string prmReckey)
    {
        string cError = "";
        dsActWriteDebitCreditViewDataSet dsActWriteDebitCreditView = new dsActWriteDebitCreditViewDataSet();
        dsActWriteDebitCreditView = null;
        AppServerConnect();
        aoObject.actdbcr_view(prmAction, prmComp, prmUser, prmcustno, prminvno, prminvdt, prmbaldu, prmdisc, prmtotl_app, prmactno, prmactdscr, prmmemodscr, prmout, prmReckey, ref dsActWriteDebitCreditView, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsActWriteDebitCreditView;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidActWriteDbCrView(string prmAction, string prmComp, string prmUser, string prmcustno, Int32 prminvno, string prminvdt, decimal prmbaldu, decimal prmdisc, decimal prmtotl_app, string prmactno, string prmactdscr, string prmmemodscr, string prmout, string prmReckey)
    {
        string cError = "";
        dsActWriteDebitCreditViewDataSet dsActWriteDebitCreditView = new dsActWriteDebitCreditViewDataSet();
        dsActWriteDebitCreditView = null;
        AppServerConnect();
        aoObject.actdbcr_view(prmAction, prmComp, prmUser, prmcustno, prminvno, prminvdt, prmbaldu, prmdisc, prmtotl_app, prmactno, prmactdscr, prmmemodscr, prmout, prmReckey, ref dsActWriteDebitCreditView, out cError);
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
    public dsCreditDebitMemoRptDataSet CreditDebitMemoRpt(string prmUser, string prmAction, string prmtrnsdt, Int32 prmPeriod, string prmout)
    {
        string cError = "";
        dsCreditDebitMemoRptDataSet dsCreditDebitMemoRpt = new dsCreditDebitMemoRptDataSet();
        dsCreditDebitMemoRpt = null;
        AppServerConnect();
        aoObject.crdbmemo(prmUser, prmAction, prmtrnsdt, prmPeriod, prmout, out cError, ref dsCreditDebitMemoRpt);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCreditDebitMemoRpt;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPrintCreditDebitMemoDataSet PrintCreditDebitMemo(string prmUser, string prmAction, string prmbegcust, Int32 prmbegmemo, string prmbegdt, string prmendcust, Int32 prmendmemo, string prmenddt, string prmprtmo, string prmpstmo, string prmexprt, string prmout)
    {
        string cError = "";
        dsPrintCreditDebitMemoDataSet dsPrintCreditDebitMemo = new dsPrintCreditDebitMemoDataSet();
        dsPrintCreditDebitMemo = null;
        AppServerConnect();
        aoObject.prnt_crdbmo(prmUser, prmAction, prmbegcust, prmbegmemo, prmbegdt, prmendcust, prmendmemo, prmenddt, prmprtmo, prmpstmo, prmexprt, prmout, out cError, ref dsPrintCreditDebitMemo);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsPrintCreditDebitMemo;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPostCRDRMemoReportDataSet PostCRDRMemoReport(string prmUser, string prmAction, string prmtrnsdt, Int32 prmperiod, string prmbegcust, string prmbegdt, string prmendcust, string prmenddt, string prmexprt, string prmout)
    {
        string cError = "";
        dsPostCRDRMemoReportDataSet dsPostCRDRMemoReport = new dsPostCRDRMemoReportDataSet();
        dsPostCRDRMemoReport = null;
        AppServerConnect();
        aoObject.post_crdbmo(prmUser, prmAction, prmtrnsdt, prmperiod, prmbegcust, prmbegdt, prmendcust, prmenddt, prmexprt, prmout, out cError, ref dsPostCRDRMemoReport);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsPostCRDRMemoReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCrDbInvoiceLookupDataSet CrDbInvoiceLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmCust)
    {
        string cError = "";
        dsCrDbInvoiceLookupDataSet dsCrDbInvoiceLookup = new dsCrDbInvoiceLookupDataSet();
        dsCrDbInvoiceLookup = null;
        AppServerConnect();
        aoObject.arinvlook(prmAction, prmUser, prmField, prmCondition, prmText, prmCust, ref dsCrDbInvoiceLookup);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCrDbInvoiceLookup;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsEnterEditCashReceiptsDataSet EnterEditCashReceipts(string prmAction, string prmComp, string prmUser, string prmcustno, string prmcustname, Int32 prmchkno, string prmchkdt, decimal prmamt, string prmbnkcd, string prmbnknam, string prmcur_cod, decimal prmex_rate, decimal prmnotapp, string prmout, string prmReckey)
    {
        string cError = "";
        dsEnterEditCashReceiptsDataSet dsEnterEditCashReceipts = new dsEnterEditCashReceiptsDataSet();
        dsEnterEditCashReceipts = null;
        AppServerConnect();
        aoObject.cshrcpt_list(prmAction, prmComp, prmUser, prmcustno, prmcustname, prmchkno, prmchkdt, prmamt, prmbnkcd, prmbnknam, prmcur_cod, prmex_rate, prmnotapp, prmout, prmReckey, ref dsEnterEditCashReceipts, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsEnterEditCashReceipts;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsEnterEditCashReceiptViewDataSet EnterEditCashReceiptView(string prmAction, string prmComp, string prmUser, string prmcustno, Int32 prminvno, string prminvdt, decimal prmbaldu, decimal prmdisc, decimal prmcashpy, decimal prmtotl_app, string prmactno, string prmactdscr, decimal prmbalaftr, string prmout, string prmReckey)
    {
        string cError = "";
        dsEnterEditCashReceiptViewDataSet dsEnterEditCashReceiptView = new dsEnterEditCashReceiptViewDataSet();
        dsEnterEditCashReceiptView = null;
        AppServerConnect();
        aoObject.cshrcpt_view(prmAction, prmComp, prmUser, prmcustno, prminvno, prminvdt, prmbaldu, prmdisc, prmcashpy, prmtotl_app, prmactno, prmactdscr, prmbalaftr, prmout, prmReckey, ref dsEnterEditCashReceiptView, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsEnterEditCashReceiptView;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validEnterEditCashReceiptView(string prmAction, string prmComp, string prmUser, string prmcustno, Int32 prminvno, string prminvdt, decimal prmbaldu, decimal prmdisc, decimal prmcashpy, decimal prmtotl_app, string prmactno, string prmactdscr, decimal prmbalaftr, string prmout, string prmReckey)
    {
        string cError = "";
        dsEnterEditCashReceiptViewDataSet dsEnterEditCashReceiptView = new dsEnterEditCashReceiptViewDataSet();
        dsEnterEditCashReceiptView = null;
        AppServerConnect();
        aoObject.cshrcpt_view(prmAction, prmComp, prmUser, prmcustno, prminvno, prminvdt, prmbaldu, prmdisc, prmcashpy, prmtotl_app, prmactno, prmactdscr, prmbalaftr, prmout, prmReckey, ref dsEnterEditCashReceiptView, out cError);
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
    public dsEditPostCashReceiptReportDataSet EditPostCashReceiptReport(string prmUser, string prmAction, string prmtrnsdt, Int32 prmperiod, string prmbegcust, string prmbegdt, string prmendcust, string prmenddt, string prmsort, string prmout)
    {
        string cError = "";
        dsEditPostCashReceiptReportDataSet dsEditPostCashReceiptReport = new dsEditPostCashReceiptReportDataSet();
        dsEditPostCashReceiptReport = null;
        AppServerConnect();
        aoObject.cashrcpt_rept(prmUser, prmAction, prmtrnsdt, prmperiod, prmbegcust, prmbegdt, prmendcust, prmenddt, prmsort, prmout, out cError, ref dsEditPostCashReceiptReport);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsEditPostCashReceiptReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsMiscellaneousCashReceiptListDataSet MiscellaneousCashReceiptList(string prmAction, string prmComp, string prmUser, string prmchkno, string prmpayr, decimal prmchkamt, string prmchkdt, string prmbnkcd, string prmdscr, string prmcur_cod, decimal prmex_rate, Int32 prmrcrd, string prmpstd, string prmpost, string prmout, string prmReckey)
    {
        string cError = "";
        dsMiscellaneousCashReceiptListDataSet dsMiscellaneousCashReceiptList = new dsMiscellaneousCashReceiptListDataSet();
        dsMiscellaneousCashReceiptList = null;
        AppServerConnect();
        aoObject.mcshrcpt_list(prmAction, prmComp, prmUser, prmchkno, prmpayr, prmchkamt, prmchkdt, prmbnkcd, prmdscr, prmcur_cod, prmex_rate, prmrcrd, prmpstd, prmpost, prmout, prmReckey, ref dsMiscellaneousCashReceiptList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsMiscellaneousCashReceiptList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validMiscellaneousList(string prmAction, string prmComp, string prmUser, string prmchkno, string prmpayr, decimal prmchkamt, string prmchkdt, string prmbnkcd, string prmdscr, string prmcur_cod, decimal prmex_rate, Int32 prmrcrd, string prmpstd, string prmpost, string prmout, string prmReckey)
    {
        string cError = "";
        dsMiscellaneousCashReceiptListDataSet dsMiscellaneousCashReceiptList = new dsMiscellaneousCashReceiptListDataSet();
        dsMiscellaneousCashReceiptList = null;
        AppServerConnect();
        aoObject.mcshrcpt_list(prmAction, prmComp, prmUser, prmchkno, prmpayr, prmchkamt, prmchkdt, prmbnkcd, prmdscr, prmcur_cod, prmex_rate, prmrcrd, prmpstd, prmpost, prmout, prmReckey, ref dsMiscellaneousCashReceiptList, out cError);
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
    public dsMiscellaneousCashReceiptViewDataSet MiscellaneousCashReceiptView(string prmAction, string prmComp, string prmUser, string prmcust, string prmchkno, decimal prmchkamt, string prmactno, string prmactdscr, string prmout, string prmReckey)
    {
        string cError = "";
        dsMiscellaneousCashReceiptViewDataSet dsMiscellaneousCashReceiptView = new dsMiscellaneousCashReceiptViewDataSet();
        dsMiscellaneousCashReceiptView = null;
        AppServerConnect();
        aoObject.mcshrcpt_view(prmAction, prmComp, prmUser, prmcust, prmchkno, prmchkamt, prmactno, prmactdscr, prmout, prmReckey, ref dsMiscellaneousCashReceiptView, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsMiscellaneousCashReceiptView;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validMiscellaneousView(string prmAction, string prmComp, string prmUser, string prmcust, string prmchkno, decimal prmchkamt, string prmactno, string prmactdscr, string prmout, string prmReckey)
    {
        string cError = "";
        dsMiscellaneousCashReceiptViewDataSet dsMiscellaneousCashReceiptView = new dsMiscellaneousCashReceiptViewDataSet();
        dsMiscellaneousCashReceiptView = null;
        AppServerConnect();
        aoObject.mcshrcpt_view(prmAction, prmComp, prmUser, prmcust, prmchkno, prmchkamt, prmactno, prmactdscr, prmout, prmReckey, ref dsMiscellaneousCashReceiptView, out cError);
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
    public dsEditPstArMiscReceiptReportDataSet EditPstArMiscReceiptReport(string prmUser, string prmAction, string prmtrnsdt, Int32 prmperiod, string prmbegdt, string prmenddt, string prmIn, string prmout)
    {
        string cError = "";
        dsEditPstArMiscReceiptReportDataSet dsEditPstArMiscReceiptReport = new dsEditPstArMiscReceiptReportDataSet();
        dsEditPstArMiscReceiptReport = null;
        AppServerConnect();
        aoObject.pstmisc_rcpt(prmUser, prmAction, prmtrnsdt, prmperiod, prmbegdt, prmenddt, prmIn, prmout, out cError, ref dsEditPstArMiscReceiptReport);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsEditPstArMiscReceiptReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsApplyReapplyCashMemoDataSet ApplyReapplyCashMemo(string prmAction, string prmComp, string prmUser, string prmPosted, string prmunPosted, Int32 prmchkno, string prmcustno, string prmcustname, string prmcustdt, Int32 prminv, decimal prmbaldue, decimal prmdisc, decimal prmpaid, string prmmemo, decimal prmaplyamt, decimal prmchkamt, string prmtyp, string prmReckey)
    {
        string cError = "";
        dsApplyReapplyCashMemoDataSet dsApplyReapplyCashMemo = new dsApplyReapplyCashMemoDataSet();
        dsApplyReapplyCashMemo = null;
        AppServerConnect();
        aoObject.aplycshmmo_list(prmAction, prmComp, prmUser, prmPosted, prmunPosted, prmchkno, prmcustno, prmcustname, prmcustdt, prminv, prmbaldue, prmdisc, prmpaid, prmmemo, prmaplyamt, prmchkamt, prmtyp, prmReckey, ref dsApplyReapplyCashMemo, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsApplyReapplyCashMemo;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsApplyReApplyCashMemoViewDataSet ApplyReApplyCashMemoView(string prmAction, string prmComp, string prmUser, string prmcust, Int32 prminv, string prminvdt, decimal prmbal, decimal prmapp, decimal prmdisc, string prmOut, string prmReckey)
    {
        string cError = "";
        dsApplyReApplyCashMemoViewDataSet dsApplyReApplyCashMemoView = new dsApplyReApplyCashMemoViewDataSet();
        dsApplyReApplyCashMemoView = null;
        AppServerConnect();
        aoObject.aplycshmmo_view(prmAction, prmComp, prmUser, prmcust, prminv, prminvdt, prmbal, prmapp, prmdisc, prmOut, prmReckey, ref dsApplyReApplyCashMemoView, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsApplyReApplyCashMemoView;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validApplyReApply(string prmAction, string prmComp, string prmUser, string prmcust, Int32 prminv, string prminvdt, decimal prmbal, decimal prmapp, decimal prmdisc, string prmOut, string prmReckey)
    {
        string cError = "";
        dsApplyReApplyCashMemoViewDataSet dsApplyReApplyCashMemoView = new dsApplyReApplyCashMemoViewDataSet();
        dsApplyReApplyCashMemoView = null;
        AppServerConnect();
        aoObject.aplycshmmo_view(prmAction, prmComp, prmUser, prmcust, prminv, prminvdt, prmbal, prmapp, prmdisc, prmOut, prmReckey, ref dsApplyReApplyCashMemoView, out cError);
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
    public dsOnAccountApplicationReportDataSet OnAccountApplicationReport(string prmUser, string prmAction, string prmtrnsdt, Int32 prmperiod, string prmbegcst, string prmendcst, string prmout)
    {
        string cError = "";
        dsOnAccountApplicationReportDataSet dsOnAccountApplicationReport = new dsOnAccountApplicationReportDataSet();
        dsOnAccountApplicationReport = null;
        AppServerConnect();
        aoObject.actapp_rprt(prmUser, prmAction, prmtrnsdt, prmperiod, prmbegcst, prmendcst, prmout, out cError, ref dsOnAccountApplicationReport);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsOnAccountApplicationReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsArVoidCashRegRcptReportDataSet ArVoidCashRegRcptReport(string prmUser, string prmAction, string prmtrnsdt, Int32 prmperiod, string prmout)
    {
        string cError = "";
        dsArVoidCashRegRcptReportDataSet dsArVoidCashRegRcptReport = new dsArVoidCashRegRcptReportDataSet();
        dsArVoidCashRegRcptReport = null;
        AppServerConnect();
        aoObject.arvoidcsh_rprt(prmUser, prmAction, prmtrnsdt, prmperiod, prmout, out cError, ref dsArVoidCashRegRcptReport);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            //HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsArVoidCashRegRcptReport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsVoidCashReceiptListDataSet VoidCashReceiptList(string prmAction, string prmComp, string prmUser, Int32 prmchkno, string prmcustno, string prmcustname, string prmcustdt, string prmbnk, string prmbnknam, string prmvoided, decimal prmamt, string prmOut, string prmReckey)
    {
        string cError = "";
        dsVoidCashReceiptListDataSet dsVoidCashReceiptList = new dsVoidCashReceiptListDataSet();
        dsVoidCashReceiptList = null;
        AppServerConnect();
        aoObject.voidcshrcpt_list(prmAction, prmComp, prmUser, prmchkno, prmcustno, prmcustname, prmcustdt, prmbnk, prmbnknam, prmvoided, prmamt, prmOut, prmReckey, ref dsVoidCashReceiptList, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsVoidCashReceiptList;
    }
    
}