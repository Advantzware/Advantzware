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
public class ledger:AppServerConnect.AppServer
{
    public ledger()
	{
		//
		// TODO: Add constructor logic here
		//
	}

   
   
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGeneralLedgerlistDataSet GeneralLedgerlist(string prmAction, string prmComp, string prmUser, Int32 prmjrn_no, string prmtr_date, Int32 prmperiod, decimal prmt_deb, decimal prmt_crd, decimal prmt_amt, string prmreverse, string prmfrom_revr, string prmcb_freq, string prmReckey)
    {
        string cError = "";
        dsGeneralLedgerlistDataSet dsGeneralLedgerlist = new dsGeneralLedgerlistDataSet();
        dsGeneralLedgerlist = null;
        AppServerConnect();
        aoObject.gljrn_list(prmAction, prmComp, prmUser, prmjrn_no, prmtr_date, prmperiod, prmt_deb, prmt_crd, prmt_amt, prmreverse, prmfrom_revr, prmcb_freq, prmReckey, ref dsGeneralLedgerlist, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGeneralLedgerlist;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateGeneralLedgerlist(string prmAction, string prmComp, string prmUser, Int32 prmjrn_no, string prmtr_date, Int32 prmperiod, decimal prmt_deb, decimal prmt_crd, decimal prmt_amt, string prmreverse, string prmfrom_revr, string prmcb_freq, string prmReckey)
    {
        string cError = "";
        dsGeneralLedgerlistDataSet dsGeneralLedgerlist = new dsGeneralLedgerlistDataSet();
        dsGeneralLedgerlist = null;
        AppServerConnect();
        aoObject.gljrn_list(prmAction, prmComp, prmUser, prmjrn_no, prmtr_date, prmperiod, prmt_deb, prmt_crd, prmt_amt, prmreverse, prmfrom_revr, prmcb_freq, prmReckey, ref dsGeneralLedgerlist, out cError);
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
    public dsGeneralLedgerViewDataSet GeneralLedgerView(string prmAction, string prmComp, string prmUser, Int32 prmline_no, string prmactnum, string prmactdscr, string prmdscr, decimal prmamt, Int32 prmjrl_no, string prmReckey)
    {
        string cError = "";
        dsGeneralLedgerViewDataSet dsGeneralLedgerView = new dsGeneralLedgerViewDataSet();
        dsGeneralLedgerView = null;
        AppServerConnect();
        aoObject.gljrn_view(prmAction, prmComp, prmUser, prmline_no, prmactnum, prmactdscr, prmdscr, prmamt, prmjrl_no, prmReckey, ref dsGeneralLedgerView, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGeneralLedgerView;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validateGeneralLedgerView(string prmAction, string prmComp, string prmUser, Int32 prmline_no, string prmactnum, string prmactdscr, string prmdscr, decimal prmamt, Int32 prmjrl_no, string prmReckey)
    {
        string cError = "";
        dsGeneralLedgerViewDataSet dsGeneralLedgerView = new dsGeneralLedgerViewDataSet();
        dsGeneralLedgerView = null;
        AppServerConnect();
        aoObject.gljrn_view(prmAction, prmComp, prmUser, prmline_no, prmactnum, prmactdscr, prmdscr, prmamt, prmjrl_no, prmReckey, ref dsGeneralLedgerView, out cError);
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
    public dsRecrGeneralLedgerlistDataSet RecrGeneralLedgerlist(string prmAction, string prmComp, string prmUser, Int32 prmjrn_no, string prmtr_date, Int32 prmperiod, decimal prmt_deb, decimal prmt_crd, decimal prmt_amt, string prmreverse, string prmfrom_revr, string prmcb_freq, string prmdscr, string prmReckey)
    {
        string cError = "";
        dsRecrGeneralLedgerlistDataSet dsRecrGeneralLedgerlist = new dsRecrGeneralLedgerlistDataSet();
        dsRecrGeneralLedgerlist = null;
        AppServerConnect();
        aoObject.recgl_list(prmAction, prmComp, prmUser, prmjrn_no, prmtr_date, prmperiod, prmt_deb, prmt_crd, prmt_amt, prmreverse, prmfrom_revr, prmcb_freq, prmdscr, prmReckey, ref dsRecrGeneralLedgerlist, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsRecrGeneralLedgerlist;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRecrGeneralLedgerlistDataSet RecrGeneralLedgerload(string prmAction, string prmComp, string prmUser, Int32 prmjrn_no, string prmtr_date, Int32 prmperiod, decimal prmt_deb, decimal prmt_crd, decimal prmt_amt, string prmreverse, string prmfrom_revr, string prmcb_freq, string prmdscr, string prmReckey)
    {
        string cError = "";
        dsRecrGeneralLedgerlistDataSet dsRecrGeneralLedgerlist = new dsRecrGeneralLedgerlistDataSet();
        dsRecrGeneralLedgerlist = null;
        AppServerConnect();
        aoObject.recgl_list(prmAction, prmComp, prmUser, prmjrn_no, prmtr_date, prmperiod, prmt_deb, prmt_crd, prmt_amt, prmreverse, prmfrom_revr, prmcb_freq, prmdscr, prmReckey, ref dsRecrGeneralLedgerlist, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
           
        }
        return dsRecrGeneralLedgerlist;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsgljrnpostingDataSet GlJrnPosting(string prmUser, string prmAction, string prmBeginjou, string prmEndjou, string prmBegindate, string prmEnddate, string prmOut, string prmsort, string prmPost)
    {
        string cError = "";
        dsgljrnpostingDataSet dsgljrnposting = new dsgljrnpostingDataSet();
        dsgljrnposting = null;
        AppServerConnect();
        aoObject.gljrnpost(prmUser, prmAction, prmBeginjou, prmEndjou, prmBegindate, prmEnddate, prmOut, prmsort, prmPost, out cError, ref dsgljrnposting);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsgljrnposting;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsgenpostdistribDataSet GenPostDistrib(string prmUser, string prmAction, string prmBegacc, string prmEndacc, string prmBegindate, string prmEnddate, string prmOut, string prmReport, string prmPost, string prmTrnsDate, string prmPeriod)
    {
        string cError = "";
        dsgenpostdistribDataSet dsgenpostdistrib = new dsgenpostdistribDataSet();
        dsgenpostdistrib = null;
        AppServerConnect();
        aoObject.genpostdist(prmUser, prmAction, prmBegacc, prmEndacc, prmBegindate, prmEnddate, prmOut, prmReport, prmPost, prmTrnsDate, prmPeriod, out cError, ref dsgenpostdistrib);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsgenpostdistrib;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGljrnRepEntriesDataSet JournalEntriRep(string prmUser, string prmAction, string prmBegJur, string prmEndJur, string prmBegDate, string prmEndDate, string prmSort, string prmPost, string prmOut)
    {
        string cError = "";
        dsGljrnRepEntriesDataSet dsGljrnRepEntries = new dsGljrnRepEntriesDataSet();
        dsGljrnRepEntries = null;
        AppServerConnect();
        aoObject.GljrnRep(prmUser, prmAction, prmBegJur, prmEndJur, prmBegDate, prmEndDate, prmSort, prmPost, prmOut, out cError, ref dsGljrnRepEntries);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            
        }
        return dsGljrnRepEntries;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPstchkRepEntriesDataSet ReferencevsPosting(string prmUser, string prmAction, string prmBegRun, string prmEndRun, string prmBegDate, string prmEndDate, string prmBegPdate, string prmEndPdate, string prmOut)
    {
        string cError = "";
        dsPstchkRepEntriesDataSet dsPstchkRepEntries = new dsPstchkRepEntriesDataSet();
        dsPstchkRepEntries = null;
        AppServerConnect();
        aoObject.PstchkRep(prmUser, prmAction, prmBegRun, prmEndRun, prmBegDate, prmEndDate, prmBegPdate, prmEndPdate, prmOut, out cError, ref dsPstchkRepEntries);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsPstchkRepEntries;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPostckRepEntriesDataSet GlPostingCheckRep(string prmUser, string prmAction, string prmBegRun, string prmEndRun, string prmBegDate, string prmEndDate,  string prmOut)
    {
        string cError = "";
        dsPostckRepEntriesDataSet dsPostckRepEntries = new dsPostckRepEntriesDataSet();
        dsPostckRepEntries = null;
        AppServerConnect();
        aoObject.PostckRep(prmUser, prmAction, prmBegRun, prmEndRun, prmBegDate, prmEndDate,  prmOut, out cError, ref dsPostckRepEntries);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsPostckRepEntries;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsPostRegisterEntriesDataSet GlPostingRegisterRep(string prmUser, string prmAction, string prmBegRun, string prmEndRun, string prmBegDate, string prmEndDate, string prmOut)
    {
        string cError = "";
        dsPostRegisterEntriesDataSet dsPostRegisterEntries = new dsPostRegisterEntriesDataSet();
        dsPostRegisterEntries = null;
        AppServerConnect();
        aoObject.PostRegister(prmUser, prmAction, prmBegRun, prmEndRun, prmBegDate, prmEndDate, prmOut, out cError, ref dsPostRegisterEntries);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsPostRegisterEntries;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGlChartReportDataSet GlChartofAccount(string prmUser, string prmAction, string prmchart, string prmOut, string prmPost)
    {
        string cError = "";
        dsGlChartReportDataSet dsGlChartReport = new dsGlChartReportDataSet();
        dsGlChartReport = null;
        AppServerConnect();
        aoObject.glchart(prmUser, prmAction, prmchart, prmOut, prmPost, out cError, ref dsGlChartReport);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsGlChartReport;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCurrentMonthDetailDataSet CurrentMonthDetail(string prmUser, string prmAction, string prmtnsdate,Int32 prmperiod,string prmBegact,string prmEndact,string  prmOut,string prmdetaild,string prmactiv)
    {
        string cError = "";
        dsCurrentMonthDetailDataSet dsCurrentMonthDetail = new dsCurrentMonthDetailDataSet();
        dsCurrentMonthDetail = null;
        AppServerConnect();
        aoObject.glmondetl(prmUser, prmAction, prmtnsdate, prmperiod, prmBegact,prmEndact,prmOut,prmdetaild,prmactiv, out cError, ref dsCurrentMonthDetail);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsCurrentMonthDetail;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsInvoiceSummaryDataSet GLInvoiceSummary(string prmUser, string prmAction, string prmchart, string prmOut, string prmPost)
    {
        string cError = "";
        dsInvoiceSummaryDataSet dsInvoiceSummary = new dsInvoiceSummaryDataSet();
        dsInvoiceSummary = null;
        AppServerConnect();
        aoObject.invsum(prmUser, prmAction, prmchart, prmOut, prmPost, out cError, ref dsInvoiceSummary);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsInvoiceSummary;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGLAccountHistoryDataSet GLAccounthistory(string prmUser, string prmAction, string prmbegdate, string prmenddate, string prmBegact, string prmEndact, string prmOut, string prmautodis, string prmdetailed)
    {
        string cError = "";
        dsGLAccountHistoryDataSet dsGLAccountHistory = new dsGLAccountHistoryDataSet();
        dsGLAccountHistory = null;
        AppServerConnect();
        aoObject.glacthist(prmUser, prmAction, prmbegdate, prmenddate, prmBegact, prmEndact, prmOut, prmautodis, prmdetailed, out cError, ref dsGLAccountHistory);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsGLAccountHistory;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsTransactionReportDataSet TransactionReport(string prmUser, string prmAction, string prmbegdate, string prmenddate, string prmbegact, string prmendact, string prmtb_acpay, string prmtb_adjust, string prmtb_appurch, string prmtb_apckr, string prmtb_apmem, string prmtb_apvoidck, string prmtb_arinv, string prmtb_autodist, string prmtb_cashr, string prmtb_cashrvd, string prmtb_cdisb, string prmtb_crmem, string prmtb_fgpost, string prmtb_general, string prmtb_jcost, string prmtb_mcshrec, string prmtb_oeinv, string prmtb_rmpost, string prmtb_void_checks, string prmOut)
    {
        string cError = "";
        dsTransactionReportDataSet dsTransactionReport = new dsTransactionReportDataSet();
        dsTransactionReport = null;
        AppServerConnect();
        aoObject.gltransrpt(prmUser, prmAction, prmbegdate, prmenddate, prmbegact, prmendact, prmtb_acpay, prmtb_adjust, prmtb_appurch, prmtb_apckr, prmtb_apmem, prmtb_apvoidck, prmtb_arinv, prmtb_autodist, prmtb_cashr, prmtb_cashrvd, prmtb_cdisb, prmtb_crmem, prmtb_fgpost, prmtb_general, prmtb_jcost, prmtb_mcshrec, prmtb_oeinv, prmtb_rmpost, prmtb_void_checks, prmOut, out cError, ref dsTransactionReport);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsTransactionReport;
    }



    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGLCloseMonthDataSet GLCloseMonth(string prmUser, string prmAction, string prmtrnsDate, Int32 prmperiod, string prmOut, string ip_oktogo)
    {
        string cError = "";
        dsGLCloseMonthDataSet dsGLCloseMonth = new dsGLCloseMonthDataSet();
        dsGLCloseMonth = null;
        AppServerConnect();
        aoObject.glmnclo(prmUser, prmAction, prmtrnsDate, prmperiod, prmOut, ip_oktogo, out cError, ref dsGLCloseMonth);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsGLCloseMonth;
    }

    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGLTrialBalanceDataSet GLTrialBalance(string prmUser, string prmAction, string prmtrnsDate, Int32 prmperiod, string prmbegact, Int32 prmbegsub, string prmendact, Int32 prmendsub, string prmZerobal, string prmsortsub, Int32 prmactlevl, string prmOut)
    {
        string cError = "";
        dsGLTrialBalanceDataSet dsGLTrialBalance = new dsGLTrialBalanceDataSet();
        dsGLTrialBalance = null;
        AppServerConnect();
        aoObject.tribal(prmUser, prmAction, prmtrnsDate, prmperiod, prmbegact, prmbegsub, prmendact, prmendsub, prmZerobal, prmsortsub, prmactlevl, prmOut, out cError, ref dsGLTrialBalance);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsGLTrialBalance;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGLYearCloseDataSet GLYearClose(string prmUser, string prmAction, Int32 prmyear, string prmmove, string prmOut)
    {
        string cError = "";
        dsGLYearCloseDataSet dsGLYearClose = new dsGLYearCloseDataSet();
        dsGLYearClose = null;
        AppServerConnect();
        aoObject.glyrclo(prmUser, prmAction, prmyear, prmmove, prmOut, out cError, ref dsGLYearClose);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsGLYearClose;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsListQueryGeneralLedgerHiDataSet ListQueryGeneralLedgerHi(string prmUser, string prmAction, string prmbegact, Int32 prmglyear, Int32 prmperiodfr, Int32 prmperiodto, decimal prmopnbal, decimal prmclsbal, string prmextra, string prmRecKey)
    {
        string cError = "";
        dsListQueryGeneralLedgerHiDataSet dsListQueryGeneralLedgerHi = new dsListQueryGeneralLedgerHiDataSet();
        dsListQueryGeneralLedgerHi = null;
        AppServerConnect();
        aoObject.listqrygl(prmUser, prmAction, prmbegact, prmglyear, prmperiodfr, prmperiodto, prmopnbal, prmclsbal, prmextra, prmRecKey, ref dsListQueryGeneralLedgerHi);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsListQueryGeneralLedgerHi;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsViewQueryGeneralLedgerhistDataSet ViewQueryGeneralLedgerhist(string prmUser, string prmAction, Int32 prmtrnum, string prmactnum, string prmjrnl, string prmtrdscr, Int32 prmtramt, string prmtrdate, string prmRecKey)
    {
        string cError = "";
        dsViewQueryGeneralLedgerhistDataSet dsViewQueryGeneralLedgerhist = new dsViewQueryGeneralLedgerhistDataSet();
        dsViewQueryGeneralLedgerhist = null;
        AppServerConnect();
        aoObject.viewqrygl(prmUser, prmAction, prmtrnum, prmactnum, prmjrnl, prmtrdscr, prmtramt, prmtrdate, prmRecKey, out cError, ref dsViewQueryGeneralLedgerhist);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsViewQueryGeneralLedgerhist;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsRunNumberLookRunDataSet RunNumberLookRun(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        string cError = "";
        dsRunNumberLookRunDataSet dsRunNumberLookRun = new dsRunNumberLookRunDataSet();
        dsRunNumberLookRun = null;
        AppServerConnect();
        aoObject.runnoLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsRunNumberLookRun);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsRunNumberLookRun;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsFinancialStatementsDataSet FinancialStatement(string prmUser, string prmAction, string prmGetTt, string prmdscr, string prmtrnsdt, string prmslctrp, Int32 prmpred, string prmprecls, string prmzeroln, string prmsuppze, string prmact, string prmdecimal, string prmmulcmp, string prmlist, Int32 prmactlvl, Int32 prmbesbact, Int32 prmendsbact, string prmext, string prmout)
    {
        string cError = "";
        dsFinancialStatementsDataSet dsFinancialStatements = new dsFinancialStatementsDataSet();
        dsFinancialStatements = null;
        AppServerConnect();
        aoObject.fnstmt(prmUser, prmAction, prmGetTt, prmdscr, prmtrnsdt, prmslctrp, prmpred, prmprecls, prmzeroln, prmsuppze, prmact, prmdecimal, prmmulcmp, prmlist, prmactlvl, prmbesbact, prmendsbact, prmext, prmout, out cError, ref dsFinancialStatements);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsFinancialStatements;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsIncomeSmtRatiosDataSet IncomeSmtRatios(string prmUser, string prmAction, string prmtrnsdt, string prmclsprd, string prmbeact1, string prmendact1, string prmbeact2, string prmendact2, string prmbegact3, string prmendact3, string prmbeact4, string prmendact4, string prmbegact5, string prmendact5, Int32 prmPeriod, string prmOut)
    {
        string cError = "";
        dsIncomeSmtRatiosDataSet dsIncomeSmtRatios = new dsIncomeSmtRatiosDataSet();
        dsIncomeSmtRatios = null;
        AppServerConnect();
        aoObject.glincome(prmUser, prmAction, prmtrnsdt, prmclsprd, prmbeact1, prmendact1, prmbeact2, prmendact2, prmbegact3, prmendact3, prmbeact4, prmendact4, prmbegact5, prmendact5, prmPeriod, prmOut, out cError, ref dsIncomeSmtRatios);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsIncomeSmtRatios;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGLAccountListDataSet GLAccountList(string prmUser, string prmAction, string prmact, string prmactdscr, string prmactype, string prmRecKey, decimal prmcry_opn, decimal prmlyr_opn, decimal prmbud1, decimal prmbud2, decimal prmbud3, decimal prmbud4, decimal prmbud5, decimal prmbud6, decimal prmbud7, decimal prmbud8, decimal prmbud9, decimal prmbud10, decimal prmbud11, decimal prmbud12, decimal prmbud13, decimal prmly_bud1, decimal prmly_bud2, decimal prmly_bud3, decimal prmly_bud4, decimal prmly_bud5, decimal prmly_bud6, decimal prmly_bud7, decimal prmly_bud8, decimal prmly_bud9, decimal prmly_bud10, decimal prmly_bud11, decimal prmly_bud12, decimal prmly_bud13, decimal prmlyr1, decimal prmlyr2, decimal prmlyr3, decimal prmlyr4, decimal prmlyr5, decimal prmlyr6, decimal prmlyr7, decimal prmlyr8, decimal prmlyr9, decimal prmlyr10, decimal prmlyr11, decimal prmlyr12, decimal prmlyr13, decimal prmcyr1, decimal prmcyr2, decimal prmcyr3, decimal prmcyr4, decimal prmcyr5, decimal prmcyr6, decimal prmcyr7, decimal prmcyr8, decimal prmcyr9, decimal prmcyr10, decimal prmcyr11, decimal prmcyr12, decimal prmcyr13, string prmtb_not_disc, string prmbtn)
    {
        string cError = "";
        dsGLAccountListDataSet dsGLAccountList = new dsGLAccountListDataSet();
        dsGLAccountList = null;
        AppServerConnect();
        aoObject.gl_account(prmUser, prmAction, prmact, prmactdscr, prmactype, prmRecKey, prmcry_opn, prmlyr_opn, prmbud1, prmbud2, prmbud3, prmbud4, prmbud5, prmbud6, prmbud7, prmbud8, prmbud9, prmbud10, prmbud11, prmbud12, prmbud13, prmly_bud1, prmly_bud2, prmly_bud3, prmly_bud4, prmly_bud5, prmly_bud6, prmly_bud7, prmly_bud8, prmly_bud9, prmly_bud10, prmly_bud11, prmly_bud12, prmly_bud13, prmlyr1, prmlyr2, prmlyr3, prmlyr4, prmlyr5, prmlyr6, prmlyr7, prmlyr8, prmlyr9, prmlyr10, prmlyr11, prmlyr12, prmlyr13, prmcyr1, prmcyr2, prmcyr3, prmcyr4, prmcyr5, prmcyr6, prmcyr7, prmcyr8, prmcyr9, prmcyr10, prmcyr11, prmcyr12, prmcyr13, prmtb_not_disc, prmbtn, out cError, ref dsGLAccountList);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");

        }
        return dsGLAccountList;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dscompanylistviewDataSet CompanyList(string prmAction, string prmComp, string prmUser, string prmCompany, string prmfid, string prmvname, string prmsid, string prmaddr1, string prmaddr2, string prmcity, string prmstate, string prmzip, string prmcoacc, string prmnumper, string prmacclevel, string prmaccdig1, string prmaccdig2, string prmaccdig3, string prmaccdig4, string prmaccdig5, string prmyendoff, string prmcurrcode, string prmyendper, string prmfirstyear, string prmprdnum, string prmprddt1, string prmprddt2, string prmcdesc, string prmReckey)
    {
        string cError = "";
        dscompanylistviewDataSet dscompanylistview = new dscompanylistviewDataSet();
        dscompanylistview = null;
        AppServerConnect();
        aoObject.company_list(prmAction, prmComp, prmUser, prmCompany, prmfid, prmvname, prmsid, prmaddr1, prmaddr2, prmcity, prmstate, prmzip, prmcoacc, prmnumper, prmacclevel, prmaccdig1, prmaccdig2, prmaccdig3, prmaccdig4, prmaccdig5, prmyendoff, prmcurrcode, prmyendper, prmfirstyear, prmprdnum, prmprddt1, prmprddt2, prmcdesc, prmReckey, ref dscompanylistview, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dscompanylistview;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsperiodlistviewDataSet PeriodList(string prmAction, string prmComp, string prmUser, string prmcompany, string prmyr, string prmpnum, string prmpst, string prmpend, string prmpstat, string prmreckey)
    {
        string cError = "";
        dsperiodlistviewDataSet dsperiodlistview = new dsperiodlistviewDataSet();
        dsperiodlistview = null;
        AppServerConnect();
        aoObject.period_list(prmAction, prmComp, prmUser, prmcompany, prmyr, prmpnum, prmpst, prmpend, prmpstat, prmreckey, ref dsperiodlistview, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsperiodlistview;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGLDistributedAccountDataSet GLDistributedAccount(string prmUser, string prmAction, string prmact, string prmactdscr, string prmcst_act, string prmcst_dscr, decimal prmc_rate, string prmactype, string prmreckey)
    {
        string cError = "";
        dsGLDistributedAccountDataSet dsGLDistributedAccount = new dsGLDistributedAccountDataSet();
        dsGLDistributedAccount = null;
        AppServerConnect();
        aoObject.gl_cstact(prmUser, prmAction, prmact, prmactdscr, prmcst_act, prmcst_dscr, prmc_rate, prmactype, prmreckey, out cError, ref dsGLDistributedAccount);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGLDistributedAccount;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGLCtrlViewDataSet GLCtrlView(string prmAction, string prmComp, string prmUser, Int32 prmjrnl, Int32 prmtrns, string prmcrtyr, string prmcrtdscr, string prmprofit, string prmprodscr, string prmreckey, string ip_field)
    {
        string cError = "";
        dsGLCtrlViewDataSet dsGLCtrlView = new dsGLCtrlViewDataSet();
        dsGLCtrlView = null;
        AppServerConnect();
        aoObject.glctrl_view(prmAction, prmComp, prmUser, prmjrnl, prmtrns, prmcrtyr, prmcrtdscr, prmprofit, prmprodscr, prmreckey, ip_field, ref dsGLCtrlView, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGLCtrlView;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsGLAccountReportUpDataSet GLAccountReportUp(string prmUser, string prmAction, string prmbeact, string prmbegactdr, string prmendact, string prmendactdr, string prmshwhist, string prmshwnote, string prmshwselc, string prmshwmisc, string prmshwadd, string prmshwphn, string prmalnote, string prmOut, string prmListOrder, string prmreckey)
    {
        string cError = "";
        dsGLAccountReportUpDataSet dsGLAccountReportUp = new dsGLAccountReportUpDataSet();
        dsGLAccountReportUp = null;
        AppServerConnect();
        aoObject.glact_report(prmUser, prmAction, prmbeact, prmbegactdr, prmendact, prmendactdr, prmshwhist, prmshwnote, prmshwselc, prmshwmisc, prmshwadd, prmshwphn, prmalnote, prmOut, prmListOrder, prmreckey, out cError, ref dsGLAccountReportUp);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGLAccountReportUp;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsbanklistbankDataSet banklistbank(string prmAction, string prmUser, string prmcomp, string prmBankcode, string prmBankname, string prmAddr1, string prmAddr2, string prmCity, string prmState, string prmZip, string prmPhone, string prmContact, string prmBkact, string prmActnum, string prmLastchk, string prmBal, string prmOchk, string prmDeptr, string prmServ, string prmCurrcode, string prmBankno, string prmReckey)
    {
        string cError = "";
        dsbanklistbankDataSet dsbanklistbank = new dsbanklistbankDataSet();
        dsbanklistbank = null;
        AppServerConnect();
        aoObject.banklist(prmAction, prmUser, prmcomp, prmBankcode, prmBankname, prmAddr1, prmAddr2, prmCity, prmState, prmZip, prmPhone, prmContact, prmBkact, prmActnum, prmLastchk, prmBal, prmOchk, prmDeptr, prmServ, prmCurrcode, prmBankno, prmReckey, ref dsbanklistbank, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsbanklistbank;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validbanklistbank(string prmAction, string prmUser, string prmcomp, string prmBankcode, string prmBankname, string prmAddr1, string prmAddr2, string prmCity, string prmState, string prmZip, string prmPhone, string prmContact, string prmBkact, string prmActnum, string prmLastchk, string prmBal, string prmOchk, string prmDeptr, string prmServ, string prmCurrcode, string prmBankno, string prmReckey)
    {
        string cError = "";
        dsbanklistbankDataSet dsbanklistbank = new dsbanklistbankDataSet();
        dsbanklistbank = null;
        AppServerConnect();
        aoObject.banklist(prmAction, prmUser, prmcomp, prmBankcode, prmBankname, prmAddr1, prmAddr2, prmCity, prmState, prmZip, prmPhone, prmContact, prmBkact, prmActnum, prmLastchk, prmBal, prmOchk, prmDeptr, prmServ, prmCurrcode, prmBankno, prmReckey, ref dsbanklistbank, out cError);
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
    public bool updatebanklistbank(string prmAction, string prmUser, string prmcomp, string prmBankcode, string prmBankname, string prmAddr1, string prmAddr2, string prmCity, string prmState, string prmZip, string prmPhone, string prmContact, string prmBkact, string prmActnum, string prmLastchk, string prmBal, string prmOchk, string prmDeptr, string prmServ, string prmCurrcode, string prmBankno, string prmReckey)
    {
        string cError = "";
        dsbanklistbankDataSet dsbanklistbank = new dsbanklistbankDataSet();
        dsbanklistbank = null;
        AppServerConnect();
        aoObject.banklist(prmAction, prmUser, prmcomp, prmBankcode, prmBankname, prmAddr1, prmAddr2, prmCity, prmState, prmZip, prmPhone, prmContact, prmBkact, prmActnum, prmLastchk, prmBal, prmOchk, prmDeptr, prmServ, prmCurrcode, prmBankno, prmReckey, ref dsbanklistbank, out cError);
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
    public dsGLBankAccountReportUpDataSet GLBankReportUp(string prmUser, string prmAction, string prmbebnk, string prmbegbnkdr, string prmendbnk, string prmendbnkdr, string prmshwnote, string prmshwselc, string prmshwmisc, string prmshwadd, string prmshwphn, string prmalnote, string prmOut, string prmListOrder, string prmreckey)
    {
        string cError = "";
        dsGLBankAccountReportUpDataSet dsGLBankAccountReportUp = new dsGLBankAccountReportUpDataSet();
        dsGLBankAccountReportUp = null;
        AppServerConnect();
        aoObject.glbank_report(prmUser, prmAction, prmbebnk, prmbegbnkdr, prmendbnk, prmendbnkdr, prmshwnote, prmshwselc, prmshwmisc, prmshwadd, prmshwphn, prmalnote, prmOut, prmListOrder, prmreckey, out cError, ref dsGLBankAccountReportUp);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsGLBankAccountReportUp;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dscurrencylistvalDataSet CurrencyList(string prmAction, string prmComp, string prmUser, string prmCcode, string prmCdesc, string prmCountry, string prmCnum, string prMinorunit, string prmIsbase, string prmExrate, string prmArastacct, string prmReckey)
    {
        string cError = "";
        dscurrencylistvalDataSet dscurrencylistval = new dscurrencylistvalDataSet();
        dscurrencylistval = null;
        AppServerConnect();
        aoObject.currency(prmAction, prmComp, prmUser, prmCcode, prmCdesc, prmCountry, prmCnum, prMinorunit, prmIsbase, prmExrate, prmArastacct, prmReckey, ref dscurrencylistval, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dscurrencylistval;
    }

    

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dscompanyprintreportDataSet companyprintreport(string prmAction, string prmComp, string prmUser, string prmBegComp, string prmBegName, string prmEndComp, string prmEndName, string prmOpenPer, string prmSpool, string prmNotes, string prmListOrder, string prmShowNote, string prmShowSelPar, string prmMisc, string prmShowAdd, string prmShowPhone, string prmReckey, string prmTotal)
    {
        string cError = "";
        dscompanyprintreportDataSet dscompanyprintreport = new dscompanyprintreportDataSet();
        dscompanyprintreport = null;
        AppServerConnect();
        aoObject.companyrep(prmAction, prmComp, prmUser, prmBegComp, prmBegName, prmEndComp, prmEndName, prmOpenPer, prmSpool, prmNotes, prmListOrder, prmShowNote, prmShowSelPar, prmMisc, prmShowAdd, prmShowPhone, prmReckey, prmTotal, ref dscompanyprintreport, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dscompanyprintreport;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsLabelMenuLoadTgLookDataSet LabelMenuLoadTgLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmitmtype, string prmcurval)
    {
        string cError = "";
        dsLabelMenuLoadTgLookDataSet dsLabelMenuLoadTgLook = new dsLabelMenuLoadTgLookDataSet();
        dsLabelMenuLoadTgLook = null;
        AppServerConnect();
        aoObject.loadtg_look(prmAction, prmUser, prmField, prmCondition, prmText, prmitmtype, prmcurval, ref dsLabelMenuLoadTgLook);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsLabelMenuLoadTgLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsLabelMenuReturnTagLookDataSet LabelMenuReturnTagLook(string prmAction, string prmUser, string prmField, string prmCondition, string prmText, string prmReTag)
    {
        string cError = "";
        dsLabelMenuReturnTagLookDataSet dsLabelMenuReturnTagLook = new dsLabelMenuReturnTagLookDataSet();
        dsLabelMenuReturnTagLook = null;
        AppServerConnect();
        aoObject.Reldtg_look(prmAction, prmUser, prmField, prmCondition, prmText, prmReTag, ref dsLabelMenuReturnTagLook);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsLabelMenuReturnTagLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsOrderLoadTagLookupDataSet OrderLoadTagLookup(string prmAction, string prmUser, string prmField, string prmCondition, string prmText)
    {
        string cError = "";
        dsOrderLoadTagLookupDataSet dsOrderLoadTagLookup = new dsOrderLoadTagLookupDataSet();
        dsOrderLoadTagLookup = null;
        AppServerConnect();
        aoObject.Ld_ordLook(prmAction, prmUser, prmField, prmCondition, prmText, ref dsOrderLoadTagLookup);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsOrderLoadTagLookup;
    }
    
}

