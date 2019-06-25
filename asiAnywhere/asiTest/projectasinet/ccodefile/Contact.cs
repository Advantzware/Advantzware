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
/// Summary description for browsinvoice
/// </summary>
[System.ComponentModel.DataObject]
public class contact : AppServerConnect.AppServer
{
    public contact()
    {
        //
        // TODO: Add constructor logic here
        //
    }

    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    
    //public bool Contact(string prmAction, string vCompany, string custNo, string shipId, string vSman, string FirstName, string LastName, string MiddleInitial, string vSirname, string ContactTitle, string vMaillist, string vType, string ContactLoc, string CustName, string vAddr1, string vAddr2, string vCity, string vState, string vZip, string vCountry, string vCounty, string vTerritory, string AccessCode, string vPhone, string CellPhone, string vFax, string vExtension, string vEmail, string vWebsite, string prmReckey, ref vReckey);
    //{
    //    AppServerConnect();
    //    aoObject.Contact(prmAction, vCompany, custNo, shipId, vSman, FirstName, LastName, MiddleInitial, vSirname, ContactTitle, vMaillist, vType, ContactLoc, CustName, vAddr1, vAddr2, vCity, vState, vZip, vCountry, vCounty, vTerritory, AccessCode, vPhone, CellPhone, vFax, vExtension, vEmail, vWebsite, prmReckey, ref vReckey);
    //    AppServerDisconnect();
    //    return false;
    //}

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dscust2DataSet SelectCustlist(string prmAction, string prmComp, string prmUser, string prmCustno, string prmCustname, string prmCity, string prmState, string prmZip, string prmType, string prmSman, string prmTerr, string prmReckey)
    {
        string cError = "";
        dscust2DataSet dscust2 = new dscust2DataSet();
        dscust2 = null;
        AppServerConnect();
        aoObject.cust_list(prmAction, prmComp, prmUser, prmCustno, prmCustname, prmCity, prmState, prmZip, prmType, prmSman, prmTerr, prmReckey, ref dscust2, out cError);
        AppServerDisconnect();

        return dscust2;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dscustupdateDataSet SelectCustomer(string prmAction, string prmComp, string prmUser, string prmcustno, string prmcustname, string prmcity, string prmstate, string prmzip, string prmtype, string prmsman, string prmterr, string prmactive, DateTime prmdate1, string prmaddr1, string prmaddr2, string prmemail, string prmterms, string prmcruse, string prmcrrating, decimal prmordlim, decimal prmdisc, string prmcurrcode, Int32 prmcrholdinvdays, decimal prmcrholdinvdue, Int32 prmcustlevel, string prmcrhold, string prmfinchg, string prmautoreprice, string prmanedicust, string prmfactored, string prmsort, string prmtaxgr, string prmtaxid, DateTime prmdatefield2, string prmcontact, string prmareacode, string prmphone, string prmfaxprefix, string prmfaxcountry, string prmfrtpay, string prmfobcode, string prmshippart, string prmloc, string prmcarrier, string prmdelzone, decimal prmunderpct, decimal prmoverpct, decimal prmmarkup, Int32 prmshipdays, string prmpallet, string prmcasebundle, Int32 prmintfield1, string prmdescsman, string prmdesctype, string prmdescterms, string prmdescloc, string prmdescarrier, string prmdesterr, string prmdeszone, decimal prmflatcomm, string prminvmeth, string prmmandatory, string prmfax, string prmfaxcode, decimal prmcrlim)
    {
        string cError = "";
        dscustupdateDataSet dscustupdate = new dscustupdateDataSet();
        dscustupdate = null;
        AppServerConnect();
        aoObject.custupdate(prmAction, prmComp, prmUser, prmcustno, prmcustname, prmcity, prmstate, prmzip, prmtype, prmsman, prmterr, prmactive, prmdate1, prmaddr1, prmaddr2, prmemail, prmterms, prmcruse, prmcrrating, prmordlim, prmdisc, prmcurrcode, prmcrholdinvdays, prmcrholdinvdue, prmcustlevel, prmcrhold, prmfinchg, prmautoreprice, prmanedicust, prmfactored, prmsort, prmtaxgr, prmtaxid, prmdatefield2, prmcontact, prmareacode, prmphone, prmfaxprefix, prmfaxcountry, prmfrtpay, prmfobcode, prmshippart, prmloc, prmcarrier, prmdelzone, prmunderpct, prmoverpct, prmmarkup, prmshipdays, prmpallet, prmcasebundle, prmintfield1, prmdescsman, prmdesctype, prmdescterms, prmdescloc, prmdescarrier, prmdesterr, prmdeszone, prmflatcomm, prminvmeth, prmmandatory, prmfax, prmfaxcode, prmcrlim, ref dscustupdate, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dscustupdate;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dscustshiptoDataSet Selectship(string prmAction, string prmComp, string prmUser, string prmCustomer, string prmshipid, string prmreckey, Int32 prmshipno, string prmshipname, string prmshipcity, string prmshipstate, string prmshipzip, string prmshipaddr1, string prmshipaddr2, string prmcontact, string prmareacode, string prmphone, string prmtaxcode, string prmbroker, string prmbill, string prmdockloc, string prmdockhour, string prmlocbin, string prmcarrier, string prmpallet, string prmshipmeth, decimal prmdelchg, decimal prmdeltime, string prmdestcode, string prmnotes1, string prmnotes2, string prmnotes3, string prmnotes4, string prmfaxAreaCode, string prmfaxNumber, string prmfi_jdedid, decimal prmtb_mandatorytax, string prmloc)
    {
        string cError = "";
        dscustshiptoDataSet dscustshipto = new dscustshiptoDataSet();
        dscustshipto = null;
        AppServerConnect();
        aoObject.custship(prmAction, prmComp, prmUser, prmCustomer, prmshipid, prmreckey, prmshipno, prmshipname, prmshipcity, prmshipstate, prmshipzip, prmshipaddr1, prmshipaddr2, prmcontact, prmareacode, prmphone, prmtaxcode, prmbroker, prmbill, prmdockloc, prmdockhour, prmlocbin, prmcarrier, prmpallet, prmshipmeth, prmdelchg, prmdeltime, prmdestcode, prmnotes1, prmnotes2, prmnotes3, prmnotes4, prmfaxAreaCode, prmfaxNumber, prmfi_jdedid, prmtb_mandatorytax, prmloc, ref dscustshipto, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dscustshipto;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dscustsoldtoDataSet Selectsoldto(string prmAction, string prmComp, string prmUser, string prmCustomer, string prmsoldid, Int32 prmsoldno, string prmsoldname, string prmsoldcity, string prmsoldstate, string prmsoldzip, string prmsoldaddr1, string prmsoldaddr2, string prmsoldreckey)
    {
        string cError = "";
        dscustsoldtoDataSet dscustsoldto = new dscustsoldtoDataSet();
        dscustsoldto = null;
        AppServerConnect();
        aoObject.custsoldto(prmAction, prmComp, prmUser, prmCustomer, prmsoldid, prmsoldno, prmsoldname, prmsoldcity, prmsoldstate, prmsoldzip, prmsoldaddr1, prmsoldaddr2, prmsoldreckey, ref dscustsoldto, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dscustsoldto;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCustStockLookDataSet SelectCustStock(string prmAction, string prmComp, string prmUser)
    {

        dsCustStockLookDataSet dsCustStockLook = new dsCustStockLookDataSet();
        dsCustStockLook = null;
        AppServerConnect();
        aoObject.custstock(prmAction, prmComp, prmUser, ref dsCustStockLook);
        AppServerDisconnect();
        return dsCustStockLook;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCustArinvoiceslistDataSet SelectCustInv(string prmAction, string prmComp, string prmUser, string prmCust, Int32 prmInv, Int32 prmOldInv, string prmPosted, string prmCustname, string prmInvdate, decimal prmGross, decimal prmPaid, decimal prmBaldue, decimal prmTaxamt, decimal prmDue, string prmShipid, string prmShipname, string prmPono, string prmDuedate, string prmTaxcode, string prmTerms, string prmTermsdesc, decimal prmDiscount, decimal prmDisctaken, Int32 prmDiscdays, string prmCarrier, decimal prmFreight, string prmCurrcode, decimal prmExrate, string prmReckey)
    {
        string cError = "";
        dsCustArinvoiceslistDataSet dsCustArinvoiceslist = new dsCustArinvoiceslistDataSet();
        dsCustArinvoiceslist = null;
        AppServerConnect();
        aoObject.custinv_list(prmAction, prmComp, prmUser,prmCust,  prmInv, prmOldInv, prmPosted, prmCustname,  prmInvdate,  prmGross,  prmPaid,  prmBaldue,  prmTaxamt,  prmDue,  prmShipid,  prmShipname,  prmPono,  prmDuedate,  prmTaxcode,  prmTerms,  prmTermsdesc,  prmDiscount,  prmDisctaken,  prmDiscdays,  prmCarrier,  prmFreight,  prmCurrcode,  prmExrate, prmReckey, ref dsCustArinvoiceslist, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCustArinvoiceslist;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsCustArViewInvlLineDataSet CustInvLine(string prmAction, string prmComp, string prmUser, Int32 prmInv, Int32 prmLine, string prmActnum, string prmActdscr, string prmIname, string prmIdscr, string prmLotno, decimal prmInvqty, string prmConsuom, decimal prmSfsht, decimal prmUnitpr, string prmQtyuom, decimal prmDisc, decimal prmCalamt, decimal prmAmtmsf, decimal prmCost, string prmDscr1, string prmSman1, decimal prmSpct1, decimal prmScomm1, string prmSman2, decimal prmSpct2, decimal prmScomm2, string prmSman3, decimal prmSpct3, decimal prmScomm3, string prmReckey)
    {
        string cError = "";
        dsCustArViewInvlLineDataSet dsCustArViewInvlLine = new dsCustArViewInvlLineDataSet();
        dsCustArViewInvlLine = null;
        AppServerConnect();
        aoObject.ArViewInvoice(prmAction, prmComp, prmUser, prmInv, prmLine, prmActnum, prmActdscr, prmIname, prmIdscr, prmLotno, prmInvqty, prmConsuom, prmSfsht, prmUnitpr, prmQtyuom, prmDisc, prmCalamt, prmAmtmsf, prmCost, prmDscr1, prmSman1, prmSpct1, prmScomm1, prmSman2, prmSpct2, prmScomm2, prmSman3, prmSpct3, prmScomm3,prmReckey, ref dsCustArViewInvlLine, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");

        }
        return dsCustArViewInvlLine;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool ValidateCustInvLine(string prmAction, string prmComp, string prmUser, Int32 prmInv, Int32 prmLine, string prmActnum, string prmActdscr, string prmIname, string prmIdscr, string prmLotno, decimal prmInvqty, string prmConsuom, decimal prmSfsht, decimal prmUnitpr, string prmQtyuom, decimal prmDisc, decimal prmCalamt, decimal prmAmtmsf, decimal prmCost, string prmDscr1, string prmSman1, decimal prmSpct1, decimal prmScomm1, string prmSman2, decimal prmSpct2, decimal prmScomm2, string prmSman3, decimal prmSpct3, decimal prmScomm3, string prmReckey)
    {
        string cError = "";
        dsCustArViewInvlLineDataSet dsCustArViewInvlLine = new dsCustArViewInvlLineDataSet();
        dsCustArViewInvlLine = null;
        AppServerConnect();
        aoObject.ArViewInvoice(prmAction, prmComp, prmUser, prmInv, prmLine, prmActnum, prmActdscr, prmIname, prmIdscr, prmLotno, prmInvqty, prmConsuom, prmSfsht, prmUnitpr, prmQtyuom, prmDisc, prmCalamt, prmAmtmsf, prmCost, prmDscr1, prmSman1, prmSpct1, prmScomm1, prmSman2, prmSpct2, prmScomm2, prmSman3, prmSpct3, prmScomm3, prmReckey, ref dsCustArViewInvlLine, out cError);
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
    public bool ValidateSelectCustInv(string prmAction, string prmComp, string prmUser, string prmCust, Int32 prmInv, Int32 prmOldInv, string prmPosted, string prmCustname, string prmInvdate, decimal prmGross, decimal prmPaid, decimal prmBaldue, decimal prmTaxamt, decimal prmDue, string prmShipid, string prmShipname, string prmPono, string prmDuedate, string prmTaxcode, string prmTerms, string prmTermsdesc, decimal prmDiscount, decimal prmDisctaken, Int32 prmDiscdays, string prmCarrier, decimal prmFreight, string prmCurrcode, decimal prmExrate, string prmReckey)
    {
        string cError = "";
        dsCustArinvoiceslistDataSet dsCustArinvoiceslist = new dsCustArinvoiceslistDataSet();
        dsCustArinvoiceslist = null;
        AppServerConnect();
        aoObject.custinv_list(prmAction, prmComp, prmUser, prmCust, prmInv, prmOldInv, prmPosted, prmCustname, prmInvdate, prmGross, prmPaid, prmBaldue, prmTaxamt, prmDue, prmShipid, prmShipname, prmPono, prmDuedate, prmTaxcode, prmTerms, prmTermsdesc, prmDiscount, prmDisctaken, prmDiscdays, prmCarrier, prmFreight, prmCurrcode, prmExrate, prmReckey, ref dsCustArinvoiceslist, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            return false;

        }
        return true;
    }

}

