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
public class contact_list : AppServerConnect.AppServer
{
    public contact_list()
    {
        //
        // TODO: Add constructor logic here
        //
    }


    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsContactListDataSet Contact(string prmAction, string vCompany, string custNo, string shipId, string vSman, string FirstName, string LastName, string MiddleInitial, string vSirname, string ContactTitle, string vMaillist, string vType, string ContactLoc, string CustName, string vAddr1, string vAddr2, string vCity, string vState, string vZip, string vCountry, string vCounty, string Territory, string AccessCode, string vPhone, string CellPhone, string vFax, string vExtension, string vEmail, string vWebsite,string prmReckey,  string vReckey )
    //{

    //    dsContactListDataSet dsContact = new dsContactListDataSet();
    //    dsContact = null;
    //    AppServerConnect();
    //    aoObject.Contact(prmAction, vCompany, custNo, shipId, vSman, FirstName, LastName, MiddleInitial, vSirname, ContactTitle, vMaillist, vType, ContactLoc, CustName, vAddr1, vAddr2, vCity, vState, vZip, vCountry, vCounty, Territory, AccessCode, vPhone, CellPhone, vFax, vExtension, vEmail, vWebsite, prmReckey,  vReckey, ref dsContact);
    //    AppServerDisconnect();

    //    return dsContact;
    //}
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUserCompDataSet SelUsercomp(string prmUser, string prmAction, string prmComp, string prmName, string prmCompDef, string vComp)
    {
        dsUserCompDataSet dsUserComp = new dsUserCompDataSet();
        dsUserComp = null;
        AppServerConnect();
        aoObject.UserComp(prmUser, prmAction, prmComp, prmName, prmCompDef, vComp, ref dsUserComp);
        AppServerDisconnect();
        return dsUserComp;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUserlocDataSet SelUserLocation(string prmComp, string prmUser, string prmAction, string prmLoc, string prmDscr, string prmLocDef, string vLoc)
    {
        dsUserlocDataSet dsUserloc = new dsUserlocDataSet();
        dsUserloc = null;
        AppServerConnect();
        aoObject.UserLoc(prmComp, prmUser, prmAction, prmLoc, prmDscr, prmLocDef, vLoc, ref dsUserloc);
        AppServerDisconnect();
        return dsUserloc;
    }
    //public bool ViewUser(string prmAction, string vCompany, string vUserId, string vUname, string vTrack, string vInternal, string vColor, string vFont, string vKeys, string vDevlop, string vFile, string vImage, string vEmail, string vLang, Int32 vRows)
    public bool ViewUser(string prmAction, string vCompany,string vUserId, string vUname, string vEmail, string vPassword)
    {
        AppServerConnect();
        //aoObject.ViewUser(prmAction, vCompany, vUserId, vUname, vTrack, vInternal, vColor, vFont, vKeys, vDevlop, vFile, vImage, vEmail,vLang, vRows );
        aoObject.ViewUser(prmAction, vCompany, vUserId, vUname, vEmail, vPassword);
        AppServerDisconnect();
        return false;
    }
    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsContactList2DataSet SelCustContact(string prmAction, string prmUser, string prmComp, string prmCust)
    //{
    //    dsContactList2DataSet dsContactList2 = new dsContactList2DataSet();
    //    dsContactList2 = null;
    //    AppServerConnect();
    //    aoObject.Cust_contact(prmAction, prmUser, prmComp, ref dsContactList2);
    //    AppServerDisconnect();
    //    return dsContactList2;
    //}
    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsContactList3DataSet SelShipContact(string prmAction, string prmUser, string prmComp, string prmCust)
    //{
    //    dsContactList3DataSet dsContactList3 = new dsContactList3DataSet();
    //    dsContactList3 = null;
    //    AppServerConnect();
    //    aoObject.Ship_contact(prmAction, prmUser, prmComp, ref dsContactList3);
    //    AppServerDisconnect();
    //    return dsContactList3;
    //}
    //[System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    //public dsNotes2DataSet SelNotesContact2(string prmAction, string prmUser, string prmComp)
    //{
    //    dsNotes2DataSet dsNotes2 = new dsNotes2DataSet();
    //    dsNotes2 = null;
    //    AppServerConnect();
    //    aoObject.CustNotes2(prmAction, prmUser, prmComp, ref dsNotes2);
    //    AppServerDisconnect();
    //    return dsNotes2;
    //}
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUserCustomerDataSet SelUserCustomer(string prmUser, string prmAction, string prmComp, string prmCust, string prmCustName, string prmCustDef)
    {
        dsUserCustomerDataSet dsUserCustomer = new dsUserCustomerDataSet();
        dsUserCustomer = null;
        AppServerConnect();
        aoObject.UserCustomer(prmUser,prmAction, prmComp, prmCust, prmCustName, prmCustDef, ref dsUserCustomer);
        AppServerDisconnect();
        return dsUserCustomer;
    }
    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUserVendorDataSet SelUserVend(string prmUser, string prmAction, string prmComp, string prmVend, string prmVendName, string prmVendDef)
    {
        dsUserVendorDataSet dsUserVendor = new dsUserVendorDataSet();
        dsUserVendor = null;
        AppServerConnect();
        aoObject.UserVendor(prmUser, prmAction, prmComp, prmVend, prmVendName, prmVendDef, ref dsUserVendor);
        AppServerDisconnect();
        return dsUserVendor;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public dsUserSalesDataSet SelUserSales(string prmUser, string prmAction, string prmComp, string prmSman, string prmSName, string prmSmanDef)
    {
        dsUserSalesDataSet dsUserSales = new dsUserSalesDataSet();
        dsUserSales = null;
        AppServerConnect();
        aoObject.userSales(prmUser, prmAction, prmComp, prmSman, prmSName, prmSmanDef, ref dsUserSales);
        AppServerDisconnect();
        return dsUserSales;
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validatecontact2(string prmComp, string prmUser, string prmAction, string prmtitle, string prmcustcode, string prmcompany, string prmaddr, string prmcity, string prmstate, string prmterr, string prmsman, string prmcustype, string prmzip)
    {
        string cError = "";

        AppServerConnect();
        aoObject.validatecontact(prmComp, prmUser, prmAction, prmtitle, prmcustcode, prmcompany, prmaddr, prmcity, prmstate, prmterr, prmsman, prmcustype, prmzip, out cError);
        AppServerDisconnect();

        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
            //HttpContext.Current.Response.Write("<script>confirm('Do you want to continue')</script>");
            return false;
        }
        else
        {
            return true;
        }
    }


    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validatecomp2(string prmComp, string prmUser, string prmAction,  string prmstate,  string prmzip)
    {
        string cError = "";

        AppServerConnect();
        aoObject.validatecomp(prmComp, prmUser, prmAction,  prmstate,  prmzip, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
            return false;
        }
        else
        {
            return true;
        }
    }

    [System.ComponentModel.DataObjectMethodAttribute(System.ComponentModel.DataObjectMethodType.Select, true)]
    public bool validatecustomer1(string prmComp, string prmUser, string prmAction, string prmstate, string prmzip, string prmcity, string prmtype, string prmtypedscr, string prmsman, string prmsmandscr, string prmterms, string prmtermsdscr, string prmcurrency, string prmloc, string prmlocdscr, string prmcarrier, string prmcarrierdscr, string prmdelzone, string prmdelzonedscr, string prmterr, string prmterrdscr, string prmtaxcode)
    {
        string cError = "";

        AppServerConnect();
        aoObject.validatecustomer(prmComp, prmUser, prmAction, prmstate, prmzip, prmcity, prmtype, prmtypedscr, prmsman, prmsmandscr, prmterms, prmtermsdscr, prmcurrency, prmloc, prmlocdscr, prmcarrier, prmcarrierdscr, prmdelzone, prmdelzonedscr, prmterr, prmterrdscr, prmtaxcode, out cError);
        AppServerDisconnect();
        if (cError != "")
        {
            HttpContext.Current.Response.Write("<script>alert('" + cError + "')</script>");
            HttpContext.Current.Response.Write("<script>history.back()</script>");
            return false;
        }
        else
        {
            return true;
        }
    }    

}