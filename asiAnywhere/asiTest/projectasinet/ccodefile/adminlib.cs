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
/// Summary description for ProgramMaster
/// </summary>
/// 
[System.ComponentModel.DataObject]
public class func1 : AppServerConnect.AppServer
{
    public func1()
	{
		//
		// TODO: Add constructor logic here
		//
	}
//   
    public bool CheckProgramPermissions(string vPage, string vUserId,ref bool vCanCreate, ref bool vCanRun, ref bool vCanUpdate, ref bool vCanDelete,  ref string PrmComp, ref string aUsers)
    {
        string vUsers = null;
        string prmComp = null;
        bool CanCreate;
        bool CanRun;
        bool CanUpdate;
        bool CanDelete;

        AppServerConnect();
        aoObject.CheckProgramSecurity(vPage, vUserId, out CanCreate, out CanRun, out CanUpdate, out CanDelete, out prmComp, out vUsers);
        AppServerDisconnect();
        if (CanCreate == true)
        {
            vCanCreate = true;
        }
        if (CanRun == false)
        {
            vCanRun = false;
        }
        if (CanRun == true)
        {
            vCanRun = true;
        }
        if (CanUpdate == false)
        {
            vCanUpdate = false;
        }
        if (CanUpdate == true)
        {
            vCanUpdate = true;
        }
        if (CanCreate == false)
        {
            vCanCreate = false;
        }
        if (CanDelete == true)
        {
            vCanDelete = true;
        }
        if (CanDelete == false)
        {
            vCanDelete = false;
        }
        
        if (vUsers == "internal")
        {
            aUsers = "internal";
        }
        if (vUsers == "external")
        {
            aUsers = "external";
        }
        if (vUsers == null )
        {
            aUsers = "internal";

        }
        if (prmComp != null) 
        {
            PrmComp = prmComp;
        }
        return true;
        

         
    }

    public bool CheckUserCustomer(string prmCompany, string prmUser, ref string DefaultCust)
    {
        string aDefaultCust = null;
        
        AppServerConnect();
        aoObject.CheckUserCust(prmCompany, prmUser, out aDefaultCust);
        AppServerDisconnect();

        DefaultCust = aDefaultCust;
       
        
        return true;



    }
}
