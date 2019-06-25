using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class topformno_lookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        if (!Page.IsPostBack)
        {
            //ddlSearchOperation.SelectedIndex = 1;
        }
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        //ObjectDataSource1.SelectParameters["prmComp"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        //ObjectDataSource1.SelectParameters["prmEstNo"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        

    }
    
}
