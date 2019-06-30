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

public partial class JobMatLook : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (!Page.IsPostBack)
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "PoSelect";
            //ddlSearchOperation.SelectedIndex = 1;
        }
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;    

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();        
        
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        txtSearchValue.Text = "";
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "PoSelect";        
        
    }
    
}
