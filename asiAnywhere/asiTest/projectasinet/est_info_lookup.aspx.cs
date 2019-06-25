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

public partial class est_info_lookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["PrmUser"].DefaultValue = UserLogin.UserName;

        if (!Page.IsPostBack)
        {
            ddlSearchOperation.SelectedIndex = 1;
            Session["genestno"] = null;
            if (ddlSearchField.SelectedValue == "Estimate")
            {
                ddlSearchOperation.Items[0].Enabled = false;
            }
            else
            {
                ddlSearchOperation.Items[0].Enabled = true;
            }
        }

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
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "sea";
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
               
    }
    protected void searchindexchanged(object sender, EventArgs e)
    {
        if (ddlSearchField.SelectedValue == "Estimate")
        {
            ddlSearchOperation.Items[0].Enabled = false;
        }
        else
        {
            ddlSearchOperation.Items[0].Enabled = true;
        }
    }
}
