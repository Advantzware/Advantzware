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

public partial class QFgItemLookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        //Response.Write(Session["rfqcust"]);
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        if (!Page.IsPostBack)
        {
            ddlSearchOperation.SelectedIndex = 1;
        }

        string frmval = Request.QueryString["from"];
        if (frmval == "custpart")
        {   
            ddlSearchField.Items[0].Enabled = false;
            ddlSearchField.Items[1].Enabled = false;
        }
    }
    

    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = Convert.ToString(Session["rfqcust"]);
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {

    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        Response.Redirect("QFgItemLook.aspx");
    }

    protected void searchindexchanged(object sender, EventArgs e)
    {
        if (ddlSearchField.SelectedValue == "est-no")
        {
            ddlSearchOperation.Items[1].Enabled = false;
        }
        else
        {
            ddlSearchOperation.Items[1].Enabled = true;
        }
    }
   
}
