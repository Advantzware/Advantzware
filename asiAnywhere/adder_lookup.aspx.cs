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

public partial class adder_lookup : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {        
            
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_corrugated_est"]);
        if (!Page.IsPostBack)
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "jhg";    
            //ddlSearchOperation.SelectedIndex = 1;
        }

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "search";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();        
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_corrugated_est"]);
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        txtSearchValue.Text = "";
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "jhg";        
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_corrugated_est"]);
    }
    
}
