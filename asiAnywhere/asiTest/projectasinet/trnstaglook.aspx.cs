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

public partial class trnstag : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {        
       if (!Page.IsPostBack)
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";    
            //ddlSearchOperation.SelectedIndex = 1;
        }

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "PoSearch";
        ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
        ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();        
        
    }
    protected void btnShowAll_Click(object sender, EventArgs e)
    {
        txtSearchValue.Text = "";
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";        
        
    }

    protected void GridView1_RowCreated(object sender, GridViewRowEventArgs e)
    {
        try
        {
        
            if (Request.QueryString["conslook"] != "cons")
            {               
                e.Row.Cells[1].Visible = false;
                e.Row.Cells[13].Visible = false;
                
            }
        }
        catch { }
    }
    
}
