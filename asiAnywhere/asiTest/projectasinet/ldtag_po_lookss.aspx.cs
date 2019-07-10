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

namespace projectasinet
{
    public partial class ldtag_po_lookss : System.Web.UI.Page
    {
        protected void Page_Load(object sender, EventArgs e)
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "sea";
            if (!Page.IsPostBack)
            {
                //gridviewdiv.Visible = false;
                //searchdiv.Visible = false;                        
                ddlSearchOperation.SelectedIndex = 0;

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
            txtSearchValue.Text = "";
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "select";
            ObjectDataSource1.SelectParameters["prmField"].DefaultValue = ddlSearchField.SelectedValue.Trim();
            ObjectDataSource1.SelectParameters["prmCondition"].DefaultValue = ddlSearchOperation.SelectedValue.Trim();
            ObjectDataSource1.SelectParameters["prmText"].DefaultValue = txtSearchValue.Text.Trim();

        }


    }
}