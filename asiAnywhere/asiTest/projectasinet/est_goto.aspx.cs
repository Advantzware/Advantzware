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

public partial class est_goto2 : System.Web.UI.Page
{
    protected void Page_PreRender(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        
        ObjectDataSource_list.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource_list.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        ObjectDataSource_list.SelectParameters["prmAction"].DefaultValue = "Select";
        ObjectDataSource_list.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        GridView1.DataBind();

        if (Session["grid_corr_go_index"] != null)
        {
            GridView1.SelectedIndex = Convert.ToInt32(Session["grid_corr_go_index"]);
        }
        if (Session["grid_corr_go_index"] == null)
        {
            try
            {
                GridView1.SelectedIndex = 0;
                Session["order_corr_go_formno"] = GridView1.SelectedRow.Cells[1].Text;
                Session["order_corr_go_blankno"] = GridView1.SelectedRow.Cells[2].Text;
                Session["order_corr_go_type"] = ((Label)GridView1.SelectedRow.FindControl("type_label")).Text;
                
            }
            catch { }
        }
    }
    

    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["grid_corr_go_index"] = GridView1.SelectedIndex;
        Session["order_corr_go_formno"] = GridView1.SelectedRow.Cells[1].Text;
        Session["order_corr_go_blankno"] = GridView1.SelectedRow.Cells[2].Text;
        Session["order_corr_go_type"] = ((Label)GridView1.SelectedRow.FindControl("type_label")).Text;
      
    }

    protected void update_button_Clock(object sender, EventArgs e)
    {
        TextBox form = (TextBox)FormView1.FindControl("formnoTextBox");
        TextBox blank = (TextBox)FormView1.FindControl("blanknoTextBox");
        TextBox part = (TextBox)FormView1.FindControl("partnoTextBox");
        TextBox bl = (TextBox)FormView1.FindControl("blqtyTextBox");
        TextBox yld = (TextBox)FormView1.FindControl("yldqtyTextBox");
        RadioButtonList price = (RadioButtonList)FormView1.FindControl("Price_radiobuttonlist");
        TextBox wid = (TextBox)FormView1.FindControl("widTextBox");
        TextBox len = (TextBox)FormView1.FindControl("lenTextBox");
        TextBox up = (TextBox)FormView1.FindControl("numupTextBox");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource_view.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource_view.SelectParameters["prmAction"].DefaultValue = "Update";        
        ObjectDataSource_view.SelectParameters["prmFormNo"].DefaultValue = form.Text.Trim();
        ObjectDataSource_view.SelectParameters["prmBlankNo"].DefaultValue = blank.Text.Trim();
        ObjectDataSource_view.SelectParameters["prmPartno"].DefaultValue = part.Text.Trim();
        ObjectDataSource_view.SelectParameters["prmBlQty"].DefaultValue = bl.Text.Trim();
        ObjectDataSource_view.SelectParameters["prmYldQty"].DefaultValue = yld.Text.Trim();
        ObjectDataSource_view.SelectParameters["prmPrice"].DefaultValue = price.SelectedValue;
        ObjectDataSource_view.SelectParameters["prmWid"].DefaultValue = wid.Text.Trim();
        ObjectDataSource_view.SelectParameters["prmLen"].DefaultValue = len.Text.Trim();
        ObjectDataSource_view.SelectParameters["prmUp"].DefaultValue = up.Text.Trim();
        GridView1.DataBind();
        
        Response.Write("<script>window.location.href='est_goto.aspx?ID={0}','','600,400' @'estgo';</script>");        

    }

    protected void formview_OnDataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            TextBox form = (TextBox)FormView1.FindControl("formnoTextBox");
            TextBox blank = (TextBox)FormView1.FindControl("blanknoTextBox");
            TextBox part = (TextBox)FormView1.FindControl("partnoTextBox");
            TextBox bl = (TextBox)FormView1.FindControl("blqtyTextBox");
            TextBox yld = (TextBox)FormView1.FindControl("yldqtyTextBox");            
            TextBox wid = (TextBox)FormView1.FindControl("widTextBox");
            TextBox len = (TextBox)FormView1.FindControl("lenTextBox");
            TextBox up = (TextBox)FormView1.FindControl("numupTextBox");
           
            if (Convert.ToInt32(Session["order_corr_go_type"]) == 6 || Convert.ToInt32(Session["order_corr_go_type"]) == 2)
            {

                bl.Enabled = false;
                yld.Enabled = false;
                wid.Enabled = false;
                len.Enabled = false;
               
            }

        }
    }
    
    
}
