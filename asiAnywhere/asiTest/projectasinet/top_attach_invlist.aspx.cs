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

public partial class top_attach_invlist : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
       
        try
        {
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "top_attach_invlist.aspx";
                string aUsers = null;
                string PrmComp = null;
                bool vCanCreate = false;
                bool vCanRun = false;
                bool vCanUpdate = false;
                bool vCanDelete = false;

                func1 f1 = new func1();
                f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                lblUser.Text = UserLogin.UserName;
                lblComp.Text = PrmComp;
                if (aUsers == "external")
                {
                }
                if (vCanRun == false)
                {
                    Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                    Response.Write("<script>window.location.href = 'login.aspx';</script>");
                }
            }
        }
        catch { }
        try
        {
            if (Session["invtop_list_attach_index"] != null)
            {
                try
                {
                    GridView1.SelectedIndex = Convert.ToInt32(Session["invtop_list_attach_index"]);
                    Session["invtop_list_attach_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("rec_key_label")).Text;
                }
                catch
                {
                    GridView1.SelectedIndex = 0;
                }
            }
            if (Session["invtop_list_attach_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["invtop_list_attach_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("rec_key_label")).Text;

             }
             
        }
        catch { }
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["invtop_list_attach_index"] = GridView1.SelectedIndex;
        Session["invtop_list_attach_rec_key"] = ((Label)GridView1.SelectedRow.FindControl("rec_key_label")).Text;
        
    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Search";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmPoNo"].DefaultValue = txt_att_inv.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = txt_att_date.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFgitem"].DefaultValue = type_TextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmAttFile"].DefaultValue = att_TextBox.Text.Trim();
    }
    protected void btn_reset_Click(object sender, EventArgs e)
    {
        txt_att_date.Text = "";
        txt_att_inv.Text = "";
        type_TextBox.Text = "";
        att_TextBox.Text = "";
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
    }
    protected void img_btn_list_attach_click(object sender, EventArgs e)
    {
        Response.Redirect("top_attach_invlist.aspx");
    }
    protected void img_btn_view_attach_click(object sender, EventArgs e)
    {
        Response.Redirect("top_attach_invview.aspx");
    }
    
}
