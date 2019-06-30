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

public partial class topspec_list_notes : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
            
        Session["spec_list_notes_index"] = null;
        //UserClass.CheckLogin(Page);
        try
        {
         //   UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            if (Session["User"] != null)
            {
                string vUserId = UserLogin.UserName;
                string vPage = "toporder_list_notes.aspx";
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
            GridView1.SelectedIndex = Convert.ToInt32(Session["top_spec_list_notes_index"]);
            if (Session["top_spec_list_notes_index"] == null)
            {
                GridView1.SelectedIndex = 0;
                Session["spec_list_notes_reckey"] = ((Label)GridView1.SelectedRow.FindControl("rec_key_label")).Text;
                Session["spec_list_note_fg_item"] = GridView1.SelectedRow.Cells[1].Text;
                
            }
           
        }
        catch { }
        if (!Page.IsPostBack)
        {
            try
            {
                if (GridView1.Rows.Count.ToString() == "1")
                {
                    Response.Redirect("spec_list_notes.aspx");
                }
            }
            catch { }
        }
    }
    protected void GridView1_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["top_spec_list_notes_index"] = GridView1.SelectedIndex;
        Session["spec_list_notes_reckey"] = ((Label)GridView1.SelectedRow.FindControl("rec_key_label")).Text;
        Session["spec_list_note_fg_item"] = GridView1.SelectedRow.Cells[1].Text;
            
        
    }
    protected void Button_ok_Click(object sender, EventArgs e)
    {
        Response.Redirect("spec_list_notes.aspx");
    }

    protected void grid_unload(object sender, EventArgs e)
    {
        
        
    }
    
}
