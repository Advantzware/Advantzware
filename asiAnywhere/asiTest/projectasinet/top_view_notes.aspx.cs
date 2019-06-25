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

public partial class top_view_notes : System.Web.UI.Page
{
    protected void Page_PreRender(object sender, EventArgs e)
    {
        Session["top_list_notes_title"] = Session["top_list_notes_newtitle"];
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        UserClass UserLogin = (UserClass)Session["User"];
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        try
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        }
        catch { }
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "toporder_view_notes.aspx";
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

        try
        {
           
            Label view = (Label)FormView1.FindControl("vNoteViewedLabel");
            CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox1");
            if (view.Text == "Yes" )
            {
                chk.Checked = true;
            }
            else
            {
                chk.Checked = false;
            }
        }
        catch
        {
        }
    }
    protected void btn_add_new_click(object sender, EventArgs e)
    {
        FormView1.ChangeMode(FormViewMode.Insert);
        btn_add_new.Visible = false;
    }
    protected void img_btn_list_notes_click(object sender, EventArgs e)
    {
        Response.Redirect("top_list_notes.aspx");
    }
    protected void img_btn_view_notes_click(object sender, EventArgs e)
    {
        Response.Redirect("top_view_notes.aspx");
    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            
            if (FormView1.DataItemCount == 0)
            {
                btn_add_new.Visible = true;
            }
            if (FormView1.DataItemCount != 0)
            {
                btn_add_new.Visible = false;
            }
            
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            Label date = (Label)FormView1.FindControl("vNoteDateLabel");
            Label time = (Label)FormView1.FindControl("vNoteTimeLabel");
            Label user = (Label)FormView1.FindControl("vUserIdLabel");

            DateTime tval = DateTime.Parse(DateTime.Now.ToLongTimeString());

            Int32 hrs = Convert.ToInt32(tval.Hour) * 3600;
            Int32 min = Convert.ToInt32(tval.Minute) * 60;
            Int32 sec = tval.Second;
            Int32 totalval = hrs + min + sec;
            
            //Int32 diff=
            date.Text = DateTime.Now.ToShortDateString();
            user.Text = UserLogin.UserName;
            time.Text = Convert.ToString(totalval);
            //string
        }
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        
        TextBox dept = (TextBox)FormView1.FindControl("vDeptCodeTextBox");
        TextBox form = (TextBox)FormView1.FindControl("vNoteFrmNoTextBox");
        TextBox title = (TextBox)FormView1.FindControl("vNoteTitleTextBox");
        TextBox text = (TextBox)FormView1.FindControl("vNoteTextTextBox");
        
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = Convert.ToString(Session["top_list_notes_rec_key"]);
        ObjectDataSource1.SelectParameters["prmDeptCode"].DefaultValue = dept.Text.Trim();        
        ObjectDataSource1.SelectParameters["prmForm"].DefaultValue = form.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteTitle"].DefaultValue = Convert.ToString(Session["top_list_notes_title"]);
        ObjectDataSource1.SelectParameters["prmNewNoteTitle"].DefaultValue = title.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteText"].DefaultValue = text.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["top_entry_est_no"]);

    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        Label date = (Label)FormView1.FindControl("vNoteDateLabel");
        Label time = (Label)FormView1.FindControl("vNoteTimeLabel");
        Label user = (Label)FormView1.FindControl("vUserIdLabel");
        TextBox dept = (TextBox)FormView1.FindControl("vDeptCodeTextBox");
        TextBox form = (TextBox)FormView1.FindControl("vNoteFrmNoTextBox");
        TextBox title = (TextBox)FormView1.FindControl("vNoteTitleTextBox");
        TextBox text = (TextBox)FormView1.FindControl("vNoteTextTextBox");

        Session["top_list_notes_time"] = time.Text.Trim();
        Session["top_list_notes_date"] = date.Text.Trim();
        Session["top_list_notes_title"] = title.Text.Trim();

        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = Convert.ToString(Session["top_list_notes_rec_key"]);
        ObjectDataSource1.SelectParameters["prmNoteDate"].DefaultValue = date.Text;
        ObjectDataSource1.SelectParameters["prmNoteTime"].DefaultValue = time.Text;
        ObjectDataSource1.SelectParameters["prmUserId"].DefaultValue = user.Text;
        ObjectDataSource1.SelectParameters["prmDeptCode"].DefaultValue = dept.Text.Trim();
        ObjectDataSource1.SelectParameters["prmForm"].DefaultValue = form.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteTitle"].DefaultValue = title.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNewNoteTitle"].DefaultValue = title.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteText"].DefaultValue = text.Text.Trim();
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_entry_est_no"]);
    }
    protected void btn_delete_click(object sender, EventArgs e)
    {
        Label date = (Label)FormView1.FindControl("vNoteDateLabel");
        Label time = (Label)FormView1.FindControl("vNoteTimeLabel");
        Label title = (Label)FormView1.FindControl("vNoteTitleLabel");


        DateTime tval = DateTime.Parse(Convert.ToDateTime(time.Text.Trim()).ToLongTimeString());

        Int32 hrs = Convert.ToInt32(tval.Hour) * 3600;
        Int32 min = Convert.ToInt32(tval.Minute) * 60;
        Int32 sec = tval.Second;
        Int32 totalval = hrs + min + sec;
        //Response.Write(totalval);
        Session["top_list_notes_time"] = totalval;
        Session["top_list_notes_date"] = date.Text.Trim();
        Session["top_list_notes_title"] = title.Text.Trim();
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = Convert.ToString(Session["top_list_notes_rec_key"]);
        ObjectDataSource1.SelectParameters["prmNoteDate"].DefaultValue = Convert.ToString(Session["top_list_notes_date"]);
        ObjectDataSource1.SelectParameters["prmNoteTime"].DefaultValue = Convert.ToString(Session["top_list_notes_time"]);
        ObjectDataSource1.SelectParameters["prmNoteTitle"].DefaultValue = Convert.ToString(Session["top_list_notes_title"]);
    }
    protected void FormView1_Unload(object sender, EventArgs e)
    {
        try
        {
            Label title = (Label)FormView1.FindControl("vNoteTitleLabel");
            Session["top_list_notes_newtitle"] = title.Text.Trim();
        }
        catch { }
    }
    
}
