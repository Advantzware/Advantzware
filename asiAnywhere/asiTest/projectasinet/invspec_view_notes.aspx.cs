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

public partial class invspec_view_notes : System.Web.UI.Page
{
    protected void Page_PreRender(object sender, EventArgs e)
    {        
        
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
            string vPage = "invspec_view_notes.aspx";
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
    protected void img_btn_list_notes_click(object sender, ImageClickEventArgs e)
    {
        Response.Redirect("invspec_list_notes.aspx");
    }
    protected void img_btn_view_notes_click(object sender, ImageClickEventArgs e)
    {
        Response.Redirect("invspec_view_notes.aspx");
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
            //Label view = (Label)FormView1.FindControl("vNoteViewedLabel");
            //CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox1");
            //if (view.Text == "Yes")
            //{
            //    chk.Checked = true;
            //}
            //else
            //{
            //    chk.Checked = false;
            //}
        }
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            //Label view = (Label)FormView1.FindControl("vNoteViewedLabel");
            //CheckBox chk = (CheckBox)FormView1.FindControl("CheckBox1");
            //if (view.Text == "Yes")
            //{
            //    chk.Checked = true;
            //}
            //else
            //{
            //    chk.Checked = false;
            //}
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            Label date = (Label)FormView1.FindControl("vNoteDateLabel");
            Label time = (Label)FormView1.FindControl("vNoteTimeLabel");
            Label user = (Label)FormView1.FindControl("vUserIdLabel");
            RadioButtonList notety = (RadioButtonList)FormView1.FindControl("NoteRadioButtonList1");
            DateTime tval = DateTime.Parse(DateTime.Now.ToLongTimeString());

            Int32 hrs = Convert.ToInt32(tval.Hour) * 3600;
            Int32 min = Convert.ToInt32(tval.Minute) * 60;
            Int32 sec = tval.Second;
            Int32 totalval = hrs + min + sec;
            
            //Int32 diff=
            date.Text = DateTime.Now.ToShortDateString();
            user.Text = UserLogin.UserName;
            time.Text = Convert.ToString(totalval);
            notety.SelectedIndex = 0;
            //string
        }
    }
    protected void UpdateButton_Click(object sender, EventArgs e)
    {
        //Label date = (Label)FormView1.FindControl("");
        //Label time = (Label)FormView1.FindControl("");
        //Label user = (Label)FormView1.FindControl("");
        //Label date = (Label)FormView1.FindControl("");
        TextBox dept = (TextBox)FormView1.FindControl("vDeptCodeTextBox");        
        TextBox title = (TextBox)FormView1.FindControl("vNoteTitleTextBox");
        TextBox text = (TextBox)FormView1.FindControl("vNoteTextTextBox");
        TextBox grp = (TextBox)FormView1.FindControl("grp_TextBox");
        RadioButtonList notety = (RadioButtonList)FormView1.FindControl("NoteRadioButtonList1");
        //Session["order_list_notes_title"] = title.Text.Trim();
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = Convert.ToString(Session["invspec_view_notes_reckey_rec"]);        
        ObjectDataSource1.SelectParameters["prmDeptCode"].DefaultValue = dept.Text.Trim();

        ObjectDataSource1.SelectParameters["prmNoteTitle"].DefaultValue = title.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNewNoteTitle"].DefaultValue = title.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteText"].DefaultValue = text.Text.Trim();
        ObjectDataSource1.SelectParameters["prmGroup"].DefaultValue = grp.Text.Trim();
        ObjectDataSource1.SelectParameters["prmType"].DefaultValue = notety.SelectedValue;

    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        Label date = (Label)FormView1.FindControl("vNoteDateLabel");
        Label time = (Label)FormView1.FindControl("vNoteTimeLabel");
        Label user = (Label)FormView1.FindControl("vUserIdLabel");
        //Label date = (Label)FormView1.FindControl("");
        TextBox dept = (TextBox)FormView1.FindControl("vDeptCodeTextBox");        
        TextBox title = (TextBox)FormView1.FindControl("vNoteTitleTextBox");
        TextBox text = (TextBox)FormView1.FindControl("vNoteTextTextBox");
        TextBox grp = (TextBox)FormView1.FindControl("grp_TextBox");
        RadioButtonList notety = (RadioButtonList)FormView1.FindControl("NoteRadioButtonList1");
               

        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;               
        ObjectDataSource1.SelectParameters["prmNoteDate"].DefaultValue = date.Text;
        ObjectDataSource1.SelectParameters["prmNoteTime"].DefaultValue = time.Text;
        ObjectDataSource1.SelectParameters["prmUserId"].DefaultValue = user.Text;        
        ObjectDataSource1.SelectParameters["prmDeptCode"].DefaultValue = dept.Text.Trim();        
        ObjectDataSource1.SelectParameters["prmNoteTitle"].DefaultValue = title.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteText"].DefaultValue = text.Text.Trim();
        ObjectDataSource1.SelectParameters["prmGroup"].DefaultValue = grp.Text.Trim();
        ObjectDataSource1.SelectParameters["prmType"].DefaultValue = notety.SelectedValue;
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
        
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = Convert.ToString(Session["invspec_view_notes_reckey_rec"]);
        
    }
    protected void FormView1_Unload(object sender, EventArgs e)
    {
        try
        {
            Label reckey = (Label)FormView1.FindControl("reckeyLabel");
            Session["invspec_view_notes_reckey_rec"] = reckey.Text.Trim();
        }
        catch { }
    }
    
}
