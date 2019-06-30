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

public partial class cust_view_notes : System.Web.UI.Page
{
    protected void Page_PreRender(object sender, EventArgs e)
    {        
        //Session["spec_list_notes_title"] = Session["spec_list_notes_newtitle"];
    }
    protected void Page_Load(object sender, EventArgs e)
    {
        if (Session["User"] == "")
            Response.Write("User logout");
        UserClass UserLogin = (UserClass)Session["User"];
       
        try
        {
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        }
        catch { }
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "cust_view_notes.aspx";
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
        if (!Page.IsPostBack)
        {
            FormView1.ChangeMode(FormViewMode.ReadOnly);
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
        Response.Redirect("cust_list_notes.aspx");
    }
    protected void img_btn_view_notes_click(object sender, ImageClickEventArgs e)
    {
        Response.Redirect("cust_view_notes.aspx");
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
            RadioButtonList type = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
            TextBox group = (TextBox)FormView1.FindControl("vNoteGroupTextBox");
            TextBox dept = (TextBox)FormView1.FindControl("vCustCodeTextBox");
            Image img = (Image)FormView1.FindControl("Image14");
            Image image2 = (Image)FormView1.FindControl("Image1");
            try
            {
                if (type.SelectedIndex == 0)
                {
                    group.Visible = false;
                    dept.Visible = false;
                    img.Visible = false;
                    image2.Visible = false;
                }
                if (type.SelectedIndex == 1)
                {

                    dept.Visible = false;
                    image2.Visible = false;
                    group.Visible = true;
                    img.Visible = true;

                }
                if (type.SelectedIndex == 2)
                {
                    group.Visible = false;
                    img.Visible = false;
                    dept.Visible = true;
                    image2.Visible = true;

                }
            }
            catch { }
            
        }
        if (FormView1.CurrentMode == FormViewMode.Insert)
        {
            UserClass UserLogin = (UserClass)Session["User"];
            Label date = (Label)FormView1.FindControl("vNoteDateLabel");
            Label time = (Label)FormView1.FindControl("vNoteTimeLabel");
            Label user = (Label)FormView1.FindControl("vUserIdLabel");
            RadioButtonList type = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
            TextBox group = (TextBox)FormView1.FindControl("vNoteGroupTextBox");
            TextBox dept = (TextBox)FormView1.FindControl("vCustCodeTextBox");
            Image img = (Image)FormView1.FindControl("Image14");
            Image image2 = (Image)FormView1.FindControl("Image1");
            type.SelectedIndex = 0;
            if (type.SelectedIndex == 0)
            {
                group.Visible = false;
                dept.Visible = false;
                img.Visible = false;
                image2.Visible = false;
            }

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
        
        TextBox group = (TextBox)FormView1.FindControl("vNoteGroupTextBox");
        TextBox dept = (TextBox)FormView1.FindControl("vCustCodeTextBox");          
        TextBox title = (TextBox)FormView1.FindControl("vNoteTitleTextBox");
        TextBox text = (TextBox)FormView1.FindControl("vNoteTextTextBox");
        RadioButtonList type = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        //Session["order_list_notes_title"] = title.Text.Trim();
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = Convert.ToString(Session["ar_inv_cust_rec_key"]);
        ObjectDataSource1.SelectParameters["prmGroup"].DefaultValue = group.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCode"].DefaultValue = dept.Text.Trim();       
        
        ObjectDataSource1.SelectParameters["prmNoteTitle"].DefaultValue = Convert.ToString(Session["spec_cust_list_notes_title"]);
        ObjectDataSource1.SelectParameters["prmNewNoteTitle"].DefaultValue = title.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteText"].DefaultValue = text.Text.Trim();
        ObjectDataSource1.SelectParameters["prmHeader"].DefaultValue = type.SelectedValue;
        
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        

    }
    protected void InsertButton_Click(object sender, EventArgs e)
    {
        Label date = (Label)FormView1.FindControl("vNoteDateLabel");
        Label time = (Label)FormView1.FindControl("vNoteTimeLabel");
        Label user = (Label)FormView1.FindControl("vUserIdLabel");
        TextBox group = (TextBox)FormView1.FindControl("vNoteGroupTextBox");
        TextBox dept = (TextBox)FormView1.FindControl("vCustCodeTextBox");        
        TextBox title = (TextBox)FormView1.FindControl("vNoteTitleTextBox");
        TextBox text = (TextBox)FormView1.FindControl("vNoteTextTextBox");
        RadioButtonList type = (RadioButtonList)FormView1.FindControl("RadioButtonList1");

        Session["spec_cust_list_notes_time"] = time.Text.Trim();
        Session["spec_cust_list_notes_date"] = date.Text.Trim();
        Session["spec_cust_list_notes_title"] = title.Text.Trim();

        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Add";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = Convert.ToString(Session["ar_inv_cust_rec_key"]);
        
        ObjectDataSource1.SelectParameters["prmNoteDate"].DefaultValue = date.Text;
        ObjectDataSource1.SelectParameters["prmNoteTime"].DefaultValue = time.Text;
        ObjectDataSource1.SelectParameters["prmUserId"].DefaultValue = user.Text;
        ObjectDataSource1.SelectParameters["prmGroup"].DefaultValue = group.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCode"].DefaultValue = dept.Text.Trim();        
        ObjectDataSource1.SelectParameters["prmNoteTitle"].DefaultValue = title.Text.Trim();
        ObjectDataSource1.SelectParameters["prmNoteText"].DefaultValue = text.Text.Trim();
        ObjectDataSource1.SelectParameters["prmHeader"].DefaultValue = type.SelectedValue;
       
        FormView1.ChangeMode(FormViewMode.ReadOnly);
        
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
        Session["spec_cust_list_notes_time"] = totalval;
        Session["spec_cust_list_notes_date"] = date.Text.Trim();
        Session["spec_cust_list_notes_title"] = title.Text.Trim();
        UserClass UserLogin = (UserClass)Session["User"];

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmRecKey"].DefaultValue = Convert.ToString(Session["ar_inv_cust_rec_key"]);
        ObjectDataSource1.SelectParameters["prmNoteDate"].DefaultValue = Convert.ToString(Session["spec_cust_list_notes_date"]);
        ObjectDataSource1.SelectParameters["prmNoteTime"].DefaultValue = Convert.ToString(Session["spec_cust_list_notes_time"]);
        ObjectDataSource1.SelectParameters["prmNoteTitle"].DefaultValue = Convert.ToString(Session["spec_cust_list_notes_title"]);
    }
    protected void FormView1_Unload(object sender, EventArgs e)
    {
        try
        {
            Label title = (Label)FormView1.FindControl("vNoteTitleLabel");
            Session["spec_cust_list_notes_title"] = title.Text.Trim();
        }
        catch { }
    }
    protected void rediobutton_selected_change(object sender, EventArgs e)
    {
        RadioButtonList type = (RadioButtonList)FormView1.FindControl("RadioButtonList1");
        TextBox group = (TextBox)FormView1.FindControl("vNoteGroupTextBox");
        TextBox dept = (TextBox)FormView1.FindControl("vCustCodeTextBox");
        Image img = (Image)FormView1.FindControl("Image14");
        Image image2 = (Image)FormView1.FindControl("Image1");
        try
        {
            if (type.SelectedIndex == 0)
            {
                group.Text = "";
                dept.Text = "";
                group.Visible = false;
                dept.Visible = false;
                img.Visible = false;
                image2.Visible = false;
            }
            if (type.SelectedIndex == 1)
            {
                dept.Text = "";
                dept.Visible = false;
                image2.Visible = false;
                img.Visible = true;
                group.Visible = true;

            }
            if (type.SelectedIndex == 2)
            {
                group.Text = "";
                group.Visible = false;
                img.Visible = false;
                dept.Visible = true;
                image2.Visible = true;

            }
        }
        catch { }

    }
    
}
